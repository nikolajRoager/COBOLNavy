using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using System.Text.Json;
using DockyardApi.Models;

namespace DockyardApi.Services.ZosmfRESTapi
{
    ///<summary>
    /// An interface for the class which interacts with Z/OS Mainframe by sending http requests, all interactions with the REST API lives here, and are not exposed outside
    ///</summary>
    public interface IZosmfRESTapi
    {
        public Task<IEnumerable<JobDocument>> getJobs ();
//        public Task<IEnumerable<Warship>>     getShips();
    }

    ///<summary>
    /// Class which interacts with Z/OS Mainframe by sending http requests, all interactions with the REST API lives here, and are not exposed outside
    ///</summary>
    public class ZosmfRESTapi : IZosmfRESTapi
    {
        private HttpClientHandler handler;
        private HttpClient client;
        private readonly String zosUsername;
        private readonly String zosmfUrl;
        public ZosmfRESTapi(string host, string port, string zosUsername, string zosPassword)
        {
            this.zosUsername=zosUsername;
        
            handler = new HttpClientHandler();
            //only for testing, disable cerfification
            handler.ServerCertificateCustomValidationCallback = (message, cert, chain, errors) => true;

            zosmfUrl = $"https://{host}:{port}/zosmf/restjobs/jobs";
            client = new HttpClient(handler);
            
            //Use the username and password
            string authInfo = Convert.ToBase64String(Encoding.ASCII.GetBytes($"{zosUsername}:{zosPassword}"));
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", authInfo);
        }


        public async Task<IEnumerable<JobDocument>> getJobs()
        {
            HttpResponseMessage response = await client.GetAsync(zosmfUrl+"?owner="+zosUsername);
            if (!response.IsSuccessStatusCode)
            {
                //Didn't work, i guess the username, password, host, port, or internet connection is wrong, throw an error and hope the user can fix it
                throw new Exception($"There was an error connecting to the mainframe Error: {response.StatusCode} Reason: {response.ReasonPhrase}");
            }
            else
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                responseBody="{\"Jobs\":"+responseBody+"}";
                //Deserialize JSON
                var jobList = JsonSerializer.Deserialize<JobList>(responseBody, new JsonSerializerOptions
                {
                    PropertyNameCaseInsensitive = true
                });

                return jobList?.Jobs ?? new List<JobDocument>();
            }
        }

/*
        public async Task<IEnumerable<Warship>> getShips()
        {

        }
        */

        ///<summary>
        ///AI generated code to sumbit a job
        ///</summary>
        static async Task<string> SubmitJob(HttpClient client, string jcl)
        {
            var content = new StringContent(jcl, Encoding.UTF8, "text/plain");
            HttpResponseMessage response = await client.PostAsync("restjobs/jobs", content);

            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                using JsonDocument doc = JsonDocument.Parse(responseBody);
                string jobId = doc.RootElement.GetProperty("jobid").GetString();
                return jobId;
            }
            return null;
        }
    }
}