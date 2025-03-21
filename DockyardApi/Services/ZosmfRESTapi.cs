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
        public Task<IEnumerable<JobDocument>> getJobs();
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
            //WARNING, DANGEROUS, only for debugging
            //Ignore SSL certificate errors (only for debugging! don't use this, seriously don't)
            //Instead install the certificate for the mainframe
            handler.ServerCertificateCustomValidationCallback = (message, cert, chain, errors) => true;

            zosmfUrl = $"https://{host}:{port}/zosmf/restjobs/jobs";
            client = new HttpClient(handler);
            
            //Use the username and password
            string authInfo = Convert.ToBase64String(Encoding.ASCII.GetBytes($"{zosUsername}:{zosPassword}"));
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", authInfo);
        }


        public async Task<IEnumerable<JobDocument>> getJobs()
        {
            //To verify the connection works, ask for the list of jobs
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
    }
}