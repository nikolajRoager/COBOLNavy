using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using System.Text.Json;
using DockyardApi.Models;
using System.Diagnostics;

namespace DockyardApi.Services.ZosmfRESTapi
{
    ///<summary>
    /// An interface for the class which interacts with Z/OS Mainframe by sending http requests, all interactions with the REST API lives here, and are not exposed outside
    ///</summary>
    public interface IZosmfRESTapi
    {
        public Task<IEnumerable<JobDocument>> getJobs ();
        public Task<IEnumerable<Warship>>     getShips();
    }

    ///<summary>
    /// Class which interacts with Z/OS Mainframe by sending http requests, all interactions with the REST API lives here, and are not exposed outside
    ///</summary>
    public class ZosmfRESTapi : IZosmfRESTapi
    {
        private HttpClientHandler handler;
        private HttpClient client;
        private readonly string zosUsername;
        private readonly string zosmfUrl;
        private readonly string getShipsJCL;
        
        public ZosmfRESTapi(string host, string port, string zosUsername, string zosPassword)
        {
            this.zosUsername=zosUsername;
        
            handler = new HttpClientHandler();
            //only for testing, disable cerfification
            handler.ServerCertificateCustomValidationCallback = (message, cert, chain, errors) => true;

            zosmfUrl = "restjobs/jobs";
            client = new HttpClient(handler);
            
            client.BaseAddress =new Uri($"https://{host}:{port}/zosmf/");

            Console.WriteLine($"using https://{host}:{port}/zosmf/");

            //Use the username and password
            string authInfo = Convert.ToBase64String(Encoding.ASCII.GetBytes($"{zosUsername}:{zosPassword}"));
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", authInfo);
            
            getShipsJCL=$"{zosUsername}.JCL(GETSHPS)";
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

        public async Task<IEnumerable<Warship>> getShips()
        {
            //Post the job using the rest api, this is the better version but is not permitted by Z-Explore
            //string response = await SubmitJob(getShipsJCL);
            //Post the job using a ZOWE hack (if the mainframe doesn't support http post)
            (string jobUrl,string jobFilesUrl) = SubmitZoweJob(getShipsJCL);
            string status="ACTIVE";
            //ONLY poll up to 15 times, spaced out over 1 minute
            for (int i = 0; i < 15; ++i)
            {
                //Wait 2 seconds
                await Task.Delay(4000);
                //Check status
                status =await getJobStatus(jobUrl);

                //Some documentation insist that this is also a valid exit code
                if (status=="ABENDED")
                {
                    //Should really not happen in normal usage
                    throw new Exception("Job exited with mainframe-side COBOL error (status: ABEND)");
                }
                else if (status!="ACTIVE" && status!="IDLE" && status!="WAITING")
                {
                    break;
                }
            }

            if (status!="OUTPUT")
            {
                throw new Exception("Job stopped or timed out, with status other than output, status: "+status);
            }
            //The job is done, now get the result
            string shiplist = await getSysout(jobFilesUrl);
            
            Console.WriteLine(shiplist);

            var jobList = JsonSerializer.Deserialize<List<Warship>>(shiplist, new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            });


            return jobList;
        }

        ///<summary>
        ///This does NOT work on Z-Explore mainframe, because users do not have POST permission
        ///I keep it around to demonstrate how it should be done
        ///</summary>
        private async Task<(string,string)> SubmitJob(string jclJob)
        {
            //reference to the job
            var jobContent = new StringContent($"{{\"file\":\"{jclJob}\"}}", Encoding.UTF8, "application/json");
            HttpResponseMessage response;
            try
            {
                //Send job and await confirmation that the job has been added to the queue
                response = await client.PostAsync(zosmfUrl, jobContent);
            }
            catch (HttpRequestException e)
            {
                throw new ArgumentException("Http error when submitting Z/OS job: "+e.Message);
            }

            if (response.IsSuccessStatusCode)
            {
                Console.WriteLine("success");
                string responseBody = await response.Content.ReadAsStringAsync();
                using JsonDocument JsonOutput= JsonDocument.Parse(responseBody);

                var jobUrl =JsonOutput.RootElement.GetProperty("data").GetProperty("url").GetString();
                var jobFilesUrl =JsonOutput.RootElement.GetProperty("data").GetProperty("files-url").GetString();
                if (jobUrl==null || jobFilesUrl==null)
                    throw new Exception($"Job was launched but zowe did not return an ID");
                else
                {
                    return (jobUrl,jobFilesUrl);
                }

            }
            else throw new ArgumentException("failed to submit Z/OS job: "+await response.Content.ReadAsStringAsync());
        }

        /// <summary>
        /// A bodge solution to not being allowed to post jobs with http: simply launch zowe cli in the commandline
        /// The function either returns the ID of the job when submitted, or throws an exception if it did not work
        /// Returns direct link to job, anddirect link to files
        /// </summary>
        private (string,string) SubmitZoweJob(string jclJob)
        {
            ProcessStartInfo processStartInfo = new ProcessStartInfo
            {
                    FileName =  @"C:\Users\KOM\AppData\Roaming\npm\zowe.cmd",
                    Arguments= $"zos-jobs submit ds \"{jclJob}\" --rfj",
                    RedirectStandardOutput=true,
                    RedirectStandardError=true,
                    UseShellExecute = false,
                    CreateNoWindow = true,
            };

            //Launch the process
            using (Process process = new Process())
            {
                process.StartInfo=processStartInfo;
                
                //Launch the process
                process.Start();

                string output = process.StandardOutput.ReadToEnd();
                string error = process.StandardError.ReadToEnd();

                //Async is not available
                process.WaitForExit();

                //This will only happen is the launching of the command fails
                if (!string.IsNullOrEmpty(error))
                {
                    throw new Exception($"Error launching Z/OS job with zowe CLI : {error}");
                }


                var JsonOutput = System.Text.Json.JsonDocument.Parse(output);
                var success = JsonOutput.RootElement.GetProperty("success").GetBoolean();
                if (success)
                {
                    var jobUrl =JsonOutput.RootElement.GetProperty("data").GetProperty("url").GetString();
                    var jobFilesUrl =JsonOutput.RootElement.GetProperty("data").GetProperty("files-url").GetString();
                    if (jobUrl==null || jobFilesUrl==null)
                        throw new Exception($"Job was launched but zowe did not return an ID");
                    else
                    {
                        return (jobUrl,jobFilesUrl);
                    }
                }
                else
                {
                    throw new Exception($"Job could not be submitted, zowe cli returned error {JsonOutput.RootElement.GetProperty("message").GetString()}");
                }
            }
        }

        /// <summary>
        /// Poll the status of a job with a particular url
        /// </summary>
        /// <param name="jobUrl"></param>
        /// <returns>one of ACTIVE, OUTPUT, COMPLETE, ABENDED, CANCELLED, HOLD, SUSPENDED, NOTRUN, WAITING</returns>
        private async Task<string> getJobStatus(string jobUrl)
        {
            Console.WriteLine($"{jobUrl}");
            var response = await client.GetAsync($"{jobUrl}");
            response.EnsureSuccessStatusCode();
            var content = await response.Content.ReadAsStringAsync();

            
            var JsonOutput = System.Text.Json.JsonDocument.Parse(content);

            string? status = JsonOutput.RootElement.GetProperty("status").GetString();

            //This is bad
            if (status==null)
                return "ABENDED";
            return status;
        }

        /// <summary>
        /// Get sysout from a particular job
        /// </summary>
        /// <param name="jobFilesUrl"></param>
        /// <returns>string containing the output data</returns>
        private async Task<string> getSysout(string jobFilesUrl)
        {
            Console.WriteLine($"get output from {jobFilesUrl}");
            
            //Get all meta-data, including the url, to all files produced by the process
            var response = await client.GetAsync($"{jobFilesUrl}");
            response.EnsureSuccessStatusCode();
            var content = await response.Content.ReadAsStringAsync();

            //Filter the list and look for sysout, (unfortunately there is no way to only query sysout)

            var JsonOutput = System.Text.Json.JsonDocument.Parse(content);
            if (JsonOutput!=null)
                foreach (var Element in JsonOutput.RootElement.EnumerateArray())
                {
                    var ddname     = Element.GetProperty("ddname").GetString();
                    var recordsUrl = Element.GetProperty("records-url").GetString();
                    if (ddname=="SYSOUT" && recordsUrl!=null)
                    {
                        //Ok, now get and return data
                        Console.WriteLine($"Getting sysout recordsUrl {recordsUrl}");
                        
                        var data_records= await client.GetAsync($"{recordsUrl}");
                        data_records.EnsureSuccessStatusCode();
                        var data = await data_records.Content.ReadAsStringAsync();
                        return data;

                    }
                }
            
            throw new Exception("Job did not produce sysout");
        }
    }
}