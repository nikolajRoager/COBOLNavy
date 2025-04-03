using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using System.Text.Json;
using DockyardApi.Models;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text.Json.Serialization;

namespace DockyardApi.Services.ZosmfRESTapi
{
    ///<summary>
    /// An interface for the class which interacts with Z/OS Mainframe by sending http requests, all interactions with the REST API lives here, and are not exposed outside
    ///</summary>
    public interface IZosmfRESTapi
    {
        public Task<IEnumerable<JobDocument>> getJobs ();
        public Task<IEnumerable<Warship>>     getShips();
        public Task<Warship>                  getShip(string id);
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

        private readonly string SearchShipRawJCL;

        private readonly string AddShipRawJCL;
        
        /// <summary>
        /// Path to executable zowe commandline Executable
        /// </summary>
        private readonly string zoweCliExe;

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
            
            zoweCliExe="";//If will be set below, but the compiler doesn't know that and keeps giving me warnings
            //First check if Zowe is in PATH
            ProcessStartInfo which = new ProcessStartInfo
            {
                FileName = OperatingSystem.IsWindows() ? "where" : "which",
                Arguments = "zowe",
                RedirectStandardOutput = true,
                UseShellExecute = false,
                CreateNoWindow = true
            };
            Process process = new Process {StartInfo = which};
            process.Start();
            string? path = process.StandardOutput.ReadLine();
            process.WaitForExit();

            bool zoweNotFound;
            if (!string.IsNullOrEmpty(path))
            {
                //Found it, just call it and see if it works
                zoweCliExe=path;
                zoweNotFound=!zoweCliWorks();
            }
            else
                zoweNotFound=true;
            if (zoweNotFound)
            {
                if (System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
                {
                    //This is where it would go if it was installed through npm
                    zoweCliExe=  $"C:\\Users\\{Environment.UserName}\\AppData\\Roaming\\npm\\zowe.cmd";
                }
                else//Linux or similar
                {
                    //get npm bin
                    ProcessStartInfo npm = new ProcessStartInfo
                    {
                        FileName = "npm",
                        Arguments = "-g",
                        RedirectStandardOutput = true,
                        UseShellExecute = false,
                        CreateNoWindow = true
                    };
                    Process npm_process = new Process {StartInfo = npm};
                    npm_process.Start();
                    string? bin_path = process.StandardOutput.ReadLine();
                    process.WaitForExit();

                    //oh well just try this
                    if (string.IsNullOrEmpty(bin_path))
                        bin_path = "/usr";

                    zoweCliExe=Path.Combine(Path.Combine(bin_path.Trim(), "bin"),"zowe");
                }
            }

                //We will insert the name on the place of replace, to get a jcl we can submit
                SearchShipRawJCL=
                    "//FINDSHP JOB (ACCT),'FINDSHP',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID\n"+
                    "//RUN EXEC PGM=FINDSHP,PARM='RNBB0029'                          \n"+
                    "//*Link libraries                                               \n"+
                    "//STEPLIB DD DSN=&SYSUID..LOAD,DISP=SHR                         \n"+
                    "//*User supplied data: allied ships                             \n"+
                    "//VSAMFILE     DD DSN=&SYSUID..VSAM,DISP=SHR                    \n"+
                    "//VSAMIN       DD DSN=&SYSUID..VSAM,DISP=SHR                    \n"+
                    "//SYSOUT    DD SYSOUT=*,OUTLIM=15000                            \n"+
                    "//CEEDUMP   DD DUMMY                                            \n"+
                    "//SYSUDUMP  DD DUMMY                                            \n";


                AddShipRawJCL=           
"//STEP2 EXEC PGM=ADDSH"
"//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR\n"
"//ALLSHPS   DD DSN=&SYSUID..VSAM,DISP=SHR\n"
"//SYSOUT    DD SYSOUT=*,OUTLIM=15000\n"
"//CEEDUMP   DD DUMMY\n"
"//SYSUDUMP  DD DUMMY\n"
"//SYSIN     DD *\n";//Paste ship data below here

            if (!zoweCliWorks())
                throw new Exception($"Zowe CLI could not be found at {zoweCliExe}, install it through npm, and add it to your path");
            Console.WriteLine($"INFO: Found Zowe CLI at {zoweCliExe}");

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


        /// <summary>
        /// Helper class for ships returned by cobol program and/or error message
        /// </summary>
        private class returnedShips
        {
            [JsonPropertyName("Success")] public int Success {get;set;}
            [JsonPropertyName("Error")] public string Error {get;set;} =null!;
            [JsonPropertyName("Ships")] public List<Warship>? Ships {get;set;}
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

            var jobList = JsonSerializer.Deserialize<returnedShips>(shiplist, new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            });

            if (jobList.Success==0)
            {
                throw new Exception($"Error getting ship-list {jobList.Error}");
            }

            return jobList.Ships;
        }

        /// <summary>
        /// Helper class for ship returned by cobol program and/or error message
        /// </summary>
        private class returnedShip
        {
            [JsonPropertyName("Success")] public int Success {get;set;}
            [JsonPropertyName("Error")] public string Error {get;set;} =null!;
            [JsonPropertyName("Ship")] public Warship? Ship {get;set;}
        }

        /// <summary>
        /// Post the job to get a single ship
        /// </summary>
        /// <returns></returns>
        /// <exception cref="Exception"></exception>
        public async Task<Warship> getShip(string ID)
        {
            //Post the job using the rest api, this is the better version but is not permitted by Z-Explore
            //string response = await SubmitJob(getShipsJCL);
            //Post the job using a ZOWE hack (if the mainframe doesn't support http post)
            (string jobUrl,string jobFilesUrl) = SubmitZoweJobArg(SearchShipRawJCL,ID);

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
            string output= await getSysout(jobFilesUrl);
            
            Console.WriteLine(output);

            
            var shipResult = JsonSerializer.Deserialize<returnedShip>(output, new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            });

            if (shipResult.Success==0 || shipResult.Ship==null)
                throw new Exception("Could not get ship: "+shipResult.Error);


            return shipResult.Ship;
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
                    FileName =  zoweCliExe,
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
        /// A bodge solution to post a job with a custom argument, 
        /// The function either returns the ID of the job when submitted, or throws an exception if it did not work
        /// Returns direct link to job, anddirect link to files
        /// I don't have a REST version as I can't debug/test it
        /// </summary>
        private (string,string) SubmitZoweJobArg(string rawJCL,string arg)
        {

            string thisJcl =rawJCL.Replace("REPLACE",$"'{arg}'");

            //Create a temporary file with the raw JCL, this is a unique name, regardless of how many threads have been launched
            string tempJCLFile= Path.GetTempFileName()+".jcl";
            Console.WriteLine("INFO: created "+tempJCLFile);
            try
            {
                File.WriteAllText(tempJCLFile,thisJcl);
            
                ProcessStartInfo processStartInfo = new ProcessStartInfo
                {
                        FileName =  zoweCliExe,
                        Arguments= $"zos-jobs submit lf {tempJCLFile} --rfj",
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
            finally
            {
                Console.WriteLine("INFO: delete "+tempJCLFile);
                //Remove the temp file
                if (File.Exists(tempJCLFile))
                    File.Delete(tempJCLFile);
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

        /// <summary>
        /// Try launching zowe cli to see if the address is right, return true if it works
        /// </summary>
        /// <returns></returns>
        public bool zoweCliWorks()
        {
            //Just inquire the version
            ProcessStartInfo processStartInfo = new ProcessStartInfo
            {
                    FileName =  zoweCliExe,
                    Arguments= $"-V",
                    RedirectStandardOutput=true,
                    RedirectStandardError=true,
                    UseShellExecute = false,
                    CreateNoWindow = true,
            };
            //Launch the process
            try
            {
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
                        return false;
                    }
                    return true;
                }
            }
            catch
            {
                return false;
            }
        }
    }
}