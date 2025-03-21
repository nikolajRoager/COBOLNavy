using System.Text.Json.Serialization;

namespace DockyardApi.Models;

//A class for receiving a list of Job Documents from the REST API
public class JobList
{
    [JsonPropertyName("jobs")]
    public List<JobDocument> Jobs { get; set; } = null!;
}