using Microsoft.AspNetCore.Mvc;
using ZAPI.Services.ZosmfRESTapi;

namespace ZAPI.Controllers;

[ApiController]
[Route("[controller]")]
public class ZController : ControllerBase
{
    private readonly IZosmfRESTapi zosmfApi;
    private readonly ILogger<ZController> logger;

    public ZController(ILogger<ZController> _logger,IZosmfRESTapi _zosmfApi)
    {
        zosmfApi = _zosmfApi;
        logger = _logger;
    }

    [HttpGet("GetZJobs")]
    public async Task<ActionResult<IEnumerable<JobDocument>>> GetZJobs()
    {
        var list = await zosmfApi.getJobs();

        return Ok(list);
    }
}
