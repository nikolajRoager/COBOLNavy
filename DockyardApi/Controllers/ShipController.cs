using Microsoft.AspNetCore.Mvc;
using DockyardApi.Services.ZosmfRESTapi;
using DockyardApi.Models;

namespace DockyardApi.Controllers;


[ApiController]
[Route("[controller]")]
public class ShipController : ControllerBase
{
    private readonly IZosmfRESTapi zosmfApi;
    private readonly ILogger<ShipController> logger;

    public ShipController(ILogger<ShipController> _logger,IZosmfRESTapi _zosmfApi)
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
