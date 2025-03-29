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

    [HttpGet("GetShips")]
    public async Task<ActionResult<IEnumerable<Warship>>> GetShips()
    {
        try
        {
            var list = await zosmfApi.getShips();
            return Ok(list);
        }
        catch (Exception e)
        {
            return Problem(e.Message);

        }
    }

/*
    //Get ship by its unique id string
    [HttpGet("GetShip/{id}")]
    public async Task<ActionResult<Warship>> GetShips(string id)
    {
        //Submit JCL to get a single ship as JSON, listen for output and return it or error
        return Ok();
    }*/


}
