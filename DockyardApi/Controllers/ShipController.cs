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

    //Get ship by its unique id string
    [HttpGet("GetShip/{id}")]
    public async Task<ActionResult<Warship>> GetShip(string id)
    {
        try
        {
            var ship = await zosmfApi.getShip(id);
            return Ok(ship);
        }
        catch (Exception e)
        {
            return Problem(e.Message);
        }
    }

    //Add a ship at the end of the list
    [HttpPost("AddShip/{id}")]
    public async Task<ActionResult<Warship>> AddShip(string id)
    {
        return Ok();
    }

    /// <summary>
    ///Update status of ship with this id, to that, there is no delete function, instead set status to "sunk" or "scrapped"
    ///For practical purposes, a museum ship is considered "scrapped"
    /// </summary>
    /// <param name="id"></param>
    /// <returns></returns>
    [HttpGet("UpdateShip/{id}")]
    public async Task<ActionResult<Warship>> UpdateShip(string id)
    {
        return Ok();
    }
}
