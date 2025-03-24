using DockyardApi.Services.ZosmfRESTapi;
using DockyardApi.Models;

//Mostly autogenerated dotnet boilerplate code, used to set up the web api
var builder = WebApplication.CreateBuilder(args);

// Injecting services
//Add a singleton handler, which handles all communication with the mainframe, throws exceptions

try
{
    builder.Services.AddScoped<IZosmfRESTapi, ZosmfRESTapi>(
        provider => new ZosmfRESTapi("204.90.115.200","10443","z67124","PDV98WJK")
    );
}
catch(Exception E)
{
    //Well, lets hope it is something the user can fix
    System.Console.WriteLine("There was an error setting up the Z/OS mainframe handler, the handler returned error:");
    System.Console.WriteLine(E.Message);
    System.Console.WriteLine();
    System.Console.WriteLine("This program can not proceed unless you fix the problem");
    System.Console.WriteLine("This program is not actively being maintained and may not work if IBM has changed the API or no longer offers this service");
    //End with an error
    return 1;
}
builder.Services.AddControllers();
// Learn more about configuring OpenAPI at https://aka.ms/aspnet/openapi

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();


var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(options =>
    {
        options.SwaggerEndpoint("/swagger/v1/swagger.json", "v1");
        options.RoutePrefix = string.Empty;

    });
}

app.UseHttpsRedirection();

app.UseAuthorization();

app.MapControllers();

app.Run();

//Since we used return value in the catch statement above, we got to do the same here
return 0;
