///Any registered warship

class Warship
{
    [JsonPropertyName("Id")] public string Id {get;set;} = null!;
    [JsonPropertyName("Navy")] public string Navy {get;set;} = null!;
    [JsonPropertyName("Type")] public string Type {get;set;} = null!;
    [JsonPropertyName("Name")] public string Name {get;set;} = null!;
    [JsonPropertyName("Pennant-nr")] public string Pennant {get;set;} = null!;
    [JsonPropertyName("Class")] public string Class {get;set;} = null!;
    [JsonPropertyName("Status")] public string Status {get;set;} = null!;
    [JsonPropertyName("Theatre")] public string Theatre {get;set;} = null!;
    [JsonPropertyName("Fleet")] public string Fleet {get;set;} = null!;
    [JsonPropertyName("Captain")] public string Captain {get;set;} = null!;
    [JsonPropertyName("Speed")] public string Speed {get;set;} = null!;
    [JsonPropertyName("BeltArmour")] public string BeltArmour {get;set;} = null!;
    [JsonPropertyName("DeckArmour")] public string DeckArmour {get;set;} = null!;
    [JsonPropertyName("MainGunNr")] public string MainGunNr {get;set;} = null!;
    [JsonPropertyName("MainGunCalibre")] public string MainGunCalibre {get;set;} = null!;
    [JsonPropertyName("SecondaryGunNr")] public string SecondaryGunNr {get;set;} = null!;
    ///<summary>
    ///Calibre of the largest secondary anti-surface guns in mm
    ///In general, smaller calibre guns fire and traverse faster, making them better for fighting small fast ships like destroyers
    ///
    ///less than 150 mm (6') are light fast guns found as main guns on destroyers or some light cruisers or as secondary guns on larger ships,
    //  Their fire-rate and traverse speed makes them ideal for fighting destroyers, surfaced submarines, or aircraft, but their poor range and light shells make them ineffective against larger ships
    // 150 to 275 mm (6' to 11') are the main guns on light to heavy cruisers.
    //   This compromise between speed and damage is ideal for fighting cruisers
    ///</summary>
    [JsonPropertyName("SecondaryGunCalibre")] public string SecondaryGunCalibre {get;set;} = null!;
    ///<summary>
    ///Model of main fire control computer (if any), more modern models significantly improves effective range and accuracy of main guns
    ///</summary>
    [JsonPropertyName("FireControlComputer")] public string FireControlComputer {get;set;} = null!;
    ///<summary>
    ///Number of heavy AA guns (above 100 mm calibre)
    ///Dual purpose guns count both here, and in the secondary or main gun category
    ///</summary>
    [JsonPropertyName("HeavyAAGunNr")] public int HeavyAAGunNr {get;set;} = null!;
    ///<summary>
    ///Number of light AA guns (20 to 100 mm calibre)
    ///Anything smaller is not worth counting 
    ///</summary>
    [JsonPropertyName("LightAAGunNr")] public int LightAAGunNr {get;set;} = null!;
    ///<summary>
    ///Model Anti Air fire control computer (if any), more modern models significantly improve effectiveness
    ///</summary>
    [JsonPropertyName("AAControlCompute")] public string AAControlCompute {get;set;} = null!;
    ///<summary>
    ///Most advanced radar model present, significantly improves surface and air spotting, nightfighting, and bad-weather fighting capabilities
    ///Also works against surfaced submarines (German submarines need to surface to recharge their batteries)
    ///</summary>
    [JsonPropertyName("RadarModel")] public string RadarModel {get;set;} = null!;
    ///<summary>
    ///Most advanced sonar present (called ASDIC in the Royal Navy), allows ship to detect submerged submarines
    ///</summary>
    [JsonPropertyName("SonarModel")] public string SonarModel {get;set;} = null!;
    ///<summary>
    ///Number of depth-charges present, this is the only weapon which is effective against submerged submarines
    ///</summary>
    [JsonPropertyName("DepthCharges")] public int DepthCharges {get;set;} = null!;
    ///<summary>
    ///Number of on-board torpedo-launchers. Actual number of reload-torpedoes is hard to find, and reloading takes a long time, so this is roughly the torpedo firepower of the ship
    ///</summary>
    [JsonPropertyName("Torpedoes")] public int Torpedoes {get;set;} = null!;
}