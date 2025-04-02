using System.Text.Json.Serialization;

namespace DockyardApi.Models;


///<summary>
/// Any commisioned warship
/// </summary>
public class Warship
{
    /// <summary>
    /// Unique id made from the navy, hull-type and pennant number
    /// </summary>
    [JsonPropertyName("Id")] public string Id {get;set;} = null!;
    /// <summary>
    /// Code for the navy this is commanded by
    /// </summary>
    [JsonPropertyName("Navy")] public string Navy {get;set;} = null!;
    /// <summary>
    /// American hull classification letters (the british pennant letter keeps changing during WW2, and the American letters are somewhat more logical)
    /// </summary>
    [JsonPropertyName("Type")] public string Type {get;set;} = null!;
    /// <summary>
    /// Name of the ship
    /// </summary>
    [JsonPropertyName("Name")] public string Name {get;set;} = null!;
    /// <summary>
    /// Royal Navy Pennant number, or US Hull number, typically unique for ships in a type
    /// </summary>
    [JsonPropertyName("Pennant-nr")] public int Pennant {get;set;}
    /// <summary>
    /// Class of the ship, typically name of the first ship of the class
    /// </summary>
    [JsonPropertyName("Class")] public string Class {get;set;} = null!;
    ///<summary>
    ///Status of the ship, one of:
    /// * Operational : The ship is fully operational, and can be used on any operations
    /// * Working up  : The ship is working up/undergoing trials after construction or a major refit, the ship can be used for operations in an emergency (but shouldn't).
    /// * Repairing   : The ship is being repaired or is on way to repair, but can still move under its own power in an emergency.
    /// * Refitting   : The ship is in drydock for a major refit (either as part of major repair, or during major upgrades).
    ///</summary>
    [JsonPropertyName("Status")] public string Status {get;set;} = null!;
    ///<summary>
    ///The theatre (large location) this ship is operating in, such as the north-east Atlantic, Mediterranean, or Indian Ocean
    ///</summary>
    [JsonPropertyName("Theatre")] public string Theatre {get;set;} = null!;
    ///<summary>
    ///The top-level command this ship is a part of
    ///</summary>
    [JsonPropertyName("Fleet")] public string Fleet {get;set;} = null!;
    ///<summary>
    ///The lower level command this ship is a part of
    ///</summary>
    [JsonPropertyName("Formation")] public string Formation {get;set;} = null!;
    ///<summary>
    ///Name of the officer commanding the ship, regardless of what their actual rank is, an admiral using this as a flagship will not be listed here, unless they also command the ship
    ///</summary>
    [JsonPropertyName("Captain")] public string Captain {get;set;} = null!;
    ///<summary>
    ///Max speed of the ship in knots as designed; actual speed may be smaller if machinery or hull is damaged or worn out, or if the boiler tupes have not been cleaned regularly, in an emergency ships can exceed their design speed (with a significant risk of damage)
    ///</summary>
    [JsonPropertyName("Speed")] public double Speed {get;set;}
    ///<summary>
    ///Main belt (side) armour thickness in mm, used to defend against gunfire at medium to short ranges
    ///</summary>
    [JsonPropertyName("BeltArmour")] public double BeltArmour {get;set;}
    ///<summary>
    ///Deck armour thickness in mm, needed in long-range gunfights or against air-attack
    ///</summary>
    [JsonPropertyName("DeckArmour")] public double DeckArmour {get;set;}
    ///<summary>
    ///Number of main battery of anti-surface guns, this is the number of individual tubes, so 2 quad-turrets or 4 twin turrets is the same number of guns
    ///</summary>
    [JsonPropertyName("MainGunNr")] public int MainGunNr {get;set;}
    ///<summary>
    ///Calibre of the largest main anti-surface guns in mm
    ///In general, smaller calibre guns fire and traverse faster, making them better for fighting small fast ships like destroyers, while heavier guns carry a larger explosive payload and have better armor penetration, making them better against heavier ships
    ///
    ///less than 150 mm (6') are light fast guns found as main guns on destroyers or some light cruisers or as secondary guns on cruisers and battleships
    ///  Their fire-rate and traverse speed makes them ideal for fighting destroyers, surfaced submarines, or aircraft, but their poor range and light shells make them ineffective against larger ships
    /// 150 to 275 mm (6' to 11') are the main guns on light to heavy cruisers, and secondary guns on battleships
    ///   This compromise between speed and damage is ideal for fighting cruisers or destroyers
    /// 275 to 380 mm (11' to 15') Light to medium battleship main guns, mainly found on older ships, has faster rate of fire and traverse at the cost of range, armour piercing abbility and damage, sometimes with many more barrels per turret (King George V) for faster aiming
    /// 380 to 460 mm (15' to 18') Heavy battleship or battlecruiser main guns, ranging from the 15' guns on Hood, Renown and Repulse, to the 460 mm massive guns on the Yamato
    ///</summary>
    [JsonPropertyName("MainGunCalibre")] public double MainGunCalibre {get;set;}
    ///<summary>
    ///Number of secondary battery of anti-surface guns, this is the number of individual tubes, so 2 quad-turrets or 4 twin turrets is the same number of guns
    ///Dual-purpose guns are both counted here, and in the heavy anti-aircraft category
    ///</summary>
    [JsonPropertyName("SecondaryGunNr")] public int SecondaryGunNr {get;set;}
    ///<summary>
    ///Calibre of the largest secondary anti-surface guns in mm
    ///In general, smaller calibre guns fire and traverse faster, making them better for fighting small fast ships like destroyers
    ///
    ///less than 150 mm (6') are light fast guns found as main guns on destroyers or some light cruisers or as secondary guns on cruisers and battleships
    ///  Their fire-rate and traverse speed makes them ideal for fighting destroyers, surfaced submarines, or aircraft, but their poor range and light shells make them ineffective against larger ships
    /// 150 to 275 mm (6' to 11') are the main guns on light to heavy cruisers, and secondary guns on battleships
    ///   This compromise between speed and damage is ideal for fighting cruisers
    /// Anything above are not used as secondaries
    ///</summary>
    [JsonPropertyName("SecondaryGunCalibre")] public double SecondaryGunCalibre {get;set;}
    ///<summary>
    ///Model of main fire control computer (if any), more modern models significantly improves effective range and accuracy of main guns
    ///</summary>
    [JsonPropertyName("FireControlComputer")] public string FireControlComputer {get;set;} = null!;
    ///<summary>
    ///Number of heavy AA guns (above 100 mm calibre)
    ///
    ///Dual purpose cruiser sized guns (100 mm to 200 mm) guns count both here, and in the secondary or main gun category
    ///To count here, the guns need to actually be useful anti-aircraft guns, hence the Yamato's 18 inch San-Shiki ammunition does count.
    ///</summary>
    [JsonPropertyName("HeavyAAGunNr")] public int HeavyAAGunNr {get;set;}
    ///<summary>
    ///Number of light AA guns (20 to 100 mm calibre)
    ///Anything smaller is not worth counting 
    ///</summary>
    [JsonPropertyName("LightAAGunNr")] public int LightAAGunNr {get;set;}
    ///<summary>
    ///Model Anti Air fire control computer (if any), more modern models significantly improve effectiveness
    ///</summary>
    [JsonPropertyName("AAControlComputer")] public string AAControlCompute {get;set;} = null!;
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
    [JsonPropertyName("DepthCharges")] public int DepthCharges {get;set;}
    ///<summary>
    ///Number of on-board torpedo-launchers. Actual number of reload-torpedoes is hard to find, and reloading takes a long time, so this is roughly the torpedo firepower of the ship
    ///</summary>
    [JsonPropertyName("Torpedos")] public int Torpedos {get;set;}

    [JsonPropertyName("Aircraft")] public List<PlaneComplement> Aircraft {get;set;}=null!;

    public class PlaneComplement
    {
        [JsonPropertyName("Model")] public string Model {get;set;} = null!;
        [JsonPropertyName("Number")] public int Number {get;set;}
    }


}