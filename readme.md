COBOL Navy, a training exercise
===========================
This is a training Exercise in COBOL, JCL, the z/OS mainframe.

This project includes a number of smaller projects, of increasing complexity, covering different learning goals.

All the smaller projects are themed around the Battle for the Atlantic during WW2.

The program is intended to run on IBM's Z-explorer free mainframe (free as in free of cost, not as in open source) [https://www.ibm.com/z/resources/zxplore](IBM Z-Explorer). You will need to register a free account and upload the files for each project to your user. Please refer to [https://github.com/openmainframeproject/cobol-programming-course/releases](the Open Mainframe Projects introduction to COBOL) for instructions on how to do that.

This project is purely educational, and does therefore comply with the terms of use for Z-Explorer.


Projects
=======

Ship Listing
----------------------

Learning goals:

* Create a new data file on the z/OS mainframe, from data wider than 80 characters per line.
* Read and write data to this file using different COBOL programs, verify that the data is uploaded correctly.
* Use COBOL intrinsic functions to cast strings to numbers (`NUMVAL`)
* Use at least one custom function, defined in a seperate file (in my case, a function which surrounds a string with quotes, and trims to content).
* Link and compile multiple functions together
* Use COBOL to print data in JSON (so a web-browser can present it)

Scenario:

*We need to keep track of allied warships available for Convoy Escort in the Atlantic ocean, we need one program which can add or update ships in a file `DATA.ALLIEDSHIPS` (from the JCL file), this file simply contains identifying information for each ship, like a unique identifier made from its navy, type, and pennant number; its name; and stats  relevant for escorting such as cruise-speed, number and size of guns, number of depth-charges, number of torpedoes, crew, radar, sonar, and aircrafts, and data about the current status like: commander name and rank, current area, what formation they are part of, and current status*

Solution and product:

I have made two data files: First I made a file with all the data, and some explanations: `alliedships_Example.txt` which contain the same data, with a bit of explanation of the format, with one line per ship. `alliedships.txt` contains the actual data we want to upload, wrapped so there is no more than 80 chars per line because backwards compatibility. You can upload it with this CLI command `zowe zos-files upload file-to-data-set P0registration/data/alliedships.txt "USERNAME.USERDATA(ALLIED)"` (After creating the partioned data-set USERDATA, you may need to shorten the names)

The data in `alliedships.txt` has been read off mainly from [https://www.naval-history.net/xDKWW2-4101-26RNHome.htm](the Naval-history.net project), with ship data verified on the [http://www.dreadnoughtproject.org/tfs/index.php/Main_Page](the dreadnought project Wiki)

WARNING, I do NOT guarantee that some errors haven't made their way into my data-set!

The first COBOL program I have made, LSSHP.cbl, lists ships in the file.

It merely loops through the file, and prints the data out as a json list of ships.

To do that, I had to make a function which can put quotation marks around string names, after trimming excess spaces. This function has been placed in the file MKQUOTE.cbl, uploaded as MKQUOTE to Z/OS.

These two files need to be compiled and linked, this is done with the file LSSHPJ.jcl