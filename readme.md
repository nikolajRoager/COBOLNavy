COBOL Navy, a training exercise
===========================
This is a training Exercise in COBOL, JCL, the z/OS mainframe.

The program is intended to run on IBM's Z-explorer free mainframe (free as in free of cost, not as in open source) [https://www.ibm.com/z/resources/zxplore](IBM Z-Explorer). You will need to register a free account and upload the files for each project to your user. Please refer to [https://github.com/openmainframeproject/cobol-programming-course/releases](the Open Mainframe Projects introduction to COBOL) for instructions on how to do that.

This project is purely educational, and does therefore comply with the terms of use for Z-Explorer.


Project: Royal Navy Registration
=======

Learning goals
----------------------

* Upload data to the z/OS mainframe, to a VSAM data set.
* Efficiently find, add, and modify data on z/OS based on some unique id.
* Read and write data to the same file using different COBOL programs, verify that the data is uploaded correctly.
* Use COBOL intrinsic functions to cast strings to numbers (`NUMVAL`) correctly.
* Use user defined functions to simplify the Cobol programs, and link and compile them together.
* Use the IBM Z/OS Mainframe Rest Api to read jobs and result from the mainframe from C\#.
* Submit jobs from C\#, including custom arguments (using Rest API or other means).

Scenario
----------

*We need to keep track of allied warships available for Convoy Escort in the Atlantic ocean, we need to be able to upload ships, modify the status and properties of the ships: including pennant number; name; stats relevant for escorting such as cruise-speed, number and size of guns, number of depth-charges, number of torpedoes, radar, sonar, and aircrafts, and data about the current status like: captain name and rank, current area, what formation they are part of, and current status*

Solution, product, and problems
---------------------
The stored data will be stored in a key-sequenced VSAM dataset (Virtual Storage Access Method dataset), this allows me to quickly find entries based on a key, at least compared to storing everything in a big file. The VSAM file is defined by submitting this JCL job:


    //VSMCRJ JOB 1,NOTIFY=&SYSUID
    //***************************************************/
    //STEP1    EXEC PGM=IDCAMS
    //SYSPRINT   DD SYSOUT=*
    //SYSIN      DD *
    DELETE Z67124.VSAM
    SET MAXCC=0
    DEFINE CLUSTER(     -
        NAME(Z67124.VSAM) -
        TRACKS(30)        -
        RECSZ(500 500)    -
        INDEXED           -
        KEYS(12 0)        -
        CISZ(2048))


This creates a VSAM file with exactly 500 characters, and 12 character key. In my case the key will have 4 letters for identifying the navy (RN for Royal Navy, MN for Marine National, and USN for United States (of America's) Navy), 4 letters for identifying the ship type (Using the American Hull Classification letter, with for examble BB for battleship, CV for non-nuclear fleet Carrier), and 4 digit pennant or hull number. For examble, the British battleship HMS Rodney is RNBB0029, The US carrier USS Enterprise is USNCV0006.

To view a ship in the list, I have made a `JSONSHIP.CBL` program, which takes the 500 character data from a ship, and prints it in JSON format (which a C\#, Java, or Javascript program can then parse) to the standard SYSOUT.

`JSONSHIP` takes an argument in its linkage section, so it can be linked and called by another program, for isntance `GETSHPS.CBL` or `FINDSHP.CBL`, the first prints all ships in a JSON list, the other takes a single `PARM` which can be set in the JCL job, and which includes the ship ID to search for.

It is possible to view the result using the REST Api, my C\# program does this: Given the Job ID, I wait for the status to beome `OUTPUT`, then I Locate the address of the SYSOUT file, download it, and parse it as JSON.

Launching a job from C\# should also be done using the Rest API, but IBM Z Xplore does not allow this, for this reason I launch the Zowe CLI Process from C\#. This allows me to either submit a JCL on the mainframe (like `GETSHPS.JCL`), or submit a custom JCL (Like requesting `FINDSHP` with custom parameter), doing so returns the job ID, which I can use to get the output


To fill the data-set, I have created a program `ADDSH.CBL` which prompts the user for all input, and either adds the ship to the dataset, or modifies an existing entry.

By redirecting SYSOUT in the JCL job file, I can automatically provide all the entries from a custom JCL file.

In principle, I have made a C# program which can upload a database from [https://www.naval-history.net/xDKWW2-4101-26RNHome.htm](the Naval-history.net project), with ship data verified on the [http://www.dreadnoughtproject.org/tfs/index.php/Main_Page](the dreadnought project Wiki) from the Royal Navy on the 1st of January 1941

However I did not upload all the data, because I made too many requests and got temporarily suspended.