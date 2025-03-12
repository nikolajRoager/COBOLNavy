COBOL Navy, a training exercise
===========================
This is a training Exercise in COBOL, JCL, the z/OS mainframe.

This project includes a number of smaller projects, of increasing complexity, covering different learning goals.

All the smaller projects are themed around the Battle for the Atlantic during WW2.

The program is intended to run on IBM's Z-explorer free mainframe (free as in free of cost, not as in open source) [https://www.ibm.com/z/resources/zxplore](IBM Z-Explorer). You will need to register a free account and upload the files for each project to your user. Please refer to [https://github.com/openmainframeproject/cobol-programming-course/releases](the Open Mainframe Projects introduction to COBOL) for instructions on how to do that.

This project is purely educational, and does therefore comply with the terms of use for Z-Explorer.


Projects
=======

Allied ship registration
----------------------

Learning goals:

* Create a new data file on the z/OS mainframe
* Read and write data to this file using different COBOL programs
* Use COBOL to print data to another output file in a Human readable format

Scenario:

*We need to keep track of allied warships available for Convoy Escort in the Atlantic ocean, we need one program which can add or update ships in a file `DATA.ALLIEDSHIPS` (from the JCL file), this file simply contains identifying information for each ship, like its name, class, what type it is, and what navy operates it, and stats  relevant for escorting or hunting like tonnage, cruise-speed, range, number and size of guns, number of depth-charges, number of torpedoes, crew, radar, sonar, and aircrafts*, and data about the current status like: commander name and rank, current area, what formation they are part of, and current status*


*I will make two COBOL programs, one which simply prints all ships in the file `DATA.ALLIEDSHIPS` to some output in a human readable table*

*The other program either writes new ships, or updates existing ships in the file. As input it should take data directly specified in the JCL file*

