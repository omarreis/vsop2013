# PlanetFun files to deploy 

In order to run PlanetFun you must deploy the following files with the program:

Note: previous version (up to 1.1) used the textures loaded in the form's FMX file, which made that file very large.
From version 1.2+ textures are deployed as external files. 
This reduced the bundle size ( important for Android because of the 150 MB limit )
and made it easier to deploy app source code to this repository.
As of v1.3 smaller textures are still embedded in fPlanetFun.fmx textures. 

Some of the deployment files are in this repo, some are available from 3rd party sites:

## from this repository:

Custom sky background textures:

* SkyMapLinesNames.png
* SkyMapPlain.png
* SkyMapLinesNamesDrawings.png 
* SkyMapDrawings.png

## Files to be download from  *Solar System Scope*
https://www.solarsystemscope.com/textures/

* 2k_mercury.jpg      
* 2k_venus_surface.jpg
* 2k_earth_daymap.jpg 
* 2k_mars.jpg         
* 2k_jupiter.jpg      
* 2k_saturn.jpg       
* 2k_uranus.jpg       
* 2k_neptune.jpg      
* PlutoTexture.jpg    
* 2k_moon.jpg    

## VSOP2013 Planet ephemeris
* Download ASCII text data file from VSOP2013 repository:
ftp://ftp.imcce.fr/pub/ephem/planets/vsop2013/ephemerides/VSOP2013.p2000 

* Use app TestVSOP2013.exe to load the text file and save binary file *vsop2013.p2000.bin*
Note: don't use binary file from the original ftp folder, which has a different binary format

Deploy textures and vsop2013 binary file to app's *Documents* folder:

* For Android to .\assets\internal
* For iOS to .\StartUp\Documents
* Documents for Windows  

c:\Users\username\OneDrive\Documents\vsop2013\    <--  or something like that (installer creates vsop2013\ )

