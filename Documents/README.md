# PlanetFun files to deploy 
In order to run PlanetFun, you must deploy the following files twith the program:

Note: previous version (up to 1.1) used the textures loaded in the form's FMX file, which was very large.
In this version, textures were remkoved and deployed as external files. 
This reduced the bundle size for iOS and Android ( important for Android because of the 150 MB limit )
and made it easier to deploy source code in this repository.

## from this folder:
* SkyMapLinesNames.png
* SkyMapPlain.png

## Files from  *Solar System Scope*
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

## Planet ephemeris
* Generate with TestVSOP2013 and deploy file vsop2013.p2000.bin do Documents

For iOS and Android, deploy to <documents> folder
For Windows, deploy to <user\documents\vsop2013\>

