# Planet Fun      ![banner](bannerPlanetFun.png)

Planet Fun is a solar system working model in 3D.  
Program is a Delphi Firemonkey 3D app. 
Tested on Windows, Android and iOS (w/ Delphi 10.4).

Full app source code is available at:

* https://github.com/omarreis/vsop2013/tree/master/planetfun

![screenshots](screenshotsPlanetFun.png)

# Program features
* Multi platform: Android, iOS and Windows
* Solar system *animation* with configurable speed
* Choose camera target (Sun or planets)
* Set date/time bewteen years 1500 and 3000
* Configurable camera distance-to-target
* Touch gestures: one finger pan, two finger zoom and two finger rotation (on mobile)
* Mouse events: mouse-move, Shift mouse-move and Alt mouse-move (on Windows)
* Planet orbit dots. Each orbit is represented by 52 dots ( For the Earth, it is 1 dot per week) )
* Solar system heliocentric axis (x and z)
* VSOP2013 planet ephemeris usage sample
* Edit date/time

# Planet positions

Planet positions are calculated using VSOP2013 ephemerides ( see https://github.com/omarreis/vsop2013 ) 
This library calculates planet's heliocentric coordinates between epochs 1500 to 3000.
( VSOP2013 by Francou and Simon )

    see ftp://ftp.imcce.fr/pub/ephem/planets/vsop2013/ephemerides/

VSOP2013 data files are not in this repository. 
For speed and bundle size, PlanetFun app uses a custom binary version of VSOP2013 data.

Steps to obtain planet ephemeris file:

* Download VSOP2013.p2000 from the FTP repository above ( a 400 MB text file) 
* Use program *TestVSOP2013* to Load VSOP2013.p2000 text file.
* Save binary file VSOP2013.p2000.bin.   Do *not* use binary files from original FTP repository.  Binary files for this app use a different custom format.
* Deploy file VSOP2013.p2000.bin to PlanetFun app documents folder. 

Resulting binary file is *only* 131 MB and doesn't require parsing to load. 

The scale factor between astronomical units and the app's 3D world:     
     1.0 AU = 10.0 3D-world-units  
    
ex: Earth orbit radius is ~around~ 10.0 3D-world-units

# Planet textures

Planets are represented by TSpheres with light reflecting surfaces ( TLightMaterialSource )
The Texture property is a bitmap that is mapped to the sphere, on a reverse-Mercator projection.  

Planet textures are *not* included in this repository. 

You can find suitable and free textures on this website:

*Solar System Scope*   https://www.solarsystemscope.com/textures/  

steps to complete PlanetFun assets:

* Download texture images: ex: 2k_earth_daymap.jpg, 2k_jupiter.jpg, 2k_mars.jpg, 2k_mercury.jpg, 2k_moon.jpg, 2k_neptune.jpg, 2k_saturn.jpg, 2k_stars_milky_way.jpg, 2k_uranus.jpg, 2k_venus_surface.jpg and PlutoTexture.jpg
* On the program Form, load each image into the corresponding lightMaterialTextureXXX objects. For example, load file '2k_jupiter.jpg' into lightMaterialTextureJupiter.Texture . And so on. Textures are all added to the .FMX file.
* Once you have loaded all the textures for the 9 planets, Moon and stars background, it will be ready to compile. ( Sun has no texture at this time, just a white ball )

Follow *Solar System Scope* license conditions ( Attribution 4.0 )

# Warning: not realistic
* Solar and planet sizes are difficult for visualization. The Sun radius is more than 100x that of the Earth. Distance between planets and the Sun are even larger. If you do a program using actual proportional object sizes, you end up with a black screen and some tiny dots. Not really exciting.   I applied a log formula to Sun and planet sizes so that the Sun is only about 4x the size of the Earth. Anyway, planet scale can be configured for more visible planet details.
* Moon position, speed, size and orbit size are not connected to real world coordinates yet. Anyway, Moon revolution ( 27.32 days) is so much faster than other celestial objects, it is just all over the place when animated. In this illustration, it is animated on a circular orbit and smooth, slow speed. Fake. (TODO)
* Stars background is too fuzzy to check texture placement. Only illustrative in this 1st version. Fake. (TODO)
* Planet rotation speeds are also not realistic (TODO). But Planet positions and revolutions are just fine!  :)

# Dependencies
*  Uses file vsop2013.pas from https://github.com/omarreis/vsop2013/

# Julian date formulas
from Astronomical Algorithms. Jean Meeus

# video
https://www.tiktok.com/@omar_reis/video/6859411602031119622

keywords: solar system planet astronomy vsop2013 Delphi Firemonkey
