# Planet Fun
==========

Planet Fun is a solar system simulation working model in 3D. 
Program is a Delphi Firemonkey 3d app, and is tested on Windows, Android and iOS (Delphi 10.4 Seattle)

App source code is available at:

https://github.com/omarreis/vsop2013/planetfun

# Planet positions

Planet positions are calculated using VSOP2013 ephemerides ( see https://github.com/omarreis/vsop2013 ) 
This is high resolution planet positions between epochs 1500 to 3000. 

# Planet textures

Program sources are *almost ready* to compile. Planet textures are *not* included in this repository. 
You can find suitable and free textures on this website:

*Solar System Scope*   https://www.solarsystemscope.com/textures/

* Download files: 2k_earth_daymap.jpg, 2k_jupiter.jpg, 2k_mars.jpg, 2k_mercury.jpg, 2k_moon.jpg, 2k_neptune.jpg, 2k_saturn.jpg, 2k_stars_milky_way.jpg, 2k_uranus.jpg, 2k_venus_surface.jpg and PlutoTexture.jpg
* Load each file into the corresponding lightMaterialTextureXXX object. For instance load file '2k_jupiter.jpg' into lightMaterialTextureJupiter. 
* Once you have loaded the textures for the 9 planets, Sun, Moon and stars background, you are ready to compile the project.

# What is not realistic
* Planet sizes in the real solar system are difficult for visualization. The Sun radius is more than 100x that of the Earth. Distance between planets and the Sun are even larger. If you do a program using real proportional sizes, you end up with a black screen with tiny dots. So I applied a log formula to Sun and planet sizes. Planet scale can be changed for more visible planets.
* Moon position, size and orbit size are not connected to real coordinates yet (TODO)



keywords: solar system planet astronomy vsop2013 
