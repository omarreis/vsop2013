## VSOP2013 for Delphi / PlanetFun app / gravityIntegration / discovery of Neptune

This repository started as a Delphi port of planet ephemerides database VSOP2013.
With time it grew to include:

* VSOP2013 planet data calculation tests and binary data file utility.
* N-to-N gravity integration by leapfrog method.
* application "PlanetFun" (a solar system simulation in 3D with augmented reality - The app is available for iOS, Android and Windows).
* Hipparchus 150 star position calculations ( 150 brightest stars )
* VSOP87 planet position theory - Another planet theory, with smaller data footprint.
* star catalogs: H150 and Navigation stars
* Moon position calculations ( ELP2000. code from TMoon component by Andreas Hörstemeier )
* Constellation images, myths and stories ( western tradition )

# Repository News

* aug/23:
PlanetFun 2.1 includes some translations ( English, Portuguese, Spanish, Italian, French ). Fixed phone attitude ( was rolling in the wrong direction )

Completed drawings of western tradition constellations, mostly characters from Greek mythology.
To go with the drawings (this is not in the software) I used AI chat bots ( MS Bing and Google Bard ) to create 
stories about the myths involving constellations, and how Zeus - mostly - designed the skies. 

Check the constellation myths ( 8 stories ): https://github.com/omarreis/vsop2013/tree/master/constellations

* apr/23: 
PlanetFun app v1.9 released for iOS and Android. 

* Added the Almanac module, which allows calculating ephemeris for various celestial objects.
Added VSOP87 planet database. H150 and Navigator star catalogs.

* COnstellation drawings ( added two new sky backgrounds, with constellation mythical greek figures )

* See https://github.com/omarreis/vsop2013/tree/master/planetfun

# VSOP 2013 

VSOP 2013 (Variations Séculaires des Orbites Planétaires) is a high precision 
planetary position model by G. FRANCOU & J.-L. SIMON - may 2013.  
This repo contains a Delphi Pascal port of original Fortran code. 

VSOP2013 calculation machinery uses Chebyshev polynomials to find position and speed 
of the 9 planets at a time (inside a 9000 years range). 

Tested with D11.2 for Win32, iOS and Android. 
Tests are for Firemonkey but should work with VCL and console app as well.

VSOP 2013 original paper with Fortran code and data files can be found at:

https://ftp.imcce.fr/pub/ephem/planets/vsop2013/ephemerides/
 
VSOP2013 README:   

https://ftp.imcce.fr/pub/ephem/planets/vsop2013/solution/README.pdf
 
Data files are large (400 MB) ASCII text containing Chebyshev polynomial of 1st kind *coefficients*. 
It is organized in 6 files, covering a 9,000 year period in all, as follows:
  
     range                file
     -4500 to -3000       VSOP2013.m4000
     -3000 to -1500       VSOP2013.m2000
     -1500 to 0           VSOP2013.m1000
     0     to +1500       VSOP2013.p1000
     +1500 to +3000       VSOP2013.p2000
     +3000 to +4500       VSOP2013.p4000

Each file is divided in 17122, 32 day intervals. Each interval has 978 coefficients, arranged in groups of 6 per line. 

File header contains a table of indexes into coefficients for 9 planets: Mercury, Venus, Earth+Moon baricenter, Mars, Jupiter, Saturn, Uranus, Neptune and Pluto.

Each planet has a number of Chebyshev polynomial terms (between 7 to 14 terms per planet). 

Results are two 3D vectors: position and speed.     
Results are in AU ( AU/day for speeds ) heliocentric ecliptic coordinates ( x,y,z ).

The FTP repository contains Fortran code:

* VSOP2013_binfile.f - Parses text file into binary file that allows fast random access.
* VSOP2013_compute.f - Retrieves 32d interval from binary file and computes planet data ( position and speed )

In this Delphi port, the whole file is pre-loaded into memory tables for fast access. 

Object T_VSOP2013_File encapsulates VSOP 2013 machinery:

* Parse original data files in text - Use only files in original format as the parser relies on fixed positions.
* Save and Load data in a custom binary format, more compact and fast to load ( Android apps have bundle size limit )
* Calculator: calculates heliocentric rectangular position and speed ( in UA and UA/day). Planetary Almanac. 

## Astronomical Algorithms

Most algorithms used for dealing with dates and astronomy are from the 
book "Astronomical Algorithms" by Jean Meeus ( AA 1st and 2nd editions )

Some formulas are from the "Almanac For Computers" publication by USNO  ( marked AfC ).  

## Moon positions

Moon position calculations use theory ELP2000 ( AA chapter 47 ). ELP (Éphéméride Lunaire Parisienne) is a lunar theory developed by Jean Chapront, Michelle Chapront-Touzé, on the Bureau des Longitudes.

The implementation in Ah.Moon.pas was extracted from TMoon component by Andreas Hörstemeier.

see http://www.hoerstemeier.com/moon.htm 


## VSOP 87

An older version of VSOP is described in Meeus book: VSOP 87
It is similar to VSOP 2013, with a lower precision but also a much smaller data footprint.
It is precise enough for most applications.

See *VSOP87* and *VSOP87/Demo* folders

https://github.com/omarreis/vsop2013/tree/master/vsop87

## Hipparchus 150 stars

A star position calculation. Lists 150 brightest stars and implements coordinate calculations for a given time.

See *TestH150* folder for test app.

https://github.com/omarreis/vsop2013/tree/master/TestH150Stars

# Sample apps

Sample apps are included in this repository:

* TestVSOP2013 - Load VSOP2013 text files. Test planet data. 2D visualization. Binary data file utility (converts txt data to floating point). 
* PlanetFun - Planetary system 3D simulation. Requires downloading and deploying planet textures from 3rd party site plus VSOP2013 binary file (see below). PlanetFun is available as executable for Windows. For Android and iOS download from stores (search "PlanetFun"). 
* gravityIntegration - Integrates planet positions using Newton's universal gravity law and compares to VSOP2013. Shows charts of diferences.. Planets Almanac. 
* TestVSOP87 - Planet position calculations using VSOP 87. 
* TestH150 - Hipparchus 150 star position calculations.

# Sample app 1: TestVSOP2013
*TestVSOP2013* is a Firemonkey app. 

To use it you have to download at least one of the data files from VSOP2013 FTP repository ( current file is VSOP2013.p2000 )  

* Download VSOP2013.p2000 from FTP repository 
* Set filename and click [Load File] - The app will freeze for a while loading the 400 MB of text data.
* To test a particular planet position, set JD epoch, planet id and click [Calc]
* Check [x]Animate to show a solar system 2D animated chart.  Use trackbars to control scale and speed of the animation. 
* Click [Run tests] to compare some calculation results with expected values from original vsop2013. 
* [Save Binary file] for deploying with *PlanetFun*  ( creates file VSOP2013.p2000.bin )

![screenshot](screenshotTestVSOP2013.png)

# Test results

I did not test the original Fortran code, so I used the results on original file VSOP2013_ctl-1.txt.

    Test of Delphi implementation of VSOP2013
    ==========================================
    from VSOP2013_ctl.txt ( original results file)
      JUPITER     JD2405730.5  X: -5.392780445602 ua    Y: -0.805698954496 ua      Z :  0.124332318817 ua  
                              X':  0.001019284060 ua/d  Y': -0.007116469431 ua/d   Z':  0.000005921462 ua/d
						   
    from TestVSOP2013   ( code in this repo )
      Jupiter     jde2405730.5 x: -5.39278044560243 ua   y: -0.80569895449564 ua    z: 0.12433231881710 ua
                              sx: 0.00101928405984 ua/d sy: -0.00711646943146 ua/d sz: 0.00000592146220 ua/d			      
    
    from https://ssd.jpl.nasa.gov/horizons.cgi ( Nasa Horizons. used Jupiter Center and Sun  Center)
      Jupiter     jd2405730.5 (A.D. 1874-Jul-26 00:00:00.0000 TDB )  
                               X =-5.392781657192185E+00 Y =-8.056977928944861E-01 Z = 1.243323711175276E-01
                               VX= 1.019554845438113E-03 VY=-7.116569436623840E-03 VZ= 5.921987272194194E-06

## Sample app 2: PlanetFun

![screenshot](planetfun/bannerPlanetFun.png)

Planet Fun is a solar system simulation in 4D. 
The 9 Planets, Sun, Moon, stars are positioned using 
calculation methods described in "Astronomical Algorithms".

Implements an Almanac ( calculator of position for Sun, Moon, 9 planets and 150 stars )

Source code:  

* https://github.com/omarreis/vsop2013/tree/master/planetfun
    
PlanetFun readme:

* https://github.com/omarreis/vsop2013/blob/master/planetfun/README.md

In order to compile and run this app, you will need to:
* Download VSOP2013.p2000 data file from VSOP2013 FTP repository. Convert it to binary format using TestVSOP2013
* Download and include planet textures from 3rd party website

# PlanetFun app for Android

* https://play.google.com/store/apps/details?id=com.omarreis.planetfun

# PlanetFun app for iOS

* https://apps.apple.com/us/app/planet-fun/id1525941640

# PlanetFun for Windows 

Install executables  PlanetFun.exe, TestVSOP2013.exe, gravityIntegration.exe and assets.

get latest release: https://github.com/omarreis/vsop2013/releases

Also available on the Microsoft Store. Search for "planetfun". 

## Sample app 3: gravityIntegration 

Windows app. Integrates planet positions using Newton's universal gravity law and compares to VSOP2013. Shows charts of diferences (residues).

see https://github.com/omarreis/vsop2013/tree/master/gravityIntegration

## discovery of planet Neptune

Numbers related to the discovery of the planet Neptune, in 1846. 
* see: https://github.com/omarreis/vsop2013/blob/master/gravityIntegration/NeptuneDiscovery/README.md
* O descobrimento de Netuno (portugues): https://github.com/omarreis/vsop2013/blob/master/gravityIntegration/NeptuneDiscovery/leiame.md 

## Facebook

FB page: https://www.facebook.com/vrtoolsoftware

## tiktok videos

* TestVSOP2013 - https://www.tiktok.com/@omar_reis/video/6850534226689805574
* PlanetFun1 https://www.tiktok.com/@omar_reis/video/6859411602031119622
* PlanetFun2 https://www.tiktok.com/@omar_reis/video/6923560996493659398
