## Newton's Gravity Integration

*GravityIntegration* app implements Newton's gravity integration over time.
It calculates planet's position and speed by integrating the attraction forces
between the 9 planets and the Sun. 

![GravityIntegration screenshot](screenshotGravityIntegration.png)

The app uses VSOP2013 ephemerides as basis for comparison, and produces charts 
of diferences between planet's calculated and "observed" positions.

You can chart diferences in positions using heliocentric spherical coordinates: 

* radius diference ( in % of radius ) 
* latitude diference ( in arcsecs )
* longitude diference ( in arcsecs )

Longitude is the most important property for us (Earth based astronomers),
because it is easily observed with telescopes. Distances (or planet orbit radius)
are more difficult to observe.

## Leapfrog integration
*Newton's Universal Gravitation* formula for the force between two objects is simple: 

    F = G * M1 * M2 / D^2
    where
      F = attraction force between objects
      G = Universal gravitation constant
      M1, M2 = masses of the objects
      D = Distance between objects centers
    
When many objects are involved, summing the effect of all the forces can be tricky. Not only does each object (ex: a planet) 
attracts all others, the magnitude and direction of the forces change continuously as they move. 

One approach to calculate position and speed of a planet at a certain time is to divide 
the time in small intervals DT, and calculate object state incrementally by adding DTs.
This numerical integration starts at a known time-position-speed T0.

     start with i=0, P0 and V0 
     Pi+1 = Pi + Vi * DT    <-- position vector change 
     Ai   = ∑(G*M/D^2)      <-- sumatory of gravity acceleration due to all objects ( N-to-N attraction)
     Vi+1 = Vi + Ai * DT    <-- speed vector acceleration 
     goto to next i 
  
This is called *Euler* method. It may look ok, but with elliptical or parabolic orbits, 
speeds are systematically off the mark which results in continuous drift from actual values. 
Total energy grows with time, since planet orbits get larger. Energy conservation is violated.

A better calculation method is the so called *leapfrog* integration.
It also starts with P0 ( position at time T0 ), but uses V1/2 (speed at time DT/2) instead of V0. 
V1/2 is a better estimate of mean speed in T0->T1 interval than V0, 
and leapfrog integration results are much better than Euler's.

Note that there are even better integration methods, but leapfrog is so simple and elegant.   :)  

The leapfrog integration algorithm goes like this:

     start with i=0, P0 and V1/2 
     Pi+1   = Pi + Vi+1/2 * DT     <-- position vector change using V1/2, in the middle of the segment
     Ai+1   = ∑(G*M/D^2)           <-- sumatory of gravity acceleration due to all objects
     Vi+3/2 = Vi+1/2 + Ai+1 * DT   <-- speed vector acceleration 
     goto to next i 
    
While Euler integration is first order, leapfrog is second order, 
meaning that the error reduces fast when we use a small enough DT.

For Mercury, the planet with shortest revolution time (88 days), 
I found using DT=0.1 day reduced the longitude error considerably (as opposed to default DT=0.5) 

![leapfrog integration](leapfrogIntegration.png)

# Dependencies

* Uses files from VSOP2013 repository:  https://github.com/omarreis/vsop2013

Full Delphi source code  in this repository

# Executables available

* PlanetFun for iOS and Android ( search your app store )
* PlanetFun, TestVSOP2013 and gravityIntegration for Windows (see below)

## Installer for Windows 
Installs TestVSOP2013.exe, PlanetFun.exe, gravityIntegration.exe and VSOP2013.p2000.bin
https://github.com/omarreis/vsop2013/releases/download/1.1/setupVSOP2013_win32.exe

( authenticode signed by Carvalho e Reis ltda )

# Discovery of planet Neptune

As an application of this leapfrog integration, I tried to reconstruct some of the 
numbers that led to the discovery of planet Neptune in 1846, even when no one 
had seen it before.  See:

https://github.com/omarreis/vsop2013/blob/master/gravityIntegration/NeptuneDiscovery/README.md


## also in this repository
* vsop2013 ephemerides for Delphi: https://github.com/omarreis/vsop2013
* gravity integration tool: https://github.com/omarreis/vsop2013/tree/master/gravityIntegration/README.md
* planetFun app: https://github.com/omarreis/vsop2013/tree/master/planetfun/README.md
* Neptune discovery: https://github.com/omarreis/vsop2013/blob/master/gravityIntegration/NeptuneDiscovery/README.md




