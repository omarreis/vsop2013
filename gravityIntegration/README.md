## Newton's Gravity Integration

*GravityIntegration* app implements Newton's gravity integration over time.

![GravityIntegration screenshot](screenshotGravityIntegration.png)

It calculates planet's position and speed by integrating the attraction forces
between the 9 planets and the Sun. 

It uses VSOP2013 ephemerides as basis for comparison, to produce charts 
of differences between planet's calculated and "observed" positions.
You can chart differences in positions using heliocentric spherical coordinates: 

* radius difference ( in % of radius ) 
* latitude difference ( in arcsecs )
* longitude difference ( in arcsecs )

## Leapfrog integration
Newton's Universal Gravitation formula for the force between two objects is simple: 

    F = G * M1 * M2 / D^2
    where
      F = attraction force between objects
      G = Universal gravitation constant
      M1, M2 = masses of the objects
      D = Distance between objects centers
    
Summing the effect of all these forces can be tricky. Not only does objects (p.e. a planet) 
attract all others, but the magnitude direction of the forces vary continuously with time,
as objects move around. 

One approach is to divide the time in small DT time intervals, and calculate object
states incrementally:

     Ai = Sum (G*M/D^2)     <-- sumatory of gravity acceleration due to all objects
     Pi+1 = Pi + Vi * DT    <-- position vector change 
     Vi+1 = Vi + Ai * DT    <-- speed vector acceleration 
     goto to next i 
  
This is called *Euler* method. It may look ok, but with elliptical Kepler orbits, 
speeds are systematically off the mark, resulting in continuous drift 
in positions from actual values.

A better calculation method is the so called *leapfrog* integration.
It also starts with P0 ( position at time 0), but uses V1/2 ( speed at time DT/2 )
instead of V0.  

Because V1/2 is a much better estimate of mean speed in the T0->T1 interval  than V0, 
results using leapfrog integration are much better than Euler's method.
Note that there are even better methods today, but leapfrog is ellegant. 

The leapfrog integration algorithm goes like this:

     Ai = Sum (G*M/D^2)            <-- sumatory of gravity acceleration due to all objects
     Pi+1   = Pi + Vi+1/2 * DT     <-- position vector change using V1/2, in the middle of the segment
     Vi+3/2 = Vi+1/2 + Ai * DT     <-- speed vector acceleration 
     goto to next i 
    
While Euler integration is first order, leapfrog is second order (meaning that the error
is proportional to 1/DT^2 instead of 1/DT ). So error reduces fast when we use
a small enough DT.

![leapfrog integration](leapfrogIntegration.png)

# Discovery of planet Neptune
As an application of leapfrog integration, I tried to reconstruct some of the 
numbers that led to the discovery of planet Neptune in 1846, even
when no one had seen it before. See:

https://github.com/omarreis/vsop2013/blob/master/gravityIntegration/NeptuneDiscovery/README.md



