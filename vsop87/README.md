# VSOP87 planet positions calculator

* VSOP87 = Variations Seculaires des Orbites Planetaires 1987
* by Bretagnon and Francou - Bureau des Longitudes of Paris
* calculates planet's ecliptical coordinates Lon,Lat and Heliocentric Radius, given a time
* This method is described in J.Meeus book "Astronomical Algorithms" - Chapter 32, used as basis for this code.
* Includes planets Mercury, Venus, Earth, Mars, Saturn, Jupiter, Neptune, Uranus

Note that this repository also contains newer VSOP2013 theory, more precise.
VSOP2013 includes 9 planets and larger time domain.
However VSOP2013 requires a large 100 MB table of coeficients, which makes it
difficult to use in mobile devices.

VSOP87 gives good enough results for most apps, using a smaller set of data coeficients.
In this implementation the coeficients are embedded as code constants, rather than a separate file ( as in VSOP2013 )

## Demo app

See ../Demo folder

![screenshot](TestVSOP87.png)

