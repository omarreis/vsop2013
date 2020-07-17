# vsop2013

VSOP 2013 (French: Variations Séculaires des Orbites Planétaires) is a high precision planetary position mathematical model, by J. Simon and G. Francou. 

It is a large series of Chebyshev polynomial coeficients. 

This is a Delphi port of original Fortran files by the theory authors.  It was tested with D10.3.3 on Win32.

VSOP original files can be found at:

* ftp://ftp.imcce.fr/pub/ephem/planets/vsop2013/ephemerides/

Data files are large (400 MB) ASCII containing Chebyshev polynomial 1st kind coeficients. 
It is organized in 6 files, covering a 9,000 year period in all, as follows:
  
     range                file
     -4500 to -3000       VSOP2013.m4000
     -3000 to -1500       VSOP2013.m2000
     -1500 to 0           VSOP2013.m1000
     0     to +1500       VSOP2013.p1000
     +1500 to +3000       VSOP2013.p2000
     +3000 to +4500       VSOP2013.p4000

Each file is divided in 17122, 32 day intervals. Each interval has 978 coeficients, arranged in groups of 6 per line. File header contains a table of indexes into coeficients for 9 planets: Mercury, Venus, Earth+Moon baricenter, Mars, Jupiter, Saturn, Uranus, Neptune and Pluto.

The FTP repository also contains Fortran code:
* VSOP2013_binfile.f - Parses text file into binary file that allows fast random access.
* VSOP2013_compute.f - Retrieves 32d interval from binary file and computes planet data ( position and speed )

In this Delphi port, the ASCII file is loaded directly into memory tables, for even faster access. 
Once loaded, computations are very fast. 

Object T_VSOP2013_File in vsop2013.pas:
* parses a data file - Use only files in original format, as the parser relies on fixed positions.
* calculates heliocentric rectangular position and speed ( in UA and UA/day)

# Test app
Program TestVSOP2013 is a Firemonkey app. To use it, you have to download at least one of the files mentioned above from theory's FTP repository. 

* Set filename and click [Load File] - This will freeze the app for a while, as it loads de 400 MB of text
* Set JDE epoch and planet id and click [Calc]
* Check animate show a solar system animation ( use track bars to control scale and speed )

# Test results

I did not test the original Fortran code, so I used the results on original file VSOP2013_ctl-1.txt.

Note: At this time, the results I got are *not* exactly the same as in VSOP2013_ctl-1.txt.
But are close. Still searching for an explanation for that, as the Delphi code is very close to original Fortran by theory authors.
Judge for yourself:

Tests of Delphi implememntation of VSOP2013
===========================================

from VSOP2013_ctl.txt
  JUPITER     JD2405730.5  X: -5.392780445602 ua    Y: -0.805698954496 ua      Z :  0.124332318817 ua  
                          X':  0.001019284060 ua/d  Y': -0.007116469431 ua/d   Z':  0.000005921462 ua/d
						   
from TestVSOP2013 (code in this repo)
  Jupiter     jd2405730.5  x: -5.39278030395508 ua   y: -0.80569893121719 ua    z: 0.12433231621981 ua
                          sx: 0.00101928401273 ua/d sy: -0.00711646955460 ua/d sz: 0.00000592146216 ua/d

from https://ssd.jpl.nasa.gov/horizons.cgi ( Nasa Horizons. used Jupiter Center and Sun  Center)
  Jupiter     jd2405730.5 (A.D. 1874-Jul-26 00:00:00.0000 TDB )  
                           X =-5.392781657192185E+00 Y =-8.056977928944861E-01 Z = 1.243323711175276E-01
                           VX= 1.019554845438113E-03 VY=-7.116569436623840E-03 VZ= 5.921987272194194E-06

















![screenshot](screenshotTestVSOP2013.png)
