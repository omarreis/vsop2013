unit PlanetData;  // PlanetFun planet data & date utils -------------//
 //--------------//                                                 //
//   github.com/omarreis/vsop2013/                                 //
//                                                                //
//   History:                                                    //
//     jul20: Om: v1.0                                          //
//-------------------------------------------------------------//

interface

uses
  System.DateUtils;

// Planet equatorial radius in Km
// from: https://www.smartconversion.com/otherinfo/Equatorial_Radius_of_planets_and_the_sun.aspx
// Source: Nasa
// 0    Sun     696340
// 1 	Mercury 2439.7
// 2 	Venus 	6051.8
// 3 	Earth 	6378.1
// 4 	Mars 	3396.2
// 5	Jupiter 71492
// 6	Saturn 	60268
// 7	Uranus 	25559
// 8	Neptune 24764
// 9  	Pluto 	1195

// 10	Moon 	1738.1

// from https://www.exploratorium.edu/ronh/age/
// Planet   Rotation Per.   Revolution Period
// Mercury  58.6 days 	    87.97   days
// Venus    243  days 	    224.7   days
// Earth    0.99 days 	    365.26  days
// Mars     1.03 days 	    686.67  days  ( 1.88   years )
// Jupiter  0.41 days 	    4331.86 days  ( 11.86  years )
// Saturn   0.45 days 	    10760.3 days  ( 29.46  years )
// Uranus   0.72 days 	    30684   days  ( 84.01  years )
// Neptune  0.67 days 	    60189   days  ( 164.79 years )
// Pluto    6.39 days 	    90797   days  ( 248.59 years )

const
  NUM_PLANETS=9;            // Use same planets as vsop2013.pas
  NUM_OBJS=NUM_PLANETS+2;   // Sun is included as planet # zero. Moon also included

  // Astronomical Unit is the mean distance between Sun and Earth
  //AUtoKm=149598000.0;   // 1 AU = 149.598.000 Km   (~ 150M km)
  AUtoM =149597870700;   // since 2012, according to https://en.wikipedia.org/wiki/Astronomical_unit
  AUtoKm=AUtoM/1000;     // 1 AU = 149.598.787 Km   (~ 150M km)
  // 1 AU in KM = 149,598,000 kilometers . From https://www.universetoday.com/41974/1-au-in-km/
  Day2Sec=24.0*3600;
  // convert m/s^2 --> AU/day^2
  M_S2toAU_day2=Day2Sec*Day2Sec/AUtoM;   // M_S2toAU_day2 ~ 0.049900175484
type
  TpfRec=record      //yet another planet data record
    name:String;     // 'Jupiter'
    radius:Double;   // in km
    mass:Double;     // in tons ?
    rotPer:Double;   // rotation period in days
    revPer:Double;   // revolution period in days (aka "year" length )
    Obliq:Double;    // Obliquity in degrees (from http://solarviews.com/cap/misc/obliquity.htm)
  end;

const   // approximate values..
  UnivGravConst = 6.67408E-11;  //  6.67408E-11 N . m^2/Kg^2 Universal Gravitational Constant ( Newton invented that ! )
  EarthRadius = 6378.1;
  
  PLANET_DATA: Array[0..NUM_OBJS-1] of TpfRec=(       // planet sizes
    (name: 'Sun'    ; radius:696340; mass:1.98847e+30; rotPer:0;    revPer:0      ; Obliq: 0.0),   // 0
    (name: 'Mercury'; radius:2439.7; mass:3.30100e+23; rotPer:58.6; revPer:87.97  ; Obliq: 0.1),   // 1
    (name: 'Venus'  ; radius:6051.8; mass:4.13800e+24; rotPer:243 ; revPer:224.7  ; Obliq: 177.4), // 2
    (name: 'Earth'  ; radius:6378.1; mass:5.97200e+24; rotPer:0.99; revPer:365.26 ; Obliq: 23.45), // 3
    (name: 'Mars'   ; radius:3396.2; mass:6.42730e+23; rotPer:1.03; revPer:686.67 ; Obliq: 25.19), // 4
    (name: 'Jupiter'; radius:71492 ; mass:1.89852e+27; rotPer:0.41; revPer:4331.86; Obliq: 3.12),  // 5
    (name: 'Saturn' ; radius:60268 ; mass:5.68460e+26; rotPer:0.45; revPer:10760.3; Obliq: 26.73), // 6
    (name: 'Uranus' ; radius:25559 ; mass:8.68190e+25; rotPer:0.72; revPer:30684  ; Obliq: 97.86), // 7
    (name: 'Neptune'; radius:24764 ; mass:1.02431e+26; rotPer:0.67; revPer:60189  ; Obliq: 29.56), // 8
    (name: 'Pluto'  ; radius:1195  ; mass:1.47100e+22; rotPer:6.39; revPer:90797  ; Obliq: 16.11), // 9
    (name: 'Moon'   ; radius:1737.1; mass:7.34600e+22; rotPer:28  ; revPer:28     ; Obliq: 0) );   // 10

// date utils
Function JD(Y,M,D,UT:Double):Double;  // encode Julian date
Function JulianToGregorianDate(const JD:Double):TDatetime;


implementation  // --------------------------------------------
// from  https://en.wikipedia.org/wiki/Planetary_mass ( in Kg )
// IAU best estimates (2009) ( DE405 for Sun and Earth )
// Sun      1.98847e+30    from https://en.wikipedia.org/wiki/Solar_mass
// Mercury  3.30100e+23
// Venus    4.13800e+24
// Earth    5.97200e+24
// Mars    	6.42730e+23
// Jupiter  1.89852e+27
// Saturn 	5.68460e+26
// Uranus  	8.68190e+25
// Neptune  1.02431e+26
// Pluto    1.47100e+22    [Pluto and Sharon system]
// Moon     7.34600e+22

// Planet masses in Kg from : https://www.smartconversion.com/otherInfo/Mass_of_planets_and_the_Sun.aspx
// Sun 	    1.9891e+30  ( in Kg )
// Mercury  3.3022e+23
// Venus 	  4.8685e+24
// Earth 	  5.9736e+24
// Mars 	  6.4185e+23
// Jupiter  1.8986e+27
// Saturn   5.6846e+26
// Uranus   8.6810e+25
// Neptune  10.243e+25
// Pluto 	  1.2500e+22
// Moon 	  7.3490e+22

// Some date utils
// from astronomical algorithms - J. Meeus
Function JD(Y,M,D,UT:Double):Double;  // encode Julian date
var A,B:double;
begin
  if (M<=2) then
    begin
      Y := Y-1;
      M := M+12;
    end;
  A := Int(Y/100);
  B := 2-A+Int(A/4);
  Result := Int(365.25*(Y+4716))+Int(30.6001*(M+1))+D+B-1524.5+UT/24;
end;

// Julian number to Gregorian Date. Astronomical Algorithms - J. Meeus
Function JulianToGregorianDate(const JD:Double):TDatetime;
var A,B,F:Double; alpha,C,E:integer; D,Z:longint; dd,mm,yy:word;
begin
  Z := trunc(JD + 0.5);
  F := (JD + 0.5) - Z;
  if (Z<2299161.0) then A:=Z
    else begin
      alpha := trunc( (Z-1867216.25)/36524.25);
      A := Z+1+alpha-(alpha div 4);
    end;
  B := A + 1524;
  C := trunc( (B - 122.1) / 365.25);
  D := trunc( 365.25 * C);
  E := trunc((B - D) / 30.6001);
  dd := Trunc(B - D - int(30.6001 * E) + F);
  if (E<14) then mm := E - 1
    else mm := E - 13;
  if mm > 2 then yy := C - 4716
    else yy := C - 4715;

  Result := EncodeDatetime(yy,mm,dd,0,0,0,0);   // ignore time
end;

end.
