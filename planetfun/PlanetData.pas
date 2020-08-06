unit PlanetData;  // PlanetFun planet data & date utils
 //--------------//
//   github.com/omarreis/vsop2013/planetfun
//
//   History:
//     jul20: Om: v1.0
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

  AUtoKm=149598000.0;   // 1 AU = 149.598.000 Km   (~ 150M km)
  // 1 AU in KM = 149,598,000 kilometers . From https://www.universetoday.com/41974/1-au-in-km/

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
  PLANET_DATA:Array[0..NUM_OBJS-1] of TpfRec=(      // planet sizes
    (name: 'Sun'    ; radius:696340; mass: 0; rotPer:0;    revPer:0      ; Obliq: 0.0),   // 0
    (name: 'Mercury'; radius:2439.7; mass: 0; rotPer:58.6; revPer:87.97  ; Obliq: 0.1),   // 1
    (name: 'Venus'  ; radius:6051.8; mass: 0; rotPer:243 ; revPer:224.7  ; Obliq: 177.4), // 2
    (name: 'Earth'  ; radius:6378.1; mass: 0; rotPer:0.99; revPer:365.26 ; Obliq: 23.45), // 3
    (name: 'Mars'   ; radius:3396.2; mass: 0; rotPer:1.03; revPer:686.67 ; Obliq: 25.19), // 4
    (name: 'Jupiter'; radius:71492 ; mass: 0; rotPer:0.41; revPer:4331.86; Obliq: 3.12),  // 5
    (name: 'Saturn' ; radius:60268 ; mass: 0; rotPer:0.45; revPer:10760.3; Obliq: 26.73), // 6
    (name: 'Uranus' ; radius:25559 ; mass: 0; rotPer:0.72; revPer:30684  ; Obliq: 97.86), // 7
    (name: 'Neptune'; radius:24764 ; mass: 0; rotPer:0.67; revPer:60189  ; Obliq: 29.56), // 8
    (name: 'Pluto'  ; radius:1195  ; mass: 0; rotPer:6.39; revPer:90797  ; Obliq: 16.11), // 9
    (name: 'Moon'   ; radius:1737.1; mass: 0; rotPer:28  ; revPer:28     ; Obliq: 0) );   // 10

// date utils
Function JD(Y,M,D,UT:Double):Double;  // encode Julian date
Function JulianToGregorianDate(const JD:Double):TDatetime;


implementation  // --------------------------------------------


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
