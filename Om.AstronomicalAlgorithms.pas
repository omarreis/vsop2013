unit Om.AstronomicalAlgorithms;   //--implementation of Astronomical Algorithms ---//
 //------------------------------//                                               //
// from excelent book "Astronomical Algorithms" by Jean Meeus                    //
// most formulas come from first edition ( some corrected w/ the second ed)     //
// AA page references in code are from the 1st ed (1991)                       //
// mostly programmed by oMAR                                                  //
//      repository: github.com/omarreis/VSOP2013                             //
// AfC refers to USNO publication Almanac for Computers                     //
// see github.com/omarreis/vsop2013                                        //
// -----------------------------------------------------------------------//

interface

uses
  System.SysUtils;

const
  SegToSec = (1/(86400.0*36525.0));   // seconds to centuries conversion  ( 86400s = 24h*3600s/h )


// date utils
Function JD(const Y,M,D,UT:Double):Double;                // encode Julian date from UT
Function JDtoDatetime(const JD:Double):TDatetime;
Function DatetimeToJD(const D:TDatetime):Double;
Function TJ2000(const K,M,I,UT:Double):Double;            // Time in centuries since  J2000.0

// mar/23: UTCtoTDB() is a work in progress. Not ready for use !!
function UTCtoTDB(const aUTC:TDatetime):Double;     // Universal Time UTC to TDB Time Dynamical Baricentric

// Sidereal time calculation
Procedure SiderealTime(D,M,A,H:Double;{out:} var GMST,GAST:Double);     {AA pag.83}

// Nutation. T in centuries since j2000
Procedure NutationCorrection(const T,aRAi,aDecli:Double; {out:} var DAlfaNu,DDeltaNu:Double);

// Nutation correction. the Nutella correction :)
Procedure CorrNut(const T:Double; var Eps,DPhy,DEps:Double);

// zenital
Procedure geoPositionToCelestial(aDay,aMonth,aYear:word; const aGMTTime,aLat,aLon:double;{out:} var aRA,aDecl:double);

// celestial coordinates (RA,Decl) of Greenwitch apparent position
Procedure GreenwitchToCelestial(const aUT:TDatetime; {out:} var aRA,aDecl:double);

implementation //--------------------------------------

uses
  Om.Trigonometry,
  Om.DeltaT,
  System.Math;


// Nutation correction ( aka: the Nutella correction :)
Procedure CorrNut(const T:Double; var Eps,DPhy,DEps:Double);
var Omega, L,Ll,T2,T3,Eps0:Double;
begin     {Nutacao e obliquidade da ecliptica Ast.Alg. pag. 132}
  T2    := T*T;  T3 := T*T2;
  Omega := 125.04452-1934.136261*T;
  L     := 280.4665 + 36000.7698*T;
  Ll    := 218.3165 +481267.8813*T;
  {nas formulas da pag 132, DPhy e DEps em " de grau, Eps0 em graus}
  DPhy := -17.2*Sing(Omega)-1.32*sing(2*L)-0.23*Sing(2*Ll)+0.21*Sing(2*Omega);
  DEps :=   9.2*cosg(Omega)+0.57*cosg(2*L)+0.10*Cosg(2*Ll)-0.09*Cosg(2*Omega);
  Eps0 :=  23.4392911+(-46.8150*T-0.00059*T2+0.001813*T3)/3600;                 {21.2}
  Eps  :=  Eps0+DEps/3600;
end;

Procedure PrecessionCorrction(const T,aRAi,aDecli:Double; var DAlfaPre,DDeltaPre:Double);
var  {T=Tempo em seculos desde j2000.0 - calcule com TJ2000() }
  m,n:Double;
  DAlfaP,DDeltaP:Double; {Correcao anual por precessao}
  NAnos:Double;
begin  {Efeito da precessao - Extraido de Astronomical Algorithms-J. Meeus p. 124}
  NAnos := T*100.0;             {Numero de anos desde J2000.0}

  m := (3.07496+0.00186*T)*15;  {*15 converte de seg p/ " }
  n := 20.0431-0.0085*T;

  DAlfaP  := m+n*Sing(aRAi)*Tang(aDecli);  {Deltas em " de arco -  formula 20.1}
  DDeltaP := n*Cosg(aRAi);                 {Esses valores sao anuais}

{  WriteLn('DA P:',DAlfaP:8:4);
   WriteLn ('DD P:',DDeltaP:8:4);}

  DAlfaPre  := DAlfaP*NAnos; {Converte em valores absolutos, multiplicando por NAnos}
  DDeltaPre := DDEltaP*NAnos;

{  WriteLn('DA Pre:',DAlfaPre:8:4);
   WriteLn ('DD Pre:',DDeltaPre:8:4);}
end;

// T in centuries since j2000
Procedure NutationCorrection(const T,aRAi,aDecli:Double; {out:} var DAlfaNu,DDeltaNu:Double);
var DPhy,DEps,Eps,TDi,SEps,CEps,SA,CA:Double;
begin
  CorrNut(T,Eps,DPhy,DEps);
  SEps := Sing(Eps);
  CEps := Cosg(Eps);
  SA := Sing(aRAi);   CA := Cosg(aRAi);   TDi := Tang(aDecli);  //memoise trigs
  DAlfaNu  := (CEps+SEps*SA*TDi)*DPhy-(CA*TDi)*DEps;            {formula 22.1 pag.139 Ast.Alg}
  DDeltaNu := (SEps*CA)*DPhy+SA*DEps;
end;

Procedure CalculaSunTrueLongitude(const T:Double; var Teta:Double);
var L0,C,T2,T3,M:Double;
begin                      {Calculo da Long Verd. do Sol}
  T2 := T*T; T3 := T*T2;
  L0 := 280.46645+36000.76983*T+0.0003032*T2;            {FORMULA 24.2}
  M  := 357.5291+35999.0503*T-0.0001559*T2-0.00000048*T3; {24.4}
  C  := +(1.9146-0.004817*T-0.000014*T2)*Sing(M)+(0.019993-0.000101*T)*Sing(2.0*M)+0.000290*Sing(3.0*M);
  Teta := L0+C;
end;

Procedure AberrationCorrection(const T,aRAi,aDecli:Double; var DAlfaAbe,DDeltaAbe:Double);
var Teta,e,Pi_,T2,T3:Double; {Teta=Sun True Longitude}
    CA,SA,Eps0,CEp,STt,CTt,SDl,CDl,SPi,CPi,K1,K2:Double; {Vars auxiliares}
    // Omega:Double;
const  Kapa=20.49552; {Constante de aberracao}

begin {Efeito da aberracao}
  T2    := T*T;  T3 := T*T2;
  // Omega := 125.04452-1934.136261*T;
  Eps0  := 23.4392911+(-46.8150*T-0.00059*T2+0.001813*T3)/3600;
  CalculaSunTrueLongitude(T,Teta);
  e     := 0.016708617-0.00004237*T-0.0000001236*T2;
  Pi_   := 102.93735+0.71953*T+0.00046*T2;
  { memoise trigs }
  CA :=Cosg(aRAi);   SA :=Sing(aRAi);
  CTt:=Cosg(Teta);   STt:=Sing(Teta);
  CEp:=Cosg(Eps0);   //SEp:=Sing(Eps0);
  CDl:=Cosg(aDecli); SDl:=Sing(aDecli);
  CPi:=Cosg(Pi_);    SPi:=Sing(Pi_);

  DAlfaAbe := -Kapa*((CA*CTt*CEp+SA*STt)/CDl) + e*Kapa*((CA*CPi*CEp+SA*SPi)/CDl);
  k1       := CEp*(Tang(Eps0)*CDl-SA*SDl); K2:=CA*SDl;
  DDeltaAbe:= -Kapa*(CTt*K1+K2*STt)+e*Kapa*CPi*K1+K2*SPi;
end;

// JD - Julian Day - Astro Algorithms J.Meeus, pg 61 formula 7.1
// Implementada em Jul/04 para ter maior validade que a do Alm For Computers
// alguns usuarios reclamaram que a formula acima não funciona para 1800 !
Function JD(const Y,M,D,UT:Double):Double;
var A,B,aM,aY:double;
begin
  aM := M;
  aY := Y;
  if (aM<=2) then
    begin
      aY := aY-1;
      aM := aM+12;
    end;
  A := Int(aY/100);
  B := 2-A+Int(A/4); //Gregoriano
  //B:=0;           //Juliano
  Result := Int(365.25*(aY+4716))+Int(30.6001*(aM+1))+D+B-1524.5+UT/24;
end;

// TJ2000 in seculae since 1-jan-2000 12 UT, using years of 365.25 days
Function TJ2000(const K,M,I,UT:Double):Double; // returns time in centuries since J2000.0
begin
  TJ2000 := (JD(K,M,I,UT)-2451545.0)/36525.0;       // 2451545.0 = JD2000
end;

// function UTCtoJD(const aUTC:TDatetime):Double;  // Universal Time UTC to Julian Date
// var ay,am,ad:word; H:Double;
// begin
//   H := Frac(aUTC)*24;          // hour 0..24
//   DecodeDate(Trunc(aUTC),{out:} ay,am,ad );
//   Result := JD(ay,am,ad,H);
// end;

// mar/23 - my first experience with chatgpt: nice work.. I guess..
// Define a function that estimates TDB - UTC in seconds
// function tdb_utc_diff(utc):
//   // Convert UTC to Julian date using some library or algorithm
//   jd = utc_to_jd(utc)
//   // Define some constants
//   T0 = 2451545 // JD2000
//   TAI_TT = 32.184 // difference between TAI and TT in seconds
//   leap_seconds = 37 // difference between TAI and UTC in seconds as of March 13, 2023
//   offset = 69.184 // constant offset in seconds
//   // Calculate L and g using formulas from https://lweb.cfa.harvard.edu/~jzhao/times.html
//   L = (1.657e-3 * sin(6283.07585 * (jd - T0) + 6.24006)) + (6.6e-4 * sin(6283.07585 * (jd - T0) /2 +4))
//   g = radians((357.5277233 +35999*(jd-T0)) %360)
//   // Estimate TDB - UTC using formula from https://www.timeanddate.com/time/terrestrial-dynamic-time.html
//   tdb_utc = offset + TAI_TT + leap_seconds + L * sin(g)
//   return tdb_utc
// Hum.. not so good.


// TDB is difficult to calculate. There are some simplified formulas, good to a point ( like the one below )
// However difference between TDB and TT ( former TDT ) is small enough to be ignored by some applications.
// TDB relates to relativistic corrections ( a clock on the solar system baricenter, near the Sun,
// runs differently from one in Paris.

// from Wikipedia and https://www.timeanddate.com/time/terrestrial-dynamic-time.html
function UTCtoTDB(const aUTC:TDatetime):Double;  // Universal Time UTC to TDB Time Dynamical Baricentric // work in progress. Do not use..
var ajd,T0,TAI_TT,leap_seconds,offset,L,g,aDelta,aJD_T0:Double;
begin
  // Convert UTC to Julian date using some library or algorithm
  //  jd = utc_to_jd(utc)
  ajd := DatetimeToJD(aUTC);
  // Define some constants
  T0      := 2451545;                 // JD2000
  TAI_TT  := 32.184;                  // difference between TAI and TT in seconds
  leap_seconds := 37;                 // difference between TAI and UTC in seconds as of March 13, 2023
  offset  := 69.184;                  // constant offset in seconds
  // Calculate L and g using formulas from https://lweb.cfa.harvard.edu/~jzhao/times.html
  aJD_T0 := ajd - T0;                 // memoise frequently used result for the formula below
  L := (1.657e-3*sing(6283.07585*aJD_T0+6.24006))+(6.6e-4*sing(6283.07585*aJD_T0/2 +4));      // seconds
  g := (357.5277233 +35999*aJD_T0)/360;                                                       // degrees
  // Estimate TDB - UTC using formula from
  //   https://www.timeanddate.com/time/terrestrial-dynamic-time.html
  aDelta := offset + TAI_TT + leap_seconds + L * sing(g);  // in secs

  Result := aUTC + aDelta/24/3600;  // correct time
end;

// function UTCtoTDB(const aUTC:TDatetime):Double;  // Universal Time UTC to TDB Time Dynamical Baricentric
// var ay,am,ad:word; H,DeltaT,g,Delta:Double; t,TT,aJD:Double;
// begin
//   H := Frac(aUTC)*24;          // hour 0..24
//   DecodeDate(Trunc(aUTC),{out:} ay,am,ad );
//   t := TJ2000(ay,am,ad,H);            // T = UT in seculae since J2000
//   DeltaT := calcDeltaT(t);           // UT to TD
//   TT  := aUTC+DeltaT/3600/24;       // to TD
//   aJD := UTCtoJD(TT);
//
//   g := 357.53 + 0.9856003*(aJD - 2451545);   //degrees.
//
//   Delta := 0.001658*sing(g) + 0.000014*sing(2*g);  // seconds
//
//   Result := TT + Delta/3600/24;
// end;

// H in hours UT
// GMST - Greenwitch Mean Sideral Time
// GAST - Greenwich Apparent Sidereal Time ( = GMST affected by nutation )
// re/turned times in hours
Procedure SiderealTime(D,M,A,H:Double;{out:} var GMST,GAST:Double); {AA pag.83}
var T,E,Eps,DPhy,DEps:Double;
begin
  T    := TJ2000(A,M,D,0);
  GMST := 24110.54841+8640184.812866*T+0.093104*T*T-0.0000062*T*T*T; {em seg, 0 UT}
  GMST := GMST/3600.0+1.00273790935*H;   {in hours}
  CorrNut(T, Eps, DPhy, DEps);           {calc Corr por nutacao}
  E    := DPhy*Cosg(Eps)/3600.0/15.0;
  GAST := GMST+E;
  AjustaHora(GAST);
  AjustaHora(GMST);
end;

// returns celestial coordinates (RA,Decl) of the zenith at position aLat,aLon
Procedure geoPositionToCelestial(aDay,aMonth,aYear:word; const aGMTTime,aLat,aLon:double; var aRA,aDecl:double);
var aGHA,aGMST,aGAST:Double;
begin
  SiderealTime(aDay,aMonth,aYear,aGMTTime,{out:} aGMST,aGAST);  //calc GAST (in hours)
  aDecl:= aLat;
  aGHA := aLon;
  aRA  := aGAST*15-aGHA;   //15 converte de horas para graus.
  AngleTo0_360(aRA);       // Ajusta o angulo colocando entre 0 e 360°
end;

// returns celestial coordinates (RA,Decl) of Greenwitch apparent geographical position
Procedure GreenwitchToCelestial(const aUT:TDatetime; {out:} var aRA,aDecl:double);  // RA and dec returns in degrees
var aGHA,aGMST,aGAST,aHour:Double; YY,MM,DD:word; D:TDatetime;
begin
  D     := Trunc( aUT );
  DecodeDate( D, {out:} YY,MM,DD);
  aHour := Frac(aUT)*24;    // in hours

  SiderealTime(DD,MM,YY,aHour,{out:} aGMST,aGAST);  //calc GAST (in hours)
  aDecl:= 0;  //
  aGHA := 0;  // greenwitch GHA=0
  // use GW apparent time  ( applies nutation to GMST )
  aRA  := aGAST*15-aGHA;   // 15 converte de horas para graus. ()
  AngleTo0_360(aRA);       // Ajusta o angulo colocando entre 0 e 360°
end;

// Some date utils
Function DatetimeToJD(const D:TDatetime):Double; // D in UT
var YY,MM,DD:Word; H:Double;
begin
  DecodeDate( Trunc(D), {out:}YY,MM,DD);
  H := Frac(D)*24;
  Result := JD(YY,MM,DD,{UT:}H  );
end;

// Julian number to Gregorian Date. Astronomical Algorithms - J. Meeus
Function JDtoDatetime(const JD:Double):TDatetime;            // convert JD to UT ( TDatetime )
var A,B,F,H:Double; alpha,C,E:integer; D,Z:longint; dd,mm,yy:word;
begin
  H := Frac(JD+0.5);        // JD starts at noon

  Z := trunc(JD + 0.5);
  F := (JD + 0.5) - Z;
  if (Z<2299161.0) then A:=Z
    else begin
      alpha := trunc( (Z-1867216.25)/36524.25 );
      A := Z+1+alpha-(alpha div 4);
    end;
  B := A + 1524;
  C := trunc( (B - 122.1) / 365.25);
  D := trunc( 365.25 * C);
  E := trunc((B - D) / 30.6001);
  dd := Trunc(B - D - int(30.6001 * E) + F);
  if (E<14) then mm:=E-1
    else mm :=E-13;
  if mm > 2 then yy := C - 4716
    else yy := C - 4715;

  Result := EncodeDate(yy,mm,dd)+ H;   // UT time
end;


end.

