unit Om.AstronomicalAlgorithms;   // implementation of
// astronomical algorithms  from like named Jean Meeus book

interface

uses
  System.SysUtils;

procedure AngleTo0_360(var A:Double); // put angle in 0..360° range
// date utils
Function JD(Y,M,D,UT:Double):Double;  // encode Julian date
Function JDtoDatetime(const JD:Double):TDatetime;
Function DatetimeToJD(const D:TDatetime):Double;
// zenital
Procedure geoPositionToCelestial(aDay,aMonth,aYear:word; const aGMTTime,aLat,aLon:double;{out:}var aRA,aDecl:double);

// returns celestial coordinates (RA,Decl) of Greenwitch apparent position
Procedure GreenwitchToCelestial(const aUT:TDatetime; {out:} var aRA,aDecl:double);

implementation //--------------------------------------

Function Sing(const G:Double):Double;  { Sin() using degrees}
begin Sing := Sin(G*Pi/180); end;

Function Cosg(const G:Double):Double;  { Cos() using degrees}
begin Cosg := Cos(G*Pi/180); end;

Function Tang(G:Double):Double;        { Tan() using degrees }
var CG:Double;
begin
  CG:=Cosg(G);
  {if CG=0.0 then CG:=1E-20;}     {= Numero bem pequeno}
  Tang:=Sing(G)/CG;
end;

// Nutation correction
Procedure CorrNut( T:Double; var Eps,DPhy,DEps:Double);
var
  Omega:Double;
  L,Ll:Double;
  T2,T3:Double; Eps0:Double;
begin            {Nutacao e obliquidade da ecliptica Ast.Alg. pag. 132}
  T2 := T*T; T3 := T*T2;
  Omega := 125.04452-1934.136261*T;
  L     := 280.4665 + 36000.7698*T;
  Ll    := 218.3165 +481267.8813*T;
  {nas formulas da pag 132, DPhy e DEps em " de grau, Eps0 em graus}
  DPhy:=-17.2*Sing(Omega)-1.32*sing(2*L)-0.23*Sing(2*Ll)+0.21*Sing(2*Omega);
  DEps:=  9.2*cosg(Omega)+0.57*cosg(2*L)+0.10*Cosg(2*Ll)-0.09*Cosg(2*Omega);
  Eps0:= 23.4392911+(-46.8150*T-0.00059*T2+0.001813*T3)/3600;    {21.2}
  Eps := Eps0+DEps/3600;
end;

{JD - Julian Day - Astro Algorithms J.Meeus, pg 61 formula 7.1 }
{Implementada em Jul/04 para ter maior validade que a do Alm For Comp }
{alguns usuarios reclamaram que a formula acima não funciona para 1800 ! }
Function JD(Y,M,D,UT:Double):Double;
var A,B:double;
begin
  if (M<=2) then
    begin
      Y:=Y-1;
      M:=M+12;
    end;
  A:=Int(Y/100);
  B:=2-A+Int(A/4); //Gregoriano
  //B:=0;          //Juliano
  Result := Int(365.25*(Y+4716))+Int(30.6001*(M+1))+D+B-1524.5+UT/24;
end;

Function TJ2000(K,M,I,UT:Double):Double; {Time in centuries since  J2000.0}
begin
  TJ2000:=(JD(K,M,I,UT)-2451545.0)/36525.0;
end;

Function HourTo0_24(const H:Double):Double; //put H in 0- 24h range
begin
  Result := H;
  if (Result<0)        then Result := Result+24
  else if (Result>=24) then Result := Result-24;
end;

procedure AngleTo0_360(var A:Double); // put angle in 0..360° range
begin
  while (A<0)      do A:=A+360.0;
  while (A>=360.0) do A:=A-360.0;
end;

// H in hours UT
// GMST - Greenwitch Mean Sideral Time
// GAST - Greenwich Apparent Sidereal Time ( = GMST affected by nutation )
// returned times in hours
Procedure SiderealTime(D,M,A,H:Double;{out:} var GMST,GAST:Double); {AA pag.83}
var T,E,Eps,DPhy,DEps:Double;
begin
  T    := TJ2000(A,M,D,0);
  GMST := 24110.54841+8640184.812866*T+0.093104*T*T-0.0000062*T*T*T; {em seg, 0 UT}
  GMST := GMST/3600.0+1.00273790935*H;   {em horas}
  CorrNut(T, Eps, DPhy, DEps);           {calc Corr por nutacao}
  E    := DPhy*Cosg(Eps)/3600.0/15.0;
  GAST := GMST+E;
  GAST := HourTo0_24(GAST);
  GMST := HourTo0_24(GMST);
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

// returns celestial coordinates (RA,Decl) of Greenwitch apparent position
Procedure GreenwitchToCelestial(const aUT:TDatetime; {out:} var aRA,aDecl:double);  // RA and dec returns in degrees
var aGHA,aGMST,aGAST,aHour:Double; YY,MM,DD:word; D:TDatetime;
begin
  D     := Trunc( aUT );
  DecodeDate( D, {out:} YY,MM,DD);
  aHour := Frac(aUT)*24;        // in hours

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
Function JDtoDatetime(const JD:Double):TDatetime;  // convert JD to UT ( TDatetime )
var A,B,F,H:Double; alpha,C,E:integer; D,Z:longint; dd,mm,yy:word;
begin
  H := Frac(JD+0.5);    // JD zeroes at noon  ( go figure... )

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

  Result := EncodeDate(yy,mm,dd)+ H;   // time
end;


end.
