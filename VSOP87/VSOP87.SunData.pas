Unit VSOP87.SunData;  //---- Sun coordinates using 2 methods ()Hi and Lo ) --->>
//-------------------//                                                        \\
// VSOP 87 calculates Heliocentric coordinates using the Sun baricenter.        \\
// Heliocentric coordinates of the Sun, of course, are 0,0,0                     \\
// This object uses VSOP87 Earth coordinates to calculate Sun's geocentric coords >>
// There is also a Lo precision simplified method from AA                        //
//  programmed by Omar                                                          //
//  repository: github.com/omarreis/VSOP2013                                   //
//---------------------------------------------------------------------------->>

interface

uses
  System.SysUtils, System.DateUtils, System.Classes, System.Math,

  StarData,           // base of celestial object hierarchy
  VSOP87.PlanetDef,  // coefficient definitions
  Om.DeltaT,
  Om.Trigonometry,
  Om.AstronomicalAlgorithms;  // date fns

type
  TSun  = Class(TCelObjBase)     // TCelObjBase in StarData.pas
  private
    fx,fy,fz:double;            // geocentric cartesian coordinates
    fUseHighPrecision:boolean;  // use hi or Lo precision

    Procedure calcCoordinatesLo(const aDia,aMes,aAno,aHora:Double);
    Procedure calcCoordinatesHi(const aDia,aMes,aAno,aHora:Double);
  public
    // fParallax:double;          // parallax in altitude
    ET:Double;      {Equacao do tempo}
    fSD:Double;     {Semidiametro}
    fR:Double;      {distancia do Sol a Terra em AU}

    constructor Create;
    destructor  Destroy; override;
    Procedure   CalcCoordinates(const aDia,aMes,aAno,aHora:Double);  override;
    Procedure   GetObjectData(SL:TStrings);    override;
  end;

  TPtoVernal=Class(TCelObjBase)   // Aries coordinates
  protected
  public
    Constructor Create;
    Procedure   CalcCoordinates(const aDia,aMes,aAno,aHora:Double);  override;
  end;

var
  Sun:TSun=nil;
  Aries:TPtoVernal=nil;

Procedure CreateSun;
Procedure FreeSun;


implementation //----------------------------------------------------------------\\//>>

uses
  VSOP87.Earth;   // planet coeficients

constructor TSun.Create;
begin
  inherited Create('Sun');
  fAltitudeOfObjectAtRise := -0.8333; {AA pag.98}
  fUseHighPrecision      := TRUE;   // use VSOP to calculate high precision Sun positions
  fMagnitude := -26.5;
end;

destructor  TSun.Destroy;
begin
  inherited Destroy;
end;

{Calculo de alta precisao, conforme teoria VSOP 87 - AA}
Procedure  TSun.calcCoordinatesHi(const aDia,aMes,aAno,aHora:Double);
var
  T,T2:Double;     {Posicao de planetas pag. 207, capitulo 31}
  L,B,R,                  {Coords do planeta}
  Lo,Bo,Ro:Double;        {Coords da terra}
  Delta,Tau,x2,y2:Double;
  Lamb,Beta,Teta:Double;
  e,Pi_,DLamb,DBeta:Double;
  Eps,DeltaT:Double;
  // nutation
  DPhy,DEps:Double;
  N:Double; {para calculo da ET}
  ax,ay,g:double;
const
   Kapa=20.49552; {Constante de aberracao}

  Procedure CalculaTau;    // uses L,B,R, calculates fx,fy,fz, Delta, Tau
  begin
    L:=0; B:=0; R:=0;      // Heliocentric coordinates of the Sun are all zero
    // calcula coords geocentricas do Sol
    fx := R*Cosg(B)*Cosg(L)-Ro*Cosg(Bo)*Cosg(Lo);  {formula 32.1}
    fy := R*Cosg(B)*Sing(L)-Ro*Cosg(Bo)*Sing(Lo);
    fz := R*Sing(B)        -Ro*Sing(Bo);
    x2 := fx*fx; y2:=fy*fy;
    Delta := Sqrt(x2+y2+fz*fz);      {dist da terra ao astro 32.3}
    Tau   := 0.0057755183*Delta;     {Tau=tempo que a luz leva pra chegar do planeta ate a terra}
  end;

begin     { calcCoordinatesHi }
  T := TJ2000(aAno,aMes,aDia,aHora);  // aAno,aMes,aDia,aHora em UT. Retorna T em seculos desde 2000
  DeltaT := calcDeltaT(T);           // UT to TD
  T:=T+DeltaT*SegToSec;             // converte T de UT para TD, usado nos calculos. SegToSec = seg/seculo

  T2 := T*T; // T3 := T2*T;
  CalcCoordinatesVSOP87(T, @CEarth, Lo,Bo,Ro);   // calc Earth coordinates
  CalculaTau;                                  // ..calcula do planeta e tempo de viagem da luz

  Lamb := Atan2(fy,fx);      {Calc lat e long}
  Beta := ATan2(fz,Sqrt(x2+y2));

  // calc aberration correction
  CalcSunTrueLongitude(T,Teta);
  e     := 0.016708617-0.00004237*T-0.0000001236*T2;
  Pi_   := 102.93735+0.71953*T+0.00046*T2;
  DLamb := (-Kapa*Cosg(Teta-Lamb)+e*kapa*Cosg(Pi_-Lamb))/Cosg(Beta);
  DBeta := -Kapa*Sing(Beta)*(Sing(Teta-Lamb)-e*Sing(Pi_-Lamb));

  Lamb := Lamb+DLamb/3600;    {aplica Correcao pela aberracao}
  Beta := Beta+DBeta/3600;

  CorrNut(T,Eps,DPhy,DEps); {Calc Correcao para Nutacao e obliquidade}

  Lamb := Lamb+DPhy/3600;     {Corrige lamb p/ nutacao p.213}

  ay  := (Sing(Lamb)*Cosg(Eps)-Tang(Beta)*Sing(Eps));
  ax  := Cosg(Lamb);
  fRA := Atan2(ay,ax);     {12.3 pag. 89}

  fDecl := Sing(Beta)*Cosg(Eps)+Cosg(Beta)*Sing(Eps)*Sing(Lamb);
  fDecl := ASing(fDecl); {12.4}
  // Calc ET to maintain compat with Lo precision
  N := JD(aAno,aMes,aDia,aHora)-2451545.0;  //serve p/ calc da ET e SD
  L := 280.460+0.9856474*N;
  AjustaAngulo(L);      //Poe angulo no range 0..360
  // Eq of time
  ET := (L-fRA)*4.0;
  ET := ET/60;           // Minutes --> Hours
  if ET>12 then         //was: ET:=ET-12;
     ET:=ET-24;        //coloca ET no range -12..+12
  // calc Semidiametro do Sol
  g   := 357.528+0.9856003 * N;     // g=Mean Anomaly
  AjustaAngulo(g);               // Put g in the range 0° to 360° by adding multiples of 360°.
  // R=Distance of Sun - Earth, in au:
  fR  :=  1.00014 - 0.01671 * cosg(g) - 0.00014 * cosg (2*g);
  fSD := 0.2666/fR;          // Semi diameter fSD in degrees
end; {calcCoordinatesHi}

// Calc AR, Decl, GHA e ET - Astronomical Almanac - C24
Procedure TSun.calcCoordinatesLo(const aDia,aMes,aAno,aHora:Double);
var N,L,g,Lamb,Eps,Alfa:Double;
begin
  N := JD(aAno,aMes,aDia,aHora)-2451545.0;
  L := 280.460+0.9856474*N;
  g := 357.528+0.9856003*N;
  AjustaAngulo(L);                {Poe angulo no range 0..360}
  AjustaAngulo(g);
  Lamb := L+1.915*Sing(g)+0.020*Sing(2*g);
  Eps  := 23.439-0.0000004*N;
  Alfa := ATang(Cosg(Eps)*Tang(Lamb));
  if AngleQuadrant(Alfa)<>AngleQuadrant(Lamb) then
    Alfa := Alfa+180;           // Gira alfa p/ o mesmo quadrante de Lamb
  AjustaAngulo(Alfa);         // Poe Alfa no range 0-360
  fRA   := Alfa;               // Ascensao reta
  fDecl := ASing(Sing(Eps)*Sing(Lamb));
  // Equation of time
  ET := (L-Alfa)*4.0;
  ET := ET/60;     // Min --> Horas
  if (ET>12) then  // bar/05 - bug grave era: ET:=ET-12;
     ET:=ET-24;    // coloca ET no range -12..+12
  // calc Semidiametro do Sol
  g := 357.528+0.9856003 * N;     //g=Mean Anomaly
  AjustaAngulo(g);                // Put g in the range 0° to 360° by adding multiples of 360°.
  // R=Distance of Sun from Earth, in au:
  fR  := 1.00014 - 0.01671 * cosg(g) - 0.00014 * cosg (2*g);
  fSD := 0.2666/fR;        //fSD em graus
end;

Procedure TSun.CalcCoordinates(const aDia,aMes,aAno,aHora:Double); // calc fRA, fDecl, fGHA
begin
  if fUseHighPrecision then calcCoordinatesHi(aDia,aMes,aAno,aHora)
    else calcCoordinatesLo(aDia,aMes,aAno,aHora);
end;

Procedure  TSun.GetObjectData(SL:TStrings);
var aSHA,aDummy:Double;
begin
  SL.Add(Name);
  SL.Add('');
  SL.Add('at time='+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fGMT) +' UT' );     // GMT = Universal Time
  aSHA := 360.0 - fRA;                     // SHA and RA are the same thing, but with a different convention
  SL.Add('SHA= '+  floatToGMSD(aSHA)+ ' ('+ R2GMD(aSHA,aDummy,' -')  +')' );

  SL.Add('RA=  '+  floatToGMSD(fRA) +' ( '+ floatToHHMMSS(fRA*24/360)+')' );   // degrees (hours)
  SL.Add('Decl='+  floatToGMSD_Lat(fDecl) + ' ('+ R2GMD(fDecl,aDummy,'NS')  +')' );

  SL.Add('GHA= '+  floatToGMSD(fGHA)      + ' ('+ R2GMD(fGHA,aDummy,' -')  +')' );

  SL.Add('Mag= '+  Format('%5.2f',[fMagnitude]) );
end;

{ TPtoVernal }

constructor TPtoVernal.Create;
begin
  inherited Create('Aries *');
  fAltitudeOfObjectAtRise := 0; {para estrelas e planetas -Mudar p/ Sol e Lua - AA.pag 98}
  fRA   := 0.0;
  fDecl := 0.0;
end;

procedure TPtoVernal.CalcCoordinates(const aDia, aMes, aAno, aHora: Double);
begin  // Vernal equinox always at RA=0
  // fRA   := 0.0;
  // fDecl : =0.0;
end;

Procedure CreateSun;
begin
  Sun   := TSun.Create;
  Aries := TPtoVernal.Create;
end;

Procedure FreeSun;
begin
  Sun.Free;
  Aries.Free;
end;


end.

