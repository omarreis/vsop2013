Unit VSOP87.Planet;    //---- Planet coordinates using VSOP87 ------------------------------\
//--------------------//                                                                     \
// VSOP87 = Variations Seculaires des Orbites Planetaires 1987                                \
//    by Bretagnon and Francou - Bureau des Longitudes of Paris                                \
//                                                                                              \
// calculates planet's ecliptical coordinates Lon,Lat and Heliocentric Radius, given a time      \
//                                                                                                \
// This method is described in J.Meeus book "Astronomical Algorithms" - Chapter 32                 \
// used as basis for this code.                                                                     \
//                                                                                                   \
// Note that this repository also contains newer VSOP2013 theory, more precise.                       \
// VSOP2013 includes 9 planets and larger time domain.                                                 \
// This VSOP87 impklementation includes includes only the 4 navigation planets ( Mars, Venus, Jupiter and Saturn ) and Earth
// Note that VSOP87 includes 9 planets. See Meeus book.                                                /
// However VSOP2013 requires a large 100 MB table of coeficients, which makes it                      /
// difficult to use in mobile devices.                                                               /
// VSOP87 gives good enough results for most apps, using a smaller set of data coeficients.         /
// In this implementation the coeficcients are embedded as code constants,                         /
// rather than a separate file ( as in VSOP2013 )                                                 /
//                                                                                               /
// mostly programmed by Omar                                                                    /
//      repository: github.com/omarreis/VSOP2013                                               /
//--------------------------------------------------------------------------------------------/

interface

uses
  System.SysUtils, System.DateUtils, System.Classes, System.Math,

  doubleVector3D,    // TVector3D_D - 3d vector with Double components
  StarData,          // base of celestial object hierarchy
  VSOP87.SunData,    //
  VSOP87.PlanetDef,  // coefficient definitions
  Om.DeltaT,
  Om.Trigonometry,
  Om.AstronomicalAlgorithms;  // date fns

type
  // VSOP 87 solar system planets
  TPlanetVSOP87  = Class(TCelObjBase)     // TCelObjBase in StarData.pas
  private
    //intermediate results generated by CalcCoordinates()
    fx,fy,fz:double;    // vector between Planet and Earth
    fDelta:double;     // distance from Earth to obj
    fTau:double;      // light propagation time from obj to Earth
    fL,fB,fR:double; // planet heliocentric coordinates
    fPosition:TVector3D_D;     // planet heliocentric cartesian coordinates
    fUT_Tau:Double;

  public
    fParallax:double;          // parallax in altitude
    PlanRec:PPlanConsts;       // set of pointers to arrays of VSOP87 planet coefficients
    fDeltaT:double;            // TDT - UTC in seconds

    constructor Create(const aName:String; aPlanRec:PPlanConsts; aMag:single);
    destructor  Destroy; override;
    Procedure   CalcCoordinates(const aDia,aMes,aAno,aHora:Double); override;
    Procedure   GetObjectData(SL:TStrings);   override;
    // Procedure   AfterConstruction; override;
  end;

Procedure CreatePlanetsVSOP87;
function  FindPlanetVSOP87ByName(const aName:String):TPlanetVSOP87;
function  FindPlanetVSOP87ByIndex(aIndex:integer):TPlanetVSOP87;

Procedure FreePlanetsVSOP87;

const
  NumPlanetsVSOP87=7;

var
  Planetas:Array[1..NumPlanetsVSOP87] of TPlanetVSOP87;   // Mars, Venus, Jupiter and Saturn

implementation //-------------------------------------------------------------------------------------

uses
  VSOP87.Earth,      // planet coeficients
  VSOP87.Mars,
  VSOP87.Venus,
  VSOP87.Saturn,
  VSOP87.Jupiter,
  VSOP87.Mercury,
  VSOP87.Uranus,
  VSOP87.Neptune;

type
  // planet definition rec
  RPlanet=record
    Name:String;
    CPlan:PPlanConsts;    // pointer to planet coef set  ( see VSOP87.xxx units )
    Mag:single;           // Magnitude
  end;

const
  ConstPlanetas:Array[1..NumPlanetsVSOP87] of RPlanet=(
    (Name:'Venus'   ; CPlan:@CVenus   ; Mag:-4.9),   // 1
    (Name:'Saturn'  ; CPlan:@CSaturn  ; Mag:+0.5),   // 2
    (Name:'Mars'    ; CPlan:@CMars    ; Mag:+0.3),   // 3
    (Name:'Jupiter' ; CPlan:@CJupiter ; Mag:-2.9),   // 4
    (Name:'Mercury' ; CPlan:@CMercury ; Mag:-2.5),   // 5
    (Name:'Uranus'  ; CPlan:@CUranus  ; Mag:+5.4),   // 6        //from wikipedia
    (Name:'Neptune' ; CPlan:@CNeptune ; Mag:+7.7));  // 7

Procedure CreatePlanetsVSOP87;
var i:integer;
begin
  for i:=1 to NumPlanetsVSOP87 do
    with ConstPlanetas[i] do
      Planetas[i] := TPlanetVSOP87.Create(Name,CPlan,Mag);
end;

function  FindPlanetVSOP87ByIndex(aIndex:integer):TPlanetVSOP87;
begin
  if (aIndex>0) and (aIndex<=NumPlanetsVSOP87) then Result := Planetas[aIndex]
    else Result := nil;
end;


function  FindPlanetVSOP87ByName(const aName:String):TPlanetVSOP87;
var i:integer; aPlanet:TPlanetVSOP87;
begin
  Result:=nil;
  for i:=1 to NumPlanetsVSOP87 do      // linear search
    begin
      aPlanet := Planetas[i];
      if (aPlanet.Name=aName) then
        begin
          Result:=aPlanet;
          exit;
        end;
    end;
end;

Procedure FreePlanetsVSOP87;
var i:integer;
begin
  for i:=1 to NumPlanetsVSOP87 do
    begin
      Planetas[i].Free;
      Planetas[i]:=nil;
    end;
end;


{ TPlanetVSOP87 }
Constructor TPlanetVSOP87.Create(const aName:String; aPlanRec:PPlanConsts; aMag:single);
begin
  inherited Create(aName);
  PlanRec := aPlanRec;

  fMagnitude := aMag;
  fParallax  := 0;
  fPosition  := Vector3D_D(0,0,0);

  fDelta :=0;
  fDeltaT:=0;
  fUT_Tau:=0;
end;

destructor TPlanetVSOP87.Destroy;
begin
  inherited;
end;

Procedure  TPlanetVSOP87.CalcCoordinates(const aDia,aMes,aAno,aHora:Double);    // aDia,aMes,aAno,aHora in UT
var T,T2,JDE:Double;     {Posicao de planetas pag. 207, capitulo 31}
  Lo,Bo,Ro:Double;        {Earth coords}
  x2,y2:Double;
  Lamb,Beta,Teta:Double;
  e,Pi_,DLamb,DBeta:Double;
  ax,ay,aDTday:double;
  Eps:Double;
  {nutation vars}
  DPhy,DEps:Double;
const
  Kapa=20.49552; {Constante de aberracao}

  Procedure CalculaTau;  // Tau is a correction for light ray delay due to finite speed of light ( Earth <--> planet distance )
  begin
    CalcCoordinatesVSOP87(JDE,PlanRec,fL ,fB ,fR);  // vsop87 planet coordinates (Equatorial)
    fPosition.x := fR*Cosg(fB)*Cosg(fL);            // convert to cartesian
    fPosition.y := fR*Cosg(fB)*Sing(fL);
    fPosition.z := fR*Sing(fB);
    //calc geocentric coords
    fx := fPosition.x - Ro*Cosg(Bo)*Cosg(Lo);  {formula AA 32.1}
    fy := fPosition.y - Ro*Cosg(Bo)*Sing(Lo);
    fz := fPosition.z - Ro*Sing(Bo);
    x2 := fx*fx; y2:=fy*fy;
    fDelta := Sqrt(x2+y2+fz*fz);       {Dist Planet->Earth in AU -  AA 32.3 }
    fTau := 0.0057755183*fDelta;       {Tau=time it takes for  the planet light to reach Earth in days }
  end;

begin   {TPlanetVSOP87.CalcCoordinates}
  T := TJ2000(aAno,aMes,aDia,aHora);     // T in UT
  fDeltaT := calcDeltaT(T);
  T := T+fDeltaT*SegToSec;               // T in TDT

  aDTday  := fDeltaT/24/3600;          // aDTday = Delta T in days
  fTDT    := fGMT + aDTday;            // save TDT

  JDE :=T; T2 := T*T;
  CalcCoordinatesVSOP87(T,@CEarth,Lo,Bo,Ro); {Calc coords da terra e..}
  CalculaTau;                                {..calcula do planeta e tempo de viagem da luz}

  JDE := T-fTau/36525.0;           // instant when the light reaching us left the planet
  CalculaTau;                     // recalc

  fUT_Tau := fTDT-fTau;

  fParallax := 8.794/60/fDelta;  // AfC pg. B17
  if (fx<>0) then               // contempla o caso do planeta Terra  (x=y=z=0)
    begin
      Lamb := Atan2(fy,fx);     // Calc lat e long
      Beta := ATan2(fz,Sqrt(x2+y2));
    end
    else begin
      Lamb:=0; Beta:=0;
    end;

  // calc aberration
  CalcSunTrueLongitude(T,Teta);
  e   := 0.016708617-0.00004237*T-0.0000001236*T2;
  Pi_ := 102.93735+0.71953*T+0.00046*T2;
  DLamb := (-Kapa*Cosg(Teta-Lamb)+e*kapa*Cosg(Pi_-Lamb))/Cosg(Beta);
  DBeta := -Kapa*Sing(Beta)*(Sing(Teta-Lamb)-e*Sing(Pi_-Lamb));

  Lamb := Lamb+DLamb/3600; {apply aberration delta}
  Beta := Beta+DBeta/3600;

  CorrNut(T,Eps,DPhy,DEps);  {calc corr for nuitation and obliquity}
  Lamb := Lamb+DPhy/3600;      {correct lamb for Nutation - AA p.213}

  ay  := (Sing(Lamb)*Cosg(Eps)-Tang(Beta)*Sing(Eps));
  ax  := Cosg(Lamb);
  fRA := Atan2(ay,ax);     {AA 12.3 pg. 89}

  fDecl := Sing(Beta)*Cosg(Eps)+Cosg(Beta)*Sing(Eps)*Sing(Lamb);
  fDecl := ASing(fDecl);   {AA 12.4}
end;

(*  Omega:=125.04452-1934.136261*T; {Subst por CorrNut()}
  L :=280.4665+ 36000.7698*T;
  Ll:=218.3165+481267.8813*T;

  DPhy:=-17.2*Sing(Omega)-1.32*sing(2*L)-0.23*Sing(2*Ll)+0.21*Sing(2*Omega);
  DEps:=  9.2*cosg(Omega)+0.57*cosg(2*L)+0.10*Cosg(2*Ll)-0.09*Cosg(2*Omega);
  Eps0:= 23.4392911+(-46.8150*T-0.00059*T2+0.001813*T3)/3600;    {21.2} *)

Procedure  TPlanetVSOP87.GetObjectData(SL:TStrings);
var aSHA:Double;
begin
  SL.Add(Name);
  SL.Add('');

  SL.Add('at time='+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fGMT) +' UT' );   // GMT = Universal Time
  SL.Add('        '+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fTDT) +' TDT' );  // dynamic time
  SL.Add('DeltaT= '+  Format('%6.2f',[fDeltaT])  +  ' secs ( TDT-UT )' );
  SL.Add('Tau= '+    Format('%6.4f',[fTau])    +  '  days  ( '+ Format('%6.1f',[fTau*24*3600]) +' secs )' );
  SL.Add('TDT-Tau='+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fUT_Tau)+' TDT');

  SL.Add('RA=  '  + floatToGMSD(fRA) +' ( '+ floatToHHMMSS(fRA*24/360)+')  ('+Format('%12.7f',[fRA])+' )'   );   // in hours

  aSHA := 360.0 - fRA;                     // SHA and RA are the same thing, but with a different convention
  SL.Add('SHA= '+  floatToGMSD(aSHA) );

  SL.Add('Decl='+  floatToGMSD_Lat(fDecl)+' ('+Format('%12.7f',[fDecl])+' )' );
  SL.Add('GHA= '+  floatToGMSD(fGHA) );

  SL.Add('heliocentric coords in AU');
  SL.Add('X= '+  Format('%13.8f',[fPosition.x]) );
  SL.Add('Y= '+  Format('%13.8f',[fPosition.y]) );
  SL.Add('Z= '+  Format('%13.8f',[fPosition.z]) );

  SL.Add('Mag= '+    Format('%5.2f',[fMagnitude]) );
  SL.Add('Delta= '+  Format('%6.4f',[fDelta])  +  '  (dist Planet->Earth in AU)' );

  SL.Add('Hz Parallax= '+  Format('%5.2f',[fParallax])+'''' );
end;

end.




