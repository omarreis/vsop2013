Unit VSOP2013.Planet;  //---- Planet coordinates using VSOP2013 ----------------------------\
//--------------------//                                                                     \
// VSOP 2013 = Variations Seculaires des Orbites Planetaires 2013                             \
//                                                                                             \
// calculates planet's ecliptical coordinates Lon,Lat and Heliocentric Radius, given a time     \
//                                                                                              /
// mostly programmed by Omar                                                                   /
//      repository: github.com/omarreis/VSOP2013                                              /
//-------------------------------------------------------------------------------------------/

interface

uses
  System.SysUtils, System.DateUtils, System.Classes, System.Math,
  doubleVector3D,             // TVector3D_D - 3D vectors with Double components
  vsop2013,                   // VSOP2013 engine
  StarData,
  Om.DeltaT,
  Om.Trigonometry,
  Om.AstronomicalAlgorithms;  // date fns

type
  // VSOP 2013 solar system planets
  TPlanetVSOP2013 = Class(TCelObjBase)     // TCelObjBase in StarData.pas
  private
    //intermediate results generated by CalcCoordinates()
    fx,fy,fz:double;         // geocentric cartesian coordinates
    fDelta:double;          // distance from Earth to obj
    fTau:double;           // light propagation time from obj to Earth
    fL,fB,fR:double;      // planet heliocentric coordinates
    fPosition,fSpeed:TVector3D_D;
    fUT_Tau:Double;
  public
    fParallax:double;       // parallax in altitude
	  fIP:integer;           // Planet number in VSOP2013 file
    fDeltaT:double;       //  delta UTC --> TT ( former TDT )

    constructor Create(const aName:String; aIP:integer; aMag:single);
    destructor  Destroy;   override;
    Procedure   CalcCoordinates(const aDia,aMes,aAno,aHora:Double); override;
    Procedure   GetObjectData(SL:TStrings);   override;
  end;

function  CreatePlanetsVSOP2013:boolean;
function  FindPlanetVSOP2013ByName(const aName:String):TPlanetVSOP2013;
function  FindPlanetVSOP2013ByIndex(const aIndex:integer):TPlanetVSOP2013;
function  TranslateIndexVSOP2013to87(aIndex2013:integer):integer;  // index for use w/ vsop87

Procedure FreePlanetsVSOP2013;

var
  PlanetsVSOP2013:Array[1..NUM_PLANETS] of TPlanetVSOP2013;   // Mars, Venus, Jupiter and Saturn

implementation   //---------------------------------------------------

// requires Assigned(VSOP_File) and VSOP_File.fLoaded
function  CreatePlanetsVSOP2013:boolean;
var i:integer;
begin
  Result := false;
  if not ( Assigned(VSOP_File) and VSOP_File.fLoaded ) then exit;

  for i:=1 to NUM_PLANETS do  // create planets catalog
    PlanetsVSOP2013[i] := TPlanetVSOP2013.Create( PLANET_NAMES[i], i, 0);

  Result := true;
end;

// planet indexes are according to original theory indexes
//      vsop87              vsop2013
//
//     'Venus'    1        'Mercury'    1
//     'Saturn'   2        'Venus'      2
//     'Mars'     3        'Earth'      3
//     'Jupiter'  4        'Mars'       4
//     'Mercury'  5        'Jupiter'    5
//     'Uranus'   6        'Saturn'     6
//     'Neptune'  7        'Uranus'     7
//                         'Neptune'    8
//                         'Pluto'      9

function TranslateIndexVSOP2013to87(aIndex2013:integer):integer;
begin
  case aIndex2013 of
    1: Result := 5;
    2: Result := 1;
    3: Result := 0;   // 0 = invalid  - Earth not in vsop87
    4: Result := 3;
    5: Result := 4;
    6: Result := 2;
    7: Result := 6;
    8: Result := 7;
    9: Result := 0;   // 0 = invalid  - Pluto is out
  else
    Result := 0 ;  // 0 = invalid
  end;
end;

function  FindPlanetVSOP2013ByIndex(const aIndex:integer):TPlanetVSOP2013;
begin
  Result:=nil;
  if not ( Assigned(VSOP_File) and VSOP_File.fLoaded ) then exit;

  if (aIndex>0) and (aIndex<=NUM_PLANETS) then Result := PlanetsVSOP2013[aIndex];
end;

function  FindPlanetVSOP2013ByName(const aName:String):TPlanetVSOP2013;  // don't use translated planet names !!
var i:integer; aPlanet:TPlanetVSOP2013;
begin
  Result:=nil;
  if not ( Assigned(VSOP_File) and VSOP_File.fLoaded ) then exit;

  for i:=1 to NUM_PLANETS do              // linear search
    begin
      aPlanet := PlanetsVSOP2013[i];
      if (aPlanet.Name=aName) then
        begin
          Result := aPlanet;
          exit;
        end;
    end;
end;

Procedure FreePlanetsVSOP2013;
var i:integer;
begin
  for i:=1 to NUM_PLANETS do
    begin
      PlanetsVSOP2013[i].Free;
      PlanetsVSOP2013[i]:=nil;
    end;
end;

{ TPlanetVSOP2013 }

constructor TPlanetVSOP2013.Create(const aName: String; aIP: integer; aMag: single);
begin
  inherited Create(aName);
  fIP := aIP;

  fMagnitude := aMag;
  fParallax  := 0;
  fDeltaT    := 0;   // 2 deltas - fDeltaT is the diff TT-UTC in seconds
  fDelta     := 0;   //            fDelta is the distance earth->planet
  fUT_Tau:=0;
end;

destructor TPlanetVSOP2013.Destroy;
begin
  inherited;
end;

// calc apparent coordinates RA,Decl
procedure TPlanetVSOP2013.CalcCoordinates(const aDia, aMes, aAno, aHora: Double);
var T,T2:Double;     {Posicao de planetas pag. 207, capitulo 31}
  Lo,Bo,Ro:Double;        {Earth coords}
  x2,y2:Double;
  Lamb,Beta,Teta:Double;
  e,Pi_,DLamb,DBeta:Double;
  ax,ay:double;
  Eps:Double;
  {nutation vars}
  DPhy,DEps,aDTday:Double;
  Dt:TDatetime;
  aJD:Double;
  Position0,Speed0: TVector3D_D;

const
  Kapa=20.49552; {Constante de aberracao}

  // CalculaTau not only computes Tau, but also updates the position for time aJD
  Procedure CalculaTau;  // Tau = light ray delay due to propagation of light ( planet distance --> Earth )
  begin
    VSOP_File.calculate_coordinates(fIP, aJD, fPosition,fSpeed );  //call VSOP 2013 module
    //calc coord geocentricas
    fx := fPosition.x-Position0.x;  // vector Planet -> Earth
    fy := fPosition.y-Position0.y;
    fz := fPosition.z-Position0.z;
    x2 := fx*fx;
    y2 := fy*fy;
    fDelta := Sqrt(x2+y2+fz*fz);      {Dist Planet->Earth in AU -  AA 32.3 }
    fTau   := 0.0057755183*fDelta;    {Tau=time it takes for  the planet light to reach Earth in days }
  end;

begin    { TPlanetVSOP87.CalcCoordinates }
  Dt  := EncodeDate( Trunc(aAno), Trunc(aMes), Trunc(aDia) )+aHora/24;
  aJD := DatetimeToJD( Dt );   // in UT

  T := TJ2000(aAno,aMes,aDia,aHora);     // aAno,aMes,aDia,aHora em UT. T in seculae since 2000
  fDeltaT := calcDeltaT(T);              // DeltaT in seconds
  aDTday  := fDeltaT/24/3600;            // aDTday = Delta T in days
  fTDT    := fGMT + aDTday;

  T := T + fDeltaT*SegToSec;             // convert T from UTC para TDT in seculae

  aJD := aJD+aDTday;                     // add delta in days --> TDT

  T2:=T*T;
  // CalcCoordinates VSOP 2013 (T,@CEarth,Lo,Bo,Ro); {Calc coords da terra e..}
  VSOP_File.calculate_coordinates({Earth:}3, aJD, {out:} Position0, Speed0 );   // calc Earth coordinates

  CalculaTau;       // ..calcula do planeta e tempo de viagem da luz}

  aJD := aJD-fTau;  // instant when the light reaching us left the planet

  CalculaTau;       // recalc planet pos when the light ray left it
  fUT_Tau := JDToDatetime(aJD);     // save fGMT - Tau

  // this leaves fPosition loaded with VSOP

  fParallax:=8.794/60/fDelta;  //AfC pg. B17
  if (fx<>0) then      //contempla o caso do planeta Terra  (x=y=z=0)
    begin
      Lamb := Atan2(fy,fx);     {Calc lat e long}
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

  Lamb := Lamb+DLamb/3600;     {apply aberration delta.  check: is it ok apply aberration to planets ? }
  Beta := Beta+DBeta/3600;

  CorrNut(T,Eps,DPhy,DEps);    {calc corr for nutation and obliquity}
  Lamb := Lamb+DPhy/3600;      {correct lamb for Nutation - AA p.213}

  ay  := (Sing(Lamb)*Cosg(Eps)-Tang(Beta)*Sing(Eps));
  ax  := Cosg(Lamb);
  fRA := Atan2(ay,ax);     {AA 12.3 pg. 89}

  fDecl := Sing(Beta)*Cosg(Eps)+Cosg(Beta)*Sing(Eps)*Sing(Lamb);
  fDecl := ASing(fDecl);   {AA 12.4}
end;

procedure TPlanetVSOP2013.GetObjectData(SL: TStrings);
var aSHA:Double;
begin
  SL.Add(Name);
  SL.Add('');

  SL.Add('at time='+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fGMT) +' UT' );   // GMT = Universal Time
  SL.Add('        '+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fTDT) +' TDT' );  // dynamic time
  SL.Add('DeltaT= '+  Format('%6.2f',[fDeltaT])  +  ' secs ( TDT-UT )' );
  SL.Add('Tau= '  +  Format('%6.4f',[fTau])    +  '  days  ( '+ Format('%6.1f',[fTau*24*3600]) +' secs )' );
  SL.Add('UT-Tau='+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fUT_Tau)+' TDT');

  SL.Add('apparent coordinates');
  SL.Add('RA=  '  + floatToGMSD(fRA) +' ( '+ floatToHHMMSS(fRA*24/360)+')  ('+Format('%12.7f',[fRA])+' )' );   // in hours

  aSHA := 360.0 - fRA;                     // SHA and RA are the same thing, but with a different convention
  SL.Add('SHA= '+ floatToGMSD(aSHA) );
  SL.Add('Decl='+ floatToGMSD_Lat(fDecl) +' ('+Format('%12.7f',[fDecl])+' )' );
  SL.Add('GHA= '+ floatToGMSD(fGHA) );

  SL.Add('raw VSOP2013 coords (astrometric in JD-Tau) ');
  SL.Add('X= '+  Format('%13.8f',[fPosition.x]) );
  SL.Add('Y= '+  Format('%13.8f',[fPosition.y]) );
  SL.Add('Z= '+  Format('%13.8f',[fPosition.z]) );

  SL.Add('Mag= '  +  Format('%5.2f',[fMagnitude]) );
  SL.Add('Delta= '+  Format('%6.4f',[fDelta])  +  '  (dist Planet->Earth in AU)' );

  SL.Add('Hz Parallax= '+  Format('%5.2f',[fParallax])+'''' );
end;

end.

