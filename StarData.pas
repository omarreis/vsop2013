unit StarData;    //--- PlanetFun star data - Calculates positions of Stars -\\
 //--------------//                                                           ||
//                                                                           //
//   Uses a simplified version of Hipparcus catalog ( Hipparcus 150 )       //                                                                         //    --  --
//   source: github.com/omarreis/vsop2013/                                 //
//   programmed by oMAR                            _/\_  _/\_             //
//                                                 \  /  \  /            //
//                                                 /^^\  /^^\           //
// two star catalogs: Hipparcos 150 and older Navigator                //
//   Hipparcos uses 1991.25 positions and proper motion               //
//   Navigator is based in 1993 Astronomical Almanac positions       //
//   both methods bring positions to a given time considering       //
//   corrections precession, nutation, aberration                  //
//   and proper motion.                                           //
//                                                               //
//--------------------------------------------------------------//
//
//   History:
//     fev23: Om: added navigation stars to PlanetFun 3D app

interface

uses
  System.SysUtils, System.DateUtils, System.Classes;

const
  NumH150stars=150;         // Numbner of stars of Hipparcos 15 catalog - actually 149
  //InitEpochH150=2000.0;  // Epoca inicial das coordenadas das estrelas. é J2000 mesmo ?
  EpochiPM_H150=1991.25;

  EpochJ2000=2000.0;

  NESTRELAS=60;              // number of stars in Nav catalog
  EpochiNav=1993.0;          // epoch of initial coorinates for Nav catalog

var
  UseRigorousPrecession:boolean=false;   // see AA chapter 21. There are 1) low accuracy 2) Rigorous methods
  NavVerboseReport:boolean=true;        // verbose shows corrections for prec, nut, aberr and proper motion...

type
  /// TCelObjBase - abstract class for celestial object hierarchy .. stars, planets, Sun, Moons .. exo-planets
  TCelObjBase = class(TObject)
  private
    procedure CalcDeclination_GHA(D, M, A, H: Double; var aDecl, aGHA: Double);
    procedure SetGMT(Value: TDateTime);     { Altitude of object at the time of rise }
    Procedure CalculaSunTrueLongitude(T:Double; var Teta:Double);
    Function  AlreadyCalculatedToday(aDJ:Double):Boolean;
  public
    Name:String;         // common name ( for navigation stars )
    // celestial coordinates
    // current coordinates at time fGMT ( UTC )
    fGMT:TDateTime;      // current time ( set GMT to recalculate star coordinates )
    // celestial coordinates
    fRA,fDecl:Double;     // current coordinates
    fGHA:Double;         // Current Greenwich Hour Angle
    fTeta:Double;        // Sun true longitude

    fLastCoordCalculation:Double;   // Julian date of last coordinate calculation

    fMagnitude:Single;           // visual magnitude - low magnitude <--> high brightness
    fAltitudeOfObjectAtRise:Double;

    constructor Create(const aName:String);
    destructor  Destroy; override;

    procedure CalcCoordinates(const aDia, aMes, aAno, aHora: Double); virtual; abstract;  // calc fGMT,fRA,fDecl,fGHA..
    Procedure GetObjectData(SL:TStrings); virtual;                                     // return sumary of star data to Memos

    Property  GMT:TDateTime    read fGMT write SetGMT;  // set UT time ti calc coordinates ( UT and GMT are the same thing )
  end;

  // Hipparcos 150 stars - includes navigation stars (56) and some 94 more
  RStarH150=record          // star initialization rec - H150 data is included in this source as a constant. No need to load a data file...
    Name:String;           // common name ( for navigation stars )
    Greek:String;          // star name within a constelation, using geek letters
    Constellation:String;  // 3 char constellation symbol
    RA,Decl:Double;        // coordinates Right Ascension and Declination for J1991.25 ref ICRF ( ICRF is J2000.0 inertial reference oriented to extra galactic radio sources )
    PM_RA,PM_Dec:single;   // proper motion in arcsecs/year J2000
    Mag:Single;            // visual Magnitude ( small number --> brighter star )
    HipNum:integer;        // Hipparcos number
    HD:integer;            // HD number
    Parallax:double;
    TPM:double;            // total proper motion
    TrVel:double;          // transverse velocity
  end;

  TStarH150  = Class(TCelObjBase)
  protected
    fpm_RA,fpm_Dec:double;         // proper motion factors in arcsecs/year J2000 - from Smithsonian Star Catalog
    // star position at initial epoch
    EpochiRef:Double;
    EpochiPM:Double;

    RAi,Decli:Double;             // Coordinates in initial Epoch  ( J2000.0 )
    DAlfai,DDeltai:Double;       // accumulated corrections at initial Epoch (from J2000)

    {fDpmra,fDpmDec} fDpm: Double;  //accumulated deltas ?

    //detailed corrections
    fDeltaPmRA,   fDeltaPmDecl:double;
    fDeltaPrecRA, fDeltaPrecDecl:double;
    fDeltaAberRA, fDeltaAberDecl:double;
    fDeltaNutRA,  fDeltaNutDecl:double;
    fDeltaRA,     fDeltaDecl:double;

    fAlreadyCalculatedInitialCoordinates:Boolean; //indicates if initial coord calculation performed

    procedure CorrectionForAberration(T, aRAi, aDecli: Double; var DAlfaAbe, DDeltaAbe: Double);
    procedure CorrectionForNutation(T, aRAi, aDecli: Double; var DAlfaNu, DDeltaNu: Double);
    procedure CorrectionForPrecession(T, aRAi, aDecli: Double; var DAlfaPre, DDeltaPre: Double);
    procedure CorrectionForPrecessionRigorous(const aAno,aMes,aDia:word;const aHora,aEpochi,aRAi,aDecli: Double; {out:} var DAlfaPre, DDeltaPre: Double); { AA 21.2..21.4}

    procedure CalculaJ2000Correction(aDia, aMes, aAno, aHora: Double; var aDAlfa, aDDelta: Double);
  public
    fConst,fGreek:string;   // constelacao e letra grega designativa
    fHipNum:integer;       // Hipparcus 150 designation

    constructor Create(const StarRec: RStarH150);       // create obj with H150 star record
    destructor  Destroy; override;
    Procedure   GetObjectData(SL:TStrings); override;                     // return sumary of star data to Memos
    Procedure   CalcCoordinates(const aDia,aMes,aAno,aHora:Double); override;  // calc fRA, fDecl at fGMT
  end;

  { Estrelas }
  REstrela=record
    Nome:String;
    RA,Decl:Double;
    Mag:Single;
    PM_RA,PM_Dec:single;   //jul/04 adicionei efeitos da proper motion (in arcsecs/year J2000 - Smithsonian Star Catalog
  end;

  TNavStar = Class(TStarH150)
  public
    constructor Create(const StarRec: REstrela);       // same as TStarH150, but with a different constructor
    // Procedure   CalcCoordinates(const aDia,aMes,aAno,aHora:Double); override;  // calc fRA, fDecl at fGMT
    Procedure   GetObjectData(SL:TStrings); override;                     // return sumary of star data to Memos
  End;

// catalogs of objects
var
  StarsH150:Array[1..NumH150stars] of TStarH150;       //static db
  Estrelas:Array[1..NEstrelas] of TNavStar;

Procedure CreateAstrosHipparcos150;
function  FindAstroH150ByName(const aName:String):TStarH150;
Procedure FreeAstrosHipparcos150;

Procedure CreateNavStars;
function  FindNavStar(const aName:String):TNavStar;
Procedure FreeAstrosNavStars;


implementation   //--------------------------------------------

uses
  Om.Trigonometry,             // trigonometric utils
  Om.AstronomicalAlgorithms;  // AA

{ TCelObjBase }

Constructor TCelObjBase.Create(const aName:String);
begin
  inherited Create;

  Name  := aName;
  // no calculation yet
  fGMT := 0;      fRA   := 0;    fDecl := 0;
  fGHA := 0;      fMagnitude := 0;
  fAltitudeOfObjectAtRise := -0.5667;    //  for stars and planets - AA.pag 98 - due to atmospheric refraction
  fLastCoordCalculation   := 0;         //  0=never
  fTeta := 0;
end;

destructor  TCelObjBase.Destroy;
begin
  inherited;
end;

Procedure  TCelObjBase.GetObjectData(SL:TStrings);
var aSHA,aDummy:Double;
begin
  SL.Add(Name);

  aSHA := 360.0 - fRA;                     // SHA and RA are the same thing, but with a different convention
  SL.Add('SHA= '+  floatToGMSD(aSHA)+ ' ('+ R2GMD(aSHA,aDummy,' -')  +')' );

  SL.Add('Decl='+ floatToGMSD_Lat(fDecl) );
  SL.Add('GHA= '+  floatToGMSD(fGHA) );

  SL.Add('Mag= '+  Format('%5.2f',[fMagnitude]) );
end;

// setGMT recalculates coordinates, if needed
Procedure TCelObjBase.SetGMT(Value:TDateTime);
var aDia,aMes,aAno:Word; aHora:Double;
begin
  fGMT  := Value;
  DecodeDate(Int(fGMT), aAno, aMes, aDia);
  aHora := Frac(fGMT)*24;
  CalcDeclination_GHA(aDia,aMes,aAno,aHora,fDecl,fGHA);
end;

Procedure TCelObjBase.CalcDeclination_GHA(D,M,A,H:Double; {out:} var aDecl,aGHA:Double); // calc gha,decl
var GMST,GAST,RAh:Double;
begin
  SiderealTime(D,M,A,H, {out:} GMST,GAST);

  CalcCoordinates(D,M,A,H);           // calc obj coordinates fRA,fDecl

  aDecl := fDecl;                   // return
  RAh   := fRA/15;                 // Transform RA from degrees to hours
  AjustaHora(RAh);                // Ajusta no range 0-24 }

  aGHA  := 15*(GAST-RAh);       // Calcula GHA, convertendo p/ graus}
  AjustaAngulo(aGHA);          // Ajusta o angulo colocando entre 0 e 360°}
  fGHA  := aGHA;              // save

  {GHAAries:=15*GAST;}  {Calcula GHA de Aries}
  {AjustaAngulo(GHAAries);}
end;

Function  TCelObjBase.AlreadyCalculatedToday(aDJ:Double):Boolean;
begin
  AlreadyCalculatedToday := ( Round(aDJ)=Round(fLastCoordCalculation) );     // same day
end;

Procedure TCelObjBase.CalculaSunTrueLongitude(T:Double; {out:} var Teta:Double);
var L0,C,T2,T3,M:Double;
begin {Calculo da Long Verd. do Sol}
  T2 := T*T; T3 := T*T2;
  L0 := 280.46645+36000.76983*T+0.0003032*T2;            {FORMULA 24.2}
  M  := 357.5291+35999.0503*T-0.0001559*T2-0.00000048*T3; {24.4}
  C  := +(1.9146-0.004817*T-0.000014*T2)*Sing(M)+(0.019993-0.000101*T)*Sing(2.0*M)+0.000290*Sing(3.0*M);
  Teta:=L0+C;
end;


{ TStarH150 }

Constructor TStarH150.Create(const StarRec: RStarH150);
begin
  inherited Create(StarRec.Name);

  fConst:=StarRec.Constellation;
  fGreek:=StarRec.Greek;

  // star positions calculated from initial state
  EpochiPM     := EpochiPM_H150;
  EpochiRef    := EpochJ2000;

  RAi        := StarRec.RA;          // RA here already in degrees
  Decli      := StarRec.Decl;
  fAlreadyCalculatedInitialCoordinates:=FALSE;

  fMagnitude := StarRec.Mag;
  fHipNum    := StarRec.HipNum;      // Hipparcos number
  // PM_RA,PM_Dec in mas / year

  fpm_RA     := StarRec.PM_RA/1000;    // /15/Cosg(Decli);   // in mas -- convert to seconds/year
  fpm_Dec    := StarRec.PM_Dec/1000;   // jul/04 adicionei efeitos da proper motion (in arcsecs/year J2000 - Smithsonian Star Catalog
                                       //
  //fDpmra := 0;
  //fDpmDec:= 0;
  fDpm   := 0;

  fDeltaPmRA   := 0; fDeltaPmDecl    := 0;
  fDeltaPrecRA := 0; fDeltaPrecDecl  := 0;
  fDeltaAberRA := 0; fDeltaAberDecl  := 0;
  fDeltaNutRA  := 0; fDeltaNutDecl   := 0;
  fDeltaRA     := 0; fDeltaDecl      := 0;
end;

Procedure  TStarH150.GetObjectData(SL:TStrings);
var aSHA,aDummy:Double;
begin
  SL.Add(Name+' ('+fGreek+' '+fConst+')');

  SL.Add('epoch J1991.25' );
  SL.Add('RAi=  '  + floatToGMSD(RAi) +' ( '+floatToHHMMSS(RAi*24/360)+')' ); // degrees (hours)

  SL.Add('Decli='+ floatToGMSD_Lat(Decli) );
  SL.Add('');

  SL.Add('at time='+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fGMT) +' UT' );      // GMT = Universal Time
  SL.Add('Teta='  + Format('%7.32f',[fTeta])+'°  (Sun true long)'); // Sun true longitude
  SL.Add('RA=  '  + floatToGMSD(fRA)  +' ( '+floatToHHMMSS(fRA*24/360)+')' );  // degrees (hours)

  aSHA := 360.0 - fRA;                     // SHA and RA are the same thing, but with a different convention
  SL.Add('SHA= '+ floatToGMSD(aSHA) + ' ('+ R2GMD(aSHA,aDummy,' -')  +')' );

  SL.Add('Decl='+ floatToGMSD_Lat(fDecl) + ' ('+ R2GMD(fDecl,aDummy,'NS')  +')' );
  SL.Add('GHA= '+ floatToGMSD(fGHA) );

  SL.Add('Mag= '+ Format('%5.2f',[fMagnitude]) );

  if NavVerboseReport then
    begin
      SL.Add('Corrections---/\----\/---');
      SL.Add('   pm spd '+  Format('%6.4f %6.4f',[fpm_RA,fpm_Dec])              +' "/y ');
      SL.Add('   Nut  '+  Format('%5.2f %5.2f',[fDeltaNutRA, fDeltaNutDecl] ) +' " '  );
      SL.Add('   Prec '+  Format('%5.2f %5.2f',[fDeltaPrecRA,fDeltaPrecDecl]) );
      SL.Add('   Aber '+  Format('%5.2f %5.2f',[fDeltaAberRA,fDeltaAberDecl]) );
      SL.Add('   Tot  '+  Format('%5.2f %5.2f',[fDeltaRA,fDeltaDecl]         ));
      SL.Add('   PM   '+  Format('%5.2f %5.2f',[fDeltaPmRA,  fDeltaPmDecl]   ));
    end;
end;

Destructor TStarH150.Destroy;
begin
  inherited Destroy;
end;

Procedure TStarH150.CorrectionForNutation(T,aRAi,aDecli:Double;var DAlfaNu,DDeltaNu:Double);
var DPhy,DEps:Double;
    Eps:Double;
    TDi,SEps,CEps,SA,CA:Double;
begin
  CorrNut( T, Eps, DPhy, DEps);
  SEps := Sing(Eps);
  CEps := Cosg(Eps);
  SA := Sing(aRAi);   CA := Cosg(aRAi);   TDi := Tang(aDecli);
  DAlfaNu  := (CEps+SEps*SA*TDi)*DPhy-(CA*TDi)*DEps;                     {formula 22.1 pag.139 Ast.Alg}
  DDeltaNu := (SEps*CA)*DPhy+SA*DEps;
end;

Procedure TStarH150.CorrectionForAberration(T,aRAi,aDecli:Double;{out:} var DAlfaAbe,DDeltaAbe:Double);
var e,Pi_,T2,T3:Double; {Teta=Sun True Longitude}
    CA,SA,Eps0,CEp,SEp,STt,CTt,SDl,CDl,SPi,CPi,K1,K2:Double; {Vars auxiliares}
    Omega:Double;
const  Kapa=20.49552; {Constante de aberracao}

begin
  T2    := T*T;  T3 := T*T2;
  Omega := 125.04452-1934.136261*T;
  Eps0  := 23.4392911+(-46.8150*T-0.00059*T2+0.001813*T3)/3600;
  CalculaSunTrueLongitude(T,fTeta);
  AjustaAngulo2(fTeta);                // bring angle to range 0.360
  e     := 0.016708617-0.00004237*T-0.0000001236*T2;
  Pi_   := 102.93735+0.71953*T+0.00046*T2;
  { memoise trigs }
  CA :=Cosg(aRAi);   SA :=Sing(aRAi);
  CTt:=Cosg(fTeta);  STt:=Sing(fTeta);
  CEp:=Cosg(Eps0);   SEp:=Sing(Eps0);
  CDl:=Cosg(aDecli); SDl:=Sing(aDecli);
  CPi:=Cosg(Pi_);    SPi:=Sing(Pi_);

  DAlfaAbe := -Kapa*((CA*CTt*CEp+SA*STt)/CDl) + e*Kapa*((CA*CPi*CEp+SA*SPi)/CDl);
  k1       := CEp*(Tang(Eps0)*CDl-SA*SDl); K2:=CA*SDl;
  DDeltaAbe:= -Kapa*(CTt*K1+K2*STt)+e*Kapa*CPi*K1+K2*SPi;
end;

Procedure TStarH150.CorrectionForPrecession(T,aRAi,aDecli:Double;{out:} var DAlfaPre,DDeltaPre:Double);   { AA 21.1 }
var       {T=Tempo em seculos desde j2000.0 - calcule com TJ2000() }
  m,n:Double;
  DAlfaP,DDeltaP:Double; // anual precession
  NAnos:Double;
begin                       { Efeito da precessao - Astronomical Algorithms-J. Meeus p. 124 }
  NAnos := T*100.0;         { Numero de anos desde J2000.0 }
  // this is the "low accuracy" method AA v2 p.132
  m := (3.07496+0.00186*T)*15;  { *15 converts sec --> " }
  n := 20.0431-0.0085*T;

  DAlfaP  := m+n*Sing(aRAi)*Tang(aDecli);  {Deltas em " de arco -  formula 20.1}
  DDeltaP := n*Cosg(aRAi);                 {valores sao anuais}

{  WriteLn('DA P:',DAlfaP:8:4);
   WriteLn ('DD P:',DDeltaP:8:4);}

  DAlfaPre  := DAlfaP*NAnos; {Converte em valores absolutos, multiplicando por NAnos}
  DDeltaPre := DDEltaP*NAnos;

{  WriteLn('DA Pre:',DAlfaPre:8:4);
   WriteLn ('DD Pre:',DDeltaPre:8:4);}
end;

 // calcula efeito da precessão do equinox entre Epochi and a given UT time
// Desconta efeito da precessao da Epoca inicial ate a Epoca final    Fonte: Revista Sky&Telescope - Oct 91 - Pag. 409
procedure TStarH150.CorrectionForPrecessionRigorous(const aAno,aMes,aDia:word;const aHora, aEpochi, aRAi, aDecli: Double; {out:} var DAlfaPre, DDeltaPre: Double); { AA 21.2..21.4}
var
  aDt:TDatetime;
  Teta,Zeta,Ze:Double;  {Euler Angles}
  Tm2,t2,t3,k1:Double;  {Vars auxiliares}
  A,B,C,
  X,Y,Z,
  Alfa_z,   // RA-Ze
  Alfa,Delta,
  CTe,STe,
  CDecli,SDecli,
  CRAz,SRAz:Double;
  Xx,Xy,Xz,Yx,Yy,Yz,Zx,Zy,Zz:Double;
  Epochf:Double;       {Epoca final}
  RAh:Double;          {Ascencao reta em Horas}
  Tm,t:Double;
  x0,y0,z0,x1,y1,z1:Double;
  A1,D1:Double;
begin
  aDt    := EncodeDate(aAno,aMes,aDia)+aHora/24;                       // aDt in TDatetime UT
  Epochf := aAno + DayOfTheYear(aDt)/DaysInYear(aDt) + aHora/24/365.25;  // ex: 2023.32
  Tm     := (aEpochi-2000.0)/100.0;   // T Tzão
  t      := (Epochf-aEpochi)/100.0;   // t Tzinho
  // memoise some reasults for speed
  Tm2  :=Tm*Tm;    t2:=t*t;    t3:=t2*t;

  k1   := (2306.2181+1.39656*Tm-0.000139*Tm2)*t;           //this term appears in two formulas, so memoise
  //calc greeks
  Zeta := (K1+( 0.30188-0.000345*Tm)*t2+0.017998*t3)/3600;        {AA 21.2}
  Ze   := (K1+( 1.09468+0.000066*Tm)*t2+0.018203*t3)/3600;
  K1   := (2004.3109-0.85330*Tm-0.000217*Tm2)*t;           //again
  Teta := (K1+(-0.42665-0.000217*Tm)*t2-0.041833*t3)/3600;
  //memoise trigs for speed
  CTe    := Cosg(Teta);
  STe    := Sing(Teta);
  CDecli := Cosg(aDecli);
  SDecli := Sing(aDecli);
  CRAz   := Cosg(RAi + Zeta);
  SRAz   := Sing(RAi + Zeta);

	A := CDecli * SRAz;
	B := CTe * CDecli* CRAz - STe * SDecli;
	C := STe * CDecli* CRAz + Cte * SDecli;

  Alfa_z := ATan2(A,B);
  Alfa   := Alfa_z + Ze;
  Delta  := ASing(C);

  DAlfaPre  := (Alfa  - aRAi)*3600;            //convert from deg to mas
  DDeltaPre := (Delta - aDecli)*3600;
end;

// calc correction from TJ2000
Procedure TStarH150.CalculaJ2000Correction(aDia,aMes,aAno,aHora:Double;{out:} var aDAlfa,aDDelta:Double);
var T,tsm:Double;      // time in centuries from j2000.0
    aY,aM,aD:word;
    //DAlfaPre,DAlfaNu,DAlfaAbe:Double;
    //DDeltaPre,DDeltaNu,DDeltaAbe:Double;
begin
  T := TJ2000( aAno, aMes, aDia, aHora );   // T = Time in seculae since J2000  ex: 1/1/2023 --> T = 0.230..

  fDeltaPmRA   := 0; fDeltaPmDecl    := 0;  // use obj fields to save intermediate correction results
  fDeltaPrecRA := 0; fDeltaPrecDecl  := 0;
  fDeltaAberRA := 0; fDeltaAberDecl  := 0;
  fDeltaNutRA  := 0; fDeltaNutDecl   := 0;
  fDeltaRA     := 0; fDeltaDecl      := 0;

  // epoch i for hipparcos is 1991.25
  CorrectionForAberration( T, RAi, Decli, fDeltaAberRA,fDeltaAberDecl );  // all deltas in arcseconds
  CorrectionForNutation  ( T, RAi, Decli, fDeltaNutRA, fDeltaNutDecl  );

  if UseRigorousPrecession then
    begin
      aY := Trunc(aAno); aM := Trunc(aMes); aD := Trunc(aDia);
      CorrectionForPrecessionRigorous(aY,aM,aD, aHora, EpochiRef,RAi,Decli, {out:} fDeltaPrecRA,fDeltaPrecDecl);  //teste Epochi
    end
    else CorrectionForPrecession( T, RAi, Decli, {out:} fDeltaPrecRA,fDeltaPrecDecl );

  // fDeltaPrecRA:=0; fDeltaPrecDecl:=0;    // TESTE zera precessão

  {return corrections}
  aDAlfa  := fDeltaNutRA  + fDeltaPrecRA  + fDeltaAberRA;     // return sum of corrections at spec time
  aDDelta := fDeltaNutDecl+ fDeltaPrecDecl+ fDeltaAberDecl;

{ WriteLn('DA:',aDAlfa:8:4);
  WriteLn('DD:',aDDelta:8:4);}
end;

Procedure TStarH150.CalcCoordinates(const aDia,aMes,aAno,aHora:Double); // calc fRA,fDecl at time fGMT  ( UT )
var DJ,pm_anos,DAlfa,DDelta,aEpoch:Double;
begin
  if not fAlreadyCalculatedInitialCoordinates then {calc initial coordinates in reference time }
    begin
      // TESTE.Self Removi a correcao da precessao entre 1991.25 e 2.000 uma vez que o Hipparcos T0 =1991.25, mas as coordenadas são em rel ao equinox em J2000.0
      CalculaJ2000Correction(1.0, 1.0, EpochiRef, 0.0,{out:} DAlfai,DDeltai); {Calcula corr da data inic. 1991.25 em rel a J2000.0 }
      fAlreadyCalculatedInitialCoordinates:=TRUE;
    end;

  DJ := JD(aAno,aMes,aDia,aHora);               // calc Julian date
  if AlreadyCalculatedToday(DJ) then exit;     // already calculated coord today, no need to recalculate slow changing coordinates for the same day
  fLastCoordCalculation := DJ;                // save julian date of last calculation

  CalculaJ2000Correction(aDia,aMes,aAno,aHora, {out:} fDeltaRA, fDeltaDecl);    {Calc corrections }
  { (-) correcoes ref. ao inst. em que as coordenadas foram extraidas do
   Anuario Astronomico  (DDAlfai e DDeltai) =1993.0 }
  // fDeltaRA, fDeltaDecl contain aberr+nutation+precession

  DAlfa  := fDeltaRA   - DAlfai;    // subt the said corrections from the same at initial epoch
  DDelta := fDeltaDecl - DDeltai;

  // calc proper motion
  pm_anos := (aAno+aMes/12 - EpochiPM);    //diff in relation to J1991.25 (time of Hipparcos coordinatese) in years
  // really lame year number calculation
  // PM = proper motion speed * num_years
  fDeltaPmRA   := fpm_RA *pm_anos;        // in arcseconds
  fDeltaPmDecl := fpm_Dec*pm_anos;        //    "

  fRA   := RAi+   DAlfa /3600 + fDeltaPmRA/3600;     // Apply corrections
  fDecl := Decli+ DDelta/3600 + fDeltaPmDecl/3600;   // all Deltas in arcsecs --> /3600 to degrees

  //calc vetor de proper motion
  // fDpm := fDeltaPmRA*cos(fDecl);
  // fDpm := sqrt(fDpmDec*fDpmDec+fDpm*fDpm); //modulo do vetor

 { WriteLn('DAT:',DAlfa:8:4);
   WriteLn('DDT:',DDelta:8:4);}
end;

// Hipparchus 150 database ----------------------------------------------------------
var AstrosH150Created:boolean=FALSE;
const
  PosEstrelasH150:array[1..NumH150stars] of RStarH150=(   // Hipparcos 150 catalog data embbed with code
(Name:'Sirius'; Greek:'alpha'; Constellation:'Cma';     RA:101.289 ; Decl:-16.713 ; PM_RA:-546.01 ; PM_Dec:-1223.08 ; Mag:-1.44 ; HipNum:32349 ; HD:48915 ; Parallax:379.21 ;TPM:1339.42 ;TrVel:16.74 ),
(Name:'Canopus'; Greek:'alpha'; Constellation:'Car';    RA:95.988 ; Decl:-52.696 ; PM_RA:19.99 ; PM_Dec:23.67 ; Mag:-0.62 ; HipNum:30438 ; HD:45348 ; Parallax:10.43 ;TPM:30.98 ;TrVel:14.08 ),
(Name:'Arcturus'; Greek:'alpha'; Constellation:'Boo';   RA:213.918 ; Decl:19.187 ; PM_RA:-1093.45 ; PM_Dec:-1999.40 ; Mag:-0.05 ; HipNum:69673 ; HD:124897 ; Parallax:88.85 ;TPM:2278.87 ;TrVel:121.59 ),
(Name:'Rigil Kent'; Greek:'alpha'; Constellation:'Cen'; RA:219.92 ; Decl:-60.835 ; PM_RA:-3678.19 ; PM_Dec:481.84 ; Mag:-0.01 ; HipNum:71683 ; HD:128620 ; Parallax:742.12 ;TPM:3709.62 ;TrVel:23.70 ),
(Name:'Vega'; Greek:'alpha'; Constellation:'Lyr';       RA:279.234 ; Decl:38.783 ; PM_RA:201.02 ; PM_Dec:287.46 ; Mag:0.03 ; HipNum:91262 ; HD:172167 ; Parallax:128.93 ;TPM:350.77 ;TrVel:12.90 ),
(Name:'Capella'; Greek:'alpha'; Constellation:'Aur';    RA:79.172 ; Decl:45.999 ; PM_RA:75.52 ; PM_Dec:-427.13 ; Mag:0.08 ; HipNum:24608 ; HD:34029 ; Parallax:77.29 ;TPM:433.75 ;TrVel:26.60 ),
(Name:'Rigel'; Greek:'alpha'; Constellation:'Ori';      RA:78.634 ; Decl:-8.202 ; PM_RA:1.87 ; PM_Dec:-0.56 ; Mag:0.18 ; HipNum:24436 ; HD:34085 ; Parallax:4.22 ;TPM:1.95 ;TrVel:2.19 ),
(Name:'Procyon'; Greek:'alpha'; Constellation:'Cmi';    RA:114.827 ; Decl:5.228 ; PM_RA:-716.57 ; PM_Dec:-1034.58 ; Mag:0.40 ; HipNum:37279 ; HD:61421 ; Parallax:285.93 ;TPM:1258.50 ;TrVel:20.86 ),
(Name:'Betelgeuse'; Greek:'alpha'; Constellation:'Ori'; RA:88.793 ; Decl:7.407 ; PM_RA:27.33 ; PM_Dec:10.86 ; Mag:0.45 ; HipNum:27989 ; HD:39801 ; Parallax:7.63 ;TPM:29.41 ;TrVel:18.27 ),
(Name:'Achernar'; Greek:'alpha'; Constellation:'Eri';   RA:24.428 ; Decl:-57.237 ; PM_RA:88.02 ; PM_Dec:-40.08 ; Mag:0.45 ; HipNum:7588 ; HD:10144 ; Parallax:22.68 ;TPM:96.72 ;TrVel:20.22 ),
(Name:'Hadar'; Greek:'alpha'; Constellation:'Cen';      RA:210.956 ; Decl:-60.373 ; PM_RA:-33.96 ; PM_Dec:-25.06 ; Mag:0.61 ; HipNum:68702 ; HD:122451 ; Parallax:6.21 ;TPM:42.21 ;TrVel:32.22 ),
(Name:'Altair'; Greek:'alpha'; Constellation:'Aql';     RA:297.695 ; Decl:8.867 ; PM_RA:536.82 ; PM_Dec:385.54 ; Mag:0.76 ; HipNum:97649 ; HD:187642 ; Parallax:194.44 ;TPM:660.92 ;TrVel:16.11 ),
(Name:'Acrux'; Greek:'alpha'; Constellation:'Cru';      RA:186.65 ; Decl:-63.099 ; PM_RA:-35.37 ; PM_Dec:-14.73 ; Mag:0.77 ; HipNum:60718 ; HD:108248 ; Parallax:10.17 ;TPM:38.31 ;TrVel:17.86 ),
(Name:'Aldebaran'; Greek:'alpha'; Constellation:'Tau';  RA:68.98 ; Decl:16.51 ; PM_RA:62.78 ; PM_Dec:-189.36 ; Mag:0.87 ; HipNum:21421 ; HD:29139 ; Parallax:50.09 ;TPM:199.50 ;TrVel:18.88 ),
(Name:'Spica'; Greek:'alpha'; Constellation:'Vir';      RA:201.298 ; Decl:-11.161 ; PM_RA:-42.50 ; PM_Dec:-31.73 ; Mag:0.98 ; HipNum:65474 ; HD:116658 ; Parallax:12.44 ;TPM:53.04 ;TrVel:20.21 ),
(Name:'Antares'; Greek:'alpha'; Constellation:'Sco';    RA:247.352 ; Decl:-26.432 ; PM_RA:-10.16 ; PM_Dec:-23.21 ; Mag:1.06 ; HipNum:80763 ; HD:148478 ; Parallax:5.40 ;TPM:25.34 ;TrVel:22.24 ),
(Name:'Pollux'; Greek:'beta'; Constellation:'Gem';      RA:116.331 ; Decl:28.026 ; PM_RA:-625.69 ; PM_Dec:-45.95 ; Mag:1.16 ; HipNum:37826 ; HD:62509 ; Parallax:96.74 ;TPM:627.37 ;TrVel:30.74 ),
(Name:'Fomalhaut'; Greek:'alpha'; Constellation:'PsA';  RA:344.412 ; Decl:-29.622 ; PM_RA:329.22 ; PM_Dec:-164.22 ; Mag:1.17 ; HipNum:113368 ; HD:216956 ; Parallax:130.08 ;TPM:367.90 ;TrVel:13.41 ),
(Name:'Deneb'; Greek:'alpha'; Constellation:'Cyg';      RA:310.358 ; Decl:45.28 ; PM_RA:1.56 ; PM_Dec:1.55 ; Mag:1.25 ; HipNum:102098 ; HD:197345 ; Parallax:1.01 ;TPM:2.20 ;TrVel:10.32 ),
(Name:''; Greek:'beta'; Constellation:'Cru'; RA:191.93 ; Decl:-59.689 ; PM_RA:-48.24 ; PM_Dec:-12.82 ; Mag:1.25 ; HipNum:62434 ; HD:111123 ; Parallax:9.25 ;TPM:49.91 ;TrVel:25.58 ),
//(Name:''; Greek:'alpha2'; Constellation:'Cen'; RA:219.914 ; Decl:-60.839 ; PM_RA:-3600.35 ; PM_Dec:952.11 ; Mag:1.35 ; HipNum:71681 ; HD:128621 ; Parallax:742.12 ;TPM:3724.12 ;TrVel:23.79 ),
(Name:'Regulus'; Greek:'alpha'; Constellation:'Leo'; RA:152.094 ; Decl:11.967 ; PM_RA:-249.40 ; PM_Dec:4.91 ; Mag:1.36 ; HipNum:49669 ; HD:87901 ; Parallax:42.09 ;TPM:249.45 ;TrVel:28.09 ),
(Name:'Adhara'; Greek:'epsilon'; Constellation:'CMa'; RA:104.656 ; Decl:-28.972 ; PM_RA:2.63 ; PM_Dec:2.29 ; Mag:1.50 ; HipNum:33579 ; HD:52089 ; Parallax:7.57 ;TPM:3.49 ;TrVel:2.18 ),
(Name:'Castor'; Greek:'alpha'; Constellation:'Gem'; RA:113.65 ; Decl:31.889 ; PM_RA:-206.33 ; PM_Dec:-148.18 ; Mag:1.58 ; HipNum:36850 ; HD:60179 ; Parallax:63.27 ;TPM:254.03 ;TrVel:19.03 ),
(Name:''; Greek:'gamma'; Constellation:'Cru'; RA:187.791 ; Decl:-57.113 ; PM_RA:27.94 ; PM_Dec:-264.33 ; Mag:1.59 ; HipNum:61084 ; HD:108903 ; Parallax:37.09 ;TPM:265.80 ;TrVel:33.97 ),
(Name:'Shaula'; Greek:'lambda'; Constellation:'Sco'; RA:263.402 ; Decl:-37.104 ; PM_RA:-8.90 ; PM_Dec:-29.95 ; Mag:1.62 ; HipNum:85927 ; HD:158926 ; Parallax:4.64 ;TPM:31.24 ;TrVel:31.92 ),
(Name:'Bellatrix'; Greek:'gamma'; Constellation:'Ori'; RA:81.283 ; Decl:6.35 ; PM_RA:-8.75 ; PM_Dec:-13.28 ; Mag:1.64 ; HipNum:25336 ; HD:35468 ; Parallax:13.42 ;TPM:15.90 ;TrVel:5.62 ),
(Name:'Alnath'; Greek:'beta'; Constellation:'Tau'; RA:81.573 ; Decl:28.608 ; PM_RA:23.28 ; PM_Dec:-174.22 ; Mag:1.65 ; HipNum:25428 ; HD:35497 ; Parallax:24.89 ;TPM:175.77 ;TrVel:33.48 ),
(Name:''; Greek:'beta'; Constellation:'Car'; RA:138.301 ; Decl:-69.717 ; PM_RA:-157.66 ; PM_Dec:108.91 ; Mag:1.67 ; HipNum:45238 ; HD:80007 ; Parallax:29.34 ;TPM:191.62 ;TrVel:30.96 ),
(Name:'Alnilam'; Greek:'epsilon'; Constellation:'Ori'; RA:84.053 ; Decl:-1.202 ; PM_RA:1.49 ; PM_Dec:-1.06 ; Mag:1.69 ; HipNum:26311 ; HD:37128 ; Parallax:2.43 ;TPM:1.83 ;TrVel:3.57 ),
(Name:'Alnair'; Greek:'alpha'; Constellation:'Gru'; RA:332.058 ; Decl:-46.961 ; PM_RA:127.60 ; PM_Dec:-147.91 ; Mag:1.73 ; HipNum:109268 ; HD:209952 ; Parallax:32.16 ;TPM:195.34 ;TrVel:28.79 ),
(Name:'Alnitak'; Greek:'zeta'; Constellation:'Ori'; RA:85.19 ; Decl:-1.943 ; PM_RA:3.99 ; PM_Dec:2.54 ; Mag:1.74 ; HipNum:26727 ; HD:37742 ; Parallax:3.99 ;TPM:4.73 ;TrVel:5.62 ),
(Name:''; Greek:'gamma'; Constellation:'Vel'; RA:122.383 ; Decl:-47.337 ; PM_RA:-5.93 ; PM_Dec:9.90 ; Mag:1.75 ; HipNum:39953 ; HD:68273 ; Parallax:3.88 ;TPM:11.54 ;TrVel:14.10 ),
(Name:'Alioth'; Greek:'epsilon'; Constellation:'UMa'; RA:193.507 ; Decl:55.96 ; PM_RA:111.74 ; PM_Dec:-8.99 ; Mag:1.76 ; HipNum:62956 ; HD:112185 ; Parallax:40.30 ;TPM:112.10 ;TrVel:13.19 ),
(Name:'Kaus Australis'; Greek:'epsilon'; Constellation:'Sgr'; RA:276.043 ; Decl:-34.384 ; PM_RA:-39.61 ; PM_Dec:-124.05 ; Mag:1.79 ; HipNum:90185 ; HD:169022 ; Parallax:22.55 ;TPM:130.22 ;TrVel:27.37 ),
(Name:'Mirphak'; Greek:'alpha'; Constellation:'Per'; RA:51.081 ; Decl:49.861 ; PM_RA:24.11 ; PM_Dec:-26.01 ; Mag:1.79 ; HipNum:15863 ; HD:20902 ; Parallax:5.51 ;TPM:35.47 ;TrVel:30.51 ),
(Name:'Dubhe'; Greek:'alpha'; Constellation:'UMa'; RA:165.933 ; Decl:61.751 ; PM_RA:-136.46 ; PM_Dec:-35.25 ; Mag:1.81 ; HipNum:54061 ; HD:95689 ; Parallax:26.38 ;TPM:140.94 ;TrVel:25.33 ),
(Name:''; Greek:'delta'; Constellation:'CMa'; RA:107.098 ; Decl:-26.393 ; PM_RA:-2.75 ; PM_Dec:3.33 ; Mag:1.83 ; HipNum:34444 ; HD:54605 ; Parallax:1.82 ;TPM:4.32 ;TrVel:11.25 ),
(Name:'Alkaid'; Greek:'eta'; Constellation:'UMa'; RA:206.886 ; Decl:49.313 ; PM_RA:-121.23 ; PM_Dec:-15.56 ; Mag:1.85 ; HipNum:67301 ; HD:120315 ; Parallax:32.39 ;TPM:122.22 ;TrVel:17.89 ),
(Name:''; Greek:'theta'; Constellation:'Sco'; RA:264.33 ; Decl:-42.998 ; PM_RA:6.06 ; PM_Dec:-0.95 ; Mag:1.86 ; HipNum:86228 ; HD:159532 ; Parallax:11.99 ;TPM:6.13 ;TrVel:2.43 ),
(Name:''; Greek:'epsilon'; Constellation:'Car'; RA:125.629 ; Decl:-59.51 ; PM_RA:-25.34 ; PM_Dec:22.72 ; Mag:1.86 ; HipNum:41037 ; HD:71129 ; Parallax:5.16 ;TPM:34.03 ;TrVel:31.27 ),
(Name:''; Greek:'beta'; Constellation:'Aur'; RA:89.882 ; Decl:44.947 ; PM_RA:-56.41 ; PM_Dec:-0.88 ; Mag:1.90 ; HipNum:28360 ; HD:40183 ; Parallax:39.72 ;TPM:56.42 ;TrVel:6.73 ),
(Name:''; Greek:'alpha'; Constellation:'TrA'; RA:252.166 ; Decl:-69.028 ; PM_RA:17.85 ; PM_Dec:-32.92 ; Mag:1.91 ; HipNum:82273 ; HD:150798 ; Parallax:7.85 ;TPM:37.45 ;TrVel:22.61 ),
(Name:''; Greek:'delta'; Constellation:'Vel'; RA:131.176 ; Decl:-54.709 ; PM_RA:28.78 ; PM_Dec:-104.14 ; Mag:1.93 ; HipNum:42913 ; HD:74956 ; Parallax:40.90 ;TPM:108.04 ;TrVel:12.52 ),
(Name:'Alhena'; Greek:'gamma'; Constellation:'Gem'; RA:99.428 ; Decl:16.399 ; PM_RA:-2.04 ; PM_Dec:-66.92 ; Mag:1.93 ; HipNum:31681 ; HD:47105 ; Parallax:31.12 ;TPM:66.95 ;TrVel:10.20 ),
(Name:''; Greek:'alpha'; Constellation:'Pav'; RA:306.412 ; Decl:-56.735 ; PM_RA:7.71 ; PM_Dec:-86.15 ; Mag:1.94 ; HipNum:100751 ; HD:193924 ; Parallax:17.80 ;TPM:86.49 ;TrVel:23.04 ),
(Name:'Polaris'; Greek:'alpha'; Constellation:'UMi'; RA:37.946 ; Decl:89.264 ; PM_RA:44.22 ; PM_Dec:-11.74 ; Mag:1.97 ; HipNum:11767 ; HD:8890 ; Parallax:7.56 ;TPM:45.75 ;TrVel:28.69 ),
(Name:''; Greek:'beta'; Constellation:'CMa'; RA:95.675 ; Decl:-17.956 ; PM_RA:-3.45 ; PM_Dec:-0.47 ; Mag:1.98 ; HipNum:30324 ; HD:44743 ; Parallax:6.53 ;TPM:3.48 ;TrVel:2.53 ),
(Name:'Alphard'; Greek:'alpha'; Constellation:'Hya'; RA:141.897 ; Decl:-8.659 ; PM_RA:-14.49 ; PM_Dec:33.25 ; Mag:1.99 ; HipNum:46390 ; HD:81797 ; Parallax:18.40 ;TPM:36.27 ;TrVel:9.34 ),
(Name:'Algieba'; Greek:'gamma1'; Constellation:'Leo'; RA:154.992 ; Decl:19.842 ; PM_RA:310.77 ; PM_Dec:-152.88 ; Mag:2.01 ; HipNum:50583 ; HD:89484 ; Parallax:25.96 ;TPM:346.34 ;TrVel:63.24 ),
(Name:'Hamal'; Greek:'alpha'; Constellation:'Ari'; RA:31.793 ; Decl:23.463 ; PM_RA:190.73 ; PM_Dec:-145.77 ; Mag:2.01 ; HipNum:9884 ; HD:12929 ; Parallax:49.48 ;TPM:240.06 ;TrVel:23.00 ),
(Name:'Diphda'; Greek:'beta'; Constellation:'Cet'; RA:10.897 ; Decl:-17.987 ; PM_RA:232.79 ; PM_Dec:32.71 ; Mag:2.04 ; HipNum:3419 ; HD:4128 ; Parallax:34.04 ;TPM:235.08 ;TrVel:32.74 ),
(Name:'Nunki'; Greek:'sigma'; Constellation:'Sgr'; RA:283.816 ; Decl:-26.297 ; PM_RA:13.87 ; PM_Dec:-52.65 ; Mag:2.05 ; HipNum:92855 ; HD:175191 ; Parallax:14.54 ;TPM:54.45 ;TrVel:17.75 ),
(Name:''; Greek:'theta'; Constellation:'Cen'; RA:211.672 ; Decl:-36.369 ; PM_RA:-519.29 ; PM_Dec:-517.87 ; Mag:2.06 ; HipNum:68933 ; HD:123139 ; Parallax:53.52 ;TPM:733.38 ;TrVel:64.96 ),
(Name:'Saiph'; Greek:'kappa'; Constellation:'Ori'; RA:86.939 ; Decl:-9.67 ; PM_RA:1.55 ; PM_Dec:-1.20 ; Mag:2.07 ; HipNum:27366 ; HD:38771 ; Parallax:4.52 ;TPM:1.96 ;TrVel:2.06 ),
(Name:'Alpheratz'; Greek:'delta'; Constellation:'Peg'; RA:2.097 ; Decl:29.091 ; PM_RA:135.68 ; PM_Dec:-162.95 ; Mag:2.07 ; HipNum:677 ; HD:358 ; Parallax:33.60 ;TPM:212.04 ;TrVel:29.92 ),
(Name:''; Greek:'beta'; Constellation:'Gru'; RA:340.666 ; Decl:-46.885 ; PM_RA:135.68 ; PM_Dec:-4.51 ; Mag:2.07 ; HipNum:112122 ; HD:214952 ; Parallax:19.17 ;TPM:135.75 ;TrVel:33.57 ),
(Name:'Mirach'; Greek:'beta'; Constellation:'And'; RA:17.432 ; Decl:35.621 ; PM_RA:175.59 ; PM_Dec:-112.23 ; Mag:2.07 ; HipNum:5447 ; HD:6860 ; Parallax:16.36 ;TPM:208.39 ;TrVel:60.38 ),
(Name:'Kochab'; Greek:'beta'; Constellation:'UMi'; RA:222.677 ; Decl:74.155 ; PM_RA:-32.29 ; PM_Dec:11.91 ; Mag:2.07 ; HipNum:72607 ; HD:131873 ; Parallax:25.79 ;TPM:34.42 ;TrVel:6.33 ),
(Name:'Rasalhague'; Greek:'alpha'; Constellation:'Oph'; RA:263.733 ; Decl:12.561 ; PM_RA:110.08 ; PM_Dec:-222.61 ; Mag:2.08 ; HipNum:86032 ; HD:159561 ; Parallax:69.84 ;TPM:248.34 ;TrVel:16.86 ),
(Name:'Algol'; Greek:'beta'; Constellation:'Per'; RA:47.042 ; Decl:40.956 ; PM_RA:2.39 ; PM_Dec:-1.44 ; Mag:2.09 ; HipNum:14576 ; HD:19356 ; Parallax:35.14 ;TPM:2.79 ;TrVel:0.38 ),
(Name:''; Greek:'gamma1'; Constellation:'And'; RA:30.975 ; Decl:42.33 ; PM_RA:43.08 ; PM_Dec:-50.85 ; Mag:2.10 ; HipNum:9640 ; HD:12533 ; Parallax:9.19 ;TPM:66.65 ;TrVel:34.38 ),
(Name:'Denebola'; Greek:'beta'; Constellation:'Leo'; RA:177.266 ; Decl:14.572 ; PM_RA:-499.02 ; PM_Dec:-113.78 ; Mag:2.14 ; HipNum:57632 ; HD:102647 ; Parallax:90.16 ;TPM:511.83 ;TrVel:26.91 ),
(Name:''; Greek:'gamma'; Constellation:'Cas'; RA:14.177 ; Decl:60.717 ; PM_RA:25.65 ; PM_Dec:-3.82 ; Mag:2.15 ; HipNum:4427 ; HD:5394 ; Parallax:5.32 ;TPM:25.93 ;TrVel:23.11 ),
(Name:''; Greek:'gamma'; Constellation:'Cen'; RA:190.38 ; Decl:-48.96 ; PM_RA:-187.28 ; PM_Dec:-1.20 ; Mag:2.20 ; HipNum:61932 ; HD:110304 ; Parallax:25.01 ;TPM:187.28 ;TrVel:35.50 ),
(Name:''; Greek:'zeta'; Constellation:'Pup'; RA:120.896 ; Decl:-40.003 ; PM_RA:-30.82 ; PM_Dec:16.77 ; Mag:2.21 ; HipNum:39429 ; HD:66811 ; Parallax:2.33 ;TPM:35.09 ;TrVel:71.39 ),
(Name:''; Greek:'iota'; Constellation:'Car'; RA:139.273 ; Decl:-59.275 ; PM_RA:-19.03 ; PM_Dec:13.11 ; Mag:2.21 ; HipNum:45556 ; HD:80404 ; Parallax:4.71 ;TPM:23.11 ;TrVel:23.26 ),
(Name:'Alphekka'; Greek:'alpha'; Constellation:'CrB'; RA:233.672 ; Decl:26.715 ; PM_RA:120.38 ; PM_Dec:-89.44 ; Mag:2.22 ; HipNum:76267 ; HD:139006 ; Parallax:43.65 ;TPM:149.97 ;TrVel:16.29 ),
(Name:''; Greek:'lambda'; Constellation:'Vel'; RA:136.999 ; Decl:-43.433 ; PM_RA:-23.21 ; PM_Dec:14.28 ; Mag:2.23 ; HipNum:44816 ; HD:78647 ; Parallax:5.69 ;TPM:27.25 ;TrVel:22.70 ),
(Name:''; Greek:'gamma'; Constellation:'Cyg'; RA:305.557 ; Decl:40.257 ; PM_RA:2.43 ; PM_Dec:-0.93 ; Mag:2.23 ; HipNum:100453 ; HD:194093 ; Parallax:2.14 ;TPM:2.60 ;TrVel:5.76 ),
(Name:'Mizar'; Greek:'zeta'; Constellation:'UMa'; RA:200.981 ; Decl:54.925 ; PM_RA:121.23 ; PM_Dec:-22.01 ; Mag:2.23 ; HipNum:65378 ; HD:116656 ; Parallax:41.73 ;TPM:123.21 ;TrVel:14.00 ),
(Name:'Shedir'; Greek:'alpha'; Constellation:'Cas'; RA:10.127 ; Decl:56.537 ; PM_RA:50.36 ; PM_Dec:-32.17 ; Mag:2.24 ; HipNum:3179 ; HD:3712 ; Parallax:14.27 ;TPM:59.76 ;TrVel:19.85 ),
(Name:'Etamin'; Greek:'gamma'; Constellation:'Dra'; RA:269.152 ; Decl:51.489 ; PM_RA:-8.52 ; PM_Dec:-23.05 ; Mag:2.24 ; HipNum:87833 ; HD:164058 ; Parallax:22.10 ;TPM:24.57 ;TrVel:5.27 ),
(Name:'Mintaka'; Greek:'delta'; Constellation:'Ori'; RA:83.002 ; Decl:-0.000299 ; PM_RA:1.67 ; PM_Dec:0.56 ; Mag:2.25 ; HipNum:25930 ; HD:36486 ; Parallax:3.56 ;TPM:1.76 ;TrVel:2.35 ),
(Name:'Caph'; Greek:'beta'; Constellation:'Cas'; RA:2.292 ; Decl:59.15 ; PM_RA:523.39 ; PM_Dec:-180.42 ; Mag:2.28 ; HipNum:746 ; HD:432 ; Parallax:59.89 ;TPM:553.61 ;TrVel:43.82 ),
(Name:''; Greek:'delta'; Constellation:'Sco'; RA:240.083 ; Decl:-22.622 ; PM_RA:-8.67 ; PM_Dec:-36.90 ; Mag:2.29 ; HipNum:78401 ; HD:143275 ; Parallax:8.12 ;TPM:37.90 ;TrVel:22.13 ),
(Name:''; Greek:'epsilon'; Constellation:'Sco'; RA:252.543 ; Decl:-34.293 ; PM_RA:-611.83 ; PM_Dec:-255.87 ; Mag:2.29 ; HipNum:82396 ; HD:151680 ; Parallax:49.85 ;TPM:663.18 ;TrVel:63.06 ),
(Name:''; Greek:'epsilon'; Constellation:'Cen'; RA:204.972 ; Decl:-53.466 ; PM_RA:-14.60 ; PM_Dec:-12.79 ; Mag:2.29 ; HipNum:66657 ; HD:118716 ; Parallax:8.68 ;TPM:19.41 ;TrVel:10.60 ),
(Name:''; Greek:'alpha'; Constellation:'Lup'; RA:220.482 ; Decl:-47.388 ; PM_RA:-21.15 ; PM_Dec:-24.22 ; Mag:2.30 ; HipNum:71860 ; HD:129056 ; Parallax:5.95 ;TPM:32.15 ;TrVel:25.62 ),
(Name:''; Greek:'eta'; Constellation:'Cen'; RA:218.877 ; Decl:-42.158 ; PM_RA:-35.31 ; PM_Dec:-32.44 ; Mag:2.33 ; HipNum:71352 ; HD:127972 ; Parallax:10.57 ;TPM:47.95 ;TrVel:21.50 ),
(Name:'Merak'; Greek:'beta'; Constellation:'UMa'; RA:165.46 ; Decl:56.382 ; PM_RA:81.66 ; PM_Dec:33.74 ; Mag:2.34 ; HipNum:53910 ; HD:95418 ; Parallax:41.07 ;TPM:88.36 ;TrVel:10.20 ),
(Name:'Izar'; Greek:'epsilon'; Constellation:'Boo'; RA:221.247 ; Decl:27.074 ; PM_RA:-50.65 ; PM_Dec:20.00 ; Mag:2.35 ; HipNum:72105 ; HD:129988 ; Parallax:15.55 ;TPM:54.46 ;TrVel:16.60 ),
(Name:'Enif'; Greek:'epsilon'; Constellation:'Peg'; RA:326.046 ; Decl:9.875 ; PM_RA:30.02 ; PM_Dec:1.38 ; Mag:2.38 ; HipNum:107315 ; HD:206778 ; Parallax:4.85 ;TPM:30.05 ;TrVel:29.37 ),
(Name:''; Greek:'kappa'; Constellation:'Sco'; RA:265.622 ; Decl:-39.03 ; PM_RA:-6.49 ; PM_Dec:-25.55 ; Mag:2.39 ; HipNum:86670 ; HD:160578 ; Parallax:7.03 ;TPM:26.36 ;TrVel:17.78 ),
(Name:'Ankaa'; Greek:'alpha'; Constellation:'Phe'; RA:6.57 ; Decl:-42.305 ; PM_RA:232.76 ; PM_Dec:-353.64 ; Mag:2.40 ; HipNum:2081 ; HD:2261 ; Parallax:42.14 ;TPM:423.37 ;TrVel:47.63 ),
(Name:'Phad'; Greek:'gamma'; Constellation:'UMa'; RA:178.457 ; Decl:53.695 ; PM_RA:107.76 ; PM_Dec:11.16 ; Mag:2.41 ; HipNum:58001 ; HD:103287 ; Parallax:38.99 ;TPM:108.34 ;TrVel:13.17 ),
(Name:''; Greek:'eta'; Constellation:'Oph'; RA:257.594 ; Decl:-15.725 ; PM_RA:41.16 ; PM_Dec:97.65 ; Mag:2.43 ; HipNum:84012 ; HD:155125 ; Parallax:38.77 ;TPM:105.97 ;TrVel:12.96 ),
(Name:'Scheat'; Greek:'beta'; Constellation:'Peg'; RA:345.943 ; Decl:28.082 ; PM_RA:187.76 ; PM_Dec:137.61 ; Mag:2.44 ; HipNum:113881 ; HD:217906 ; Parallax:16.37 ;TPM:232.79 ;TrVel:67.41 ),
(Name:'Alderamin'; Greek:'alpha'; Constellation:'Cep'; RA:319.644 ; Decl:62.585 ; PM_RA:149.91 ; PM_Dec:48.27 ; Mag:2.45 ; HipNum:105199 ; HD:203280 ; Parallax:66.84 ;TPM:157.49 ;TrVel:11.17 ),
(Name:''; Greek:'eta'; Constellation:'CMa'; RA:111.024 ; Decl:-29.303 ; PM_RA:-3.76 ; PM_Dec:6.66 ; Mag:2.45 ; HipNum:35904 ; HD:58350 ; Parallax:1.02 ;TPM:7.65 ;TrVel:35.54 ),
(Name:''; Greek:'kappa'; Constellation:'Vel'; RA:140.528 ; Decl:-55.011 ; PM_RA:-10.72 ; PM_Dec:11.24 ; Mag:2.47 ; HipNum:45941 ; HD:81188 ; Parallax:6.05 ;TPM:15.53 ;TrVel:12.17 ),
(Name:''; Greek:'epsilon'; Constellation:'Cyg'; RA:311.552 ; Decl:33.969 ; PM_RA:356.16 ; PM_Dec:330.28 ; Mag:2.48 ; HipNum:102488 ; HD:197989 ; Parallax:45.26 ;TPM:485.73 ;TrVel:50.87 ),
(Name:'Markab'; Greek:'alpha'; Constellation:'Peg'; RA:346.19 ; Decl:15.205 ; PM_RA:61.10 ; PM_Dec:-42.56 ; Mag:2.49 ; HipNum:113963 ; HD:218045 ; Parallax:23.36 ;TPM:74.46 ;TrVel:15.11 ),
(Name:''; Greek:'zeta'; Constellation:'Oph'; RA:249.29 ; Decl:-10.567 ; PM_RA:13.07 ; PM_Dec:25.44 ; Mag:2.54 ; HipNum:81377 ; HD:149757 ; Parallax:7.12 ;TPM:28.60 ;TrVel:19.04 ),
(Name:'Menkar'; Greek:'alpha'; Constellation:'Cet'; RA:45.57 ; Decl:4.09 ; PM_RA:-11.81 ; PM_Dec:-78.76 ; Mag:2.54 ; HipNum:14135 ; HD:18884 ; Parallax:14.82 ;TPM:79.64 ;TrVel:25.47 ),
(Name:''; Greek:'zeta'; Constellation:'Cen'; RA:208.885 ; Decl:-47.288 ; PM_RA:-57.14 ; PM_Dec:-44.75 ; Mag:2.55 ; HipNum:68002 ; HD:121263 ; Parallax:8.48 ;TPM:72.58 ;TrVel:40.57 ),
(Name:''; Greek:'beta1'; Constellation:'Sco'; RA:241.359 ; Decl:-19.805 ; PM_RA:-6.75 ; PM_Dec:-24.89 ; Mag:2.56 ; HipNum:78820 ; HD:144217 ; Parallax:6.15 ;TPM:25.79 ;TrVel:19.88 ),
(Name:''; Greek:'delta'; Constellation:'Leo'; RA:168.527 ; Decl:20.524 ; PM_RA:143.31 ; PM_Dec:-130.43 ; Mag:2.56 ; HipNum:54872 ; HD:97603 ; Parallax:56.52 ;TPM:193.78 ;TrVel:16.25 ),
(Name:''; Greek:'delta'; Constellation:'Cen'; RA:182.09 ; Decl:-50.722 ; PM_RA:-47.53 ; PM_Dec:-6.42 ; Mag:2.58 ; HipNum:59196 ; HD:105435 ; Parallax:8.25 ;TPM:47.96 ;TrVel:27.56 ),
(Name:'Arneb'; Greek:'alpha'; Constellation:'Lep'; RA:83.183 ; Decl:-17.822 ; PM_RA:3.27 ; PM_Dec:1.54 ; Mag:2.58 ; HipNum:25985 ; HD:36673 ; Parallax:2.54 ;TPM:3.61 ;TrVel:6.75 ),
(Name:''; Greek:'gamma'; Constellation:'Crv'; RA:183.952 ; Decl:-17.542 ; PM_RA:-159.58 ; PM_Dec:22.31 ; Mag:2.58 ; HipNum:59803 ; HD:106625 ; Parallax:19.78 ;TPM:161.13 ;TrVel:38.62 ),
(Name:''; Greek:'zeta'; Constellation:'Sgr'; RA:285.653 ; Decl:-29.88 ; PM_RA:-14.10 ; PM_Dec:3.66 ; Mag:2.60 ; HipNum:93506 ; HD:176687 ; Parallax:36.61 ;TPM:14.57 ;TrVel:1.89 ),
(Name:''; Greek:'beta'; Constellation:'Lib'; RA:229.252 ; Decl:-9.383 ; PM_RA:-96.39 ; PM_Dec:-20.76 ; Mag:2.61 ; HipNum:74785 ; HD:135742 ; Parallax:20.38 ;TPM:98.60 ;TrVel:22.93 ),
(Name:'Unukalhai'; Greek:'alpha'; Constellation:'Ser'; RA:236.067 ; Decl:6.426 ; PM_RA:134.66 ; PM_Dec:44.14 ; Mag:2.63 ; HipNum:77070 ; HD:140573 ; Parallax:44.54 ;TPM:141.71 ;TrVel:15.08 ),
(Name:''; Greek:'beta'; Constellation:'Ari'; RA:28.66 ; Decl:20.808 ; PM_RA:96.32 ; PM_Dec:-108.80 ; Mag:2.64 ; HipNum:8903 ; HD:11636 ; Parallax:54.74 ;TPM:145.31 ;TrVel:12.58 ),
(Name:''; Greek:'theta'; Constellation:'Aur'; RA:89.93 ; Decl:37.213 ; PM_RA:42.09 ; PM_Dec:-73.61 ; Mag:2.65 ; HipNum:28380 ; HD:40312 ; Parallax:18.83 ;TPM:84.79 ;TrVel:21.35 ),
(Name:''; Greek:'beta'; Constellation:'Crv'; RA:188.597 ; Decl:-23.397 ; PM_RA:0.86 ; PM_Dec:-56.00 ; Mag:2.65 ; HipNum:61359 ; HD:109379 ; Parallax:23.34 ;TPM:56.01 ;TrVel:11.38 ),
(Name:''; Greek:'alpha'; Constellation:'Col'; RA:84.912 ; Decl:-34.074 ; PM_RA:-0.10 ; PM_Dec:-24.05 ; Mag:2.65 ; HipNum:26634 ; HD:37795 ; Parallax:12.16 ;TPM:24.05 ;TrVel:9.38 ),
(Name:''; Greek:'delta'; Constellation:'Cas'; RA:21.453 ; Decl:60.235 ; PM_RA:297.24 ; PM_Dec:-49.49 ; Mag:2.66 ; HipNum:6686 ; HD:8538 ; Parallax:32.81 ;TPM:301.33 ;TrVel:43.54 ),
(Name:''; Greek:'eta'; Constellation:'Boo'; RA:208.671 ; Decl:18.399 ; PM_RA:-60.95 ; PM_Dec:-358.10 ; Mag:2.68 ; HipNum:67927 ; HD:121370 ; Parallax:88.17 ;TPM:363.25 ;TrVel:19.53 ),
(Name:''; Greek:'beta'; Constellation:'Lup'; RA:224.633 ; Decl:-43.134 ; PM_RA:-34.06 ; PM_Dec:-38.30 ; Mag:2.68 ; HipNum:73273 ; HD:132058 ; Parallax:6.23 ;TPM:51.25 ;TrVel:39.00 ),
(Name:''; Greek:'mu'; Constellation:'Vel'; RA:161.692 ; Decl:-49.42 ; PM_RA:62.55 ; PM_Dec:-53.57 ; Mag:2.69 ; HipNum:52727 ; HD:93497 ; Parallax:28.18 ;TPM:82.35 ;TrVel:13.85 ),
(Name:''; Greek:'alpha'; Constellation:'Mus'; RA:189.296 ; Decl:-69.136 ; PM_RA:-39.87 ; PM_Dec:-12.44 ; Mag:2.69 ; HipNum:61585 ; HD:109668 ; Parallax:10.67 ;TPM:41.77 ;TrVel:18.56 ),
(Name:''; Greek:'iota'; Constellation:'Aur'; RA:74.248 ; Decl:33.166 ; PM_RA:3.63 ; PM_Dec:-18.54 ; Mag:2.69 ; HipNum:23015 ; HD:31398 ; Parallax:6.37 ;TPM:18.89 ;TrVel:14.06 ),
(Name:''; Greek:'nu'; Constellation:'Sco'; RA:262.691 ; Decl:-37.296 ; PM_RA:-4.19 ; PM_Dec:-29.14 ; Mag:2.70 ; HipNum:85696 ; HD:158408 ; Parallax:6.29 ;TPM:29.44 ;TrVel:22.19 ),
(Name:''; Greek:'pi'; Constellation:'Pup'; RA:109.286 ; Decl:-37.097 ; PM_RA:-10.57 ; PM_Dec:7.00 ; Mag:2.71 ; HipNum:35264 ; HD:56855 ; Parallax:2.98 ;TPM:12.68 ;TrVel:20.17 ),
(Name:'Tarazed'; Greek:'gamma'; Constellation:'Aql'; RA:296.565 ; Decl:10.613 ; PM_RA:15.72 ; PM_Dec:-3.08 ; Mag:2.72 ; HipNum:97278 ; HD:186791 ; Parallax:7.08 ;TPM:16.02 ;TrVel:10.73 ),
(Name:''; Greek:'delta'; Constellation:'Sgr'; RA:275.248 ; Decl:-29.828 ; PM_RA:29.96 ; PM_Dec:-26.38 ; Mag:2.72 ; HipNum:89931 ; HD:168454 ; Parallax:10.67 ;TPM:39.92 ;TrVel:17.74 ),
(Name:''; Greek:'eta';   Constellation:'Dra'; RA:245.998 ; Decl:61.514 ; PM_RA:-16.98 ; PM_Dec:56.68 ; Mag:2.73 ; HipNum:80331 ; HD:148387 ; Parallax:37.18 ;TPM:59.17 ;TrVel:7.54 ),
(Name:''; Greek:'delta'; Constellation:'Oph'; RA:243.587 ; Decl:-3.694 ; PM_RA:-45.83 ; PM_Dec:-142.91 ; Mag:2.73 ; HipNum:79593 ; HD:146051 ; Parallax:19.16 ;TPM:150.08 ;TrVel:37.13 ),
(Name:''; Greek:'gamma'; Constellation:'Vir'; RA:190.417 ; Decl:-1.45 ; PM_RA:-616.66 ; PM_Dec:60.66 ; Mag:2.74 ; HipNum:61941 ; HD:110379 ; Parallax:84.53 ;TPM:619.64 ;TrVel:34.75 ),
(Name:''; Greek:'theta'; Constellation:'Car'; RA:160.739 ; Decl:-64.394 ; PM_RA:-18.87 ; PM_Dec:12.06 ; Mag:2.74 ; HipNum:52419 ; HD:93030 ; Parallax:7.43 ;TPM:22.39 ;TrVel:14.29 ),
(Name:''; Greek:'alpha1'; Constellation:'Lib'; RA:222.72 ; Decl:-16.042 ; PM_RA:-105.69 ; PM_Dec:-69.00 ; Mag:2.75 ; HipNum:72622 ; HD:130841 ; Parallax:42.25 ;TPM:126.22 ;TrVel:14.16 ),
(Name:''; Greek:'iota'; Constellation:'Cen'; RA:200.15 ; Decl:-36.712 ; PM_RA:-340.76 ; PM_Dec:-87.98 ; Mag:2.75 ; HipNum:65109 ; HD:115892 ; Parallax:55.64 ;TPM:351.93 ;TrVel:29.98 ),
(Name:''; Greek:'iota'; Constellation:'Ori'; RA:83.858 ; Decl:-5.91 ; PM_RA:2.27 ; PM_Dec:-0.62 ; Mag:2.75 ; HipNum:26241 ; HD:37043 ; Parallax:2.46 ;TPM:2.35 ;TrVel:4.53 ),
(Name:''; Greek:'beta'; Constellation:'Oph'; RA:265.868 ; Decl:4.567 ; PM_RA:-40.67 ; PM_Dec:158.80 ; Mag:2.76 ; HipNum:86742 ; HD:161096 ; Parallax:39.78 ;TPM:163.93 ;TrVel:19.53 ),
(Name:''; Greek:'beta'; Constellation:'Eri'; RA:76.963 ; Decl:-5.086 ; PM_RA:-83.39 ; PM_Dec:-75.44 ; Mag:2.78 ; HipNum:23875 ; HD:33111 ; Parallax:36.71 ;TPM:112.45 ;TrVel:14.52 ),
(Name:''; Greek:'beta'; Constellation:'Her'; RA:247.555 ; Decl:21.49 ; PM_RA:-98.43 ; PM_Dec:-14.49 ; Mag:2.78 ; HipNum:80816 ; HD:148856 ; Parallax:22.07 ;TPM:99.49 ;TrVel:21.37 ),
(Name:'Rasalgethi'; Greek:'alpha1/2'; Constellation:'Her'; RA:258.662 ; Decl:14.39 ; PM_RA:-6.71 ; PM_Dec:32.78 ; Mag:2.78 ; HipNum:84345 ; HD:156014 ; Parallax:8.53 ;TPM:33.46 ;TrVel:18.59 ),
(Name:''; Greek:'delta'; Constellation:'Cru'; RA:183.786 ; Decl:-58.749 ; PM_RA:-36.68 ; PM_Dec:-10.72 ; Mag:2.79 ; HipNum:59747 ; HD:106490 ; Parallax:8.96 ;TPM:38.21 ;TrVel:20.22 ),
(Name:''; Greek:'beta'; Constellation:'Dra'; RA:262.608 ; Decl:52.301 ; PM_RA:-15.59 ; PM_Dec:11.57 ; Mag:2.79 ; HipNum:85670 ; HD:159181 ; Parallax:9.02 ;TPM:19.41 ;TrVel:10.20 ),
(Name:''; Greek:'gamma'; Constellation:'Lup'; RA:233.785 ; Decl:-41.167 ; PM_RA:-16.05 ; PM_Dec:-25.52 ; Mag:2.80 ; HipNum:76297 ; HD:138690 ; Parallax:5.75 ;TPM:30.15 ;TrVel:24.85 ),
(Name:''; Greek:'zeta'; Constellation:'Her'; RA:250.323 ; Decl:31.602 ; PM_RA:-462.58 ; PM_Dec:345.05 ; Mag:2.81 ; HipNum:81693 ; HD:150680 ; Parallax:92.63 ;TPM:577.10 ;TrVel:29.53 ),
(Name:'Nihal'; Greek:'beta'; Constellation:'Lep'; RA:82.061 ; Decl:-20.759 ; PM_RA:-5.03 ; PM_Dec:-85.92 ; Mag:2.81 ; HipNum:25606 ; HD:36079 ; Parallax:20.49 ;TPM:86.07 ;TrVel:19.91 ),
(Name:''; Greek:'tau'; Constellation:'Sco'; RA:248.971 ; Decl:-28.216 ; PM_RA:-8.59 ; PM_Dec:-22.50 ; Mag:2.82 ; HipNum:81266 ; HD:149438 ; Parallax:7.59 ;TPM:24.08 ;TrVel:15.04 ),
(Name:''; Greek:'beta'; Constellation:'Hyi'; RA:6.413 ; Decl:-77.255 ; PM_RA:2220.12 ; PM_Dec:324.37 ; Mag:2.82 ; HipNum:2021 ; HD:2151 ; Parallax:133.78 ;TPM:2243.69 ;TrVel:79.50 ),
(Name:''; Greek:'lambda'; Constellation:'Sgr'; RA:276.993 ; Decl:-25.421 ; PM_RA:-44.81 ; PM_Dec:-186.29 ; Mag:2.82 ; HipNum:90496 ; HD:169916 ; Parallax:42.20 ;TPM:191.60 ;TrVel:21.52 ),
(Name:'Algenib'; Greek:'gamma'; Constellation:'Peg'; RA:3.309 ; Decl:15.184 ; PM_RA:4.70 ; PM_Dec:-8.24 ; Mag:2.83 ; HipNum:1067 ; HD:886 ; Parallax:9.79 ;TPM:9.49 ;TrVel:4.59 ),
(Name:''; Greek:'rho'; Constellation:'Pup'; RA:121.886 ; Decl:-24.304 ; PM_RA:-83.29 ; PM_Dec:46.38 ; Mag:2.83 ; HipNum:39757 ; HD:67523 ; Parallax:51.99 ;TPM:95.33 ;TrVel:8.69 ),
(Name:''; Greek:'beta'; Constellation:'TrA'; RA:238.787 ; Decl:-63.43 ; PM_RA:-188.45 ; PM_Dec:-401.92 ; Mag:2.83 ; HipNum:77952 ; HD:141891 ; Parallax:81.24 ;TPM:443.91 ;TrVel:25.90 ),
(Name:''; Greek:'beta'; Constellation:'Ara'; RA:261.325 ; Decl:-55.53 ; PM_RA:-8.23 ; PM_Dec:-24.71 ; Mag:2.84 ; HipNum:85258 ; HD:157244 ; Parallax:5.41 ;TPM:26.04 ;TrVel:22.82 ),
(Name:''; Greek:'alpha'; Constellation:'Ara'; RA:262.961 ; Decl:-49.876 ; PM_RA:-31.27 ; PM_Dec:-67.15 ; Mag:2.84 ; HipNum:85792 ; HD:158427 ; Parallax:13.46 ;TPM:74.07 ;TrVel:26.09 ),
(Name:''; Greek:'zeta'; Constellation:'Per'; RA:58.533 ; Decl:31.884 ; PM_RA:4.41 ; PM_Dec:-9.15 ; Mag:2.84 ; HipNum:18246 ; HD:24398 ; Parallax:3.32 ;TPM:10.16 ;TrVel:14.50 ),
(Name:''; Greek:'delta'; Constellation:'Cap'; RA:326.76 ; Decl:-16.127 ; PM_RA:263.26 ; PM_Dec:-296.23 ; Mag:2.85 ; HipNum:107556 ; HD:207098 ; Parallax:84.58 ;TPM:396.31 ;TrVel:22.21 ),
(Name:'Vindemiatrix'; Greek:'epsilon'; Constellation:'Vir'; RA:195.545 ; Decl:10.959 ; PM_RA:-275.05 ; PM_Dec:19.96 ; Mag:2.85 ; HipNum:63608 ; HD:113226 ; Parallax:31.90 ;TPM:275.77 ;TrVel:40.98 ),
(Name:'Alcyone'; Greek:'eta'; Constellation:'Tau'; RA:56.871 ; Decl:24.105 ; PM_RA:19.35 ; PM_Dec:-43.11 ; Mag:2.85 ; HipNum:17702 ; HD:23630 ; Parallax:8.87 ;TPM:47.25 ;TrVel:25.25 ),
(Name:''; Greek:'alpha'; Constellation:'Hyi'; RA:29.691 ; Decl:-61.57 ; PM_RA:262.54 ; PM_Dec:26.88 ; Mag:2.86 ; HipNum:9236 ; HD:12311 ; Parallax:45.74 ;TPM:263.91 ;TrVel:27.35 ),
(Name:''; Greek:'delta'; Constellation:'Cyg'; RA:296.244 ; Decl:45.131 ; PM_RA:43.22 ; PM_Dec:48.44 ; Mag:2.86 ; HipNum:97165 ; HD:186882 ; Parallax:19.07 ;TPM:64.92 ;TrVel:16.14 ),
(Name:''; Greek:'alpha'; Constellation:'Tuc'; RA:334.626 ; Decl:-60.259 ; PM_RA:-71.48 ; PM_Dec:-38.15 ; Mag:2.87 ; HipNum:110130 ; HD:211416 ; Parallax:16.42 ;TPM:81.02 ;TrVel:23.39 ),
(Name:''; Greek:'mu'; Constellation:'Gem'; RA:95.74 ; Decl:22.514 ; PM_RA:56.84 ; PM_Dec:-108.79 ; Mag:2.87 ; HipNum:30343 ; HD:44478 ; Parallax:14.07 ;TPM:122.74 ;TrVel:41.35 ),
(Name:'TPer'; Greek:'theta'; Constellation:'Per'; RA:41.0487; Decl:49.2287 ; PM_RA:334.66; PM_Dec:-89.99; Mag:11 ; HipNum:12777 ; HD:16895; Parallax:89.87 ;TPM:0; TrVel:0 )  );
// Om: theta persei was added to be able to check calculations with AA examples. Not a particularly visible star...

Procedure CreateAstrosHipparcos150;
var i:integer;
begin
  if not AstrosH150Created then
    begin
      for i:=1 to NumH150stars do
        StarsH150[i] := TStarH150.Create(PosEstrelasH150[i]);
      AstrosH150Created:=true;
    end;
end;

function  FindAstroH150ByName(const aName:String):TStarH150;
var i:integer; aHIP:integer; aAstro:TStarH150;
begin
  Result:=nil;
  for i:=1 to NumH150stars do      // linear search
    begin
      aAstro := StarsH150[i];
      if (aAstro.Name=aName) then
        begin
          Result:=aAstro;
          exit;
        end;
    end;
end;

Procedure FreeAstrosHipparcos150;
var i:integer;
begin
  for i:=1 to NumH150stars do
    begin
      StarsH150[i].Free;
      StarsH150[i]:=nil;
    end;
end;

// ---------------------------------------------------------------------------------
// navigation stars. Positions extracted from Nautical Almanac 1993 - Brazilian navy
var
  NavStarsCreated:boolean=false;

const
  // NASTROS=NESTRELAS+2+4;   {NESTRELAS+NPlanetas+sol+lua}
  {Coordenadas das estrelas extraidas do anuario astronomico 1993}
  PosEstrelas1993:array[1..NESTRELAS] of REstrela= {Pos das estrelas em 1/1/1993 0 hs}
  ((Nome:'Acamar'    ;RA:2.96711  ;Decl:-40.33500 ;Mag:+3.1  ; PM_RA:-0.0039;PM_Dec:+0.019 ), {            }
   (Nome:'Achernar'  ;RA:1.6245778;Decl:-57.275555;Mag:+0.60 ; PM_RA:+0.0127;PM_Dec:-0.034 ), { alf eri 97 }
   (Nome:'Acrux'     ;RA:12.43722 ;Decl:-63.05667 ;Mag:+1.10 ; PM_RA:-0.0044;PM_Dec:-0.012 ), { alf1 cru ??}
   (Nome:'Adhara'    ;RA:6.973178 ;Decl:-28.963389;Mag:+1.63 ; PM_RA:+0.0005;PM_Dec:+0.004 ), { eps cma 117}
   (Nome:'Albireo'   ;RA:19.507136;Decl:27.9461388;Mag:+3.24 ; PM_RA:+0.0003;PM_Dec:-0.004 ), { bet1 cyg 161}
   (Nome:'Aldebaran' ;RA:4.59265  ;Decl:16.49625  ;Mag:+1.06 ; PM_RA:+0.0045;PM_Dec:-0.19  ), { alf tau 107}
   (Nome:'Alioth'    ;RA:12.89556 ;Decl:55.99167  ;Mag:+1.7  ; PM_RA:+0.0134;PM_Dec:-0.005 ), { eps uma   }
   (Nome:'Alkaid'    ;RA:13.787772;Decl:49.342167 ;Mag:+1.91 ; PM_RA:-0.0124;PM_Dec:-0.01  ), { eta uma 138}
   (Nome:'Al Na-ir'  ;RA:22.12989 ;Decl:-46.99667 ;Mag:+2.2  ; PM_RA:+0.0128;PM_Dec:-0.153 ), {           }
   (Nome:'Alnilam'   ;RA:5.59833  ;Decl:-1.20667  ;Mag:+1.8  ; PM_RA:+0.0001;PM_Dec:-0.002 ), {           }
   (Nome:'Alphard'   ;RA:9.45467  ;Decl:-8.6300   ;Mag:+2.2  ; PM_RA:-0.0008;PM_Dec:+0.033 ), { alf hya  }
   (Nome:'Alphecca'  ;RA:15.573125;Decl:26.7350278;Mag:+2.31 ; PM_RA:+0.0091;PM_Dec:-0.088 ), { alf crb 144}
   (Nome:'Alpheratz' ;RA:0.13400  ;Decl:29.05667  ;Mag:+2.2  ; PM_RA:+0.0104;PM_Dec:-0.163 ), { alf and   }
   (Nome:'Altair'    ;RA:19.840608;Decl:8.85069444;Mag:+0.89 ; PM_RA:+0.0363;PM_Dec:+0.387 ), { alf aql 161}
   (Nome:'Ankaa'     ;RA:0.43256  ;Decl:-42.34667 ;Mag:+2.4  ; PM_RA:+0.0189;PM_Dec:-0.395 ), { alf pheonix}
   (Nome:'Antares'   ;RA:16.482953;Decl:-26.416167;Mag:+1.20 ; PM_RA:-0.0006;PM_Dec:-0.021 ), { alf sco 149}
   (Nome:'Arcturus'  ;RA:14.255792;Decl:19.215222 ;Mag:+0.24 ; PM_RA:-0.0770;PM_Dec:-1.998 ), { alf boo 139}
   (Nome:'Atria'     ;RA:16.798333;Decl:-69.013389;Mag:+1.88 ; PM_RA:+0.0035;PM_Dec:-0.032 ), { alf tra 150}
   (Nome:'Avior'     ;RA:8.37367  ;Decl:-59.48667 ;Mag:+1.7  ; PM_RA:-0.0033;PM_Dec:+0.017 ), { eps car   }
   (Nome:'Bellatrix' ;RA:5.413264 ;Decl: 6.343278 ;Mag:+1.70 ; PM_RA:-0.0006;PM_Dec:-0.015 ), { gam ori 110}
   (Nome:'Betelgeuse';RA:5.9139028;Decl:7.4054167 ;Mag:+0.10 ; PM_RA:+0.0019;PM_Dec:+0.01  ), { alf ori 113}
   (Nome:'Canopus'   ;RA:6.397375;Decl:-52.6929722;Mag:-0.86 ; PM_RA:+0.0034;PM_Dec:+0.021 ), { alf car 115}
   (Nome:'Capella'   ;RA:5.2704694;Decl:45.9925278;Mag:+0.21 ; PM_RA:+0.0074;PM_Dec:-0.424 ), { alf aur 110}
   (Nome:'Castor'    ;RA:7.57003056;Decl:31.901944;Mag:+1.99 ; PM_RA:-0.0135;PM_Dec:-0.098 ), { alf gem 120}
   (Nome:'Deneb'     ;RA:20.686303 ;Decl:45.258583;Mag:+1.33 ; PM_RA:+0.0004;PM_Dec:+0.002 ), { alf cyg 164}
   (Nome:'Denebola'  ;RA:11.8120922;Decl:14.607417;Mag:+2.23 ; PM_RA:-0.0342;PM_Dec:-0.114 ), { bet leo 132}
   (Nome:'Diphda'    ;RA:0.72089  ;Decl:-18.02500 ;Mag:+2.2  ; PM_RA:+0.0165;PM_Dec:+0.032 ), { bet cet   }
   (Nome:'Dubhe'     ;RA:11.05556 ;Decl:61.78167  ;Mag:+2.0  ; PM_RA:-0.0164;PM_Dec:-0.067 ), { alf uma   }
   (Nome:'Elnath'    ;RA:5.43156  ;Decl:28.60167  ;Mag:+1.8  ; PM_RA:+0.0018;PM_Dec:-0.176 ), { bet tau   }
   (Nome:'Eltanin'   ;RA:17.94022 ;Decl:51.48833  ;Mag:+2.4  ; PM_RA:-0.0007;PM_Dec:-0.019 ), { gam dra   }
   (Nome:'Enif'      ;RA:21.73078 ;Decl:9.84500   ;Mag:+2.5  ; PM_RA:+0.0021;PM_Dec:+0.000 ), { eps peg   }
   (Nome:'Fomalhaut' ;RA:22.954525;Decl:-29.660417;Mag:+1.29 ; PM_RA:+0.0257;PM_Dec:-0.165 ), { alf psa 173}
   (Nome:'Gacrux'    ;RA:12.513330;Decl:-57.071389;Mag:+1.61 ; PM_RA:+0.0032;PM_Dec:-0.263 ), { gam cru 134}
   (Nome:'Gienah'    ;RA:12.25778 ;Decl:-17.50333 ;Mag:+2.8  ; PM_RA:-0.0112;PM_Dec:+0.023 ), { gam crv   }
   (Nome:'Hadar'     ;RA:14.055594;Decl:-60.336417;Mag:+0.86 ; PM_RA:-0.0035;PM_Dec:-0.019 ), { bet cen 139}
   (Nome:'Hamal'     ;RA:2.11344  ;Decl:23.43333  ;Mag:+2.2  ; PM_RA:+0.0138;PM_Dec:-0.149 ), { alf ari   }
   (Nome:'Kaus Austr.';RA:18.3950  ;Decl:-34.38833 ;Mag:+2.0 ; PM_RA:-0.0026;PM_Dec:-0.126 ), { eps sgr   }
   (Nome:'Kochab'    ;RA:14.84444 ;Decl:74.17833  ;Mag:+2.2  ; PM_RA:-0.0073;PM_Dec:+0.012 ), { bet umi   }
   (Nome:'Markab'    ;RA:23.07367 ;Decl:15.1700   ;Mag:+2.6  ; PM_RA:+0.0043;PM_Dec:-0.042 ), { alf peg   }
   (Nome:'Menkar'    ;RA:3.03244  ;Decl:4.06333   ;Mag:+2.8  ; PM_RA:-0.0006;PM_Dec:-0.078 ), { alf cet   }
   (Nome:'Menkent'   ;RA:14.10467 ;Decl:-36.33500 ;Mag:+2.3  ; PM_RA:-0.0428;PM_Dec:-0.52  ), { tet cen   }
   (Nome:'Miaplacidus';RA:9.219747;Decl:-69.686889;Mag:+1.80 ; PM_RA:-0.0297;PM_Dec:+0.108 ), { bet car 125}
   (Nome:'Mirfak'    ;RA:3.3978333;Decl:49.8405833;Mag:+1.90 ; PM_RA:+0.0025;PM_Dec:-0.025 ), { alf per 103}
   (Nome:'Nunki'     ;RA:18.91378 ;Decl:-26.30500 ;Mag:+2.1  ; PM_RA:+0.0009;PM_Dec:-0.054 ), { ?? sgr     }
   (Nome:'Peacock'   ;RA:20.418081;Decl:-56.758861;Mag:+2.12 ; PM_RA:+0.0014;PM_Dec:-0.088 ), { alf pav 164}
   (Nome:'Polaris'   ;RA:2.3927777;Decl:89.2194444;Mag:+2.00 ; PM_RA:+0.2012;PM_Dec:-0.016 ), { alf umi    }
   (Nome:'Pollux'    ;RA:7.7489028;Decl:28.0408556;Mag:+1.21 ; PM_RA:-0.0474;PM_Dec:-0.046 ), { bet gem 121}
   (Nome:'Procyon'   ;RA:7.649608 ;Decl: 5.241472 ;Mag:+0.48 ; PM_RA:-0.0474;PM_Dec:-1.024 ), { alf cmi 120}
   (Nome:'Rasalhague';RA:17.57667 ;Decl:12.565    ;Mag:+2.1  ; PM_RA:+0.0082;PM_Dec:-0.226 ), { alf oph          }
   (Nome:'Regulus'   ;RA:10.133869;Decl:11.9983056;Mag:+1.34 ; PM_RA:-0.0169;PM_Dec:+0.008 ), { alf leo 127}
   (Nome:'Rigel'     ;RA:5.237325 ;Decl:-8.2102222;Mag:+0.34 ; PM_RA:+0.0002;PM_Dec:-0.002 ), { bet ori 110}
   (Nome:'Rigil Kent';RA:14.652094;Decl:-60.803778;Mag:+0.10 ; PM_RA:-0.4949;PM_Dec:+0.699 ), { alf cen 141}
   (Nome:'Sabik'     ;RA:17.16622 ;Decl:-15.71667 ;Mag:+2.6  ; PM_RA:+0.0027;PM_Dec:+0.098 ), { ??? oph   }
   (Nome:'Schedar'   ;RA:0.66889  ;Decl:56.50500  ;Mag:+2.5  ; PM_RA:+0.0065;PM_Dec:-0.032 ), { alf cas   }
   (Nome:'Shaula'    ;RA:17.55215 ;Decl:-37.098278;Mag:+1.71 ; PM_RA:+0.0001;PM_Dec:-0.028 ), { lam sco 153}
   (Nome:'Sirius'    ;RA:6.747986 ;Decl:-16.707500;Mag:-1.58 ; PM_RA:-0.0379;PM_Dec:-1.206 ), { alf cma 116}
   (Nome:'Spica'     ;RA:13.413961;Decl:-11.125861;Mag:+1.21 ; PM_RA:-0.0026;PM_Dec:-0.027 ), { alf vir 137}
   (Nome:'Suhail'    ;RA:9.129633 ;Decl:-43.40383 ;Mag:+2.22 ; PM_RA:-0.0014;PM_Dec:+0.014 ), { lam vel 125}
   (Nome:'Vega'      ;RA:18.6114  ;Decl:38.7773056;Mag:+0.14 ; PM_RA:+0.0173;PM_Dec:+0.286 ), { alf lyr 157}
   (Nome:'Zuben-ubi' ;RA:14.84156 ;Decl:-16.01333 ;Mag:+2.9  ; PM_RA:-0.1060;PM_Dec:-0.067 )  { ??  lib   }
    );

{ TNavStar }

constructor TNavStar.Create(const StarRec:REstrela);
begin
  TObject.Create;         // skip TStarH150.Create()
  Name := StarRec.Nome;

  // no calculation yet
  fGMT := 0;      fRA   := 0;    fDecl := 0;
  fGHA := 0;      fMagnitude := 0;
  fAltitudeOfObjectAtRise := -0.5667;    //  for stars and planets - AA.pag 98 - due to atmospheric refraction
  fLastCoordCalculation   := 0;         //  0=never

  fConst:='';
  fGreek:='';

  // star positions calculated from initial state
  EpochiPM   := EpochiNav;           // 1993.0
  EpochiRef  := EpochiNav;          // for Nav catalog, Epochi is the same for Ref and PM
  RAi        := StarRec.RA*15;      // aqui vem em seconds/year, mas precisa desse 15 para converter de hms --> graus
  Decli      := StarRec.Decl;
  fAlreadyCalculatedInitialCoordinates:=FALSE;

  fMagnitude := StarRec.Mag;

  //was fpm_RA     := StarRec.PM_RA  ;    //15 converte de graus p/ horas (ou vice-versa)    ?? tinha um /Cosg(Decli) ??
  //    fpm_Dec    := StarRec.PM_Dec ;    //jul/04 adicionei efeitos da proper motion (in arcsecs/year J2000 - Smithsonian Star Catalog

  // converte velocidade para mas ( mili arcsecond )
  fpm_RA     := StarRec.PM_RA*15;    // 15 converte de graus p/ horas (ou vice-versa)    ?? tinha um /Cosg(Decli) ??
  fpm_Dec    := StarRec.PM_Dec;      //jul/04 adicionei efeitos da proper motion (in arcsecs/year J2000 - Smithsonian Star Catalog

  // fDpmra := 0;
  // fDpmDec:= 0;
  fDpm   := 0;
end;

// Procedure TNavStar.CalcCoordinates(const aDia,aMes,aAno,aHora:Double); // calc fRA, fDecl at fGMT
// var DAlfa,DDelta,DJ,pm_anos:Double;
// begin
//   if not fAlreadyCalculatedInitialCoordinates then {Ve se calcula coordenada iniciais da estrela}
//     begin
//       CalculaJ2000Correction(1.0,1.0, EpochiRef, 0.0, DAlfai,DDeltai); {Calcula corr da data inic. em rel a J2000.0 }
//       fAlreadyCalculatedInitialCoordinates:=TRUE;
//     end;
//
//   DJ := JD(aAno,aMes,aDia,aHora);               // calc Julian date
//   if AlreadyCalculatedToday(DJ) then exit;     // already calculated coord today, no need to recalculate slow changing coordinates for the same day
//   fLastCoordCalculation := DJ;                // save julian date of last calculation
//
//   CalculaJ2000Correction(aDia,aMes,aAno,aHora,{out:} fDeltaRA, fDeltaDecl); {returns summed corrections for Prec, abr and nut }
//   { (-) correcoes ref. ao inst. em que as coordenadas foram extraidas do
//    Anuario Astronomico  (DDAlfai e DDeltai) =1993.0 }
//
//   DAlfa  := fDeltaRA  - DAlfai;     {Subtrai correcoes relativas ao instante inicial}
//   DDelta := fDeltaDecl- DDeltai;
//
//   //jul/04- calcula correcao relativa ao proper motion
//   pm_anos := (aAno+aMes/12-EpochiPM);    //diff in relation to J2000 (time of coord table) in years
//
//   // proper motion factors
//   fDeltaPmRA   := fpm_RA *pm_anos;  // conv de arcseconds para graus ( 15 ??? )
//   fDeltaPmDecl := fpm_Dec*pm_anos;
//
//   fRA   := RAi+  DAlfa /3600 + fDeltaPmRA/3600;    // Aplica correcoes - Deltas em " --> /3600
//   fDecl := Decli+DDelta/3600 + fDeltaPmDecl/3600;
//
//   //calc vetor de proper motion
//   // fDpm := fDpmRA*cos(fDecl);
//   // fDpm := sqrt(fDpmDec*fDpmDec+fDpm*fDpm); //modulo do vetor
//
//  { WriteLn('DAT:',DAlfa:8:4);
//    WriteLn('DDT:',DDelta:8:4);}
// end;

Procedure  TNavStar.GetObjectData(SL:TStrings);
var aSHA,aDummy:Double;
begin
  SL.Add(Name+' ('+fGreek+' '+fConst+')');

  SL.Add('epoch J1993.0');                 // nav stars use apparent positions extrated from Astronomical Almanac 1993
  SL.Add('RAi=  '+ floatToGMSD(RAi) +' ( '+floatToHHMMSS(RAi*24/360)+')' ); // degrees (hours)
  SL.Add('Decli='+ floatToGMSD_Lat(Decli) );
  SL.Add('');

  SL.Add('at time='+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', fGMT) +' UT' );  //GMT = Universal Time
  SL.Add('RA=  '  + floatToGMSD(fRA) +' ( '+ floatToHHMMSS(fRA*24/360)+')' );   // degrees (hours)

  aSHA := 360.0 - fRA;              // SHA and RA are the same thing, but with a different convention
  SL.Add('SHA= '+ floatToGMSD(aSHA) + ' ('+ R2GMD(aSHA,aDummy,' -')  +')' );
  SL.Add('Decl='+ floatToGMSD_Lat(fDecl) + ' ('+ R2GMD(fDecl,aDummy,'NS')  +')' );
  SL.Add('GHA= '+  floatToGMSD(fGHA) );

  SL.Add('Mag= '+  Format('%5.2f',[fMagnitude]) );

  if NavVerboseReport then  // verbose version shows detailed corrections
    begin
      SL.Add('Corrections---/\----\/---');
      SL.Add('   pm spd '+  Format('%6.4f %6.4f',[fpm_RA,fpm_Dec])            +' "/y' );
      SL.Add('   Nut  '+  Format('%5.2f %5.2f',[fDeltaNutRA, fDeltaNutDecl] ) +' " '  );
      SL.Add('   Prec '+  Format('%5.2f %5.2f',[fDeltaPrecRA,fDeltaPrecDecl])  );
      SL.Add('   Aber '+  Format('%5.2f %5.2f',[fDeltaAberRA,fDeltaAberDecl] ) );
      SL.Add('   Tot  '+  Format('%5.2f %5.2f',[fDeltaRA,fDeltaDecl]         ) );
      SL.Add('   PM   '+  Format('%5.2f %5.2f',[fDeltaPmRA,  fDeltaPmDecl]   ) );
    end;
end;

Procedure CreateNavStars;
var i:integer;
begin
  if not NavStarsCreated then
    begin
      for i:=1 to NEstrelas do
        Estrelas[i] := TNavStar.Create( PosEstrelas1993[i] );
      NavStarsCreated:=true;
    end;
end;

function  FindNavStar(const aName:String):TNavStar;
var i:integer; aHIP:integer; aAstro:TNavStar;
begin
  Result:=nil;
  for i:=1 to NEstrelas do      // linear search
    begin
      aAstro := Estrelas[i];
      if (aAstro.Name=aName) then
        begin
          Result:=aAstro;
          exit;
        end;
    end;
end;

Procedure FreeAstrosNavStars;
var i:integer;
begin
  for i:=1 to NEstrelas do
    begin
      Estrelas[i].Free;
      Estrelas[i]:=nil;
    end;
end;

end.
