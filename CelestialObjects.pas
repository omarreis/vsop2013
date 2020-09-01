unit CelestialObjects;      //--  Celestial objects  --\\
 //------------------------//
//   github.com/omarreis/vsop2013/planetfun
//   by oMAR
//   History:
//     ago20: Om: v1.0

interface

uses
  System.Math,
  System.Classes,
  System.Math.Vectors,
  System.SysUtils,
  System.Generics.Collections,

  doubleVector3D,   // TVector3D_D
  vsop2013,        // planetary ephemeris.  TVector3D_D
  PlanetData;     // planets physical data

type

  TCelObjType=( otStar, otPlanet, otSatellite );

  TCelestialObject=class
  private
  public
    Name    :String;          // common name
    ObjType :TCelObjType;     // basic obj classification

    Mass    :Double;          // in Kg    ( from Wikipedia )
    Radius  :Double;          // in Km    ( mean radius )

    fExists:boolean;          // fExists tells if Planet participates in the gravity Universe ( def=true )
    fChartVisible:boolean;    // chart of this obj visible ?

    // current position storage
    fJDE    :Double;          // time Julian date
    Posit   :TVector3D_D;     // heliocentric coordinates in AU  ( same units as VSOP2013 )
    Spd     :TVector3D_D;     // speed vector in AU/day          ( same units as VSOP2013 )
    // vsop2013 ephemeris storage
    vIP    :integer;          // param: planet index in vsop2013  ( 1..9 )
    vJDE   :Double;           // time Julian date
    vPos   :TVector3D_D;      // heliocentric coordinates in AU  from VSOP2013
    vSpd   :TVector3D_D;      // speed in AU/day from VSOP2013
    // temporary storage, so we can calculate all Forces, than move all planets at once
    tJDE   :Double;           // time Julian date
    tPos   :TVector3D_D;      // heliocentric coordinates in AU  from VSOP2013
    tSpd   :TVector3D_D;      // speed in AU/day from VSOP2013
    // SpdMid is used for Leapfrog integration
    SpdMid :TVector3D_D;      // Starts with V 1/2.. than V 3/2 ...

    constructor Create;
    Destructor  Destroy; override;
    Procedure   copyVSOP2013ephemerisToCurrent;
  end;

  // celestial objects database implemented as a generic TList<>
  TCelestialObjectsDatabase= class( TList< TCelestialObject > )
    function getObjectByName(const aName:String):TCelestialObject;
  end;

var
  CelestialObjectsDB:TCelestialObjectsDatabase;

Procedure CreateCelestialObjectsDB;
Procedure PositionObjectsUsingEphemeris(const JDE:Double);      // using VSOP2013
Procedure copyVSOP2013ephemerisToCurrent;

Procedure setSpdMid_forLeapfrogIntegration(const aJDE:Double);  // set initial obj speed V1/2 , for first Add_LeapfrogIntegrationDT() interation
Procedure Add_LeapfrogIntegrationDT( const aDT:Double );        // add a DeltaT using Newton's acceleration formula ( Universal gravitation )

implementation   //--------------------------------------

Procedure CreateCelestialObjectsDB;
var aObj:TCelestialObject; i:integer;
begin
  CelestialObjectsDB := TCelestialObjectsDatabase.Create;

  for i := 0 to NUM_OBJS-1 do   // Sun, planets and Moon
    begin
      aObj         := TCelestialObject.Create;
      // load  properties from const in PlanetData.pas
      aObj.Name    := PLANET_DATA[i].name;
      aObj.Mass    := PLANET_DATA[i].mass;
      aObj.Radius  := PLANET_DATA[i].radius;
      aObj.fExists       := true;
      aObj.fChartVisible := true;
      // also available: rotPer  revPer  Obliq
      // objs current pos,spd
      aObj.fJDE    := 0;  //=none
      aObj.Posit   := Vector3D_D(0,0,0);   // =not initialized
      aObj.Spd     := Vector3D_D(0,0,0);
      // vsop2013 ephemeris pos,spd
      aObj.vJDE    := 0;  //=none
      aObj.vPos    := Vector3D_D(0,0,0);
      aObj.vSpd    := Vector3D_D(0,0,0);
      // temporary storage pos,spd
      aObj.tJDE    := 0;  //=none
      aObj.tPos    := Vector3D_D(0,0,0);
      aObj.tSpd    := Vector3D_D(0,0,0);
      // SpdMid used as intermediary result for leapfrog integration
      aObj.SpdMid  := Vector3D_D(0,0,0);

      aObj.vIP    :=-1;  // -1 = invalid
      if      (i=0)  then aObj.ObjType := otStar       // Sun
      else if (i=10) then aObj.ObjType := otSatellite  // Moon
      else begin
         aObj.ObjType := otPlanet;  // planets
         aObj.vIP     := i;         // planets have index in VSOP2013 ( 1..9 )
      end;
      CelestialObjectsDB.Add( aObj );
    end;
  // TODO: SOme stars
end;

Procedure PositionObjectsUsingEphemeris(const JDE:Double);  // using  ephemeris data ( p.e. vsop2013 )
var aObj:TCelestialObject; aPosition,aSpeed:TVector3D_D;
begin
  if not ( Assigned(VSOP_File) and VSOP_File.fLoaded ) then exit;   //sanity test. a VSOP2013 file must be loaded

  for aObj in CelestialObjectsDB do
    begin
      if (aObj.Name='Sun') then
        begin
          aObj.vJDE := JDE;                // Sun is positioned in the heliocentric center
          aObj.vPos := Vector3D_D(0,0,0);
          aObj.vSpd := Vector3D_D(0,0,0);
        end
        else if (aObj.vIP>0) then      // is a vsop2013 planet?
        begin
          if VSOP_File.calculate_coordinates( {ip:} aObj.vIP , {jde:}JDE, {out:} aPosition, aSpeed) then
              begin
                // upd ephemeris fields
                aObj.vJDE := JDE;
                aObj.vPos := aPosition;
                aObj.vSpd := aSpeed;
                // current position,spd not affected by this calculation.
                //  call copyVSOP2013ephemerisToCurrent for that
              end;
        end
      else if (aObj.Name='Moon') then
        begin
          // TODO:
        end;
    end;
end;

// copy planet's vsop2013 pos/spd to Posit,Spd fields
Procedure copyVSOP2013ephemerisToCurrent;   // VSOP2013 ephemeris --> Current
var aObj:TCelestialObject;
begin
  for aObj in CelestialObjectsDB do
    begin
      if (aObj.vJDE>0) then      // is a vsop2013 planet?
        aObj.copyVSOP2013ephemerisToCurrent;
    end;
end;

// copy planet's vsop2013 spd (1/2) to Posit,Spd fields
Procedure setSpdMid_forLeapfrogIntegration(const aJDE:Double);
var aObj:TCelestialObject; aPosition,aSpeed:TVector3D_D;
begin
  for aObj in CelestialObjectsDB do
    begin
      if (aObj.Name='Sun') then
        begin
          aObj.SpdMid := Vector3D_D(0,0,0);
        end
        else if (aObj.vIP>0) then      // is a vsop2013 planet?
        begin
          if VSOP_File.calculate_coordinates( {ip:} aObj.vIP , {jde:}aJDE, {out:} aPosition, aSpeed) then
             aObj.SpdMid := aSpeed;    // we need spd 1/2 only
        end
      else if (aObj.Name='Moon') then
        begin
          // TODO:
        end;
    end;
end;

// Calc instantaneous Newton acceleration caused by all other celestial objects ( in au/day^2 )
Function calcAcceleration( aObj:TCelestialObject; const aPosit:TVector3D_D ):TVector3D_D;   // no relativistic or light speed delays considered
var aObj2:TCelestialObject;
    M2,D,FMag:Double;
    F,V,SumF,P1,P2:TVector3D_D;
begin
   P1   := aPosit;     // uses aPosit, not aObj.Posit

   SumF := Vector3D_D(0,0,0);     // sum up all forces acting on this planet
   for aObj2 in CelestialObjectsDB do   // all planets attract each other
     if (aObj<>aObj2) then             // does not attract itself..
       begin
         if (aObj2.fJDE=0) or (not aObj2.fExists) then continue;  // ignore objects not initialized  (p.e. Moon) or non existing
         P2   := aObj2.Posit;
         V    := P2-P1;              // vector between objects
         D    := V.Length * AUtoM;  // Dist between objs in m
         if ( D<>0 ) then          //??
           begin
             M2   := aObj2.Mass;
             // {*M1} commented out cause we multiplied only to divide in the end
             FMag := UnivGravConst{*M1}*M2/(D*D);    // force magnitude from Newton's Universal Gravitation  ( using international units )
             V    := V.Normalize;                   // force versor (components in N)
             F    := FMag * V;                     // actualy this F is acceleration
             SumF := SumF + F;                    // vectorial sum of all forces acting in this obj
           end;
       end;
   // SumF force vector in N
   Result := SumF * M_S2toAU_day2 {/M1} ;           // acceleration vector ( converted from m/s^2 to au/day^2 )
end;


// Leapfrog integration - See https://young.physics.ucsc.edu/115/leapfrog.pdf
//           0             1                         0) obtain V1/2  for JDEini+DT/2  ( Planet Speed in the middle of 1st DT )
//     /-----------\ /----------\ /-----
//    x0            x1           x2                  1) x1   = x0 + dt * V1/2   <--\
//    |------+------|------+------|------+------>                                  |
//    t0     t1/2   t1     t3/2   t2     t5/2                                      | repeat
//                                                                                 |
//           v1/2          v3/2          v5/2        2) V3/2 = V1/2 + dt*Acc(1)  --/
//            \___________/ \___________/
//                  0             1
//
Procedure Add_LeapfrogIntegrationDT( const aDT:Double ); // aDT in days
var aAcc,aSpd,aPosit,aSunPos,aSunSpd:TVector3D_D;
    aObj,Sun:TCelestialObject;
    aDT2:Double;
begin
  Sun  := CelestialObjectsDB.getObjectByName('Sun');  // memoise stuff for speed
  aDT2 := aDT/2;
  // calc displacements for all objs. save to temp
  for aObj in CelestialObjectsDB do
    begin
      if (aObj.fJDE=0) or                       // ignore objects not initialized  (p.e. Moon)
         (not aObj.fExists) then continue;      // fExists is used to "mute" object gravity actions
      aSpd   := aObj.SpdMid;                    // aSpd = previous mid point Spd (i.e. V n+1/2 )
      aPosit := aObj.Posit + aDT*aSpd;          // move travel segment                                  // Xn+1   = Xn + dt * Vn+1/2
      aAcc   := calcAcceleration(aObj,aPosit);  // calc overall accel at mid point
      aSpd   := aSpd + aDT*aAcc;                // calc next SpdMid                                     // Vn+3/2 = Vn+1/2 + dt*Acc  (leapfrog)
      // Save calculated to temp. Don't move planets while they are doing their action at a distance
      aObj.tJDE := aObj.fJDE  + aDT;            // increase DT
      aObj.tPos := aPosit;                      // save X n+1
      aObj.tSpd := aSpd;                        // save V n+3/2 in temporary tSpd
   end;

  // now copy temp to current for all objs at once
  aSunPos := Vector3D_D(0,0,0);
  aSunSpd := Vector3D_D(0,0,0);
  for aObj in CelestialObjectsDB do
    begin
      if not aObj.fExists then continue;  // not moving unexisting bodies..
      if (aObj=Sun) then  // get Sun position and Spd. These will be the next heliocentric center, by definition
        begin                                                     // lock in the Sun center
          aSunPos := aObj.tPos;
          aSunSpd := aObj.tSpd;
        end;

      aObj.fJDE   := aObj.tJDE;
      aObj.Posit  := aObj.tPos-aSunPos;   // upd current planet position from temp
      aObj.SpdMid := aObj.tSpd-aSunSpd;   // SpdMid = V n+3/2
      aObj.tSpd   := aObj.SpdMid;         // TOFIX: Setting speed = speed in the middle of segment ( not correct )
    end;
end;

{ TCelestialObjectsDatabase }

// linear search TODO: Change to binary search as the catalog grows..
function TCelestialObjectsDatabase.getObjectByName( const aName: String): TCelestialObject;
var aObj:TCelestialObject;
begin
  for aObj in Self do
    begin
      if (aObj.Name=aName) then
        begin
          Result := aObj;
          exit;
        end;
    end;
  Result := nil; // not found
end;

{ TCelestialObject }

constructor TCelestialObject.Create;
begin
  inherited Create;
  fExists := true;
end;

destructor TCelestialObject.Destroy;
begin
  inherited;
end;

Procedure TCelestialObject.copyVSOP2013ephemerisToCurrent;
begin
  if (vJDE<>0) then
    begin
      fJDE  := vJDE;
      Posit := vPos;
      Spd   := vSpd;
    end;
end;

end.

