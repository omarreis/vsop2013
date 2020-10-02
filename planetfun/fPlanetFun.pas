unit fPlanetFun;    //-----  planetary system 3d animation --------\\
//-----------------//                                               \\
// Source: github.com/omarreis/vsop2013    folder planetfun          \\
// History:                                                           \\
//   v1.0 - jul20 - by oMAR                                            \\
//----------------------------------------------------------------------\\

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Math.Vectors, System.IOUtils, System.DateUtils,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.MaterialSources, FMX.Objects3D, FMX.Controls3D,
  FMX.Viewport3D, FMX.Types3D, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.ListBox,FMX.Edit,FMX.Ani, FMX.Layers3D ,
  FMX.Objects, FMX.Gestures, FMX.ScrollBox,
  FMX.Memo, FMX.Memo.Types, FMX.DateTimeCtrls,

  doubleVector3D,    // TVector3D_D
  PlanetData,        // secundary planet tables. physical data
  CelestialObjects,  // celestial object db
  vsop2013;          // VSOP 2013 ephemeris

type
  TFormPlanetFun = class(TForm)
    SolarSystemViewport3D: TViewport3D;
    dummySun: TDummy;
    dummyEarth: TDummy;
    dummyCamera: TDummy;
    dummyJupiter: TDummy;
    colorPathPlanet: TColorMaterialSource;
    mjTomCamera: TCamera;                // mj Tom camera is the main camera. It points to dummyCamera
    sphereEarth: TSphere;
    sphereSun: TSphere;
    lightSun: TLight;
    TimerSolarSystem: TTimer;
    sphereJupiter: TSphere;
    comboTarget: TComboBox;
    tbDistanceToTarget: TTrackBar;
    labDistanceToTarget: TLabel;
    dummyMercury: TDummy;
    sphereMercury: TSphere;
    dummyVenus: TDummy;
    sphereVenus: TSphere;
    dummyMars: TDummy;
    sphereMars: TSphere;
    dummySaturn: TDummy;
    sphereSaturn: TSphere;
    dummyNeptune: TDummy;
    sphereNeptune: TSphere;
    dummyUranus: TDummy;
    sphereUranus: TSphere;
    btnLoadFile: TButton;
    edFilename: TEdit;
    Label2: TLabel;
    tbDate: TTrackBar;
    labJDE: TLabel;
    Label3: TLabel;
    cbAnimatePlanets: TSwitch;
    tbPlanetScale: TTrackBar;
    labPlanetScale: TLabel;
    Label5: TLabel;
    tbAnimationSpeed: TTrackBar;
    labAnimationSpeed: TLabel;
    colorPathMercury: TColorMaterialSource;
    diskSaturnDisks: TDisk;
    btnAddOrbitDots: TButton;
    colorSun: TColorMaterialSource;
    dummyMoon: TDummy;
    sphereMoon: TSphere;
    dummyMoonOrbitCenter: TDummy;
    Light1: TLight;
    lightMaterialTextureMoon: TLightMaterialSource;
    lightMaterialTextureEarth: TLightMaterialSource;
    lightMaterialTextureJupiter: TLightMaterialSource;
    lightMaterialTextureMars: TLightMaterialSource;
    lightMaterialTextureSaturn: TLightMaterialSource;
    lightMaterialTextureVenus: TLightMaterialSource;
    cbOrbitDots: TSwitch;
    Label4: TLabel;
    textureStars: TTextureMaterialSource;
    sphereSky: TSphere;
    textPlanetFunTitle: TText3D;
    dummyPluto: TDummy;
    spherePluto: TSphere;
    lightMaterialTexturePluto: TLightMaterialSource;
    Grid3D1: TGrid3D;
    Label6: TLabel;
    cbAxisVisible: TSwitch;
    rectToast: TRectangle;
    labToast: TLabel;
    LightMaterialSource1: TLightMaterialSource;
    rectTime: TRectangle;
    Label7: TLabel;
    labAbout: TLabel;
    Label9: TLabel;
    labAngleOfView: TLabel;
    Label10: TLabel;
    btnCloseTime: TSpeedButton;
    btnToggleCameraSettings: TSpeedButton;
    rectControlPanel: TRectangle;
    Label11: TLabel;
    labJDE2: TLabel;
    labFileMetadata: TLabel;
    GestureManager1: TGestureManager;
    labStatus: TLabel;
    rectAboutPlanetFun: TRectangle;
    labVersion: TLabel;
    labAuthor: TLabel;
    btnCloseAbout: TSpeedButton;
    MemoAbout: TMemo;
    rectBGCombo: TRectangle;
    lightMaterialTextureMercury: TLightMaterialSource;
    lightMaterialTextureUranus: TLightMaterialSource;
    lightMaterialTextureNeptune: TLightMaterialSource;
    lightMaterialTextureSaturnDisks: TLightMaterialSource;
    imgPlanetFunBanner: TImage;
    lightMaterialTextureBannerPlanetFun: TLightMaterialSource;
    planeBanner: TPlane;
    btnEditJDE: TSpeedButton;
    rectEditJDE: TRectangle;
    Label13: TLabel;
    dateeditJDE: TDateEdit;
    timeEditJDE: TTimeEdit;
    btnOkJDE: TSpeedButton;
    btnCloseEditJDE: TSpeedButton;
    btnJDENow: TSpeedButton;
    tbAngleOfView: TTrackBar;
    Label1: TLabel;
    cbConstLinesNames: TSwitch;
    colorPathVenus: TColorMaterialSource;
    colorPathEarth: TColorMaterialSource;
    colorPathMars: TColorMaterialSource;
    colorPathJupiter: TColorMaterialSource;
    colorPathSaturn: TColorMaterialSource;
    colorPathNeptune: TColorMaterialSource;
    colorPathUranus: TColorMaterialSource;
    colorPathPluto: TColorMaterialSource;
    spherePolaris: TSphere;
    btnCloseAboutBox2: TSpeedButton;
    rectVisibility: TRectangle;
    btnCloseVisibility: TSpeedButton;
    rectCamera: TRectangle;
    btnCloseCamera: TSpeedButton;
    btnToggleTimeSettings: TSpeedButton;
    imgTime: TImage;
    btnToggleVisibilitySettings: TSpeedButton;
    imgEye: TImage;
    imgCamera: TImage;
    dummyPolaris: TDummy;
    procedure TimerSolarSystemTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SolarSystemViewport3DMouseDown(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SolarSystemViewport3DMouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Single);
    procedure comboTargetChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;  WheelDelta: Integer; var Handled: Boolean);
    procedure tbDistanceToTargetChange(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure tbDateChange(Sender: TObject);
    procedure tbPlanetScaleChange(Sender: TObject);
    procedure tbAnimationSpeedChange(Sender: TObject);
    procedure btnAddOrbitDotsClick(Sender: TObject);
    procedure cbOrbitDotsSwitch(Sender: TObject);
    procedure cbAxisVisibleSwitch(Sender: TObject);
    procedure btnCloseTimeClick(Sender: TObject);
    procedure btnToggleCameraSettingsClick(Sender: TObject);
    procedure SolarSystemViewport3DGesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure labAboutClick(Sender: TObject);
    procedure btnCloseAboutClick(Sender: TObject);
    procedure labJDEClick(Sender: TObject);
    procedure imgPlanetFunBannerClick(Sender: TObject);
    procedure btnEditJDEClick(Sender: TObject);
    procedure btnCloseEditJDEClick(Sender: TObject);
    procedure btnOkJDEClick(Sender: TObject);
    procedure btnJDENowClick(Sender: TObject);
    procedure tbAngleOfViewChange(Sender: TObject);
    procedure cbConstLinesNamesSwitch(Sender: TObject);
    procedure MemoAboutApplyStyleLookup(Sender: TObject);
    procedure btnCloseVisibilityClick(Sender: TObject);
    procedure btnCloseCameraClick(Sender: TObject);
    procedure btnToggleTimeSettingsClick(Sender: TObject);
    procedure btnToggleVisibilitySettingsClick(Sender: TObject);
  private
    fFirstShow:boolean;
    fToastMsgStartTime:TDatetime;
    fMousePt:TPointF;
    // VSOP_File:T_VSOP2013_File;     // moved to vsop2013.pas
    fJDE:Double;

    fPlanetOrbitPoints:TList;  //save 3D dots created at runtime

    // Gesture related
    FLastZoomDistance:Single;
    FLastRotationAngle:Single;
    fLastZoomPosition:TPointF;
    fLastPanPosition:TPointF;

    procedure PositionPlanets;        // according to vsop2013
    procedure SizePlanets;
    procedure ClearOrbitDots;
    procedure showToastMessage(const S: String);
    procedure FileLoadTerminate(Sender: TObject);
    procedure HandlePan(const EventInfo: TGestureEventInfo);
    procedure HandleZoom(const EventInfo: TGestureEventInfo);
    procedure HandleRotate(const EventInfo: TGestureEventInfo);
    procedure checkSphereSkyVisibility;
    procedure LoadPlanetTextures;
  public
  end;

var
  FormPlanetFun: TFormPlanetFun;

implementation   // ---x--xx-xxx........................

{$R *.fmx}

{ TFormPlanetFun }

procedure TFormPlanetFun.FormCreate(Sender: TObject);
var Y,M,D:word;
begin
  fFirstShow := true;
  FormatSettings.DecimalSeparator  := '.';  // vsop2013 files use dots as decimal separator
  FormatSettings.ThousandSeparator := ',';

  fMousePt:= PointF(0,0);
  fToastMsgStartTime := 0;   // = never

  DecodeDate( Date, {out:}Y,M,D);
  fJDE      := JD(Y, M, D, 0);     // current Julian date = Now
  VSOP_File := nil;    // no file yet

  fPlanetOrbitPoints := TList.Create;  // list of orbit dots

  // gesture related vars
  FLastZoomDistance := 0;
  FLastRotationAngle:= 0;
  fLastZoomPosition := PointF(0,0);   //invalid
  fLastPanPosition  := PointF(0,0);
end;

procedure TFormPlanetFun.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var v1:TPoint3d;
begin
  v1 := mjTomCamera.Position.Point.Normalize;   // camera pointing versor
  mjTomCamera.Position.Point := mjTomCamera.Position.Point - ( WheelDelta/10 * v1 ); //pan camera slowly

  checkSphereSkyVisibility;

  Handled:= true;
end;

procedure TFormPlanetFun.HandlePan(const EventInfo:TGestureEventInfo);
var   aOldLoc,aNewLoc,aDelta:TPointF;
begin
  // labStatus.Text := 'pan..';

  if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then    //begin. save inicial state
    begin
      FLastPanPosition := EventInfo.Location;  //save (center?) point
    end
    else if (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then   //end. do nothing
    begin
      //nada
    end
    else begin //other gestures.
      if (FLastPanPosition.X=0) and (FLastPanPosition.Y=0) then
        fLastPanPosition := EventInfo.Location;  //inicializa

      aOldLoc  := fLastPanPosition;     //save previous states
      aNewLoc  := EventInfo.Location;   //gesture center location

      aDelta    := aNewLoc-aOldLoc;

      if (aDelta.Length<>0) then
        begin
          dummyCamera.RotationAngle.Y := dummyCamera.RotationAngle.Y - aDelta.X;
          dummyCamera.RotationAngle.X := dummyCamera.RotationAngle.X - aDelta.Y;
        end;
      fLastPanPosition := EventInfo.Location;
    end;
end;

procedure TFormPlanetFun.HandleRotate(const EventInfo:TGestureEventInfo);
var aOldAng,aNewAng,da:Single;
begin
  // labStatus.Text := 'rotate..';
  if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then    //begin. save inicial state
    begin
      FLastRotationAngle := EventInfo.Angle;  //save distance between fingers
      // FLastZoomPosition := EventInfo.Location;  //save (center?) point
    end
    else if (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then   //end. do nothing
    begin
      //nada
    end
    else begin  // rotating
      // if (FLastZoomPosition.X=0)  then
      //  FLastZoomPosition := EventInfo.Location;  // rotation location not used. rotating around target
      if (FLastRotationAngle=0) then fLastRotationAngle := EventInfo.Angle;

      aOldAng := fLastRotationAngle;
      // aNewLoc  := EventInfo.Location;   //gesture center location
      aNewAng := EventInfo.Angle;

      if ( aNewAng<>aOldAng ) then
        begin
          da := (aNewAng-aOldAng)/pi*180;     // angle delta. (angle in rads ? )
          // labStatus.Text := 'a:'+Format('%4.1f',[da]);
          dummyCamera.RotationAngle.Z := dummyCamera.RotationAngle.Z + da; //rotate target ??
        end;
      FLastRotationAngle := EventInfo.Angle;   //save new previous
    end;
end;


procedure TFormPlanetFun.HandleZoom(const EventInfo:TGestureEventInfo);
var
  aOldLoc,aNewLoc,aVC,aOldCenter,aNewCenter:TPointF;
  aOldDist,aNewDist,aK,aDelta:double;
  v1:TPoint3D;
begin
  // labStatus.Text := 'zoom..';
  if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then    //begin. save inicial state
    begin
      FLastZoomDistance := EventInfo.Distance;  //save distance between fingers
      FLastZoomPosition := EventInfo.Location;  //save (center?) point
    end
    else if (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then   //end. do nothing
    begin
      //nada
    end
    else begin //other gestures.
      if (FLastZoomPosition.X=0) and (FLastZoomPosition.Y=0) then
        FLastZoomPosition := EventInfo.Location;  //inicializa

      if (FLastZoomDistance=0) then FLastZoomDistance:=EventInfo.Distance;

      aOldLoc  := FLastZoomPosition;    //save previous states
      aNewLoc  := EventInfo.Location;   //gesture center location


      aOldDist := FLastZoomDistance;
      aNewDist := EventInfo.Distance;
      aDelta   := (aNewDist-aOldDist);


      if (aDelta<>0) then
        begin
          v1 := mjTomCamera.Position.Point.Normalize;      // camera pointing versor
          mjTomCamera.Position.Point := mjTomCamera.Position.Point - (aDelta * v1 )/20;
          checkSphereSkyVisibility;

        end;
      FLastZoomDistance := EventInfo.Distance; //save new previous
      fLastZoomPosition := EventInfo.Location;
    end;
end;

procedure TFormPlanetFun.imgPlanetFunBannerClick(Sender: TObject);
begin
  planeBanner.Visible := not planeBanner.Visible; //toggle vis of plane with app banner
end;

procedure TFormPlanetFun.labAboutClick(Sender: TObject);
begin
  rectTime.Visible := false; //hide menu to show About

  rectAboutPlanetFun.Visible := not rectAboutPlanetFun.Visible;  //toggle about

  if rectAboutPlanetFun.Visible then
    begin
      //center about box
      rectAboutPlanetFun.Position.Point := PointF( (Width-rectAboutPlanetFun.Width)/2 , rectAboutPlanetFun.Position.Y  );
      rectAboutPlanetFun.BringToFront;       //just in case
    end;

end;

procedure TFormPlanetFun.labJDEClick(Sender: TObject);   //click JDE sets JDE time to Now
var T:TDatetime; Y,M,D:word; Year,H:Double;
begin
  // set planets to Now!

  T := TTimeZone.Local.ToUniversalTime( Now );   //apply time zone
  DecodeDate( Trunc(T), {out:}Y,M,D);
  H := Frac(T);

  fJDE  := JD(Y, M, D, H*24);     // current Julian date = Now

  PositionPlanets;  // re pos with new jde
end;

// set about TMemo background color
procedure TFormPlanetFun.MemoAboutApplyStyleLookup(Sender: TObject);
var Obj: TFmxObject; Rectangle1: TRectangle;
begin
     Obj := MemoAbout.FindStyleResource('background');
     if Obj <> nil then
     begin
          TControl(Obj).Margins   := TBounds.Create(TRectF.Create(-2,-2,+2,+2));
          Rectangle1              := TRectangle.Create(Obj);
          Obj.AddObject(Rectangle1);
          Rectangle1.Align        := TAlignLayout.Client;
          Rectangle1.Fill.Color   := TAlphaColors.Black;    //black memo w/ green text
          Rectangle1.Stroke.Color := TAlphaColorRec.Black;
          Rectangle1.Stroke.Kind  := TBrushKind.None;

          Rectangle1.HitTest      := False;
          Rectangle1.YRadius      := 0;
          Rectangle1.XRadius      := 0;

          Rectangle1.SendToBack;
     end;
end;

// interactive gestures for mobile ( on Windows use mouse events combined with shift keys. See MouseMove() event )
procedure TFormPlanetFun.SolarSystemViewport3DGesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    igiPan:    HandlePan(EventInfo);      // one finger drag
    igiRotate: HandleRotate(EventInfo);   // two finger rotation
    igiZoom:   HandleZoom(EventInfo);     // two finger zoom
  end;
  Handled := true;
end;

procedure TFormPlanetFun.SolarSystemViewport3DMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  fMousePt := PointF(X,Y);
end;

procedure TFormPlanetFun.showToastMessage(const S:String);
begin
  fToastMsgStartTime := Now;
  labToast.Text      := S;
  rectToast.Position.Point := PointF( (Width-rectToast.Width)/2 , rectToast.Position.Y  ); //center it
  rectToast.Visible  := true;
end;

procedure TFormPlanetFun.SizePlanets;     // set objects proportional to real sizes
var r,sc,k:Double;
const SCALE_PLANETS=true;
begin    // 3d coordinates are in AU
  if SCALE_PLANETS then
    begin                            // tbPlanetScale.Value in range 1..10
        sc := tbPlanetScale.Value;   // scale planets, so they don't look so small as they really are..
        // The Sun is much larger than the planets.
        // Applied a log formula to reduce that difference when showing
        sc := sc/100;                // ad hoc factor

        // The problem w/ viewing the solar system is that the Sun is very large when compared to tiny planets.
        // And distances between bodies are even larger. When the whole Sun is visible, planets are small.
        // When the whole solar system is visible, most planets are too small to be seen.
        // So I applyed a log transformation to planet sizes. The Sun becomes 3 times larger than the Earth.
        // Better for visualization, but is a cheat...
        k := 700;  // this k was adjusted so that, w/ formula R=LN(radiusKm/k)*sc  (w/ sc in range 0.1..1.0 )
        // 1- Size of celestial bodyes are proportional to LN() of size
        // 2- when earth size set to 1 --> Sun size~3, jupiter~2 and pluto~0.23

        r := LN( PLANET_DATA[0].radius/k  )*sc;   sphereSun.Scale.Point     := Point3D(r,r,r);
        r := LN( PLANET_DATA[1].radius/k  )*sc ;  sphereMercury.Scale.Point := Point3D(r,r,r);
        r := LN( PLANET_DATA[2].radius/k  )*sc ;  sphereVenus.Scale.Point   := Point3D(r,r,r);
        r := LN( PLANET_DATA[3].radius/k  )*sc ;  sphereEarth.Scale.Point   := Point3D(r,r,r);
        r := LN( PLANET_DATA[4].radius/k  )*sc ;  sphereMars.Scale.Point    := Point3D(r,r,r);
        r := LN( PLANET_DATA[5].radius/k  )*sc ;  sphereJupiter.Scale.Point := Point3D(r,r,r);
        r := LN( PLANET_DATA[6].radius/k  )*sc ;  sphereSaturn.Scale.Point  := Point3D(r,r,r);
        r := LN( PLANET_DATA[7].radius/k  )*sc ;  sphereUranus.Scale.Point  := Point3D(r,r,r);
        r := LN( PLANET_DATA[8].radius/k  )*sc ;  sphereNeptune.Scale.Point := Point3D(r,r,r);
        r := LN( PLANET_DATA[9].radius/k  )*sc ;  spherePluto.Scale.Point   := Point3D(r,r,r);
        r := LN( PLANET_DATA[10].radius/k )*sc ;  sphereMoon.Scale.Point    := Point3D(r,r,r);

        r := LN(360000/k)*sc /2.0;               //size moon orbit . ad hoc factor applied again
        dummyMoon.Position.Point := Point3D( r, 0, 0);
        // Pluto
    end
    else begin  //planet real sizes ( cannot see most of them. too small )
        sc := tbPlanetScale.Value;   // scale planets, so they don't look so small as they really are..
        sc := Exp((sc-1)/15);        // Exponential scale in range  1..785
        sc := sc*1;                  // 1 au = 1 3d unit   //era *10

        r := PLANET_DATA[0].radius/AUtoKm  *sc;  sphereSun.Scale.Point     := Point3D(r,r,r);
        r := PLANET_DATA[1].radius/AUtoKm  *sc;  sphereMercury.Scale.Point := Point3D(r,r,r);
        r := PLANET_DATA[2].radius/AUtoKm  *sc;  sphereVenus.Scale.Point   := Point3D(r,r,r);
        r := PLANET_DATA[3].radius/AUtoKm  *sc;  sphereEarth.Scale.Point   := Point3D(r,r,r);
        r := PLANET_DATA[4].radius/AUtoKm  *sc;  sphereMars.Scale.Point    := Point3D(r,r,r);
        r := PLANET_DATA[5].radius/AUtoKm  *sc;  sphereJupiter.Scale.Point := Point3D(r,r,r);
        r := PLANET_DATA[6].radius/AUtoKm  *sc;  sphereSaturn.Scale.Point  := Point3D(r,r,r);
        r := PLANET_DATA[7].radius/AUtoKm  *sc;  sphereUranus.Scale.Point  := Point3D(r,r,r);
        r := PLANET_DATA[8].radius/AUtoKm  *sc;  sphereNeptune.Scale.Point := Point3D(r,r,r);
        r := PLANET_DATA[9].radius/AUtoKm  *sc;  spherePluto.Scale.Point   := Point3D(r,r,r);
        r := PLANET_DATA[10].radius/AUtoKm *sc;  sphereMoon.Scale.Point    := Point3D(r,r,r);
        dummyMoon.Position.Point := sc* Point3D( 360000.0/AUtoKm, 0, 0);    // size moon orbit . ad hoc factor applied
        // Pluto
    end;

  // set planets obliquities. Check: apply obliquity to which axis ?    -x
  sphereMercury.RotationAngle.x := - PLANET_DATA[1].Obliq;
  sphereVenus.RotationAngle.x   := - PLANET_DATA[2].Obliq;
                              r := - PLANET_DATA[3].Obliq;  // Earth obliquity
  sphereEarth.RotationAngle.x            := r;    // apply to Earth axis and to orbit of the Moon. Is this right ?
  // dummyMoonOrbitCenter.RotationAngle.x   := r;
  sphereMars.RotationAngle.x    := - PLANET_DATA[4].Obliq;
  sphereJupiter.RotationAngle.x := - PLANET_DATA[5].Obliq;
  sphereSaturn.RotationAngle.x  := - PLANET_DATA[6].Obliq;
  sphereUranus.RotationAngle.x  := - PLANET_DATA[7].Obliq;
  sphereNeptune.RotationAngle.x := - PLANET_DATA[8].Obliq;
  spherePluto.RotationAngle.x   := - PLANET_DATA[9].Obliq;
end;

procedure TFormPlanetFun.PositionPlanets;   // according to vsop2013 at epoch fJDE
var ip:integer; aPosition,aSpeed:TVector3D_D; aDummy:TDummy; Year:Double; aPosition3d:TPoint3d; aUT:TDatetime;
begin
  if not ( Assigned(VSOP_File) and VSOP_File.fLoaded ) then exit;  //sanity test. File must be loaded

  for ip := 1 to NUM_PLANETS do   // all vsop2013 planets
    begin
      // vsop2013 returns rectangular heliocentric coordinates x,y,z in UA
      if VSOP_File.calculate_coordinates( {ip:}ip , {jde:}fJDE, {out:} aPosition, aSpeed) then
            begin
              // translate between Astronomical coordinates and 3D world
              aPosition3d := Point3D(   // 3DWorld   Universe
                 aPosition.x,           //   x          x
                -aPosition.z,           //   y         -z
                 aPosition.y  );        //   z          y

              aDummy := nil;
              case ip of
                //0: aDummy := dummySun;
                1: aDummy := dummyMercury;
                2: aDummy := dummyVenus;
                3: aDummy := dummyEarth;
                4: aDummy := dummyMars;
                5: aDummy := dummyJupiter;
                6: aDummy := dummySaturn;
                7: aDummy := dummyUranus;
                8: aDummy := dummyNeptune;
                9: aDummy := dummyPluto;
              end;
              if Assigned(aDummy) then
                begin
                  //position planet
                  aDummy.Position.Point := aPosition3d;   // 1 = scale --> 1 au = 1.0 3D world unit
                end;
            end;
    end;

  // show date
  Year   :=   (fJDE-jd2000)/365.2422+2000.0;      // more or less :)

  labJDE.Text  := Format('%6.1f', [Year] );
  labJDE2.Text := labJDE.Text;

  aUT := JulianToGregorianDate(fJDE);
  labStatus.Text := FormatDatetime('dd-mmm-yyyy',aUT );
end;

procedure TFormPlanetFun.SolarSystemViewport3DMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var dMouse:TPointF; v1,v2,u:TPoint3d;  k,a:double;
begin
  {$IFDEF MsWindows}   //MouseMove only for Windows. On mobile, use gestures
  if ssLeft in Shift then  // left mouse btn pressed
    begin
      if ssCtrl in Shift then // shift-drag --> rotates Z .. really nada
        begin
          // labStatus.Text := 'C-MM';
          dMouse := PointF(X,Y) - fMousePt;
          // mjTomCamera.RotationAngle.Y := mjTomCamera.RotationAngle.Y - dMouse.X;
          // mjTomCamera.RotationAngle.Z := mjTomCamera.RotationAngle.Z - dMouse.Y;
          // nada

          fMousePt := PointF(X,Y);  //save it
        end
      else if ssAlt in Shift then      // Alt-MouseDrag --> controls camera distance to target ( x delta only)
        begin
          // labStatus.Text := 'A-MM';
          v1 := mjTomCamera.Position.Point.Normalize;   // camera pointing versor
          dMouse := PointF(X,Y) - fMousePt;             // calc mouse delta
          mjTomCamera.Position.Point := mjTomCamera.Position.Point - (dMouse.X * v1 )/20; //pan camera slowly
          checkSphereSkyVisibility;

          // y does what ?
          fMousePt := PointF(X,Y);  //save it
        end
      else if ssShift in Shift then                 // shift-drag rotates around target
        begin
          // labStatus.Text := 'S-MM';
          dMouse := PointF(X,Y) - fMousePt;
          a := mjTomCamera.AngleOfView-dMouse.Y;     //change camera AngleOfView
          if ( a>0) and (a<360) then                //sanity test
            begin
              mjTomCamera.AngleOfView := a;
              labAngleOfView.Text := Trim( Format('%5.0f°',[a]));
          end;
          dummyCamera.RotationAngle.Z := dummyCamera.RotationAngle.Z - dMouse.x;  // and Z
          fMousePt := PointF(X,Y);  //save it
        end
      else begin   // left btn mouse drag rotates camera around target
          // labStatus.Text := 'MM';
          dMouse := PointF(X,Y) - fMousePt;
          dummyCamera.RotationAngle.Y := dummyCamera.RotationAngle.Y - dMouse.X;
          dummyCamera.RotationAngle.X := dummyCamera.RotationAngle.X - dMouse.Y;
          fMousePt := PointF(X,Y);  //save it
      end;
    end
  {$ENDIF MsWindows}
end;

procedure TFormPlanetFun.tbDistanceToTargetChange(Sender: TObject);
var d,t:Double; tp, v1,c:TPoint3d; aTarget:TControl3d;
begin
  aTarget := mjTomCamera.Target;
  if Assigned(aTarget) then
    begin
      v1 := mjTomCamera.Position.Point.Normalize;   // camera pointing versor
      t := tbDistanceToTarget.Value;     // value in range 1..101
      d := ( exp(t/60)-1.0 )*20.0;       // d in au  range 0.32..88 au
      mjTomCamera.Position.Point := d*v1;

      checkSphereSkyVisibility;

      labDistanceToTarget.Text := Format('%5.2f',[d] )+ ' au';
    end;
end;

// call after camera position change ( pan )
procedure TFormPlanetFun.checkSphereSkyVisibility;
var aCamPos:TPoint3D;
begin
  // if camera outside the sphere w/ stars, we dont want it to be visible and obstruct vision of the little planets
  aCamPos := dummyCamera.Position.Point + mjTomCamera.Position.Point;  // heliocentric position of our camera
  // sphereSky.Visible := (aCamPos.Length < sphereSky.Scale.x*0.5 );      //hide the sky sphere before we go out
end;


procedure TFormPlanetFun.tbPlanetScaleChange( Sender: TObject );
var sc:Double;
begin
  sc := tbPlanetScale.Value;   // 1..101
  labPlanetScale.Text := Trim( Format('%6.1f',[sc]) );
  SizePlanets;  //acording to new scale
end;

procedure TFormPlanetFun.tbAngleOfViewChange(Sender: TObject);
var a:Single;
begin
  a := tbAngleOfView.Value;      //change camera AngleOfView
  if ( a>0) and (a<360) then     //sanity test
    begin
      mjTomCamera.AngleOfView := a;
      labAngleOfView.Text := Trim( Format('%5.0f°',[a]));
    end;
end;

procedure TFormPlanetFun.tbAnimationSpeedChange(Sender: TObject);
begin
  labAnimationSpeed.Text := Format('%4.0f',[tbAnimationSpeed.Value])+' days/tick (t=100ms)';
end;

procedure TFormPlanetFun.tbDateChange(Sender: TObject);
begin
  fJDE := jd2000+tbDate.Value; //in days
  PositionPlanets;
end;

procedure TFormPlanetFun.btnAddOrbitDotsClick(Sender: TObject);
var aDot:TSphere; x,y,z:Double; i,ip:integer; aProxy:TProxyObject;
    yearLen,weekLen,aJDE,radiusDot:Double; aPos,aSpeed:TVector3D_D;
    aPoint3d:TPoint3d;
begin
  x := 0;    y := 0;   z := 0;
  if not ( Assigned(VSOP_File) and VSOP_File.fLoaded ) then exit; //sanity

  // SolarSystemViewport3D.BeginUpdate;
  try
     for ip := 1 to 9 do // build orbiots for all planets
        begin
          yearLen  := PLANET_DATA[ip].revPer;  // rev period = planet "year"
          weekLen := yearLen/52;             // = planet week len in days

          // was  := 0.2*Point3D( 0.5, 0.5, 0.5 );   // scale 0.5 --> small dot

          // dot size proportional to Log of month len, so that far planets have larger dots and is visible
          // when the whole orbit is visible
          radiusDot := LN( weekLen )/8/7;     // can range from a ew days to many years. ad hoc factor applied

          // create dot orbits for each planet
          aDot := nil;
          for i:= 1 to 52 do      // calc one position for each month of the "year". Drop a ball on each
              begin
                aJDE := fJDE + i*weekLen;  //calc data
                if VSOP_File.calculate_coordinates(ip, aJDE, {out:} aPos,aSpeed) then
                  begin
                    // translate between Astronomical coordinates and 3D world coordinates
                    aPoint3d := Point3D(   // 3DWorld   Universe
                    aPos.x,                //   x          x
                   -aPos.z,                //   y         -z
                    aPos.y  );             //   z          y

                    if (i=1) then  // 1st is a sphere
                      begin
                        aDot := TSphere.Create( Self );
                        SolarSystemViewport3D.AddObject(aDot);
                        x := i;
                        aDot.Position.Point := aPoint3d;    //
                        aDot.Scale.Point    := Point3D( radiusDot,radiusDot,radiusDot );
                        case ip of
                          1: aDot.MaterialSource := colorPathMercury;  // colorPathxx contains the texture color
                          2: aDot.MaterialSource := colorPathVenus;
                          3: aDot.MaterialSource := colorPathEarth;
                          4: aDot.MaterialSource := colorPathMars;
                          5: aDot.MaterialSource := colorPathJupiter;
                          6: aDot.MaterialSource := colorPathSaturn;
                          7: aDot.MaterialSource := colorPathUranus;
                          8: aDot.MaterialSource := colorPathNeptune;
                          9: aDot.MaterialSource := colorPathPluto;
                        else
                          aDot.MaterialSource := colorPathPlanet;  //generic
                        end;

                        aDot.Opacity        := 0.20;      //transparent

                        fPlanetOrbitPoints.Add( aDot );
                      end
                      else begin  //other 119 dots are proxies
                        aProxy := TProxyObject.Create(Self);
                        SolarSystemViewport3D.AddObject(aProxy);

                        aProxy.SourceObject := aDot;
                        x := i;
                        aProxy.Position.Point := aPoint3d;
                        aProxy.Scale.Point    := Point3D( radiusDot,radiusDot,radiusDot );
                        aProxy.Opacity        := 0.20;

                        fPlanetOrbitPoints.Add( aProxy );
                      end;

                  end;
                // SolarSystemViewport3D.AddObject(aDot);
              end;   // /60dots
        end;        // /9planets
  finally
    // SolarSystemViewport3D.EndUpdate;
  end;

end;

procedure TFormPlanetFun.ClearOrbitDots;
var aObj:TControl3D; i:integer;
begin
  // SolarSystemViewport3D.BeginUpdate;
  try
      // delete all dots
      for i:=0 to fPlanetOrbitPoints.Count-1 do
        begin
          aObj := TControl3D(fPlanetOrbitPoints.Items[i]);
          SolarSystemViewport3D.RemoveObject( aObj );
          // call aObj.Free; //??
        end;
      fPlanetOrbitPoints.Clear;

  finally
    // SolarSystemViewport3D.EndUpdate;
  end;

end;

procedure TFormPlanetFun.btnCloseAboutClick(Sender: TObject);
begin
  rectAboutPlanetFun.Visible := false;
end;

procedure TFormPlanetFun.btnCloseCameraClick(Sender: TObject);
begin
  rectCamera.Visible := false;
end;

procedure TFormPlanetFun.btnCloseEditJDEClick(Sender: TObject);
begin
  rectEditJDE.Visible:= false;
end;

procedure TFormPlanetFun.btnCloseTimeClick(Sender: TObject);
begin
//  mvPlanetFun.HideMaster; //close multiview (menu)
  rectTime.Visible := false;
end;

procedure TFormPlanetFun.btnCloseVisibilityClick(Sender: TObject);
begin
  rectVisibility.Visible := false;
end;

procedure TFormPlanetFun.btnEditJDEClick(Sender: TObject);
begin
  rectTime.Visible := false;
  rectEditJDE.Visible := true;
end;

procedure TFormPlanetFun.btnJDENowClick(Sender: TObject);
var D,T:TDatetime;
begin
  T := Now;
  D := Trunc(T);
  T := Frac(T);
  dateeditJDE.Date := D;
  timeEditJDE.Time := T;
end;

procedure TFormPlanetFun.btnLoadFileClick(Sender: TObject);
var aFN:String;
const sVSOP2013file = 'VSOP2013.p2000.bin';  // custom binary format
begin
  labFileMetadata.Text := 'loading.. wait..';  //wait

  if not Assigned(VSOP_File) then
    VSOP_File := T_VSOP2013_File.Create;   // vsop file parser and position calculator

  // VSOP_File.OnLoadProgress := Form2LoadPropgress;
  // reads and parses long ASCII file: wait..

  // file 'VSOP2013.p2000'  1500-3000. ( includes current time )
  // loaded on a thread  at activation

  {$ifdef MsWindows}
  // TODO: set a folder app documents
  // aFN := Trim( edFilename.Text )+'.bin';    // '\dpr4\vsop2013\VSOP2013.p2000'  1500-3000. ( includes current time )
  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+
         'vsop2013'+ System.SysUtils.PathDelim+  //    /users/<username>/Documents/vsop2013/vsop2013.p2000.bin'
         sVSOP2013file;
  {$endif MsWindows}

  {$ifdef Android}
  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+sVSOP2013file;
  {$endif Android}

  {$ifdef iOS}
  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+sVSOP2013file;
  {$endif iOS}

  if FileExists(aFN) then  // load
    begin
      VSOP_File.OnLoadTerminate := FileLoadTerminate;
      VSOP_File.Threaded_ReadBinaryFile( aFN );       // load data file on a separate thread ( takes some time )
      showToastMessage('Loading VSOP2013 data.  Wait..');
    end
    else showToastMessage('VSOP2013 file not found');
  // Memo1.Lines.Add(aFN+' Loaded');   //not so fast
  // PositionPlanets;
end;

procedure TFormPlanetFun.btnToggleCameraSettingsClick(Sender: TObject);
begin
  rectTime.Visible := false;
  rectVisibility.Visible := false;
  rectCamera.Visible := not rectCamera.Visible;  //toggle time settings
end;

procedure TFormPlanetFun.btnToggleTimeSettingsClick(Sender: TObject);
begin
  rectVisibility.Visible := false;
  rectCamera.Visible := FALSE;
  rectTime.Visible:= not rectTime.Visible;
end;

procedure TFormPlanetFun.btnToggleVisibilitySettingsClick(Sender: TObject);
begin
  rectTime.Visible := false;
  rectCamera.Visible := FALSE;
  rectVisibility.Visible := not rectVisibility.Visible;
end;

procedure TFormPlanetFun.btnOkJDEClick(Sender: TObject);
var D,T:TDatetime;   YY,MM,DD:Word; H:Double;
begin
  D := dateeditJDE.Date;
  T := timeEditJDE.Time;

  T := D+T;
  T := TTimeZone.Local.ToUniversalTime( T );   //apply time zone

  DecodeDate( Trunc(T), {out:}YY,MM,DD);
  H := Frac(T);
  fJDE  := JD( YY, MM, DD, H*24);     // current Julian date = Now
  PositionPlanets;

  rectEditJDE.Visible:= false;
end;

procedure TFormPlanetFun.FileLoadTerminate(Sender:TObject);
var S:String;
begin
  if VSOP_File.fLoaded then
    begin
      S := S+'VSOP2013.p2000.bin loaded';
    end
    else S := ' vsop file read error';

  showToastMessage( S );  // notify finished loading

  if VSOP_File.fLoaded then
    begin
      textPlanetFunTitle.Visible := false;     // hide app title after the file is loaded, and we are open for business
      planeBanner.Visible := false;

      labFileMetadata.Text := VSOP_File.getMetadata;  //show vsop file metadata ( header )
      labJDEClick(nil);    // sets fJDE to current and repos planets
    end
    else labFileMetadata.Text := 'ops..some error loading file';
end;

procedure TFormPlanetFun.cbAxisVisibleSwitch(Sender: TObject);
begin
   Self.Grid3D1.Visible := cbAxisVisible.IsChecked;
end;

const
  planetTextureFiles:Array[1..NUM_PLANETS+1] of String= (
    '2k_mercury.jpg',        // 1:Mercury;
    '2k_venus_surface.jpg',  // 2:Venus;
    '2k_earth_daymap.jpg',   // 3:Earth;
    '2k_mars.jpg',           // 4:Mars;
    '2k_jupiter.jpg',        // 5:Jupiter;
    '2k_saturn.jpg',         // 6:Saturn;
    '2k_uranus.jpg',         // 7:Uranus;
    '2k_neptune.jpg',        // 8:Neptune;
    'PlutoTexture.jpg',      // 9:Pluto;
    '2k_moon.jpg' );         // 10:Moon
// not used yet
//    '2k_earth_clouds.jpg'
//    '2k_earth_nightmap.jpg'
//    '2k_venus_atmosphere.jpg'
//    '2k_saturn_ring_alpha.png'

procedure TFormPlanetFun.LoadPlanetTextures;  //Loads
var aPath, aFN:String; ip:integer;
    aMaterial:TLightMaterialSource;
begin
  // on Windows 10 it is  'C:\Users\<user>\OneDrive\Documentos\vsop2013\'
  aPath := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim; // mk file path
  {$IFDEF MsWindows}
  aPath := aPath +'vsop2013'+ System.SysUtils.PathDelim;  // for Windows there is an extra 'vsop2013\'
  {$ENDIF MsWindows}
  for ip := 1 to NUM_PLANETS+1 do
    begin
      aFN := aPath + planetTextureFiles[ip];
      if FileExists(aFN) then      //sanity test
        begin
          case ip of
            1: aMaterial := lightMaterialTextureMercury;
            2: aMaterial := lightMaterialTextureVenus;
            3: aMaterial := lightMaterialTextureEarth;
            4: aMaterial := lightMaterialTextureMars;
            5: aMaterial := lightMaterialTextureJupiter;
            6: aMaterial := lightMaterialTextureSaturn;
            7: aMaterial := lightMaterialTextureUranus;
            8: aMaterial := lightMaterialTextureNeptune;
            9: aMaterial := lightMaterialTexturePluto;
            10:aMaterial := lightMaterialTextureMoon;
          else aMaterial := nil;
          end;
          if Assigned( aMaterial ) then aMaterial.Texture.LoadFromFile(aFN);
        end;
        // else
    end;
end;

procedure TFormPlanetFun.cbConstLinesNamesSwitch(Sender: TObject);
var aFN:String;
begin
  // on (my) Windows 10 it is  'C:\Users\<user>\OneDrive\Documentos\vsop2013\'
  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim; // mk file path
  {$IFDEF MsWindows}
  aFN := aFN +'vsop2013'+ System.SysUtils.PathDelim;  // for Windows there is an extra 'vsop2013\'
  {$ENDIF MsWindows}
  if cbConstLinesNames.IsChecked then aFN:=aFN+'SkyMapLinesNames.png'
    else aFN:=aFN+'SkyMapPlain.png';
  if FileExists(aFN) then      //sanity test
    begin
      textureStars.Texture.LoadFromFile(aFN);
      sphereSky.Visible := true;
    end;
end;

procedure TFormPlanetFun.cbOrbitDotsSwitch(Sender: TObject);
begin
  if cbOrbitDots.IsChecked then
    btnAddOrbitDotsClick(nil)
    else ClearOrbitDots;
end;

procedure TFormPlanetFun.comboTargetChange(Sender: TObject);
var aDummy:TDummy;
begin
  case comboTarget.ItemIndex of
    0: aDummy := dummySun;
    1: aDummy := dummyMercury;
    2: aDummy := dummyVenus;
    3: aDummy := dummyEarth;
    4: aDummy := dummyMars;
    5: aDummy := dummyJupiter;
    6: aDummy := dummySaturn;
    7: aDummy := dummyUranus;
    8: aDummy := dummyNeptune;
    9: aDummy := dummyPluto;
    else exit;
  end;

  dummyCamera.Parent := aDummy;  //parent dummyCamera to target planet dummy
  // move camera to 0,0,0  (on parent scale)
  dummyCamera.Position.Point := Point3d(0,0,0); // aDummy.Position.Point;  // move camera to position. TODO: animate camera shift

  // move camera ( TODO: animate that )
  // mjTomCamera.Target  := aDummy; //camera pointing to dummy
end;

procedure TFormPlanetFun.FormActivate(Sender: TObject);
begin
   if fFirstShow then  // once:  load vsop2013 ephemerides binary file ( threaded load )
     begin
       TimerSolarSystem.Enabled := true;  // and God said: may Time start...now !
       SizePlanets;
       Grid3D1.Visible := false;        //didn't manage to do that as design time
       rectTime.Visible := false;      // menus starts hiden

       LoadPlanetTextures;            // load planet texture images from Documents
       cbConstLinesNamesSwitch(nil);  // this loads defaut sky texture from file

       btnLoadFileClick(nil);         // load data file on startup
       btnLoadFile.Visible := false;  // hide filename editor ( for debug only)
       edFilename.Visible := false;   // and associated btn

       fFirstShow:= false;
     end;
end;

procedure TFormPlanetFun.TimerSolarSystemTimer(Sender: TObject); // 100 or 200 ms ticks
var rot:double; T:TDAtetime;
const XSeg= 3/24/3600;   // X=3
begin
  // rotate planets
  rot := 1.0;      // 1.0 deg/tick = 10 deg/sec

  // rotate stuff ( fake speed. not realistic )
  sphereEarth.RotationAngle.Y   := sphereEarth.RotationAngle.Y -5*rot; //rotate earth   Ad hoc factor to make earth spin fast

  sphereJupiter.RotationAngle.Y := sphereJupiter.RotationAngle.Y +2*rot; //jupiter too

  sphereVenus.RotationAngle.Y   := sphereVenus.RotationAngle.Y   +rot;
  sphereSaturn.RotationAngle.Y  := sphereSaturn.RotationAngle.Y  +rot;
  sphereMars.RotationAngle.Y    := sphereMars.RotationAngle.Y    +rot;
  sphereNeptune.RotationAngle.Y := sphereNeptune.RotationAngle.Y +rot;
  spherePluto.RotationAngle.Y   := spherePluto.RotationAngle.Y   +rot;


  if textPlanetFunTitle.Visible then
    textPlanetFunTitle.RotationAngle.Y :=  textPlanetFunTitle.RotationAngle.Y +rot;

  if planeBanner.Visible  then
     planeBanner.RotationAngle.Y :=  planeBanner.RotationAngle.Y +1.2*rot;

  dummyMoon.RotationAngle.Y     := dummyMoon.RotationAngle.Y + rot;
  dummyMoonOrbitCenter.RotationAngle.Y := dummyMoonOrbitCenter.RotationAngle.Y - rot;  //move moon in its orbit

  if cbAnimatePlanets.IsChecked then
    begin
      fJDE := fJDE + tbAnimationSpeed.Value;     // tbAnimationSpeed.Value = time/"100ms timer tick"

      try
        PositionPlanets;  // repositrion planets every 200 ms ??!!
      except
        cbAnimatePlanets.IsChecked := false; //error.. stop animation

        Raise;
      end;
    end;


  if rectToast.Visible then  //hide Toast message after 3 secs
    begin
      T := Now-fToastMsgStartTime;
      if (T>XSeg) then
        rectToast.Visible := false;
    end;

end;

end.
