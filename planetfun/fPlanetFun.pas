unit fPlanetFun;    //-----  planetary system simulation in 4D ----\\
//-----------------//                                               \\
// App for delphi firemonkey ( Win32, iOS, Android )                ||
// Source: github.com/omarreis/vsop2013  ->   folder planetfun       \\
// History:                                                           \\
//   v1.0 - jul20 - by oMAR                                            \\
//   v1.3 - oct20 Om Added lighthouse, virtual phone and sensors       //
//          augmented reality mode if camera attached to the phone     \\
//   v1.4 - nov20 Om Added Moon correct positioning                    //
//          from Astronomical Algorithms - ELP2000 Chapront-Touze      \\
//          Andreas Hörstemeier TMoon V 2.0                            //
//          see http://www.hoerstemeier.com/moon.htm                  //
//   v1.6. -  dec22: Om: fixed startup crash on Android 13 (w/ D11.2) \\
//   v1.7 - * Added the brightest stars to the celestial sphere       ||
//            Stars are spheres at R=200 ( the same radious           ||
//            of the sky background image, so stars are               ||
//            half-in,half-out the big celestial sphere               ||
//          * added a new form with Almanac w/ various                ||
//            calculation engines for Sun, Moon, Planets and stars    ||
//            vsop2013, vsop87, ELP2000, epoch reduction              ||
//          * solar system animation                                  //
//   v1.8 - apr23: Fix ecliptic obliquity, which was set             //
//                 to 0 by mistake !                                //
//   v1.9 - may23: constellation drawings                          //
//----------------------------------------------------------------//

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
  FMX.Platform,

  {$IFDEF ANDROID}
  FMX.Platform.Android,
  DW.PermissionsRequester,   // include /DelphiWorlds/KastriFree for Android permissions
  DW.PermissionsTypes,       // Android API Level 26+ permissions handling
  {$ENDIF ANDROID}
  MagnetometerAccelerometerFusion,  // TMagnetoAccelerometerFusion object ( augmented reality )

  fPlanetFunAlmanac,  // Almanac Form
  doubleVector3D,    // TVector3D_D - vector w/ 3 doubles ( instead of the 3 Singles in TVector3D )
  StarData,          // Hipparcos 150 and Navigation stars
  PlanetData,        // secundary planet tables. physical data
  CelestialObjects,  // celestial object database ( Currently w/ Sun, planets and Moon )
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
    LightMaterialTextureEarthNight: TLightMaterialSource;
    lightMaterialTextureJupiter: TLightMaterialSource;
    lightMaterialTextureMars: TLightMaterialSource;
    lightMaterialTextureSaturn: TLightMaterialSource;
    lightMaterialTextureVenus: TLightMaterialSource;
    cbOrbitDots: TSwitch;
    Label4: TLabel;
    textureStars: TTextureMaterialSource;
    sphereSkyBackground: TSphere;
    textPlanetFunTitle: TText3D;
    dummyPluto: TDummy;
    spherePluto: TSphere;
    lightMaterialTexturePluto: TLightMaterialSource;
    Grid3D1: TGrid3D;
    Label6: TLabel;
    cbAxisVisible: TSwitch;
    rectToast: TRectangle;
    labToast: TLabel;
    lightMaterialTexturePhoneBack: TLightMaterialSource;
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
    cylinderLighthouse: TCylinder;
    dummyLighthouse: TDummy;
    lightMaterialTextureFarol: TLightMaterialSource;
    dummyPhone: TDummy;
    cubePhone: TCube;
    dummyPhoneTarget: TDummy;
    lightMaterialTexturePhone: TLightMaterialSource;
    planePhoneBack: TPlane;
    labStatus2: TLabel;
    labStatus3: TLabel;
    label8: TLabel;
    cbSensorsOn: TSwitch;
    btnPhoneCamera: TSpeedButton;
    imgBtnPhone: TImage;
    timerStartSensorsiOS: TTimer;
    labStatus4: TLabel;
    Label12: TLabel;
    cbShowLightHouseAndPhone: TSwitch;
    imgCameraManipulationToolbar: TImage;
    sphereEarthNight: TSphere;
    RectAnimation1: TRectAnimation;
    btnAlmanac: TSpeedButton;
    ImgAlmanac: TImage;
    RectAnimation2: TRectAnimation;
    labAlmanac: TLabel;
    labTimeBtn: TLabel;
    labView: TLabel;
    labCamera: TLabel;
    btnAddStars: TSpeedButton;
    Label14: TLabel;
    cbSkyBG: TSwitch;
    Label15: TLabel;
    cbStarSpheres: TSwitch;
    btnCloseAbout2: TSpeedButton;
    rectBigToast: TRectangle;
    labBigToast: TLabel;
    labBigToastTitle: TLabel;
    btnCloseBigToast: TSpeedButton;
    dummyBanner: TDummy;
    Label16: TLabel;
    cbShowBanner: TSwitch;
    dummyCelestialSphere: TDummy;
    Label17: TLabel;
    cbEcliptic: TSwitch;
    Label18: TLabel;
    cbConstDrawings: TSwitch;
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
    procedure SolarSystemViewport3DGesture(Sender: TObject;  const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure labAboutClick(Sender: TObject);
    procedure btnCloseAboutClick(Sender: TObject);
    procedure labJDEClick(Sender: TObject);
    procedure imgPlanetFunBannerClick(Sender: TObject);
    procedure btnEditJDEClick(Sender: TObject);
    procedure btnCloseEditJDEClick(Sender: TObject);
    procedure btnOkJDEClick(Sender: TObject);
    procedure btnJDENowClick(Sender: TObject);
    procedure tbAngleOfViewChange(Sender: TObject);
    procedure MemoAboutApplyStyleLookup(Sender: TObject);
    procedure btnCloseVisibilityClick(Sender: TObject);
    procedure btnCloseCameraClick(Sender: TObject);
    procedure btnToggleTimeSettingsClick(Sender: TObject);
    procedure btnToggleVisibilitySettingsClick(Sender: TObject);
    procedure btnAlmanacClick(Sender: TObject);
    procedure cbSensorsOnSwitch(Sender: TObject);
    procedure btnPhoneCameraClick(Sender: TObject);
    procedure timerStartSensorsiOSTimer(Sender: TObject);
    procedure cbShowLightHouseAndPhoneSwitch(Sender: TObject);
    procedure imgCameraManipulationToolbarMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure On3dObjClick(Sender: TObject);
    procedure btnAddStarsClick(Sender: TObject);
    procedure cbStarSpheresSwitch(Sender: TObject);
    procedure btnCloseBigToastClick(Sender: TObject);
    procedure cbShowBannerSwitch(Sender: TObject);
    procedure cbEclipticSwitch(Sender: TObject);
    procedure cbSkyTextureSwitch(Sender: TObject);               // Using AA chapter 45
  private
    fFirstShow:boolean;
    fToastMsgStartTime:TDatetime;
    fMousePt:TPointF;
    fJDE:Double;

    fPlanetOrbitPoints:TList;  //save 3D dots created at runtime
    fStarDots:TList;          //save 3D dots created at runtime
    fEclipticDots:TList;

    // Gesture related
    FLastZoomDistance:Single;
    FLastRotationAngle:Single;
    fLastZoomPosition:TPointF;
    fLastPanPosition:TPointF;

    fMagAccelFusion:TMagnetoAccelerometerFusion;  // phone sensors
    fLastSensorFusionUpdate:TDatetime;

    {$IFDEF Android}
    FRequester: TPermissionsRequester;    // Android permission request mechanism
    {$ENDIF Android}

    // sensor logging handler
    procedure PlanetFunLogger(Sender:TObject; const aMsg:String);

    procedure FusionSensorHeadingAltitudeChanged(Sender:TObject);
    function  AppEventHandler(AAppEvent: TApplicationEvent;  AContext: TObject): Boolean;
    {$IFDEF Android}
    procedure DoRequestSensorPermissionsToAndroid;
    procedure PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
    {$ENDIF Android}

    procedure ClearOrbitDots;
    procedure showToastMessage(const S: String);
    procedure HandlePan(const EventInfo: TGestureEventInfo);
    procedure HandleZoom(const EventInfo: TGestureEventInfo);
    procedure HandleRotate(const EventInfo: TGestureEventInfo);
    procedure checkSphereSkyVisibility;
    procedure LoadPlanetTextures;
    procedure doPositionLighthouse(const aLat, aLon: Double);
    procedure getRandomTestPlace(var aLat, aLon: Double);

    procedure SizePlanets;
    procedure PositionPlanets;            // according to vsop2013
    procedure PositionEarthDailyRotation; // Earth rotation according to hour angle
    procedure PositionMoon;
    procedure DoMoveCamera(const step: TPointF);
    procedure DoRotateCamera(const step: integer);
    procedure DoCameraDolly(const step: integer);
    procedure ClearStarDots;
    procedure btnAddEclipticDotsClick(Sender: TObject);
    procedure ClearEclipticDots;
  public
    procedure FileLoadTerminate(Sender: TObject);
  end;

var
  FormPlanetFun: TFormPlanetFun=nil;

implementation   // ---x--xx-xxx........................

// solar system setup . Key properties to get the balls right
//  sphereEarth.RotationAngle.X = 336.566666666667    ( Obliquity 360-(23+26/60)    E=23o26'
//  dummyCelestialSpehere.RotationAngle.X = 336.566666666667      ( same obliquity as the earth. Same equator by definition )

// celestial sphere hierarchy
//  --- dummyCelestialSpehere
//                          +--- sphereSkyBackground ( with background image as texture R=200 AU )
//                          +--- stars[] - 3d spheres ( diam proportional to magnitude )

uses
  Om.Trigonometry,
  Om.AstronomicalAlgorithms,   // astronomical algorithm formulas from Meeus book
  Ah.Moon,                     // Moon positions ( from AA chapter 45 and Andreas Hörstemeier TMoon }
  quaternionRotations,
  CameraMovementToolbar;

{$R *.fmx}

{$IFDEF Android}  // Android permissions
const
  cPermissionsSensors=3;
  cPermissionAccessCoarseLocation = 'android.permission.ACCESS_COARSE_LOCATION';
  cPermissionAccessFineLocation   = 'android.permission.ACCESS_FINE_LOCATION';
{$ENDIF Android}  // request permissions to work

{ TFormPlanetFun }

procedure TFormPlanetFun.FormCreate(Sender: TObject);
var Y,M,D:word; UT:Double;
    AppEventSvc: IFMXApplicationEventService;
   {$IFDEF IOS} AEService: IFMXApplicationEventService;  {$ENDIF IOS}
begin
  fFirstShow := true;
  fLastSensorFusionUpdate := 0;  // =never

  FormatSettings.DecimalSeparator  := '.';  // vsop2013 text files use dots as decimal separator
  FormatSettings.ThousandSeparator := ',';

  fMousePt := PointF(0,0);
  fToastMsgStartTime := 0;   // = never

  DecodeDate( Date, {out:}Y,M,D);
  UT        := Time*24;             // in hours
  fJDE      := JD(Y, M, D, UT);     // current Julian date = Now

  fPlanetOrbitPoints := TList.Create;  // list of orbit dots
  fStarDots          := TList.Create;  // list star dots
  fEclipticDots      := TList.Create;  // list of ecliptic dots

  // gesture related vars
  FLastZoomDistance := 0;
  FLastRotationAngle:= 0;
  fLastZoomPosition := PointF(0,0);   //invalid
  fLastPanPosition  := PointF(0,0);

  //create sensors
  fMagAccelFusion := TMagnetoAccelerometerFusion.Create(Self);           //use sensor fusion
  //fMagAccelFusion.OnAccelerometerChange  := FusionSensorAccelChanged;  //not using those
  //fMagAccelFusion.OnMagnetometerChange   := FusionSensorMagChanged;
  fMagAccelFusion.OnHeadingAltitudeChange:= FusionSensorHeadingAltitudeChanged; // attitude change handler

  // Uncomment the line below to recv sensor log events in the main form
  // fMagAccelFusion.LoggerProc := PlanetFunLogger;  //activate sensor logging while debugging

  {$IFDEF ANDROID}
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(AppEventSvc)) then
    AppEventSvc.SetApplicationEventHandler(AppEventHandler);

  //permission requester for Android API 26+ permissions
  FRequester := TPermissionsRequester.Create;
  FRequester.OnPermissionsResult := PermissionsResultHandler;
  {$ENDIF ANDROID}

  {$IFDEF IOS} // Home btn handler for iOS
  if TPlatformServices.Current.SupportsPlatformService(StringToGUID('{F3AAF11A-1678-4CC6-A5BF-721A24A676FD}'),
      IInterface(AEService)) then
        AEService.SetApplicationEventHandler(AppEventHandler);
  {$ENDIF IOS}
end;

{$IFDEF Android}      // Android requires permissions for things like sensors
procedure TFormPlanetFun.PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
var LDeniedResults: TPermissionResults;
    LDeniedPermissions: string; i:integer;
begin
  case ARequestCode of     // Android permission request handler
    cPermissionsSensors:
      begin
        if AResults.AreAllGranted then  //all granted, start sensors on Android
          begin
            fMagAccelFusion.StartStopSensors({bStart:} true );  //now we can start sensor feed
          end
          else begin   // denied permissions ? wtf ??
            LDeniedPermissions := '';
            LDeniedResults := AResults.DeniedResults;
            for I := 0 to LDeniedResults.Count - 1 do
              LDeniedPermissions := LDeniedPermissions + ', ' + LDeniedResults[I].Permission;
            showToastMessage('You denied permissons ' + LDeniedPermissions + '. We need those!');
          end;
      end;
  end;
end;
{$ENDIF Android}

procedure TFormPlanetFun.btnLoadFileClick(Sender: TObject);
// var aFN:String;
// const sVSOP2013file = 'VSOP2013.p2000.bin';  // custom binary format
begin
  labFileMetadata.Text := 'loading.. wait..';  //wait

  //  defer vsop data file load ( the Almanac does that )

  //  if not Assigned(VSOP_File) then
  //    VSOP_File := T_VSOP2013_File.Create;   // vsop file parser and position calculator
  //
  //  // VSOP_File.OnLoadProgress := Form2LoadPropgress;
  //  // reads and parses long ASCII file: wait..
  //
  //  // file 'VSOP2013.p2000'  1500-3000. ( includes current time )
  //  // loaded on a thread  at activation
  //
  //  {$ifdef MsWindows}
  //  // TODO: set a folder app documents
  //  // aFN := Trim( edFilename.Text )+'.bin';    // '\dpr4\vsop2013\VSOP2013.p2000'  1500-3000. ( includes current time )
  //  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+
  //         'vsop2013'+ System.SysUtils.PathDelim+  //    /users/<username>/Documents/vsop2013/vsop2013.p2000.bin'
  //         sVSOP2013file;
  //  {$endif MsWindows}
  //
  //  {$ifdef Android}
  //  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+sVSOP2013file;
  //  {$endif Android}

  //  {$ifdef iOS}
  //  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+sVSOP2013file;
  //  {$endif iOS}

  //  if FileExists(aFN) then  // load
  //    begin
  //      VSOP_File.OnLoadTerminate := FileLoadTerminate;
  //      VSOP_File.Threaded_ReadBinaryFile( aFN );       // load data file on a separate thread ( takes some time )
  //      showToastMessage('Loading VSOP2013 data.  Wait..');
  //    end
  //    else showToastMessage('VSOP2013 file not found');

  // Memo1.Lines.Add(aFN+' Loaded');   //not so fast
  // PositionPlanets;

end;

//------------------------------------------
//       phone attitude axis ( Euler angles )
//          -Y     Z       altitude X up positive
//           |    /        heading  Y down positive
//           |   /         roll     Z positive into the screen
//       /=======\
//       |   | / |
//       |   |/  |
//       |   *---|--------- X
//       |       |
//       |   O   |
//       \-------/
//
//------------------------------------------

function normalize360(const a:Single):Single;
begin
  Result := a;
  while (Result<0) do Result := Result +360;
  while (Result>=360) do Result := Result -360;
end;

// log sensor messsages to about box memo ( debug only )
procedure TFormPlanetFun.PlanetFunLogger(Sender:TObject; const aMsg:String);
begin
  MemoAbout.Lines.Add(aMsg);
end;

// handler for sensor fusion readings ( phone attitude chg )
procedure TFormPlanetFun.FusionSensorHeadingAltitudeChanged(Sender:TObject);
var aAlt,aHead,aRoll,aLat,aLon:Single; s:String;  aSignal:integer;
  Q:TQuaternion3D;
  aSensorVec,tbVec,defVec:TVector3D;
  T:TDatetime;

begin
  T := Now;
  {$IFDEF Android}
  // Limit num,ber of sensor change events on Android ( native sensor ticks at 20 ms )
  // 0.1 seconds min between sensor updates --> 10 FPS
  if (T-fLastSensorFusionUpdate<(0.1/3600/24)) then
    exit;
  {$ENDIF Android}
  fLastSensorFusionUpdate := T;

  // aHead := fMagAccelFusion.fTCMagHeading;  // sensor fusion
  aHead := fMagAccelFusion.fTCTrueHeading;  // use true heading
  if (aHead=270.000000) then aHead:=0;    // TESTE 270.00000 means no magnetic readings ??

  aAlt  := fMagAccelFusion.fAltitude;
  aRoll := fMagAccelFusion.fRoll;

  s := ' Az:'+     Trim(Format('%5.0f°', [aHead] ))+'T'+
       ' Ele:'+    Trim(Format('%5.0f°', [aAlt ] ))+
       ' Roll:'+   Trim(Format('%5.0f°', [aRoll] ));   // roll  -- az
  labStatus2.Text := s;

  // use quaternion to apply sensor fusion readings
  ToQuaternion( aRoll, aHead-90 , aAlt, Q );
  // ToQuaternion( aRoll, aHead+90 , aAlt, Q );

  Self.SolarSystemViewport3D.BeginUpdate;  //needed ??
  try
   cubePhone.SetMatrix( Q );   // rotate phone model using the quaternion

   // dummySeaDisk.RotationAngle.Y := aSensorVec.Y;       // rotate compass disk

   // getRandomTestPlace(aLat,aLon);
   aLat := fMagAccelFusion.fLocationLat;
   aLon := fMagAccelFusion.fLocationLon;

   doPositionLighthouse(aLat,aLon);        // position lighthouse and phone on GPS position ( me )

   s := floatToLatitudeStr(fMagAccelFusion.fLocationLat) +'  '+
        floatToLongitudeStr(fMagAccelFusion.fLocationLon)+'  d:'+
        Format('%5.1f°', [fMagAccelFusion.fMagDeclination]);

   labStatus3.Text := s;  // show geographical coordinates (GPS)

   // attach virtual camera to the phone .._ enter augmented reality mode..

  finally
    Self.SolarSystemViewport3D.EndUpdate;
  end;

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

procedure TFormPlanetFun.DoRotateCamera(const step:integer);  // step +1 or -1
var aOldAng,aNewAng,da:Single;
begin
  aOldAng := fLastRotationAngle;
  aNewAng := aOldAng + step*5;      // 5 deg each click
  da      := (aNewAng-aOldAng);     // angle delta. (angle in rads ? )
  dummyCamera.RotationAngle.Z := dummyCamera.RotationAngle.Z + da; //incremental rotation in target direction axis ??
  fLastRotationAngle := aNewAng;
end;

procedure TFormPlanetFun.DoMoveCamera(const step:TPointF);  //2d camera movement
var   aOldLoc,aNewLoc,aDelta:TPointF;
begin
  // labStatus.Text := 'pan..';

  aOldLoc  := fLastPanPosition;     //save previous states
  aNewLoc  := aOldLoc + step*0.5;   //gesture center location
  aDelta   := aNewLoc-aOldLoc;

  if (aDelta.Length<>0) then
   begin
     dummyCamera.RotationAngle.Y := dummyCamera.RotationAngle.Y - aDelta.X;
     dummyCamera.RotationAngle.X := dummyCamera.RotationAngle.X - aDelta.Y;
   end;
   fLastPanPosition := aNewLoc;
end;

procedure TFormPlanetFun.DoCameraDolly(const step:integer);  // camera near <--> far   step +1/-1
var aOldDist,aNewDist,aDelta:Single; aOldLoc,aNewLoc:TPointF; v1:TPoint3D;
begin
  aOldDist := mjTomCamera.Position.Point.Length;
  aNewDist := aOldDist+step*0.5;
  if (aNewDist>0) then
    begin
      aDelta  := (aNewDist-aOldDist);
      v1      := mjTomCamera.Position.Point.Normalize;     // get camera pointing versor
      mjTomCamera.Position.Point := mjTomCamera.Position.Point + (aDelta*v1);
      checkSphereSkyVisibility;
    end;
end;

procedure TFormPlanetFun.imgCameraManipulationToolbarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var ix:integer;
begin
  x := x*5;    // original bmp=400x500  toolbar size=80x100 (5x)
  y := y*5;
  //map img click to btn action
  ix := toolbarCameraManipulation_mapClickToButton(x,y);  // CameraMovementToolbar.pas
  case ix of
    0: DoRotateCamera(+1);              // 0 rot left      <-- camera roll
    1: DoRotateCamera(-1);              // 1 rot right
    2: DoMoveCamera( PointF(0,+1) );    // 2 move up       <-- camera Az and elev
    3: DoMoveCamera( PointF(+1,0) );    // 3 move right
    4: DoMoveCamera( PointF(0,-1) );    // 4 move down
    5: DoMoveCamera( PointF(-1,0) );    // 5 move left
    6: DoCameraDolly(+1);    // 6 plus move far            <-- camera dolly ( change distance to target )
    7: DoCameraDolly(-1);    // 7 minus move near
  end;
  // labStatus.Text := IntToStr(ix)+' - '+FloatToStr(x)+','+FloatToStr(y);  //teste
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
  PositionEarthDailyRotation;
  PositionMoon;
end;

procedure TFormPlanetFun.PositionEarthDailyRotation;    // rotate earth according to hour angle
var aUT:TDatetime; aRot,aRA,aDecl:Double;
begin
  aUT := JDtoDatetime( fJDE );
  // labStatus2.Text := FormatDatetime('dd-mmm-yyyy hh:nn:ss',aUT);

  GreenwitchToCelestial( aUT, {out:} aRA,aDecl);     //RA returns in degrees

  aRot := 180-aRA;       // greenwich meridian is in the middle of the texture ( rot=180 )
  AngleTo0_360(aRot);

  // labStatus3.Text:= 'RA='+Trim( Format('%5.1f',[aRA]) )+' rot='+Trim( Format('%5.1f',[aRot]) ) ;

  // y = Earth rotation axis
  sphereEarth.RotationAngle.Y       := aRot; //rotate earth    Ad hoc factor to make earth spin fast
  sphereEarthNight.RotationAngle.Y  := aRot; //rotate earth night in sync

  // labStatus.Text := Format('%5.1f',[aRot]);
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

procedure TFormPlanetFun.On3dObjClick(Sender: TObject);    // clicked a 3d obj. show object data...
var aObjName:String; aOBj:TCelObjBase; aGMT:TDatetime;
    SL:TStringList;
begin                                                      // object must have HitTest = true
  aObjName:='';
  if ((Sender as TControl3D).TagString<>'') then     //stars etc
      aObjName := (Sender as TControl3D).TagString
  else if (Sender=sphereMoon)    then aObjName := 'Moon'
  else if (Sender=sphereEarth)   then aObjName := 'Earth'
  else if (Sender=sphereMercury) then aObjName := 'Mercury'
  else if (Sender=sphereVenus)   then aObjName := 'Venus'
  else if (Sender=sphereMars)    then aObjName := 'Mars'
  else if (Sender=sphereJupiter) then aObjName := 'Jupiter'
  else if (Sender=sphereSaturn)  then aObjName := 'Saturn'
  else if (Sender=sphereUranus)  then aObjName := 'Uranus'
  else if (Sender=sphereNeptune) then aObjName := 'Neptune'
  else if (Sender=spherePluto)   then aObjName := 'Pluto';
  // else ..
  if (aObjName<>'') then
    begin
      aObj := FrmPlanetFunAlmanac.getCelObjByName(aObjName);   // generic obj query
      if Assigned(aObj) then
        begin
          aObj.GMT := JDtoDatetime( fJDE );  // to UT
          SL := TStringList.Create;
          aObj.GetObjectData(SL);        // get obj text
          if SL.Count>1 then
             begin
               labBigToastTitle.Text := aObj.Name;
               SL.Delete( 0 );                    //delete o nome. vou repor maior em outro label
               labBigToast.Text      := SL.Text;
               rectBigToast.Visible  := true;
               rectBigToast.BringToFront;
             end;
          SL.Free;
        end;
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

        // set planet scales = planet radius
        r := LN( PLANET_DATA[0].radius/k  )*sc;   sphereSun.Scale.Point     := Point3D(r,r,r);
        r := LN( PLANET_DATA[1].radius/k  )*sc ;  sphereMercury.Scale.Point := Point3D(r,r,r);
        r := LN( PLANET_DATA[2].radius/k  )*sc ;  sphereVenus.Scale.Point   := Point3D(r,r,r);
        r := LN( PLANET_DATA[3].radius/k  )*sc ;  sphereEarth.Scale.Point   := Point3D(r,r,r);
                                             sphereEarthNight.Scale.Point   := Point3D(r,r,r); //same scale
        r := LN( PLANET_DATA[4].radius/k  )*sc ;  sphereMars.Scale.Point    := Point3D(r,r,r);
        r := LN( PLANET_DATA[5].radius/k  )*sc ;  sphereJupiter.Scale.Point := Point3D(r,r,r);
        r := LN( PLANET_DATA[6].radius/k  )*sc ;  sphereSaturn.Scale.Point  := Point3D(r,r,r);
        r := LN( PLANET_DATA[7].radius/k  )*sc ;  sphereUranus.Scale.Point  := Point3D(r,r,r);
        r := LN( PLANET_DATA[8].radius/k  )*sc ;  sphereNeptune.Scale.Point := Point3D(r,r,r);
        r := LN( PLANET_DATA[9].radius/k  )*sc ;  spherePluto.Scale.Point   := Point3D(r,r,r);
        r := LN( PLANET_DATA[10].radius/k )*sc ;  sphereMoon.Scale.Point    := Point3D(r,r,r);

        r := LN(360000/k)*sc /1.0;               //size moon orbit . ad hoc factor applied again
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
                                              sphereEarthNight.Scale.Point := Point3D(r,r,r);
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
  sphereEarth.RotationAngle.x      := r; // apply to Earth axis and to orbit of the Moon. Is this right ?
  sphereEarthNight.RotationAngle.x := r;
  // dummyMoonOrbitCenter.RotationAngle.x   := r;

  // set other planets approximate obliquities
  sphereMars.RotationAngle.x    := - PLANET_DATA[4].Obliq;
  sphereJupiter.RotationAngle.x := - PLANET_DATA[5].Obliq;
  sphereSaturn.RotationAngle.x  := - PLANET_DATA[6].Obliq;
  sphereUranus.RotationAngle.x  := - PLANET_DATA[7].Obliq;
  sphereNeptune.RotationAngle.x := - PLANET_DATA[8].Obliq;
  spherePluto.RotationAngle.x   := - PLANET_DATA[9].Obliq;
end;

procedure TFormPlanetFun.getRandomTestPlace(var aLat,aLon:Double);
var aCity:String;
begin
  //aLat:=-23.5;
  //aLon:=-46.5;
  //exit; //
  case  Random(5) of
    0: begin aLat:=-33.9; aLon:=+151.2; aCity:='Sydney'; end;  // Sydney 33.9S 151.2E
    1: begin aLat:=+51.5; aLon:=-0.12;  aCity:='London'; end;  // London 51.5N 0.12W
    2: begin aLat:=+37.8; aLon:=-122.4; aCity:='S Fco';  end;  // S Fco  37.8N 122.4W
    3: begin aLat:=-23.5; aLon:=-46.5;  aCity:='SPaulo'; end;  // S Paulo 23.5S 46.5W
  else begin aLat:=+35.6; aLon:=+139.7; aCity:='Tokyo';  end;  // Tokio 35.6N 139.7E
  end;
  labStatus2.Text := aCity;
end;

// 3d hierarchy for Lighthouse and Phone:
//    dummyEarth --> sphereEarth --> dummyLighthouse --> cylinderLighthouse --> dummyPhone --> cubePhone

procedure TFormPlanetFun.doPositionLighthouse(const aLat,aLon:Double);
begin
   //cylinderLighthouse.Scale.Point    := sphereEarth.Scale.Point /2;  // Lighthouse scale prop to Earth ( ad hoc factor )
   //cylinderLighthouse.Position.Point := Point3d( 0.51, 0, 0);       //some fixed point w/ R=0.55 floating

  // cubePhone.Scale.Point    := sphereEarth.Scale.Point;  //phone scaled similarly
  // cubePhone.Position.Point := Point3d( 0.65, 0, 0);     //some fixed point w/ R=0.55 floating

  // use euler angles ( RotationAngle) to set lighthouse attitude
  dummyLighthouse.RotationAngle.z := 0;          // reset lat before applying Lon
  dummyLighthouse.RotationAngle.y := 180-aLon;   // rotate lon
  dummyLighthouse.RotationAngle.z := -aLat;      // rotate lat
  // this shoud position the Lighthouse standing in earth surface at <---- GPS position
  // the phone is over it, with phone sensors rotations
end;

procedure TFormPlanetFun.PositionMoon; // according to AA chapter 45 at epoch fJDE
var  aUT:TDatetime;  aCoord:t_coord; k,r,sha,aRot:double;
begin
  aUT    := JDtoDatetime( fJDE );
  aCoord := moon_coordinate( aUT );  // use
  // Memo1.Lines.Add( 'rad: '+Format('%9.2f',[aCoord.radius   ]) );
  // Memo1.Lines.Add( 'lat: '+floatToGMSD(aCoord.latitude ) );
  // Memo1.Lines.Add( 'lon: '+floatToGMSD(aCoord.longitude) );
  // Memo1.Lines.Add( 'RA:  '+ floatToHHMMSS(aCoord.rektaszension) );
  // Memo1.Lines.Add( 'Decl:'+ floatToGMSD(aCoord.declination) );

  // dummyMoon.RotationAngle.Y     := dummyMoon.RotationAngle.Y + rot;
  // AUtoKm=AUtoM/1000;     // 1 AU = 149.598.787 Km   (~ 150M km)

  // k := aCoord.radius/EarthRadius;   //keep scale between earth radious and moon distance

  // dummyMoon.Position.x := 3;  // km --> au

  dummyMoonOrbitCenter.RotationAngle.z := 0;          // reset lat before applying Lon
  dummyMoonOrbitCenter.RotationAngle.y := -aCoord.longitude;  // rotate lon
  dummyMoonOrbitCenter.RotationAngle.z := -aCoord.latitude;      // rotate lat

  aRot := aCoord.longitude;   // greenwich meridian is in the middle of the texture ( rot=180 )
  // 180+ was removed ad hoc
  AngleTo0_360(aRot);
  // y = moon rotation axis
  // Moon rotates so the same part of the surface points to earth ( light face )
  sphereMoon.RotationAngle.Y  := aRot; //rotate Moon as it revolves

  // labStatus3.Text := 'Moon:'+floatToGMSD( aCoord.latitude )+ //teste
  //                    '  '+floatToGMSD( aCoord.longitude );
  // sha := 360-aCoord.rektaszension*15;    // ra in  hours, sha in degrees
  // AngleTo0_360(sha);                    // 0..sha..360
  // labStatus4.Text := 'R:'+ floatToHHMMSS(aCoord.rektaszension)+
  //                    'D:' + floatToGMSD(aCoord.declination)    +
  //                    's:' + floatToGMSD(sha);
end;

procedure TFormPlanetFun.PositionPlanets;   // according to vsop2013 at epoch fJDE
var ip:integer;  aPosition,aSpeed:TVector3D_D;
    aDummy:TDummy; Year:Double; aPosition3d:TPoint3d; aUT:TDatetime; SunEarthVec:TVector3D;
const
  AUinKm=149598000;    // au in km: 150M
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
           if Assigned(aDummy) then  //position planet
             aDummy.Position.Point := aPosition3d;   // 1 = scale --> 1 au = 1.0 3D world unit
         end;

      // displace earthNight 100 km away from the Sun
      SunEarthVec := dummyEarth.Position.Vector;
      // sphereEarthNight.Position.Point := -SunEarthVec/100;   //*1000/AUinKm;
    end;

  // show date
  Year :=  (fJDE-jd2000)/365.2422+2000.0;      // more or less :)

  labJDE.Text  := Format('%6.1f', [Year] );
  labJDE2.Text := labJDE.Text;

  aUT := JDtoDatetime( fJDE );
  labStatus.Text := FormatDatetime('dd-mmm-yyyy hh:nn:ss',aUT );
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
      // camera hierarchy:   dummyCamera --> mjTomCamera ( the main camera )
      // mjTomCamera.Position is in relation to dummyCamera ( its parent )
      // since mjTomCamera.Target = dummyCamera, the camera always point to dummyCamera
      v1 := mjTomCamera.Position.Point.Normalize;   // get previous camera pointing versor
      t  := tbDistanceToTarget.Value;     // value in range 1..101
      d  := ( exp(t/60)-1.0 )*20.0;       // d in au range 0.32..88 au ( the exponent formula increases granularity when camera is near )

      mjTomCamera.Position.Point := d*v1; // set distance from camera to target
      checkSphereSkyVisibility;  // make sure the sky background is not visible from outside the celestial sphere

      labDistanceToTarget.Text := Format('%5.2f',[d] )+ ' au';
    end;
end;

// call after camera distance change
procedure TFormPlanetFun.checkSphereSkyVisibility;
var aCamPos:TPoint3D;
begin
  // if camera outside the sphere w/ stars, we dont want it to be visible and obstruct vision of the little planets
  aCamPos := dummyCamera.Position.Point + mjTomCamera.Position.Point;     // heliocentric position of our camera
  // sphereSky.Visible := (aCamPos.Length < sphereSky.Scale.x*0.5 );      //hide the sky sphere before we go out
end;

procedure TFormPlanetFun.tbPlanetScaleChange( Sender: TObject );
var sc,aLat,aLon:Double;
begin
  sc := tbPlanetScale.Value;   // 1..101
  labPlanetScale.Text := Trim( Format('%6.1f',[sc]) );
  SizePlanets;                 //resize according to new scale

  // aLat := -23.5;           //Lighthouse at home
  // aLon := -46.5;
  // getRandomTestPlace(aLat,aLon);
  // doPositionLighthouse(aLat,aLon);
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
  labAnimationSpeed.Text := Trim(Format('%4.1f',[ tbAnimationSpeed.Value/10] ))+' days/s';
end;

procedure TFormPlanetFun.tbDateChange(Sender: TObject);
begin
  fJDE := jd2000+tbDate.Value;      // in days
  PositionPlanets;
  // PositionEarthDailyRotation;   this makes earth spin crazy - suppresed
  PositionMoon;
end;

function TFormPlanetFun.AppEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
var s:String;
begin
  if (AAppEvent =  TApplicationEvent.EnteredBackground) then  // Home btn pressed
     begin
       fMagAccelFusion.StartStopSensors({bStart:} false );  //stop sensor feed. Not using sensors at background
     end
  else if (AAppEvent = TApplicationEvent.BecameActive )  then
    begin   //returned from Home
        {$IFDEF Android}
        if cbSensorsOn.IsChecked then            // if sensors perviously active..
          DoRequestSensorPermissionsToAndroid;   // ..restart sensor feed
        {$ENDIF Android}

        {$IFDEF iOS}
        if cbSensorsOn.IsChecked then            // if sensors perviously active..
          fMagAccelFusion.StartStopSensors({bStart:} true );
        {$ENDIF iOS}

        {$IFDEF MsWindows}
        if cbSensorsOn.IsChecked then            // if sensors perviously active..
          fMagAccelFusion.StartStopSensors({bStart:} true );
        {$ENDIF MsWindows}
    end;
  Result := True; // apparently this doesn't matter on iOS
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
     for ip := 1 to 9 do // build orbits with dots, for all planets
        begin
          yearLen  := PLANET_DATA[ip].revPer;  // rev period = planet "year"
          weekLen := yearLen/52;              // = planet week len in days

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
                        else aDot.MaterialSource := colorPathPlanet;  //generic
                        end;

                        aDot.Opacity        := 0.20;      //transparent

                        fPlanetOrbitPoints.Add( aDot );
                      end
                      else begin    // other 119 dots are proxies
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

// Adds a number of 3d spheres at radius = 200 AU
// Since the celestial sphere also has r=200,
// the stars are on the surface of the 3d sphere (half-in half-out)
// so they can be seen from inside and outside the 200 au background sky ball
procedure TFormPlanetFun.btnAddStarsClick(Sender: TObject);
var aStarSphere:TSphere; i,ip:integer; aProxy:TProxyObject;
    yearLen,weekLen,aJDE,radiusDot:Double; aPos,aSpeed:TVector3D_D;
    aPoint3d:TPoint3d;
    aStar:TStarH150; aGMT:TDAtetime;
    A,B,C:Double;

begin
   if not ( Assigned(VSOP_File) and VSOP_File.fLoaded ) then exit; //sanity

   aGMT := JDtoDatetime( fJDE );  // to UT

   // SolarSystemViewport3D.BeginUpdate;
   try
       for i:=1 to NumH150stars do
         begin
           aStar := StarsH150[i];

           radiusDot := 4.0-aStar.fMagnitude;        //
           if ( radiusDot<0.5 ) then continue;       // too small ... don't include

           aStar.GMT := aGMT;        // calc coordinates

           A := aStar.fRA;           // these are equatorial
           B := aStar.fDecl;
           C := 200;                 //-radiusDot/2;       // external celestial sphere background has radius 200, so dots are half in half out

           // to heliocentric ecliptic cartesian :
           aPos.x := (C * cosg(B)) * cosg(A);
           aPos.y := (C * cosg(B)) * sing(A);
           aPos.z := (C * sing(B));

           aPoint3d := Point3D(   // 3DWorld   Universe   ( ecliptic coordinates )
           aPos.x,                //   x          x
          -aPos.z,                //   y         -z       ( 3d y points down while universe z points up )
           aPos.y  );             //   z          y

           if (i=1) then     // 1st star is a sphere, other are proxys of that 1st star.
             begin
               aStarSphere := TSphere.Create( Self );
               dummyCelestialSphere.AddObject(aStarSphere);
               aStarSphere.SubdivisionsAxes    := 8;   //def=16     // make low tesselation star spheres ( only seem from far )
               aStarSphere.SubdivisionsHeight  := 8;   //def=12

               // Note: The app uses ecliptic cartesian coordinates ( x,y,z in au, same system as vsop2013 )
               // and regular Almanac coodinates RA,Decl are equatorial. Between the two there is
               // the obliquity of the ecliptic, which is about 23g26'
               //
               // By setting dummyCelestialSphere.RotationAmngle.x=336.5667 ( = -26g44' )
               // and parenting the stars to dummyCelestialSphere (the celestial sphere) we gain automatic translation
               // between the two systems (equ and cliptic).
               // sphereEarth is also rotated in the same fashion, so both have the same equators.

               // sphereSkyBackground (the sphere w/ sky background image) is also parented to dummyCelestialSphere.
               // sphereSkyBackground.RotationAnle.y was set 359.6 to make the texture "fit" the actual star spheres
               // ( probably because the image was generated for J2000 and there was precession since)

               aStarSphere.Position.Point := aPoint3d;
               aStarSphere.SetSize( radiusDot,radiusDot,radiusDot );

               aStarSphere.TagString := aStar.Name;         // save starname.  When a star is clicked, Almanac data is shown
               aStarSphere.MaterialSource := colorSun;

               aStarSphere.Opacity    := 0.5;      //semi transparent
               aStarSphere.HitTest    := true;
               aStarSphere.OnClick    := On3dObjClick;

               fStarDots.Add( aStarSphere );   // fStarDots contains the 3d dots
             end
             else begin    // other 119 dots are proxies
               aProxy := TProxyObject.Create(Self);
               dummyCelestialSphere.AddObject(aProxy);  //parent star to the celestial sphere

               aProxy.SourceObject   := aStarSphere;    // mk copies of star
               aProxy.Position.Point := aPoint3d;
               aProxy.SetSize( radiusDot, radiusDot, radiusDot );
               aProxy.Opacity        := 0.5;
               aProxy.TagString      := aStar.Name;
               aProxy.HitTest        := true;
               aProxy.OnClick        := On3dObjClick;

               fStarDots.Add( aProxy );
             end;
         end;

   finally
     // SolarSystemViewport3D.EndUpdate;
   end;
end;

procedure TFormPlanetFun.ClearStarDots;
var aObj:TControl3D; i:integer;
begin
  // SolarSystemViewport3D.BeginUpdate;
  try
      // delete all dots
      for i:=0 to fStarDots.Count-1 do
        begin
          aObj := TControl3D(fStarDots.Items[i]);
          dummyCelestialSphere.RemoveObject(aObj);
          aObj.Free; //??
        end;
      fStarDots.Clear;
  finally
    // SolarSystemViewport3D.EndUpdate;
  end;
end;

procedure TFormPlanetFun.btnAddEclipticDotsClick(Sender: TObject);
var aEclipticDot:TSphere; i,ip:integer; aProxy:TProxyObject;
    yearLen,weekLen,aJDE,radiusDot:Double; aPos,aSpeed:TVector3D_D;
    aPoint3d:TPoint3d;
    aGMT:TDAtetime;
    A,B,C:Double;
    Delta:Double;

const
  NumEclipticDots=100;

begin
   // SolarSystemViewport3D.BeginUpdate;
   try
     Delta:= 360/NumEclipticDots;
     radiusDot := 2.5;
     for i:=1 to NumEclipticDots do
         begin
           A := i*Delta;   // RA 0..360
           B := 0;         // Decl=0
           C := 200;       // external celestial sphere background has radius 200, so dots are half in half out
           // to heliocentric cartesian
           aPos.x := (C * cosg(B)) * cosg(A);
           aPos.y := (C * cosg(B)) * sing(A);
           aPos.z := (C * sing(B));

           aPoint3d := Point3D(   // 3DWorld   Universe   ( ecliptic coordinates )
           aPos.x,                //   x          x
          -aPos.z,                //   y         -z       ( 3d y points down while universe z points up )
           aPos.y  );             //   z          y

           if (i=1) then     // 1st star is a sphere, other are proxys of that 1st star.
             begin
               aEclipticDot := TSphere.Create( Self );
               SolarSystemViewport3D.AddObject( aEclipticDot );  //parent ecliptic to viewport root
               aEclipticDot.SubdivisionsAxes    := 6;   //def=16     // make low tesselation spheres ( only seem from far )
               aEclipticDot.SubdivisionsHeight  := 6;   //def=12

               aEclipticDot.Position.Point := aPoint3d;
               aEclipticDot.SetSize( radiusDot,radiusDot,radiusDot );

               aEclipticDot.MaterialSource := colorPathEarth;

               aEclipticDot.Opacity    := 0.3;      //semi transparent
               aEclipticDot.HitTest    := true;
               aEclipticDot.OnClick    := On3dObjClick;

               fEclipticDots.Add( aEclipticDot );   //save ecliptic dots, so we can destroy them later
             end
             else begin    // other 119 dots are proxies
               aProxy := TProxyObject.Create(Self);
               SolarSystemViewport3D.AddObject(aProxy);  //parent ecliptic to viewport root

               aProxy.SourceObject   := aEclipticDot;    // mk copies of star
               aProxy.Position.Point := aPoint3d;
               aProxy.SetSize( radiusDot, radiusDot, radiusDot );
               aProxy.Opacity        := 0.3;
               aProxy.TagString      := aEclipticDot.Name;
               aProxy.HitTest        := true;
               aProxy.OnClick        := On3dObjClick;

               fEclipticDots.Add( aProxy );
             end;
         end;

   finally
     // SolarSystemViewport3D.EndUpdate;
   end;
end;

procedure TFormPlanetFun.ClearEclipticDots;
var aObj:TControl3D; i:integer;
begin
  // SolarSystemViewport3D.BeginUpdate;
  try
      // delete all dots
      for i:=0 to fEclipticDots.Count-1 do
        begin
          aObj := TControl3D(fEclipticDots.Items[i]);
          SolarSystemViewport3D.RemoveObject(aObj);
          aObj.Free; //??
        end;
      fEclipticDots.Clear;
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

procedure TFormPlanetFun.btnCloseBigToastClick(Sender: TObject);
begin
  rectBigToast.Visible := false;
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

//  procedure TFormPlanetFun.btnAlmanacClick(Sender: TObject);
//  var sc,aLat,aLon:Double;
//  begin
//    // aLat := -23.5;  //Lighthouse at home
//    // aLon := -46.5;
//    getRandomTestPlace(aLat,aLon);
//    doPositionLighthouse(aLat,aLon);
//  end;
//

procedure TFormPlanetFun.btnAlmanacClick(Sender: TObject);
begin
  if Assigned(FrmPlanetFunAlmanac) then
    begin
      FrmPlanetFunAlmanac.Show;
      {$IFNDEF MsWindows}      // On Win32 we don't hide the main form as we show the almanac form ( the two remain visible )
      Hide;
      {$ENDIF MsWindows}
    end;
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
  PositionEarthDailyRotation;
  PositionMoon;

  rectEditJDE.Visible:= false;
end;

procedure TFormPlanetFun.btnPhoneCameraClick(Sender: TObject);
const
  ixLightHouseTarget=11;
  ixPhoneTarget=12;
var S:String;
begin
  // btn toggles between target = phone and lighthouse ( one attached to the phone sensors, one fixed )
  S:='';
  if (comboTarget.ItemIndex=ixPhoneTarget) then
    begin
      S :='target = lighthouse';
      comboTarget.ItemIndex := ixLightHouseTarget
    end
    else begin
      S :='target = phone';
      comboTarget.ItemIndex := ixPhoneTarget;
    end;
  showToastMessage( S );
end;

procedure TFormPlanetFun.FileLoadTerminate(Sender:TObject);   // thread load of VSOP2013.p2000.bin terminated
var S:String;                                                // ready to calculate ephemeris
begin
  if VSOP_File.fLoaded then
    begin
      S := S+'VSOP2013.p2000.bin loaded'+ #13#10+
             'Dates between 1500 and 3000';
    end
    else S := ' vsop file read error';       //not supposed to happen.....

  showToastMessage( S );  // notify finished download

  if VSOP_File.fLoaded then
    begin
      textPlanetFunTitle.Visible := false;     // hide app title after the file is loaded, and we are open for business
      // planeBanner.Visible        := false;  //keep banner visible in space..

      labFileMetadata.Text := VSOP_File.getMetadata;  //show vsop file metadata ( header )
      labJDEClick(nil);    // sets fJDE to current and repos planets
      Self.btnAddStarsClick(nil);     //add big star dots
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

  EarthNigthTex='2k_earth_nightmap.jpg';
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

  // night texture
  aFN := aPath + EarthNigthTex;
  lightMaterialTextureEarthNight.Texture.LoadFromFile(aFN);
end;

// sky background texture files - 4 high res images w/ different visibility options
// bytes     file
// 2,439,862 SkyMapDrawings.png
// 1,857,870 SkyMapLinesNames.png
// 2,800,244 SkyMapLinesNamesDrawings.png
// 1,304,827 SkyMapPlain.png

const
  sSkyMapLinesNamesDrawings = 'SkyMapLinesNamesDrawings.png';
  sSkyMapLinesNames         = 'SkyMapLinesNames.png';
  sSkyMapDrawings           = 'SkyMapDrawings.png';
  sSkyMapPlain              = 'SkyMapPlain.png';

procedure TFormPlanetFun.cbSkyTextureSwitch(Sender: TObject);
var aFN,aFile:String;
begin
  // on (my) Windows 10 it is  'C:\Users\<user>\OneDrive\Documentos\vsop2013\'  may include \OneDrive\
  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim;     // mk file path
  {$IFDEF MsWindows}
  aFN := aFN +'vsop2013'+ System.SysUtils.PathDelim;  // for Windows there is an extra 'vsop2013\'
  {$ENDIF MsWindows}

  aFile := '';    // no file --> hide sphere

  if cbSkyBG.IsChecked then   // 3 visibility checkbox determine which sky texture to use
    begin
      if cbConstLinesNames.IsChecked then
        begin                                                                  //  visibility options determine which texture to load
          if cbConstDrawings.IsChecked then aFile:=sSkyMapLinesNamesDrawings   //  starBG   linesNames   drawings
            else aFile:=sSkyMapLinesNames;                                     //  starBG   linesNames
        end
        else begin  // no lines/names
          if cbConstDrawings.IsChecked then aFile:=sSkyMapDrawings             //  starBG                drawings
            else aFile:=sSkyMapPlain;                                          //  starBG   <--
        end;
    end;

  if (aFile<>'') then  // load it
    begin
        aFN := aFN+aFile;            //  path + filename
        if FileExists(aFN) then      //  sanity test
          begin
            textureStars.Texture.LoadFromFile(aFN);     // load file
            sphereSkyBackground.Visible := true;
          end
          else begin
            // expected file not found... bad deployment.. do something...
          end;
    end
    else begin
      sphereSkyBackground.Visible := false;
    end;
end;

procedure TFormPlanetFun.cbEclipticSwitch(Sender: TObject);
begin
  if cbEcliptic.IsChecked then
    btnAddEclipticDotsClick(nil)
    else ClearEclipticDots;
end;

procedure TFormPlanetFun.cbOrbitDotsSwitch(Sender: TObject);
begin
  if cbOrbitDots.IsChecked then
    btnAddOrbitDotsClick(nil)
    else ClearOrbitDots;
end;

procedure TFormPlanetFun.cbSensorsOnSwitch(Sender: TObject);
begin
  {$IFDEF Android}  // On Android always request permission to start sensors
  if cbSensorsOn.IsChecked then DoRequestSensorPermissionsToAndroid
    else  fMagAccelFusion.StartStopSensors({bStart:} false );
  {$ENDIF Android}

  {$IFDEF iOS}
  fMagAccelFusion.StartStopSensors({bStart:} cbSensorsOn.IsChecked );  //now we can start sensor feed
  {$ENDIF iOS}

  {$IFDEF MsWindows}
  fMagAccelFusion.StartStopSensors({bStart:} cbSensorsOn.IsChecked );  //now we can start sensor feed
  {$ENDIF MsWindows}
end;

procedure TFormPlanetFun.cbShowBannerSwitch(Sender: TObject);
begin
  dummyBanner.Visible := cbShowBanner.IsChecked;   // banner is a TPlane with app's About image
end;

procedure TFormPlanetFun.cbShowLightHouseAndPhoneSwitch(Sender: TObject);
begin
  dummyLighthouse.Visible := cbShowLightHouseAndPhone.IsChecked;  // show/hide LH n phone
end;

procedure TFormPlanetFun.cbStarSpheresSwitch(Sender: TObject);
begin
  if cbStarSpheres.IsChecked then btnAddStarsClick(nil)       // add/remove star spheres for the brightest stars
    else ClearStarDots;
end;

procedure TFormPlanetFun.comboTargetChange(Sender: TObject);
var aDummy:TDummy;
begin
  case comboTarget.ItemIndex of           // combo items:
    0: aDummy := dummySun;                // Sun
    1: aDummy := dummyMercury;            // Mercury
    2: aDummy := dummyVenus;              // Venus
    3: aDummy := dummyEarth;              // Earth
    4: aDummy := dummyMoon;               // Moon
    5: aDummy := dummyMars;               // Mars
    6: aDummy := dummyJupiter;            // Jupiter
    7: aDummy := dummySaturn;             // Saturn
    8: aDummy := dummyUranus;             // Uranus
    9: aDummy := dummyNeptune;            // Neptune
    10: aDummy := dummyPluto;             // Pluto
    11: aDummy := dummyLighthouse;        // Lighthouse
    12: aDummy := dummyPhoneTarget;       // Phone ( Augmented Reality mode )
    13: aDummy := dummyBanner;            // PlanetFun plate ..
    else exit;
  end;

  dummyCamera.Parent := aDummy;  //parent dummyCamera to target planet dummy
  // move camera to 0,0,0  (on parent scale)
  dummyCamera.Position.Point := Point3d(0,0,0);          // aDummy.Position.Point;  // move camera to position. TODO: animate camera shift
  dummyCamera.RotationAngle.Point := Point3d(0,0,0);    // aDummy.Position.Point;  // reset camera angles

  tbDistanceToTarget.Value := 30;    // 30 = 15% of the trackbar - set camera at small distance from target ( but not too close )
  // DoCameraDolly(+1);   // get far from

  // move camera ( TODO: animate that )
  // mjTomCamera.Target  := aDummy; //camera pointing to dummy
end;

{$IFDEF Android}  // request permissions to work
procedure TFormPlanetFun.DoRequestSensorPermissionsToAndroid;
begin
  FRequester.RequestPermissions([ cPermissionAccessCoarseLocation,  // used sensors: location, magnetic , accelerometer
                                  cPermissionAccessFineLocation],
                                  cPermissionsSensors);     // commented out cPermissionAccessMockLocation
end;
{$ENDIF Android}

procedure TFormPlanetFun.timerStartSensorsiOSTimer(Sender: TObject);
begin
  fMagAccelFusion.StartStopSensors({bStart:} true );  //start ios sensor feed
  timerStartSensorsiOS.Enabled := false;              //once

  // planeBanner.Visible        := false;  //keep banner visible in space..
end;

procedure TFormPlanetFun.FormActivate(Sender: TObject);
var aLat,aLon:Double;
begin
   if fFirstShow then  // once:  load vsop2013 ephemerides binary file ( threaded load )
     begin
       // init date/time edit filds with now
       dateeditJDE.Date := Date;
       timeeditJDE.Time := Time; //local

       {$IFDEF Android}  // request permissions to work
       DoRequestSensorPermissionsToAndroid;  // sensors are started when perm recvd
       //  On Android, sensors are started after permission is checked
       {$ENDIF Android}

       {$IFDEF IOS}  //
       // On D10.4.1 I found that one cannot start LocationSensor from FormActivate,
       // or the sensor feed doesnt start
       // used a Timer to defer sensor start 2 seconds
       timerStartSensorsiOS.Enabled := true;
       {$ENDIF IOS}

       {$IFDEF MsWindows}
       fMagAccelFusion.StartStopSensors({bStart:} true );  // for Windows, start simulated sensor feed (timer )
       {$ENDIF MsWindows}

       TimerSolarSystem.Enabled := true;  // ..and God said: may Time start...now !
       SizePlanets;
       // aLat := -23.5;  //Lighthouse at home   23.5S / 46.5W
       // aLon := -46.5;
       // getRandomTestPlace(aLat,aLon);
       // doPositionLighthouse(aLat,aLon);

       Grid3D1.Visible := false;        //didn't manage to do that as design time
       rectTime.Visible := false;      // menus starts hiden

       LoadPlanetTextures;            // load planet texture images from Documents
       cbSkyTextureSwitch(nil);       // this loads defaut sky texture from file

       btnLoadFileClick(nil);         // load data file on startup
       btnLoadFile.Visible := false;  // hide filename editor ( for debug only)
       edFilename.Visible := false;   // and associated btn

       fFirstShow:= false;
     end;
end;

procedure TFormPlanetFun.TimerSolarSystemTimer(Sender: TObject); // 200 ms ticks
var rot:double; T:TDAtetime;
const XSeg= 3/24/3600;   // X=3 secs
begin
  // rotate planets
  rot := 0.1;      // 0.1 deg/tick = 1 deg/sec

  // slow rotation, to animate stuff ( fake speed. not realistic )

  // earth is not rotated randomly but set according to hour angle in fJDE
  // sphereEarth.RotationAngle.Y   := sphereEarth.RotationAngle.Y -5*rot; //rotate earth   Ad hoc factor to make earth spin fast
  SolarSystemViewport3D.BeginUpdate;
  try
      sphereJupiter.RotationAngle.Y := sphereJupiter.RotationAngle.Y +2*rot; //jupiter too
      sphereVenus.RotationAngle.Y   := sphereVenus.RotationAngle.Y   +rot;
      sphereSaturn.RotationAngle.Y  := sphereSaturn.RotationAngle.Y  +rot;
      sphereMars.RotationAngle.Y    := sphereMars.RotationAngle.Y    +rot;
      sphereNeptune.RotationAngle.Y := sphereNeptune.RotationAngle.Y +rot;
      spherePluto.RotationAngle.Y   := spherePluto.RotationAngle.Y   +rot;

      if textPlanetFunTitle.Visible then //that too
        textPlanetFunTitle.RotationAngle.Y :=  textPlanetFunTitle.RotationAngle.Y +rot;

      if planeBanner.Visible  then   //and that
         planeBanner.RotationAngle.Y :=  planeBanner.RotationAngle.Y +1.2*rot;

      // dummyMoon.RotationAngle.Y     := dummyMoon.RotationAngle.Y + rot;
      // dummyMoonOrbitCenter.RotationAngle.Y := dummyMoonOrbitCenter.RotationAngle.Y - rot;  //move moon in its orbit

      if cbAnimatePlanets.IsChecked then
        begin
          fJDE := fJDE + tbAnimationSpeed.Value/10;     // tbAnimationSpeed.Value = time change/s
          try
            PositionPlanets;  // reposition planet ephemeris every 200 ms
            // test uncommented below
            PositionEarthDailyRotation;   // not rotating earth here
            PositionMoon;                 // not rotating the moon either ??
          except
            cbAnimatePlanets.IsChecked := false; // error.. probably time outside ephemeris range.. stop animation
            Raise;
          end;
        end;
  finally
    SolarSystemViewport3D.EndUpdate;
  end;

  // hide toast message after some time ( currently 3 secs )
  if rectToast.Visible then  //hide Toast message after 3 secs
    begin
      T := Now-fToastMsgStartTime;
      if (T>XSeg) then rectToast.Visible := false;
    end;
end;

end.
