unit fPlanetFunAlmanac;    //------ Form for the PlanetFun Almanac ------\\
  //----------------------// performs astrometric calculations with       \\
 // various engines.. mostly from Astronomical Algorithms ( J Meeus )      \\
// Sun, Planets - VSOP87, VSOP2013                                          \\
// Moon ELP2000 (code by Andreas Hörstemeier TMoon component v2.0 )          \\
// Stars - position on initial epoch plus epoch change calculations          //
//         2 star catalogs: Hipparcos 150 and Navigation (56 stars)         //
//         Animated Solar System diagram ( using vsop2013 )                //
//  by oMAR                                                               //
//  source: github.com/omarreis/VSOP2013                                 //
//----------------------------------------------------------------------//
//  History:                                                            ||
//           om:mar23: v1.0  - included in PF 1.7                       ||
//----------------------------------------------------------------------//

interface

uses
  // System
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.DateUtils,
  System.Math.Vectors, System.Sensors,
  System.IOUtils, System.Permissions,

  System.Sensors.Components,    // LocationSensor
  // FMX
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.Edit, FMX.ListBox,FMX.Objects,
  FMX.DialogService,

  // PlanetFun Almanac
  StarData,                     // TCelObjBase, Hipparcos 150 and Navigation stars
  vsop2013,                     // vsop2013 database
  doubleVector3D,
  PlanetData;                   // planet object

type
  TFrmPlanetFunAlmanac = class(TForm)
    TabControlAlmanac: TTabControl;
    tabPlanets: TTabItem;
    tabStars: TTabItem;
    tabDiagrama: TTabItem;
    tabAbout: TTabItem;
    GestureManager1: TGestureManager;
    pnlTimePos: TPanel;
    Memo1: TMemo;
    Label10: TLabel;
    edJD: TEdit;
    btnJD2UTC: TButton;
    btnUTC2JD: TButton;
    Label1: TLabel;
    edDate: TDateEdit;
    edTime: TTimeEdit;
    btnUTCtoTDB: TButton;
    btnNow: TButton;
    Label8: TLabel;
    Label9: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edLon: TEdit;
    Label5: TLabel;
    Label4: TLabel;
    edLat: TEdit;
    Label2: TLabel;
    btnGPS_position: TButton;
    btnCalcPlanet: TButton;
    comboPlanets: TComboBox;
    Label11: TLabel;
    labPlanetsHint: TLabel;
    btnCalcMoon: TButton;
    btnCalcSun: TButton;
    Label3: TLabel;
    comboStars: TComboBox;
    btnCalcStar: TButton;
    btnNavigatorAlmanac: TButton;
    btnH150Almanac: TButton;
    Label12: TLabel;
    Label13: TLabel;
    pnlSolarSystemDiagram: TPanel;
    pnlTopAnimation: TPanel;
    labAnimationSpeed: TLabel;
    labScale: TLabel;
    labTime: TLabel;
    tbAnimationSpeed: TTrackBar;
    tbScale: TTrackBar;
    cbAnimatePlanets: TSwitch;
    Label14: TLabel;
    pbChart: TPaintBox;
    TimerAnimatePlanets: TTimer;
    btnCalcSolarSystem: TButton;
    Label15: TLabel;
    LocationSensor1: TLocationSensor;
    imgPlanetFunBanner: TImage;
    Label16: TLabel;
    labAlmanacVersion: TLabel;
    Label17: TLabel;
    btnCloseAlmanacForm: TButton;
    labAlmanacTitle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure StartVSOP2013DataFileLoad;

    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;  var Handled: Boolean);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
    procedure btnGPS_positionClick(Sender: TObject);
    procedure btnJD2UTCClick(Sender: TObject);
    procedure btnUTC2JDClick(Sender: TObject);
    procedure btnUTCtoTDBClick(Sender: TObject);
    procedure btnNowClick(Sender: TObject);
    procedure btnCalcPlanetClick(Sender: TObject);
    procedure btnCalcMoonClick(Sender: TObject);
    procedure btnCalcSunClick(Sender: TObject);
    procedure btnCalcStarClick(Sender: TObject);
    procedure btnNavigatorAlmanacClick(Sender: TObject);
    procedure btnH150AlmanacClick(Sender: TObject);
    procedure Memo1ApplyStyleLookup(Sender: TObject);
    procedure pbChartPaint(Sender: TObject; Canvas: TCanvas);
    procedure pbChartResize(Sender: TObject);
    procedure TimerAnimatePlanetsTimer(Sender: TObject);
    procedure cbAnimatePlanetsSwitch(Sender: TObject);
    procedure TabControlAlmanacChange(Sender: TObject);
    procedure btnCalcSolarSystemClick(Sender: TObject);
    procedure tbScaleChange(Sender: TObject);
    procedure tbAnimationSpeedChange(Sender: TObject);
    procedure OnTimeEditChange(Sender: TObject);
    procedure btnCloseAlmanacFormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fBitmap:TBitmap;             // solar system chart bitmap
    fPosPlanets:Array[1..NUM_PLANETS] of TVector3D_D;
    fanimJDE:double;             // animation time

    fInsideTimeEditChange:boolean;

    procedure populate_comboPlanets;
    procedure populate_comboStars;
    procedure SetSolarSystemTime;
    procedure LoadCatalogs;
    procedure FileDownloadTerminated(Sender: TObject);
    procedure LogMsg(const M:String);
  public
    function getCelObjByName(const aName:String):TCelObjBase;
  end;

var
  FrmPlanetFunAlmanac: TFrmPlanetFunAlmanac;

implementation     // - - - - - - - - - - - - - - - - - - - -

uses

  VSOP87.Planet,                   // VSOP87 planets
  VSOP87.SunData,                 // Sun obj
  Ah.Moon,                       // Moon obj
  VSOP2013.Planet,              // VSOP2013 planets
  Om.Trigonometry,             // trigonometric utils
  Om.AstronomicalAlgorithms,  // UTCtoTDB()
  Om.SphericalTriangle,         // Spherical triangle solution to obtain Altitude and Az
  fPlanetFun;                     // PlanetFun main form ( bad dependence )


{$R *.fmx}

// format string w/ fixed lenght by padding spaces
function FormatStrWithSpaces(const S:String; aSize:integer):String;
var L,i:Integer;
begin
  Result := S;
  L := aSize-Length(S);
  if L>0 then for i := 1 to L do Result := Result+' ';   //pad spaces to fixed size
end;

{ TFrmPlanetFunAlmanac }

procedure TFrmPlanetFunAlmanac.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FormPlanetFun)  then
    begin
      FormPlanetFun.Visible := true;  //make sure main form is visible
      Hide;                          //hide this
    end;
end;

procedure TFrmPlanetFunAlmanac.FormCreate(Sender: TObject);
begin
  fInsideTimeEditChange := false;

  // load celestial objects databases

  StartVSOP2013DataFileLoad;  // load vsop file --> create planet catalog

  fBitmap := TBitmap.Create;  // solar system diagram BMP

  { This defines the default active tab at runtime }
  TabControlAlmanac.ActiveTab := tabPlanets;

  {$IFDEF Android}
  Memo1.TextSettings.Font.Family := 'monospace';  // The Windows default 'Courier New' doesnt work on mobile
  {$ENDIF Android}
  // for iOS and Windows the default 'Courier New' works
end;

procedure TFrmPlanetFunAlmanac.StartVSOP2013DataFileLoad;
var aVSOPbinFile:String;
const sVSOP2013file='VSOP2013.p2000.bin';  // custom VSOP2013 data file in binary format) remember to deploy it
begin
  {$IFDEF Android}  aVSOPbinFile := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+sVSOP2013file; {$ENDIF Android}
  {$IFDEF iOS}      aVSOPbinFile := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+sVSOP2013file; {$ENDIF iOS}
  {$IFDEF MsWindows}
    aVSOPbinFile := '\dpr4\vsop2013\'+sVSOP2013file;   //VSOP2013.p2000.bin
  {$ENDIF MsWindows}

  VSOP_File := T_VSOP2013_File.Create;
  VSOP_File.OnLoadTerminate := FileDownloadTerminated;
  VSOP_File.Threaded_ReadBinaryFile( aVSOPbinFile );      // start data file load on a different thread
  // aVSOPbinFile is fairly large (130MB) so don't lock the app while loading it
end;

// call this only after VSOP file loaded terminated
procedure TFrmPlanetFunAlmanac.LoadCatalogs;
begin
  // planet theories
  CreatePlanetsVSOP87;      // create planets array (VSOP87.Planet.pas)
  if not CreatePlanetsVSOP2013 then ;   // open vsop2013 data file
  // TODO: report data file error...

  // Star catalogs
  CreateAstrosHipparcos150;
  CreateNavStars;
  // SUn and Moon
  CreateSun;         // create Sun and Aries
  CreateMoon;       // Create Moon
end;

// vsop2013 binary data file load terminated. We can load the catalaogs
procedure TFrmPlanetFunAlmanac.FileDownloadTerminated( Sender:TObject );
begin
  LoadCatalogs;  // load all Almanac catalogs

  btnNowClick(nil);      // click now to initialize time edits

  populate_comboPlanets; // populate combo boxes ( stars and planets )
  populate_comboStars;

  if Assigned( FormPlanetFun ) then        // propagate message to PlanetFun main form
    FormPlanetFun.FileLoadTerminate(nil);  // if any
end;

function TFrmPlanetFunAlmanac.getCelObjByName(const aName:String):TCelObjBase;
begin
  Result := FindAstroH150ByName(aName);         // StarData.pas
  if not Assigned(Result) then
    Result := FindPlanetVSOP2013ByName(aName);  // VSOP2013.Planet.pas
  if not Assigned(Result) then
    begin
      if (aName='Moon')     then Result := Moon        // Ah.Moon.pas
      else if (aName='Sun') then Result := Sun;        // VSOP87.SunData.pas
    end;
end;

const
  PLANET_COLORS:Array[1..NUM_PLANETS] of TAlphaColor=
    ( TAlphaColorRec.Red,          // 1 Mercury
      TAlphaColorRec.Lightsalmon,  // 2 Venus
      TAlphaColorRec.Aqua,         // 3 Earth
      TAlphaColorRec.Red,          // 4 Mars
      TAlphaColorRec.White,        // 5 Jupiter
      TAlphaColorRec.Green,        // 6 Saturn
      TAlphaColorRec.Aliceblue,    // 7 Uranus
      TAlphaColorRec.Azure,        // 8 Neptune
      TAlphaColorRec.Bisque        // 9 Pluto
    );

  PLANET_RADIUS:Array[1..NUM_PLANETS] of integer=  // in pixels
    ( 2,     // 1 Mercury
      3,     // 2 Venus
      3,     // 3 Earth
      3,     // 4 Mars
      5,     // 5 Jupiter
      5,     // 6 Saturn
      4,     // 7 Uranus
      4,     // 8 Neptune
      1      // 9 Pluto
    );

procedure TFrmPlanetFunAlmanac.pbChartPaint(Sender: TObject; Canvas: TCanvas);
var ip:integer; aPos:TVector3D_D; aScP:TVector; aR:TRectF; C:TVector; aScale,aRadius:Double;
    Sz:TControlSize;  aPlanetName:String;  aColor:TAlphaColor;
begin
  // paint fBitmap
  Sz := pbChart.Size;

  // C := Vector(300,300); //center of chart = Sun pos (heliocantric coordinates)
  C := Vector(Sz.Width/2,Sz.Height/2); //center of chart = Sun pos (heliocantric coordinates)

  if (fBitmap.Width=0) then //first paint
    begin
      fBitmap.SetSize(Trunc(Sz.Width),Trunc(Sz.Height));
      fBitmap.Canvas.BeginScene;
      fBitmap.Canvas.Clear( TAlphacolorRec.Black );  //reset
    end
    else begin
      if (fBitmap.Width<>Sz.Width) or (fBitmap.Height<>Sz.Height) then
        begin
          fBitmap.SetSize(Trunc(Sz.Width),Trunc(Sz.Height));   //resize cleans shadows ?
          fBitmap.Clear( TAlphacolorRec.Black );               // clear shadows
        end;

      fBitmap.Canvas.BeginScene;

      aR := RectF(0,0,Sz.Width,Sz.Height);
      fBitmap.Canvas.Fill.Color := $08000000;               // draw transparent dark rect to darken the past gradually
      fBitmap.Canvas.FillRect(aR,0,0,AllCorners,1.0);       // so that old positions fade out gradually ( ghosts )
     end;

  //draw Sun at the center
  aR := RectF(C.x-6, C.y-6, C.x+6, C.y+6);
  // fBitmap.Canvas.Stroke.Kind  := TBrushKind.Solid;
  fBitmap.Canvas.Fill.Kind   := TBrushKind.Solid;
  fBitmap.Canvas.Fill.Color  := TAlphaColorRec.Yellow;
  fBitmap.Canvas.Stroke.Kind := TBrushKind.Solid;

  fBitmap.Canvas.FillEllipse(aR, 1.0);

  aScale := tbScale.Value;  //=pix/au

  for ip := 1 to NUM_PLANETS do
     begin
       aPos := fPosPlanets[ip];
       //convert to screen coordinates
       aScP := Vector( C.x+aPos.x*aScale,C.Y+aPos.y*aScale);  //convert au-->pix

       aRadius := PLANET_RADIUS[ip]/2;

       aR := RectF(aScP.X-aRadius, aScP.Y-aRadius, aScP.X+aRadius, aScP.Y+aRadius);
       fBitmap.Canvas.Fill.Color := PLANET_COLORS[ip];
       fBitmap.Canvas.FillEllipse(aR, 1.0);

       if (ip=6) then //saturn rings ;)
         begin
           fBitmap.Canvas.Stroke.Color := PLANET_COLORS[ip];
           fBitmap.Canvas.DrawLine( PointF(aScP.X-8,aScP.Y), PointF(aScP.X+8,aScP.Y), 1.0);;
         end;
     end;

   fBitmap.Canvas.EndScene;

   //copy bitmap to paintbox canvas
   aR := RectF(0,0,Sz.Width,Sz.Height);
   Canvas.DrawBitmap(fBitmap,aR,aR,1.0);

   //   write planet names direct on paintbox canvas.. so text leaves no shadow
   Canvas.BeginScene;
   for ip := 1 to NUM_PLANETS do
     begin
       aPos := fPosPlanets[ip];
       //convert to screen coordinates
       aScP := Vector( C.x+aPos.x*aScale,C.Y+aPos.y*aScale);  //convert au-->pix
       aR := RectF(aScP.X+3, aScP.Y+3, aScP.X+120, aScP.Y+20);
       aColor := PLANET_COLORS[ip];
       aPlanetName := PLANET_DATA[ip].Name;
       Canvas.Font.Size  := 9;
       Canvas.Fill.Color := aColor;    // paint a small planet name w/ opacity = 0.7
       Canvas.FillText(aR, aPlanetName,{WordWrap=}False, 0.7, [],TTextAlign.Leading,TTextAlign.Center);
     end;
   Canvas.EndScene;

end;

procedure TFrmPlanetFunAlmanac.LogMsg(const M:String);
begin
  Memo1.Lines.Add(M);  // show status in app Memo
end;

procedure TFrmPlanetFunAlmanac.pbChartResize(Sender: TObject);
begin
  if Assigned(fBitmap) then
    fBitmap.Clear(TAlphacolors.Black);
end;

procedure TFrmPlanetFunAlmanac.populate_comboPlanets;
var i:integer;
begin

  // for i := 1 to NumPlanetsVSOP07 do                 // vsop87 planets
  //   comboPlanets.Items.Add(Planetas[i].Name);

  for i:=1 to NUM_PLANETS do        // Use planet list from vsop2013 ( larger
    comboPlanets.Items.Add( PLANET_NAMES[i] );

  comboPlanets.ItemIndex := 0;  //select 1st planet
end;

procedure TFrmPlanetFunAlmanac.populate_comboStars;
var i:integer; aName:String; aSL:TStringList;
begin
  aSL := TStringList.Create;
  // populate combo with navigation stars ( those with common names )
  for i := 1 to NumH150stars do
    begin
      aName := StarsH150[i].Name;
      if (aName<>'') then  aSL.Add(aName);
    end;

  aSL.Sort;  // sort list by name

  comboStars.Items.Assign(aSL);  //set combobox items

  aSL.Free;

  comboStars.ItemIndex:=0;  //select 1st star (Achernar)
end;

procedure TFrmPlanetFunAlmanac.TabControlAlmanacChange(Sender: TObject);
var bMemoVisible:boolean;
begin
  bMemoVisible := (TabControlAlmanac.ActiveTab=tabPlanets) or (TabControlAlmanac.ActiveTab=tabStars);
  Memo1.Visible := bMemoVisible;

  pnlSolarSystemDiagram.Visible := (TabControlAlmanac.ActiveTab=tabDiagrama);
end;

procedure TFrmPlanetFunAlmanac.tbAnimationSpeedChange(Sender: TObject);
begin
  labAnimationSpeed.Text := Format('%4.0f',[tbAnimationSpeed.Value])+' days/tick (t=200ms)';
end;

procedure TFrmPlanetFunAlmanac.tbScaleChange(Sender: TObject);
begin
  labScale.Text := IntToStr( Trunc(tbScale.Value) )+' pix/au';
  if (fBitmap.Width<>0) then
    begin
      fBitmap.Canvas.BeginScene;
      fBitmap.Canvas.Clear( TAlphacolorRec.Black );
      fBitmap.Canvas.EndScene;
    end;

end;

procedure TFrmPlanetFunAlmanac.SetSolarSystemTime;  // time in fanimJDE
var ip:integer; aPosition,aSpeed:TVector3D_D; Year:Double; D:TDatetime;
begin
  if not ( Assigned(VSOP_File) and VSOP_File.fLoaded) then exit;  //

  // Year := (fanimJDE-jd2000)/365.2422+2000.0;  // more or less :)
  // labTime.Text := Format('%6.2f',[Year]);
  D := JulianToGregorianDate( fanimJDE );
  labTime.Text := FormatDateTime('yyyy mmm dd',D);

  for ip := 1 to NUM_PLANETS do  //position planets in next date
    begin
      try
        if VSOP_File.calculate_coordinates( {ip:}ip , {jde:}fanimJDE, {out:} aPosition, aSpeed) then
          fPosPlanets[ip] := aPosition;
      except
        TimerAnimatePlanets.Enabled := false;
        raise Exception.Create('error in calculation: file range exceded');
      end;
    end;
end;

procedure TFrmPlanetFunAlmanac.TimerAnimatePlanetsTimer(Sender: TObject);
begin
  if (TabControlAlmanac.ActiveTab = tabDiagrama) then   //only animate when looking
    begin
        if Assigned(VSOP_File) and VSOP_File.fLoaded then //upd
          begin

            fanimJDE := fanimJDE + tbAnimationSpeed.Value;  // + advance animation time
            SetSolarSystemTime;  // time in fanimJDE

            pbChart.Repaint;
          end;
    end;
end;

procedure TFrmPlanetFunAlmanac.btnCalcMoonClick(Sender: TObject);
var aGMT:TDAtetime;
    aLat,aLon,aHcalc,aLHA,aDelta,aAz,Dummy:Double; IsVisible:boolean;
begin
  try
    aLat  := StrToFloat(edLat.Text);
    aLon  := StrToFloat(edLon.Text);
  except
    aLat := NaN;
    aLon := NaN;
  end;

  Memo1.Lines.Clear;
  aGMT := edDate.Date + edTime.Time;     // get GMT time

  Moon.GMT := aGMT;                   // calc coordinates
  Moon.GetObjectData( {out:} Memo1.Lines );  // show data on Memo
  Memo1.Lines.Add('');

  if not IsNaN(aLat) then  // valid earth position available
    begin
      calcPositionTriangle({in:}Moon.fDecl,Moon.fGHA,aLat,aLon,{Aic:} 0, {out:} aHcalc,aLHA,aDelta,aAz,IsVisible);
      if IsVisible then
        begin
          Memo1.Lines.Add('On position '+R2GMD(aLat,Dummy,'NS')+'  '+R2GMD(aLon,Dummy,'WE') );
          Memo1.Lines.Add('LHA= '+ floatToGMSD(aLHA) );
          Memo1.Lines.Add('Hc= '+R2GMD(aHcalc,Dummy,' -')+'  (calc Altitude) ');
          Memo1.Lines.Add('Az= '+Format('%6.2f',[aAz])+'°  (Azimuth)');
        end
        else begin
          Memo1.Lines.Add('Moon not visible from position');
        end;
      Moon.GetRiseSetTrans(aGMT,aLat,aLon,{out:} Memo1.Lines);
    end;

end;

procedure TFrmPlanetFunAlmanac.btnCalcPlanetClick(Sender: TObject);
var aPlanet87:TPlanetVSOP87;
    aPlanet13:TPlanetVSOP2013;
    aName:String; aGMT:TDAtetime;
    aIndex,aIndex87:integer;
    aLat,aLon,aHcalc,aLHA,aDelta,aAz,Dummy:Double; IsVisible:boolean;
begin
  // aName := Trim( comboPlanets.Selected.Text );

  try
    aLat  := StrToFloat(edLat.Text);
    aLon  := StrToFloat(edLon.Text);
  except
    aLat := NaN;
    aLon := NaN;
  end;

  aIndex := comboPlanets.ItemIndex+1;  // aIndex uses vsop2013 planet indexes

  Memo1.Lines.Clear;

  // aPlanet87 := FindPlanetVSOP87ByName(aName);  //search planet using name  : failed because interface is automatically translated ( planet names )

  aIndex87  := TranslateIndexVSOP2013to87( aIndex );

  aPlanet87 := FindPlanetVSOP87ByIndex(aIndex87);

  if Assigned(aPlanet87) then
    begin
      aGMT := edDate.Date + edTime.Time;        // get GMT time
      aPlanet87.GMT := aGMT;                   // calc coordinates
      Memo1.Lines.Add('vsop87');
      aPlanet87.GetObjectData( Memo1.Lines );  // show data on Memo
      Memo1.Lines.Add('');

      if not IsNaN(aLat) then  // valid earth position available
        begin
          calcPositionTriangle({in:}aPlanet87.fDecl,aPlanet87.fGHA,aLat,aLon,{Aic:} 0, {out:} aHcalc,aLHA,aDelta,aAz,IsVisible);
          if IsVisible then
            begin
              Memo1.Lines.Add('On position '+R2GMD(aLat,Dummy,'NS')+'  '+R2GMD(aLon,Dummy,'WE') );
              Memo1.Lines.Add('LHA= '+ floatToGMSD(aLHA) );
              Memo1.Lines.Add('Hc= '+R2GMD(aHcalc,Dummy,' -')+'  (calc Altitude) ');
              Memo1.Lines.Add('Az= '+Format('%6.2f',[aAz])+'°  (Azimuth)');
            end
            else begin
              Memo1.Lines.Add('planet not visible from position');
            end;
          //aPlanet87.GetRiseSetTrans(aGMT,aLat,aLon, Memo1.Lines);
        end;
      Memo1.Lines.Add('-----------');
    end
    else Memo1.Lines.Add(aName+'vsop87 planet not found');

  aPlanet13 := FindPlanetVSOP2013ByIndex(aIndex);  // search planet using index ( name may be translated )
  if Assigned(aPlanet13) then
    begin
      aGMT := edDate.Date + edTime.Time;     // get GMT time
      aPlanet13.GMT := aGMT;                   // calc coordinates
      Memo1.Lines.Add('vsop2013');
      aPlanet13.GetObjectData( Memo1.Lines );  // show data on Memo
      Memo1.Lines.Add('');

      if not IsNaN(aLat) then  // valid earth position available
        begin
          calcPositionTriangle({in:}aPlanet13.fDecl,aPlanet13.fGHA,aLat,aLon,{Aic:} 0, {out:} aHcalc,aLHA,aDelta,aAz,IsVisible);
          if IsVisible then
            begin
              Memo1.Lines.Add('On position '+R2GMD(aLat,Dummy,'NS')+'  '+R2GMD(aLon,Dummy,'WE') );
              Memo1.Lines.Add('LHA= '+ floatToGMSD(aLHA) );
              Memo1.Lines.Add('Hc= '+R2GMD(aHcalc,Dummy,' -')+'  (calc Altitude) ');
              Memo1.Lines.Add('Az= '+Format('%6.2f',[aAz])+'°  (Azimuth)');
            end
            else begin
              Memo1.Lines.Add('planet not visible from position');
            end;
          //aPlanet13.GetRiseSetTrans(aGMT,aLat,aLon, Memo1.Lines);
        end;
    end
    else Memo1.Lines.Add(aName+'vsop2013 planet not found');


end;

procedure TFrmPlanetFunAlmanac.btnCalcSolarSystemClick(Sender: TObject);
var D:TDatetime;
begin
  D         := edDate.Date;
  fanimJDE  := DatetimeToJD( D );
  SetSolarSystemTime;  // time in fanimJDE
  pbChart.Repaint;
end;

procedure TFrmPlanetFunAlmanac.btnCalcStarClick(Sender: TObject);
var aStar:TStarH150; aNavStar:TNavStar; aName:String; aGMT:TDAtetime;
    aLat,aLon,aHcalc,aLHA,aDelta,aAz,Dummy:Double; IsVisible:boolean;

begin
  aName := Trim(comboStars.Selected.Text);

  try
    aLat  := StrToFloat(edLat.Text);
    aLon  := StrToFloat(edLon.Text);
  except
    aLat := NaN;
    aLon := NaN;
  end;

  Memo1.Lines.Clear;

  aStar := FindAstroH150ByName(aName);
  if Assigned(aStar) then
    begin
      aGMT := edDate.Date + edTime.Time;     // add time zone 3 to time ( Sao Paulo time)
      aStar.GMT := aGMT;                     // this triggers coordinates calculation
      Memo1.Lines.Add('H150 star catalog ---------------');
      aStar.GetObjectData( Memo1.Lines );    // show data
      Memo1.Lines.Add('');

      if not IsNaN(aLat) then  // valid earth position available
        begin
          calcPositionTriangle({in:}aStar.fDecl,aStar.fGHA,aLat,aLon,{Aic:} 0, {out:} aHcalc,aLHA,aDelta,aAz,IsVisible);

          if IsVisible then
            begin
              Memo1.Lines.Add('On position '+R2GMD(aLat,Dummy,'NS')+'  '+R2GMD(aLon,Dummy,'WE') );
              Memo1.Lines.Add('LHA= '+ floatToGMSD(aLHA) );
              Memo1.Lines.Add('Hc= '+R2GMD(aHcalc,Dummy,' -')+'  (calc Altitude)' );
              Memo1.Lines.Add('Az= '+Format('%6.2f',[aAz])+'°  (Azimuth)');
            end
            else begin
              Memo1.Lines.Add(' not visible from position');
            end;
        end;
    end
    else Memo1.Lines.Add(aName+' not found');

  aNavStar := FindNavStar(aName);

  if Assigned(aNavStar) then
    begin
      aGMT := edDate.Date + edTime.Time;     // add time zone 3 to time ( Sao Paulo time)
      aNavStar.GMT := aGMT;                     // this triggers coordinates calculation
      Memo1.Lines.Add('Navigation Star Cat ------------');
      aNavStar.GetObjectData( Memo1.Lines );    // show data

      if not IsNaN(aLat) then  // valid earth position available
        begin
          calcPositionTriangle({in:}aNavStar.fDecl,aNavStar.fGHA,aLat,aLon,{Aic:} 0, {out:} aHcalc,aLHA,aDelta,aAz,IsVisible);

          if IsVisible then
            begin
              Memo1.Lines.Add('On position '+R2GMD(aLat,Dummy,'NS')+'  '+R2GMD(aLon,Dummy,'WE') );
              Memo1.Lines.Add('LHA= '+ floatToGMSD(aLHA) );
              Memo1.Lines.Add('Hc= '+R2GMD(aHcalc,Dummy,' -')+'  (calc Altitude)' );
              Memo1.Lines.Add('Az= '+Format('%6.2f',[aAz])+'°  (Azimuth)');
            end
            else begin
              Memo1.Lines.Add(' not visible from position');
            end;
        end;
    end
    else Memo1.Lines.Add(aName+' not found');

end;

procedure TFrmPlanetFunAlmanac.btnCalcSunClick(Sender: TObject);
var aGMT:TDAtetime;
    aLat,aLon,aHcalc,aLHA,aDelta,aAz,Dummy:Double; IsVisible:boolean;
    aYear,aMonth,aDay:word;
    morningTwilight,eveningTwilight :Double;
begin
  try
    aLat  := StrToFloat(edLat.Text);
    aLon  := StrToFloat(edLon.Text);
  except
    aLat := NaN;
    aLon := NaN;
  end;

  Memo1.Lines.Clear;
  aGMT := edDate.Date + edTime.Time;     // get GMT time

  Sun.GMT := aGMT;                   // calc coordinates
  Sun.GetObjectData( Memo1.Lines );  // show data on Memo
  Memo1.Lines.Add('');

  if not IsNaN(aLat) then  // valid earth position available
    begin
      calcPositionTriangle({in:}Sun.fDecl,Sun.fGHA,aLat,aLon,{Aic:} 0, {out:} aHcalc,aLHA,aDelta,aAz,IsVisible);
      if IsVisible then
        begin
          Memo1.Lines.Add('On position '+R2GMD(aLat,Dummy,'NS')+'  '+R2GMD(aLon,Dummy,'WE') );
          Memo1.Lines.Add('LHA= '+ floatToGMSD(aLHA) );
          Memo1.Lines.Add('Hc= '+R2GMD(aHcalc,Dummy,' -')+'  (calc Altitude) ');
          Memo1.Lines.Add('Az= '+Format('%6.2f',[aAz])+'°  (Azimuth)');
        end
        else begin
          Memo1.Lines.Add('Sun not visible from position');
        end;

      Sun.GetRiseSetTrans(aGMT,aLat,aLon, Memo1.Lines);
      DecodeDate(aGMT, aYear,aMonth,aDay);
      if Sun.calcTwilights(aDay,aMonth,aYear,aLat,aLon, {out:} morningTwilight,eveningTwilight) then
        begin
          Memo1.Lines.Add('');
          Memo1.Lines.Add('Civil Twilights (Sun altitude= -6°)');
          showAdjustedTime(' morning twilight: ',morningTwilight,2,Memo1.Lines);
          showAdjustedTime(' evening twilight: ',eveningTwilight,2,Memo1.Lines);
        end;
    end;
end;


procedure TFrmPlanetFunAlmanac.btnCloseAlmanacFormClick(Sender: TObject);
begin
  if Assigned(FormPlanetFun) then   // bring FormPlanetFun to the top
    FormPlanetFun.Show;             // hide this
  Hide;
end;

procedure TFrmPlanetFunAlmanac.btnGPS_positionClick(Sender: TObject);
const PermissionAccessFineLocation = 'android.permission.ACCESS_FINE_LOCATION';
begin
  {$IFDEF ANDROID}
  PermissionsService.RequestPermissions([PermissionAccessFineLocation],
    procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
        LocationSensor1.Active := True   { activate or deactivate the location sensor }
      else begin
        TDialogService.ShowMessage('Location permission not granted');
      end;
    end);
  {$ELSE}
  LocationSensor1.Active := true;  // start
  {$ENDIF}
end;

procedure TFrmPlanetFunAlmanac.btnH150AlmanacClick(Sender: TObject);
var i:integer; aStar:TStarH150; aGMT:TDAtetime;
   aSL:TStringList; aDummy,aSHA:Double;
begin
  aGMT := edDate.Date + edTime.Time;     // add time zone 3 to time ( Sao Paulo time)

  aSL := TStringList.Create;

  for i:=1 to NumH150stars do
    begin
      aStar := StarsH150[i];
      if (aStar.Name<>'') then
        begin
          aStar.GMT := aGMT;             // calc coordinates
          aSHA := 360.0-aStar.fRA;
          aSL.Add( FormatStrWithSpaces(aStar.Name,11)                 +' '+
               FormatStrWithSpaces(R2GMD(aSHA,aDummy,' -'),10)         +' '+
               FormatStrWithSpaces(R2GMD(aStar.fDecl,aDummy,'NS'),10)  );
        end;
    end;

  aSL.Sort;  //alphabetic stars

  aSL.Insert(0,'H150 star Almanac for ');
  aSL.Insert(1,FormatDateTime('dd/mmm/yyyy hh:nn:ss', aGMT) +' UT' );  //GMT = Universal Time
  aSL.Insert(2,'              SHA       Decl');      //table header

  Memo1.Lines.Assign(aSL);
  aSL.Free;
end;

procedure TFrmPlanetFunAlmanac.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  edLat.Text := Format('%2.6f', [ NewLocation.Latitude  ] );
  edLon.Text := Format('%2.6f', [-NewLocation.Longitude ] );

  LocationSensor1.Active := false;  // only once per click
end;

// from http://www.programering.com/a/MDO5gzNwATU.html

procedure TFrmPlanetFunAlmanac.Memo1ApplyStyleLookup(Sender: TObject);
var Obj: TFmxObject; Rectangle1: TRectangle;
begin
     Obj := Memo1.FindStyleResource('background');
     if Obj <> nil then
     begin
          TControl(Obj).Margins   := TBounds.Create(TRectF.Create(-2,-2,+2,+2));
          Rectangle1              := TRectangle.Create(Obj);
          Obj.AddObject(Rectangle1);
          Rectangle1.Align        := TAlignLayout.Client;
          Rectangle1.Fill.Color   := TAlphaColors.Black;   //black memo w/ green text
          Rectangle1.Stroke.Color := TAlphaColors.Null;
          Rectangle1.HitTest      := False;
          Rectangle1.YRadius      := 10;
          Rectangle1.XRadius      := 10;
          Rectangle1.SendToBack;
     end;
end;

procedure TFrmPlanetFunAlmanac.OnTimeEditChange(Sender: TObject);
var aJD:Double; aUT:TDatetime;
begin
  // fInsideTimeEditChange avoids on change reentring
  if fInsideTimeEditChange then exit;

  fInsideTimeEditChange:=TRUE;
  try
    if (Sender=edJD) then
      begin
        try
          aJD := StrToFloat( edJD.Text );
        except
          exit;
        end;
        aUT := JDtoDatetime( aJD );
        edDate.Date := Trunc(aUT);
        edTime.Time := Frac(aUT);
      end
    else if (Sender=edDate) or (Sender=edTime) then
      begin
        aUT  := edDate.Date + edTime.Time;
        aJD  := DatetimeToJD(aUT);
        edJD.Text := Trim( Format('%12.5f',[aJD]) );
      end;
  finally
    fInsideTimeEditChange:=TRUE;
  end;
end;

procedure TFrmPlanetFunAlmanac.btnJD2UTCClick(Sender: TObject);
var aJD:Double; aUT:TDatetime;
begin
  aJD := StrToFloat( edJD.Text );
  aUT := JDtoDatetime( aJD );

  edDate.Date := Trunc(aUT);
  edTime.Time := Frac(aUT);
end;

procedure TFrmPlanetFunAlmanac.btnNavigatorAlmanacClick(Sender: TObject);
var i:integer; aStar:TNavStar; aGMT:TDAtetime;
   aSL:TStringList; aDummy,aSHA:Double;
begin
  aGMT := edDate.Date + edTime.Time;     // add time zone 3 to time ( Sao Paulo time)

  aSL := TStringList.Create;

  for i:=1 to NumNavStars do
    begin
      aStar := StarsNav[i];
      if (aStar.Name<>'') then
        begin
          aStar.GMT := aGMT;             // calc coordinates
          aSHA := 360.0-aStar.fRA;
          aSL.Add( FormatStrWithSpaces(aStar.Name,11)                  +' '+
               FormatStrWithSpaces(R2GMD(aSHA,aDummy,' -'),10)         +' '+
               FormatStrWithSpaces(R2GMD(aStar.fDecl,aDummy,'NS'),10)  );
        end;
    end;

  aSL.Sort;
  aSL.Insert(0,'Navigation star Almanac for ');
  aSL.Insert(1,FormatDateTime('dd/mmm/yyyy hh:nn:ss', aGMT) +' UT' );  //GMT = Universal Time
  aSL.Insert(2,'              SHA       Decl');                 //table header
  
  Memo1.Lines.Assign(aSL);
  aSL.Free;

end;

procedure TFrmPlanetFunAlmanac.btnNowClick(Sender: TObject);
var T:TDatetime; aJD:Double;
begin
  T := TTimeZone.Local.ToUniversalTime(Now);
  edDate.Date := Trunc(T);
  edTime.Time := Frac(T);
  aJD         := DatetimeToJD(T);
  edJD.Text := Trim( Format('%12.5f',[aJD]));
end;

procedure TFrmPlanetFunAlmanac.btnUTC2JDClick(Sender: TObject);
var aJD:Double; aUT:TDatetime;
begin
  aUT := edDate.Date + edTime.Time;
  aJD         := DatetimeToJD(aUT);
  edJD.Text := Trim( Format('%12.5f',[aJD]));
end;

procedure TFrmPlanetFunAlmanac.btnUTCtoTDBClick(Sender: TObject);
var aTDB,aGMT,aDelta,aJD:Double;
begin
  aGMT   := edDate.Date + edTime.Time;     // get GMT time
  aTDB   := UTCtoTDB(aGMT);
  aDelta := (aTDB - aGMT)*24*3600;
  aJD    := DatetimeToJD(aTDB);

  Memo1.Lines.Clear;
  Memo1.Lines.Add('time='+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', aGMT) +' UT' );     // GMT = Universal Time
  Memo1.Lines.Add('TDB= '+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', aTDB) +' TDB' );    // TDB Time Baricentric Dynamic
  Memo1.Lines.Add('JD=  '+ Format('%13.6f',[aJD]) );

  Memo1.Lines.Add('Delta='+ Format('%15.7f',[adelta])+'°  (secs)');
end;

procedure TFrmPlanetFunAlmanac.cbAnimatePlanetsSwitch(Sender: TObject);
var D:TDatetime;
begin
  TimerAnimatePlanets.Enabled := cbAnimatePlanets.IsChecked;
  if TimerAnimatePlanets.Enabled then
    begin
      D         := edDate.Date;
      fanimJDE  := DatetimeToJD( D );

      if Assigned(fBitmap) then
        fBitmap.Clear(TAlphacolors.Black);   //start with clear diagram
    end;
end;

procedure TFrmPlanetFunAlmanac.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if TabControlAlmanac.ActiveTab <> TabControlAlmanac.Tabs[TabControlAlmanac.TabCount-1] then
        TabControlAlmanac.ActiveTab := TabControlAlmanac.Tabs[TabControlAlmanac.TabIndex+1];
      Handled := True;
    end;

    sgiRight:
    begin
      if TabControlAlmanac.ActiveTab <> TabControlAlmanac.Tabs[0] then
        TabControlAlmanac.ActiveTab := TabControlAlmanac.Tabs[TabControlAlmanac.TabIndex-1];
      Handled := True;
    end;
  end;
{$ENDIF}
end;


end.
