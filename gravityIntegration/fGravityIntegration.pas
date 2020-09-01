unit fGravityIntegration;   // integrate Newton gravitation ( chart )

interface

uses
  System.SysUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Math.Vectors,
  System.DateUtils,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart, FMX.Layouts,
  FMX.ListBox, FMX.Edit, FMX.DateTimeCtrls,

  doubleVector3D,  // TVector3D_D
  vsop2013,
  PlanetData,      // planets physical data
  CelestialObjects;

type
  TFormGravityIntegration = class(TForm)
    btnCompareVSOP2013wNewton: TButton;
    Memo1: TMemo;
    Chart1: TChart;
    SeriesMercury: TFastLineSeries;
    SeriesVenus: TFastLineSeries;
    SeriesEarth: TFastLineSeries;
    SeriesMars: TFastLineSeries;
    SeriesJupiter: TFastLineSeries;
    SeriesSaturn: TFastLineSeries;
    SeriesUranus: TFastLineSeries;
    SeriesNeptune: TFastLineSeries;
    SeriesPluto: TFastLineSeries;
    lbPlanets: TListBox;
    Label1: TLabel;
    edNumberOfDays: TEdit;
    lab1: TLabel;
    edDT: TEdit;
    labSelectedPlanetName: TLabel;
    cbSelectedPlanetExists: TSwitch;
    Label2: TLabel;
    Label3: TLabel;
    cbLogScale: TSwitch;
    rbRadius: TRadioButton;
    rbLatitude: TRadioButton;
    rbLongitude: TRadioButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edDateIni: TDateEdit;
    labxx: TLabel;
    procedure btnCompareVSOP2013wNewtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lbPlanetsChangeCheck(Sender: TObject);
    procedure lbPlanetsClick(Sender: TObject);
    procedure cbSelectedPlanetExistsSwitch(Sender: TObject);
    procedure cbLogScaleSwitch(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    fSelectedObj: TCelestialObject;
    fFirstShow:boolean;
    procedure btnLoadFileClick(Sender: TObject);
    procedure FileLoadTerminate(Sender: TObject);
    procedure clearSeries;
  public
  end;

var
  FormGravityIntegration: TFormGravityIntegration;

implementation   //-----------------------

{$R *.fmx}

procedure TFormGravityIntegration.FormActivate(Sender: TObject);
begin
  if fFirstShow then //once
    begin
      btnLoadFileClick(nil);    // load vsop2013 ephemerides engine
      fFirstShow := false;
    end;
end;

procedure TFormGravityIntegration.FormCreate(Sender: TObject);
var i:integer;   aItem:TListboxItem;
begin
  FormatSettings.DecimalSeparator  := '.';  //allways
  FormatSettings.ThousandSeparator := ',';

  VSOP_File := nil;        // no file yet
  CreateCelestialObjectsDB;

  fSelectedObj := nil;

  for i:=0 to 9-1 do        // all checked
     lbPlanets.ListItems[i].IsChecked := true;

  fFirstShow := true;
end;

procedure TFormGravityIntegration.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbRight then       //right click edit
     Chart1.ShowInDesigner;
end;

// check in listbox = chart visible
procedure TFormGravityIntegration.lbPlanetsChangeCheck(Sender: TObject);
var ip:integer; aObj: TCelestialObject;
begin
  ip := lbPlanets.ItemIndex;
  if ip>=0 then
    begin
      aObj := CelestialObjectsDB.Items[ip+1];
      aObj.fChartVisible := lbPlanets.ListItems[ip].IsChecked;

      case ip+1 of
        1: SeriesMercury.Visible := aObj.fChartVisible;
        2: SeriesVenus  .Visible := aObj.fChartVisible;
        3: SeriesEarth  .Visible := aObj.fChartVisible;
        4: SeriesMars   .Visible := aObj.fChartVisible;
        5: SeriesJupiter.Visible := aObj.fChartVisible;
        6: SeriesSaturn .Visible := aObj.fChartVisible;
        7: SeriesUranus .Visible := aObj.fChartVisible;
        8: SeriesNeptune.Visible := aObj.fChartVisible;
        9: SeriesPluto  .Visible := aObj.fChartVisible;
      end;
    end;
end;

procedure TFormGravityIntegration.lbPlanetsClick(Sender: TObject);
var ip:integer;
begin
  ip := lbPlanets.ItemIndex;
  if (ip>=0) then
    begin
      fSelectedObj := CelestialObjectsDB.Items[ip+1];
      labSelectedPlanetName.Text       := fSelectedObj.Name;
      cbSelectedPlanetExists.IsChecked := fSelectedObj.fExists;
    end
    else begin
      labSelectedPlanetName.Text       := '';
      fSelectedObj := nil;
    end;
end;

procedure TFormGravityIntegration.btnLoadFileClick(Sender: TObject);
var aFN:String;
const sVSOP2013file      = 'VSOP2013.p2000.bin';  // custom binary format
      sVSOP2013PathWin32 = '\dpr4\vsop2013\';    // win32 data file folder
begin
  if not Assigned(VSOP_File) then
     VSOP_File := T_VSOP2013_File.Create;   // vsop file parser and position calculator

  // VSOP_File.OnLoadProgress := Form2LoadPropgress;
  // reads and parses long ASCII file: wait..

  // file 'VSOP2013.p2000'  1500-3000. ( includes current time )
  // loaded on a thread  at activation

  {$ifdef MsWindows}
  // TODO: set a folder app documents
  aFN := sVSOP2013PathWin32+sVSOP2013file;  // '\dpr4\vsop2013\VSOP2013.p2000'  1500-3000. ( includes current time )
  {$endif MsWindows}

  // for Android and iOS data file VSOP2013.p2000.bin must be deployed to docs
  {$ifdef Android}
  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+sVSOP2013file;
  {$endif Android}

  {$ifdef iOS}  //same
  aFN := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim+sVSOP2013file;
  {$endif iOS}

  if FileExists(aFN) then  // load
    begin
      VSOP_File.OnLoadTerminate := FileLoadTerminate;     // threaded data file load calls FileLoadTerminate
      VSOP_File.Threaded_ReadBinaryFile( aFN );           // load data file on a separate thread ( takes some time )
      Memo1.Lines.Add('Loading VSOP2013 bin file.  Wait..');
    end
    else Memo1.Lines.Add('VSOP2013 data file not found');
end;

procedure TFormGravityIntegration.FileLoadTerminate(Sender:TObject);
var S:String;
begin
  if VSOP_File.fLoaded then S := 'VSOP2013.p2000.bin loaded'
    else S := ' vsop file read error';
  Memo1.Lines.Add( S );  // notify finished loading. we're go for launch
end;

function NowJD:Double;     // now in Julian Date
var T:TDatetime;  YY,MM,DD:Word; H:Double;
begin
  T := TTimeZone.Local.ToUniversalTime( Now );   //apply time zone
  DecodeDate( Trunc(T), {out:}YY,MM,DD);
  H := Frac(T);
  Result := JD( YY, MM, DD, H*24);
end;


procedure TFormGravityIntegration.cbLogScaleSwitch(Sender: TObject);
begin
  Chart1.LeftAxis.Logarithmic := cbLogScale.IsChecked;
end;

procedure TFormGravityIntegration.cbSelectedPlanetExistsSwitch(
  Sender: TObject);
begin
  if Assigned(fSelectedObj) then
    fSelectedObj.fExists := cbSelectedPlanetExists.IsChecked;
end;

procedure TFormGravityIntegration.clearSeries;  // init charts
begin
  SeriesMercury.Clear;
  SeriesVenus  .Clear;
  SeriesEarth  .Clear;
  SeriesMars   .Clear;
  SeriesJupiter.Clear;
  SeriesSaturn .Clear;
  SeriesUranus .Clear;
  SeriesNeptune.Clear;
  SeriesPluto  .Clear;
end;

Function R_inRange(aR,aRmin,aRMax:double):double;
var aRangeSz:Double;
begin
  Result := aR;
  if (Result<aRmin) then
  begin
    aRangeSz := aRMax-aRMin;
    while (Result<aRmin) do Result:=Result+aRangeSz;
  end
  else if (Result>aRmax) then
  begin
    aRangeSz := aRMax-aRMin;
    while (Result>aRmax) do Result:=Result-aRangeSz;
  end;
end;

procedure TFormGravityIntegration.btnCompareVSOP2013wNewtonClick( Sender: TObject);
var JDE,JDEfin,K,R,D,DT,NDays,aDay,prevDay, vLat,vLon,vRad,aLat,aLon,aRad:Double;
    aDate:TDatetime;  i,ip,nd,whichChart:integer;   aObj,Mars,Sun:TCelestialObject; V:TVector3D_D;  S:String;

begin
  if not ( Assigned(VSOP_File) and VSOP_File.fLoaded ) then exit; //?

  clearSeries;   //clear previous charts
  // get integration params
  nd := StrToInt(edNumberOfDays.Text);         // number of days
  DT := StrToFloat(edDT.Text);                 // integration DT in days ( def = 0.1 days )

  whichChart := -1;  // -1 = none
  if rbRadius.IsChecked   then
    begin
      whichChart:=0;                   // radius comparison
      Chart1.LeftAxis.Title.Caption := '% percent of Radius';   // set left axis chart text
    end
  else if rbLatitude.IsChecked  then
    begin
      whichChart:=1;                   // latitude
      Chart1.LeftAxis.Title.Caption := 'Lat diff in arcsec';
    end
  else if rbLongitude.IsChecked then
    begin
      whichChart:=2;                   // longitude
      Chart1.LeftAxis.Title.Caption := 'Lon diff in arcsec';
    end;

  Mars := CelestialObjectsDB.getObjectByName('Mars');
  Sun  := CelestialObjectsDB.getObjectByName('Sun');
  if not Assigned(Mars) then exit; //??


  // JDE    := NowJD;                        // JDE = Now in Julian date
  // aDate  := EncodeDate( {Y:}1781, {M:}03, {D:}13 );  //Year of Uranus discovery 13/mar/1781
  aDate  := edDateIni.Date;
  JDE    := DateToJD( aDate );

  JDEfin := JDE+nd;   //4332; //687.0;     // 687 is mars year len, in Earth days
  i      := 0;
  NDays  := 0;
  aDay   := 0;        // day of previous chart point ( max of 1 pt per day in chart )
  prevDay:= 0;

  while (JDE<=JDEfin) do   // integration loop
    begin
      PositionObjectsUsingEphemeris( JDE );   // set Obj vPos,vSpd using vsop2013. This will be our "observation"
      if (i=0) then                           // init aObj.SpdMid with V1/2, for leapfrog integration
        begin
          copyVSOP2013ephemerisToCurrent;                // set init position at "T0",  using vsop2013
          setSpdMid_forLeapfrogIntegration( JDE+DT/2 )   // set spd at "T 1/2", for leapfrog integration
        end
        else Add_LeapfrogIntegrationDT( DT );  // use Newton's universal gravitation formula, move all planets a DT

      // show results for Mars
      // S := 'i: '+IntToStr(i);
      // Memo1.Lines.Add( S );

      //   Memo1.Lines.Add('xv: '+Trim(Format('%18.14f',[ Mars.vPos.x  ])));
      //   Memo1.Lines.Add('xn: '+Trim(Format('%18.14f',[ Mars.Posit.x ])));
      //
      //   Memo1.Lines.Add('');
      //   Memo1.Lines.Add('yv: '+Trim(Format('%18.14f',[ Mars.vPos.y  ])));
      //   Memo1.Lines.Add('yn: '+Trim(Format('%18.14f',[ Mars.Posit.y ])));
      //
      //   Memo1.Lines.Add('');
      //   Memo1.Lines.Add('zv: '+Trim(Format('%18.14f',[ Mars.vPos.z  ])));
      //   Memo1.Lines.Add('zn: '+Trim(Format('%18.14f',[ Mars.Posit.z ])));

      // calc difference between vsop2013 and Newton pos, in relation to planet radius

      aDay := Trunc(JDE);         // aDay is used to add at most 1 chart pt / day
      if (aDay<>prevDay) then     // max of one chart pt per day
        begin
          // R := Mars.vPos.Length;
          // S := S+' Rv: '+Trim(Format('%7.4f',[ R ]));
          // R := Mars.Posit.Length;
          // S := S+ ' Rn:  '+Trim(Format('%7.4f',[ R ]));
          // if (R<>0) then
          //   begin
          //     V := ( Mars.vPos-Mars.Posit );
          //     D := V.Length;                // dif vector len
          //     K := 100 *D/R;
          //     S := S+' dif: '+Trim( Format('%8.3f',[ K ])) +'%';
          //   end;
          // Memo1.Lines.Add( S );

          for ip := 1 to 9 do  // build planet's chart series
            begin             // y axis is difference between Newton integration and VSOP2013 in % of the planet's heliocentric radius )
              aObj := CelestialObjectsDB.Items[ip];  // 0 = Sun. Not charted ?
              if not aObj.fExists  then continue;   // planet vanished ?..
              // show/hide planet charts
              case ip of
                1: SeriesMercury.Visible := aObj.fChartVisible;
                2: SeriesVenus  .Visible := aObj.fChartVisible;
                3: SeriesEarth  .Visible := aObj.fChartVisible;
                4: SeriesMars   .Visible := aObj.fChartVisible;
                5: SeriesJupiter.Visible := aObj.fChartVisible;
                6: SeriesSaturn .Visible := aObj.fChartVisible;
                7: SeriesUranus .Visible := aObj.fChartVisible;
                8: SeriesNeptune.Visible := aObj.fChartVisible;
                9: SeriesPluto  .Visible := aObj.fChartVisible;
              end;
              // if not aObj.fChartVisible then continue;  //no chart..

              K := 0;      // K holds our comparision chart point

              case whichChart of
                0:  begin //radius comparison
                      R := aObj.vPos.Length;              // calc vsop radius
                      // S := S+ ' Rv:  '+Trim(Format('%7.4f',[ R ]));
                      if (R<>0) then  //shoud be n.e. 0 , unless aObj is the Sun itself
                        begin
                          V := ( aObj.vPos-aObj.Posit );  // vector between vsop2013 pos and calculated pos
                          D := V.Length;                  // diff vector len
                          K := 100 *D/R;                  // "error" of calculated position in relation to vsop2013
                          // S := S+' dif: '+Trim( Format('%8.3f',[ K ])) +'%';
                        end;
                    end;
                1:  begin // latitude comparison
                      aObj.Posit.getLatLonRadius(aLat,aLon,aRad);   // convert heliocentric coordinates to heliocentric lat,lon,radius
                      aObj.vPos.getLatLonRadius(vLat,vLon,vRad);
                      K := (aLat-vLat)*3600;                        //chart = lat difference ( 3600 = sec/degree )
                    end;
                2:  begin // longitude comparison
                      aObj.Posit.getLatLonRadius(aLat,aLon,aRad);
                      aObj.vPos.getLatLonRadius(vLat,vLon,vRad);
                      K := R_inRange( aLon-vLon, -180, +180)*3600;      // lon difference in arcsec
                    end;
              end;  // /case

             if (K<>0) then     // if K=0, no point for the chart
                case ip of      // add chart pt
                  1: SeriesMercury.AddXY( aDate, K, '' );
                  2: SeriesVenus  .AddXY( aDate, K, '' );
                  3: SeriesEarth  .AddXY( aDate, K, '' );
                  4: SeriesMars   .AddXY( aDate, K, '' );
                  5: SeriesJupiter.AddXY( aDate, K, '' );
                  6: SeriesSaturn .AddXY( aDate, K, '' );
                  7: SeriesUranus .AddXY( aDate, K, '' );
                  8: SeriesNeptune.AddXY( aDate, K, '' );
                  9: SeriesPluto  .AddXY( aDate, K, '' );
                end;
            end;
          prevDay := aDay;
        end;   // /if aDay<>prevDay

      JDE   := JDE+DT;      // inc integration time by DT
      NDays := NDays+DT;    // count days
      aDate := aDate+DT;    // redundant counters :(
      inc(i);
    end;
end;

end.
