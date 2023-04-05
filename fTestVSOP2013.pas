unit fTestVSOP2013;   //-----------------------------------------//
//-------------------//                                         //
// Test app and utility for VSOP2013 calculations              //
// Delphi Firemonkey app - tested on Windows                  //
//                                                           //
// programmed by oMAR                                       //
//      repository: github.com/omarreis/VSOP2013           //
//   History:                                             //
//      jul20: Om: v1.0                                  //
//------------------------------------------------------//

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math.Vectors,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Objects, FMX.Edit, FMX.Memo.Types,
  FMX.DateTimeCtrls,

  doubleVector3D,
  vsop2013,
  VSOP2013.Planet,

  Om.AstronomicalAlgorithms,
  PlanetData;       // VSOP2013

type
  TFormVSOP2013Tests = class(TForm)
    Memo1: TMemo;
    btnLoadFile: TButton;
    labPercent: TLabel;
    pbChart: TPaintBox;
    TimerAnimatePlanets: TTimer;
    cbAnimatePlanets: TSwitch;
    Label1: TLabel;
    labTime: TLabel;
    tbScale: TTrackBar;
    labScale: TLabel;
    tbAnimationSpeed: TTrackBar;
    labAnimationSpeed: TLabel;
    edFilename: TEdit;
    btnTests: TButton;
    btnCalc: TButton;
    edPlanet: TEdit;
    Label2: TLabel;
    C: TLabel;
    edJDE: TEdit;
    btnSaveBinFile: TButton;
    btnLoadBinFile: TButton;
    edDate: TDateEdit;
    Label3: TLabel;
    pnlTopAnimation: TPanel;
    pnlRight: TPanel;
    Splitter1: TSplitter;
    pnlLeft: TPanel;
    Label4: TLabel;
    lblClear: TLabel;
    procedure btnLoadFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbAnimatePlanetsSwitch(Sender: TObject);
    procedure TimerAnimatePlanetsTimer(Sender: TObject);
    procedure pbChartPaint(Sender: TObject; Canvas: TCanvas);
    procedure tbScaleChange(Sender: TObject);
    procedure tbAnimationSpeedChange(Sender: TObject);
    procedure btnTestsClick(Sender: TObject);
    procedure btnCalcClick(Sender: TObject);
    procedure btnSaveBinFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadBinFileClick(Sender: TObject);
    procedure edDateChange(Sender: TObject);
    procedure pbChartResize(Sender: TObject);
    procedure lblClearClick(Sender: TObject);
  private
    procedure Form2LoadPropgress(Sender: TObject; aPerc: integer);
    procedure showVectors(ip: integer; const jde: Double; const Position,Speed: TVector3D_D);
  public
    fVSOPFile:T_VSOP2013_File;
    fBitmap:TBitmap;                   // solar system chart bitmap
    fPosPlanets:Array[1..NUM_PLANETS] of TVector3D_D;

    fanimJDE:double;
  end;

var
  FormVSOP2013Tests: TFormVSOP2013Tests;

implementation

{$R *.fmx}

procedure TFormVSOP2013Tests.FormCreate(Sender: TObject);
var i:integer;
begin
  fVSOPFile := T_VSOP2013_File.Create;

  for i:=1 to NUM_PLANETS do fPosPlanets[i] := Vector3D_D(0,0,0);

  fBitmap := TBitmap.Create;
end;

procedure TFormVSOP2013Tests.FormDestroy(Sender: TObject);
begin
  fVSOPFile.Free;

end;

procedure TFormVSOP2013Tests.lblClearClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
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


procedure TFormVSOP2013Tests.pbChartPaint(Sender: TObject; Canvas: TCanvas);
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
      fBitmap.Canvas.Fill.Color := $08000000;          // draw transparent dark rect to darken the past gradually
      fBitmap.Canvas.FillRect(aR,0,0,AllCorners,1.0);
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

procedure TFormVSOP2013Tests.pbChartResize(Sender: TObject);
begin
  if Assigned(fBitmap) then
    fBitmap.Clear(TAlphacolors.Black);
end;

procedure TFormVSOP2013Tests.tbAnimationSpeedChange(Sender: TObject);
begin
  labAnimationSpeed.Text := Format('%4.0f',[tbAnimationSpeed.Value])+' days/tick (t=200ms)';
end;

procedure TFormVSOP2013Tests.tbScaleChange(Sender: TObject);
begin
  labScale.Text := IntToStr( Trunc(tbScale.Value) )+' pix/au';
  if (fBitmap.Width<>0) then
    begin
      fBitmap.Canvas.BeginScene;
      fBitmap.Canvas.Clear( TAlphacolorRec.Black );
      fBitmap.Canvas.EndScene;
    end;
end;

procedure TFormVSOP2013Tests.showVectors(ip:integer; const jde:Double; const Position,Speed:TVector3D_D);
begin
  Memo1.Lines.Add('');
  Memo1.Lines.Add( PLANET_NAMES[ip] );
  Memo1.Lines.Add('jde:'+Trim(Format('%8.1f',[ jde ])));
  // pos
  Memo1.Lines.Add('x: '+Trim(Format('%18.14f ua',[ Position.x ])));
  Memo1.Lines.Add('y: '+Trim(Format('%18.14f ua',[ Position.y ])));
  Memo1.Lines.Add('z: '+Trim(Format('%18.14f ua',[ Position.z ])));
  // spd
  Memo1.Lines.Add('sx: '+Trim(Format('%18.14f ua/d',[ Speed.x ])));
  Memo1.Lines.Add('sy: '+Trim(Format('%18.14f ua/d',[ Speed.y ])));
  Memo1.Lines.Add('sz: '+Trim(Format('%18.14f ua/d',[ Speed.z ])));
end;


procedure TFormVSOP2013Tests.btnTestsClick(Sender: TObject);
var Position,Speed:TVector3D_D; ip:integer; jde:double;
begin
  // calculation tests extracted from VSOP2013_ctl.txt

  // MERCURY     JD2405730.5  X :  0.242020971329 ua    Y : -0.352713705683 ua    Z : -0.051047411323 ua
  //                            X':  0.017592067303 ua/d  Y':  0.017315449357 ua/d  Z': -0.000208715093 ua/d
  ip  := 1;            //Mercury
  jde := 2405730.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');
  Memo1.Lines.Add('X :  0.242020971329 ua    Y : -0.352713705683 ua    Z : -0.051047411323 ua expected');

  //JUPITER     JD2816124.5  X : -0.280774899591 ua    Y :  5.124152830950 ua    Z : -0.018441408820 ua
  //                         X': -0.007646343544 ua/d  Y': -0.000050903873 ua/d  Z':  0.000168106750 ua/d
  ip  := 5;            //jupiter
  jde := 2816124.5;

  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');

  Memo1.Lines.Add('X : -0.280774899591 ua    Y :  5.124152830950 ua    Z : -0.018441408820 ua expected');


  // JUPITER     JD2405730.5  X : -5.392780445602 ua    Y : -0.805698954496 ua    Z :  0.124332318817 ua
  //                          X':  0.001019284060 ua/d  Y': -0.007116469431 ua/d  Z':  0.000005921462 ua/d
  ip  := 5;            //jupiter
  jde := 2405730.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');
  Memo1.Lines.Add('-5.392780445602 ua    Y : -0.805698954496 ua    Z :  0.124332318817 ua expected');


  // EARTH-MOON  JD2268932.5  X : -0.437662445161 ua    Y :  0.880925943295 ua    Z :  0.000970542639 ua
  //                          X': -0.015692704507 ua/d  Y': -0.007712480753 ua/d  Z': -0.000010013711 ua/d
  ip  := 3;            // Earth
  jde := 2268932.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');
  Memo1.Lines.Add('X : -0.437662445161 ua    Y :  0.880925943295 ua    Z :  0.000970542639 ua expected');


  // MARS        JD2542528.5  X : -1.501298042540 ua    Y :  0.720630252585 ua    Z :  0.051244837632 ua
  //                          X': -0.005550687750 ua/d  Y': -0.011409922095 ua/d  Z': -0.000106501220 ua/d
  ip  := 4;            //Mars
  jde := 2542528.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');
  Memo1.Lines.Add('X : -1.501298042540 ua    Y :  0.720630252585 ua    Z :  0.051244837632 ua expected');


  // SATURN      JD2542528.5  X : -7.484629691185 ua    Y : -6.379153813306 ua    Z :  0.409052899765 ua
  //                          X':  0.003302297240 ua/d  Y': -0.004260528208 ua/d  Z': -0.000060046705 ua/d
  ip  := 6;            //Saturn
  jde := 2542528.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');
  Memo1.Lines.Add('X : -7.484629691185 ua    Y : -6.379153813306 ua    Z :  0.409052899765 ua expected');

end;

procedure TFormVSOP2013Tests.cbAnimatePlanetsSwitch(Sender: TObject);
begin
  TimerAnimatePlanets.Enabled := cbAnimatePlanets.IsChecked;
  if TimerAnimatePlanets.Enabled then
     fanimJDE := StrToFloat(edJDE.Text);    //start animation from specified JD
end;

procedure TFormVSOP2013Tests.edDateChange(Sender: TObject);
var D:TDatetime; aJD:Double;
begin
  D     := edDate.Date;
  aJD   := DatetimeToJD( D );
  edJDE.Text := Format('%6.1f',[aJD]);
end;

// 200 ms = 5 ticks per second
procedure TFormVSOP2013Tests.TimerAnimatePlanetsTimer(Sender: TObject);
var ip:integer; aPosition,aSpeed:TVector3D_D; Year:Double; D:TDatetime;
begin
  if Assigned(fVSOPFile) then //upd
    begin
      // Year := (fanimJDE-jd2000)/365.2422+2000.0;  // more or less :)
      // labTime.Text := Format('%6.2f',[Year]);
      D := JulianToGregorianDate( fanimJDE );
      labTime.Text := FormatDateTime('yyyy mm',D);
      for ip := 1 to NUM_PLANETS do
        begin
          try
            if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}fanimJDE, {out:} aPosition, aSpeed) then
              fPosPlanets[ip] := aPosition;
          except
            TimerAnimatePlanets.Enabled := false;
            raise Exception.Create('error in calculation: file range exceded');
          end;
        end;
      fanimJDE := fanimJDE + tbAnimationSpeed.Value;  // + advance animation time

      pbChart.Repaint;
    end;

end;

// Loads vsop2013 ASCII file and performs a few calculations
procedure TFormVSOP2013Tests.btnCalcClick(Sender: TObject);
var Position,Speed:TVector3D_D; ip:integer; jde:double;
begin
  ip  := StrToInt( edPlanet.Text );
  jde := StrToFloat( edJDE.Text );

  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');

  // position chart
  fanimJDE := jde;
  TimerAnimatePlanetsTimer(nil);  // update planet chart to calc data
end;

procedure TFormVSOP2013Tests.btnLoadBinFileClick(Sender: TObject);
var aFN:String;
begin
  aFN := Trim( edFilename.Text )+'.bin';    // '\dpr4\vsop2013\VSOP2013.p2000.bno'  1500-3000. ( includes current time )
  if fVSOPFile.ReadBinaryFile(aFN) then Memo1.Lines.Add(aFN+' Loaded')
     else Memo1.Lines.Add(aFN+' write error');
end;

procedure TFormVSOP2013Tests.btnLoadFileClick(Sender: TObject);
var aFN:String;
begin
  fVSOPFile.OnLoadProgress := Form2LoadPropgress;
  // reads and parses long ASCII file: wait..
  // te
  aFN := Trim( edFilename.Text );      // '\dpr4\vsop2013\VSOP2013.p2000'  1500-3000. ( includes current time )
  fVSOPFile.Read_ASCII_File( aFN );    // load test file

  Memo1.Lines.Add(aFN+' Loaded');

end;

procedure TFormVSOP2013Tests.btnSaveBinFileClick(Sender: TObject);
var aFN:String;
begin
  aFN := Trim( edFilename.Text )+'.bin';    // '\dpr4\vsop2013\VSOP2013.p2000.bin'  1500-3000. ( includes current time )
  if fVSOPFile.WriteBinaryFile(aFN) then Memo1.Lines.Add(aFN+' saved')
     else Memo1.Lines.Add(aFN+' write error');
end;

procedure TFormVSOP2013Tests.Form2LoadPropgress(Sender:TObject; aPerc:integer);
begin
  labPercent.Text := IntToStr(aPerc)+'%';
  labPercent.Repaint;
end;

end.
