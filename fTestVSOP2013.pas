unit fTestVSOP2013;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math.Vectors,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Objects, FMX.Edit,

  vsop2013;

type
  TForm2 = class(TForm)
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
    Labw: TLabel;
    edJDE: TEdit;
    procedure btnLoadFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbAnimatePlanetsSwitch(Sender: TObject);
    procedure TimerAnimatePlanetsTimer(Sender: TObject);
    procedure pbChartPaint(Sender: TObject; Canvas: TCanvas);
    procedure tbScaleChange(Sender: TObject);
    procedure tbAnimationSpeedChange(Sender: TObject);
    procedure btnTestsClick(Sender: TObject);
    procedure btnCalcClick(Sender: TObject);
  private
    procedure Form2LoadPropgress(Sender: TObject; aPerc: integer);
    procedure showVectors(ip: integer; const jde: Double; const Position,
      Speed: TCoord3D);
  public
    fVSOPFile:T_VSOP2013_File;
    fBitmap:TBitmap;             // Sky chart
    fPosPlanets:Array[1..NUM_PLANETS] of TCoord3D;

    fanimJDE:double;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
var i:integer;
begin
  fVSOPFile := nil;
  for i:=1 to NUM_PLANETS do fPosPlanets[i] := Coord3D(0,0,0);

  fBitmap := TBitmap.Create;
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

  PLANET_RADIUS:Array[1..NUM_PLANETS] of integer=  //in pixels
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


procedure TForm2.pbChartPaint(Sender: TObject; Canvas: TCanvas);
var ip:integer; aPos:TCoord3D; aScP:TVector; aR:TRectF; C:TVector; aScale,aRadius:Double;
begin
  // paint fBitmap
  C := Vector(300,300); //center of chart = Sun pos (heliocantric coordinates)

  if (fBitmap.Width=0) then //first paint
    begin
      fBitmap.SetSize(600,600);
      fBitmap.Canvas.BeginScene;
      fBitmap.Canvas.Clear( TAlphacolorRec.Black );  //reset
    end
    else begin
      fBitmap.Canvas.BeginScene;
      aR := RectF(0,0,600,600);
      fBitmap.Canvas.Fill.Color := $08000000;   // draw transparent dark rect to darken the past gradually
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

       aRadius := PLANET_RADIUS[ip];

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

   aR := RectF(0,0,600,600);           //copy to pb
   Canvas.DrawBitmap(fBitmap,aR,aR,1.0);
end;

procedure TForm2.tbAnimationSpeedChange(Sender: TObject);
begin
  labAnimationSpeed.Text := Format('%4.0f',[tbAnimationSpeed.Value])+' days/tick (t=200ms)';
end;

procedure TForm2.tbScaleChange(Sender: TObject);
begin
  labScale.Text := IntToStr( Trunc(tbScale.Value) )+' pix/au';
  if (fBitmap.Width<>0) then
    begin
      fBitmap.Canvas.BeginScene;
      fBitmap.Canvas.Clear( TAlphacolorRec.Black );
      fBitmap.Canvas.EndScene;
    end;
end;

procedure TForm2.showVectors(ip:integer; const jde:Double; const Position,Speed:TCoord3D);
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


procedure TForm2.btnTestsClick(Sender: TObject);
var Position,Speed:TCoord3D; ip:integer; jde:double;
begin
  // calculation tests extracted from VSOP2013_ctl.txt

  // MERCURY     JD2405730.5  X :  0.242020971329 ua    Y : -0.352713705683 ua    Z : -0.051047411323 ua
  //                            X':  0.017592067303 ua/d  Y':  0.017315449357 ua/d  Z': -0.000208715093 ua/d
  ip  := 1;            //Mercury
  jde := 2405730.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');

  //JUPITER     JD2816124.5  X : -0.280774899591 ua    Y :  5.124152830950 ua    Z : -0.018441408820 ua
  //                         X': -0.007646343544 ua/d  Y': -0.000050903873 ua/d  Z':  0.000168106750 ua/d
  ip  := 5;            //jupiter
  jde := 2816124.5;

  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');

  // JUPITER     JD2405730.5  X : -5.392780445602 ua    Y : -0.805698954496 ua    Z :  0.124332318817 ua
  //                          X':  0.001019284060 ua/d  Y': -0.007116469431 ua/d  Z':  0.000005921462 ua/d
  ip  := 5;            //jupiter
  jde := 2405730.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');


  // EARTH-MOON  JD2268932.5  X : -0.437662445161 ua    Y :  0.880925943295 ua    Z :  0.000970542639 ua
  //                          X': -0.015692704507 ua/d  Y': -0.007712480753 ua/d  Z': -0.000010013711 ua/d
  ip  := 3;            // Earth
  jde := 2268932.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');


  // MARS        JD2542528.5  X : -1.501298042540 ua    Y :  0.720630252585 ua    Z :  0.051244837632 ua
  //                          X': -0.005550687750 ua/d  Y': -0.011409922095 ua/d  Z': -0.000106501220 ua/d
  ip  := 4;            //Mars
  jde := 2542528.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');


  // SATURN      JD2542528.5  X : -7.484629691185 ua    Y : -6.379153813306 ua    Z :  0.409052899765 ua
  //                          X':  0.003302297240 ua/d  Y': -0.004260528208 ua/d  Z': -0.000060046705 ua/d
  ip  := 6;            //Saturn
  jde := 2542528.5;
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');

end;



procedure TForm2.cbAnimatePlanetsSwitch(Sender: TObject);
begin
  TimerAnimatePlanets.Enabled := cbAnimatePlanets.IsChecked;
  if TimerAnimatePlanets.Enabled then
       fanimJDE := jd2000;    //start animation from jd2000
end;

// 200 ms = 5 ticks per second
procedure TForm2.TimerAnimatePlanetsTimer(Sender: TObject);
var ip:integer; aPosition,aSpeed:TCoord3D; Year:Double;
begin
  if Assigned(fVSOPFile) then //upd
    begin
      Year := (fanimJDE-jd2000)/365.2422+2000.0;  // more or less :)
      labTime.Text := Format('%6.2f',[Year]);
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
procedure TForm2.btnCalcClick(Sender: TObject);
var Position,Speed:TCoord3D; ip:integer; jde:double;
begin
  ip  := StrToInt(edPlanet.Text);
  jde := StrToFloat(edJDE.Text);
  if fVSOPFile.calculate_coordinates( {ip:}ip , {jde:}jde, {out:} Position, Speed) then
   showVectors(ip,jde,Position,Speed) else Memo1.Lines.Add('error');
end;

procedure TForm2.btnLoadFileClick(Sender: TObject);
var aFN:String;
begin
  fVSOPFile := T_VSOP2013_File.Create;
  fVSOPFile.OnLoadProgress := Form2LoadPropgress;
  // reads and parses long ASCII file: wait..
  // te
  aFN := Trim( edFilename.Text );    // '\dpr4\vsop2013\VSOP2013.p2000'  1500-3000. ( includes current time )
  fVSOPFile.Read_ASCII_File( aFN );  // load test file

  Memo1.Lines.Add(aFN+' Loaded');

end;

procedure TForm2.Form2LoadPropgress(Sender:TObject; aPerc:integer);
begin
  labPercent.Text := IntToStr(aPerc)+'%';
  labPercent.Repaint;
end;

end.
