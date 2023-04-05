unit fTestVSOP87;    // test VSOP87 - planet coordinates calculation

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.DateUtils,System.Math,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.DateTimeCtrls, FMX.ListBox;

type
  TFormTestVSOP87 = class(TForm)
    btnCalcPlanet: TButton;
    Memo1: TMemo;
    edDate: TDateEdit;
    edTime: TTimeEdit;
    Label1: TLabel;
    Label2: TLabel;
    comboPlanets: TComboBox;
    Label3: TLabel;
    btnNow: TButton;
    Label4: TLabel;
    Label5: TLabel;
    edLat: TEdit;
    edLon: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    btnCalcSun: TButton;
    btnCalcMoon: TButton;
    btnUTCtoTDB: TButton;
    Label10: TLabel;
    edJD: TEdit;
    btnJD2UTC: TButton;
    btnUTC2JD: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCalcPlanetClick(Sender: TObject);
    procedure btnNowClick(Sender: TObject);
    procedure btnCalcSunClick(Sender: TObject);
    procedure btnCalcMoonClick(Sender: TObject);
    procedure btnUTCtoTDBClick(Sender: TObject);
    procedure btnJD2UTCClick(Sender: TObject);
    procedure btnUTC2JDClick(Sender: TObject);
  private
  public
  end;

var
  FormTestVSOP87: TFormTestVSOP87;

implementation    //----------------------------------------------------------

uses
  VSOP87.Planet,                   // VSOP87 planets
  VSOP87.SunData,                 // Sun
  Ah.Moon,                       // Moon
  VSOP2013.Planet,              // VSOP2013 planets
  Om.Trigonometry,             // trigonometric utils
  Om.AstronomicalAlgorithms,  // UTCtoTDB()
  Om.SphericalTriangle;      // Spherical triangle solution

{$R *.fmx}

procedure TFormTestVSOP87.FormCreate(Sender: TObject);
var i:integer;
begin
  CreatePlanetsVSOP87;   // create planets array (VSOP87.Planet.pas)
  CreatePlanetsVSOP2013('c:\dpr4\vsop2013\VSOP2013.p2000.bin');
  CreateSun;             // create Sun and Aries
  CreateMoon;            // Create Moon

  btnNowClick(nil);

  for i := 1 to NumPlanetsVSOP07 do  // populate combo
      comboPlanets.Items.Add(Planetas[i].Name);

  comboPlanets.ItemIndex:=0;  //select 1st planet
end;

procedure TFormTestVSOP87.btnCalcSunClick(Sender: TObject);
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

procedure TFormTestVSOP87.btnJD2UTCClick(Sender: TObject);
var aJD:Double; aUT:TDatetime;
begin
  aJD := StrToFloat( edJD.Text );
  aUT := JDtoDatetime( aJD );

  edDate.Date := Trunc(aUT);
  edTime.Time := Frac(aUT);
end;

procedure TFormTestVSOP87.btnNowClick(Sender: TObject);
var T:TDatetime; aJD:Double;
begin
  T := TTimeZone.Local.ToUniversalTime(Now);
  edDate.Date := Trunc(T);
  edTime.Time := Frac(T);
  aJD         := DatetimeToJD(T);
  edJD.Text := Trim( Format('%12.5f',[aJD]));
end;

procedure TFormTestVSOP87.btnUTC2JDClick(Sender: TObject);
var aJD:Double; aUT:TDatetime;
begin
  aUT := edDate.Date + edTime.Time;
  aJD         := DatetimeToJD(aUT);
  edJD.Text := Trim( Format('%12.5f',[aJD]));
end;

procedure TFormTestVSOP87.btnUTCtoTDBClick(Sender: TObject);
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

procedure TFormTestVSOP87.btnCalcMoonClick(Sender: TObject);
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

procedure TFormTestVSOP87.btnCalcPlanetClick(Sender: TObject);
var aPlanet87:TPlanetVSOP87;
    aPlanet13:TPlanetVSOP2013;
    aName:String; aGMT:TDAtetime;
    aLat,aLon,aHcalc,aLHA,aDelta,aAz,Dummy:Double; IsVisible:boolean;
begin
  aName := Trim(comboPlanets.Selected.Text);

  try
    aLat  := StrToFloat(edLat.Text);
    aLon  := StrToFloat(edLon.Text);
  except
    aLat := NaN;
    aLon := NaN;
  end;

  Memo1.Lines.Clear;

  aPlanet87 := FindPlanetVSOP87ByName(aName);  //search planet using name
  if Assigned(aPlanet87) then
    begin
      aGMT := edDate.Date + edTime.Time;     // get GMT time
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

  aPlanet13 := FindPlanetVSOP2013ByName(aName);  // search planet using name
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

end.
