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
    procedure FormCreate(Sender: TObject);
    procedure btnCalcPlanetClick(Sender: TObject);
    procedure btnNowClick(Sender: TObject);
  private
  public
  end;

var
  FormTestVSOP87: TFormTestVSOP87;

implementation

uses
  VSOP87.Planet,               // VSOP87.xxx Units
  Om.Trigonometry,             // trigonometric utils
  Om.SphericalTriangle;        // Spherical triangle solution

{$R *.fmx}

procedure TFormTestVSOP87.btnNowClick(Sender: TObject);
var T:TDatetime;
begin
  T := TTimeZone.Local.ToUniversalTime(Now);
  edDate.Date := Trunc(T);
  edTime.Time := Frac(T);
end;

procedure TFormTestVSOP87.FormCreate(Sender: TObject);
var i:integer;
begin
  CreatePlanetsVSOP87;       // create planets array (VSOP87.Planet.pas)

  btnNowClick(nil);

  for i := 1 to NumPlanetsVSOP07 do  // populate combo
      comboPlanets.Items.Add(Planetas[i].Name);

  comboPlanets.ItemIndex:=0;  //select 1st planet
end;

procedure TFormTestVSOP87.btnCalcPlanetClick(Sender: TObject);
var aPlanet:TPlanetVSOP87; aName:String; aGMT:TDAtetime;
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
  aPlanet := FindPlanetVSOP87ByName(aName);  //search planet using name
  if Assigned(aPlanet) then
    begin
      aGMT := edDate.Date + edTime.Time;     // get GMT time
      aPlanet.GMT := aGMT;                   // calc coordinates
      aPlanet.GetObjectData( Memo1.Lines );  // show data on Memo
      Memo1.Lines.Add('');

      if not IsNaN(aLat) then  // valid earth position available
        begin
          calcPositionTriangle({in:}aPlanet.fDecl,aPlanet.fGHA,aLat,aLon,{Aic:} 0, {out:} aHcalc,aLHA,aDelta,aAz,IsVisible);
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
        end;
    end
    else Memo1.Lines.Add(aName+' not found');
end;

end.
