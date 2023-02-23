unit fTestH150Stars;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.DateUtils, System.Math,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.DateTimeCtrls,
  FMX.ListBox;

type
  TFormStarEphemeris = class(TForm)
    btnCalcStar: TButton;
    Memo1: TMemo;
    edDate: TDateEdit;
    edTime: TTimeEdit;
    Label3: TLabel;
    btnNow: TButton;
    comboStars: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    edLat: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    edLon: TEdit;
    Label5: TLabel;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCalcStarClick(Sender: TObject);
    procedure btnNowClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormStarEphemeris: TFormStarEphemeris;

implementation

uses
  StarData,                 // Hipparcos 150 stars
  Om.Trigonometry,         // trigonometric utils
  Om.SphericalTriangle;   // Spherical triangle solution


{$R *.fmx}


procedure TFormStarEphemeris.FormCreate(Sender: TObject);
var i:integer; aName:String; aSL:TStringList;
begin
  CreateAstrosHipparcos150;

  btnNowClick(nil);

  aSL:=TStringList.Create;

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

procedure TFormStarEphemeris.btnNowClick(Sender: TObject);
var T:TDatetime;
begin
  T := TTimeZone.Local.ToUniversalTime(Now);  // current UT
  edDate.Date := Trunc(T);
  edTime.Time := Frac(T);
end;

procedure TFormStarEphemeris.btnCalcStarClick(Sender: TObject);
var aStar:TStarH150; aName:String; aGMT:TDAtetime;
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
      aStar.GMT := aGMT;                     // calc coordinates
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
end;

end.
