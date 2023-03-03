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
    btnH150Almanac: TButton;
    btnNavigatorAlmanac: TButton;
    cbUseRigorousPrecession: TSwitch;
    Label10: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure btnCalcStarClick(Sender: TObject);
    procedure btnNowClick(Sender: TObject);
    procedure btnH150AlmanacClick(Sender: TObject);
    procedure btnNavigatorAlmanacClick(Sender: TObject);
    procedure cbUseRigorousPrecessionSwitch(Sender: TObject);
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


function FormatStrWithSpaces(const S:String; aSize:integer):String;
var L,i:Integer;
begin
  Result := S;
  L := aSize-Length(S);
  if L>0 then for i := 1 to L do Result := Result+' ';   //pad spaces to fixed size
end;

{ TFormStarEphemeris }

procedure TFormStarEphemeris.FormCreate(Sender: TObject);
var i:integer; aName:String; aSL:TStringList;
begin
  CreateAstrosHipparcos150;
  CreateNavStars;

  btnNowClick(nil);

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

procedure TFormStarEphemeris.btnNavigatorAlmanacClick(Sender: TObject);
var i:integer; aStar:TNavStar; aGMT:TDAtetime;
   aSL:TStringList; aDummy,aSHA:Double;
begin
  aGMT := edDate.Date + edTime.Time;     // add time zone 3 to time ( Sao Paulo time)

  aSL := TStringList.Create;

  for i:=1 to NEstrelas do
    begin
      aStar := Estrelas[i];
      if (aStar.Name<>'') then
        begin
          aStar.GMT := aGMT;             // calc coordinates
          aSHA := 360.0-aStar.fRA;
          aSL.Add( FormatStrWithSpaces(aStar.Name,15)                  +' '+
               FormatStrWithSpaces(R2GMD(aSHA,aDummy,' -'),10)         +' '+
               FormatStrWithSpaces(R2GMD(aStar.fDecl,aDummy,'NS'),10)  );
        end;
    end;

  aSL.Sort;
  aSL.Insert(0,'Nav star Almanac for '+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', aGMT) +' UT' );  //GMT = Universal Time
  aSL.Insert(1,'                   SHA        Decl');      //table header
  Memo1.Lines.Assign(aSL);
  aSL.Free;
end;

procedure TFormStarEphemeris.btnNowClick(Sender: TObject);
var T:TDatetime;
begin
  T := TTimeZone.Local.ToUniversalTime(Now);  // current UT
  edDate.Date := Trunc(T);
  edTime.Time := Frac(T);
end;

procedure TFormStarEphemeris.cbUseRigorousPrecessionSwitch(Sender: TObject);
begin
  UseRigorousPrecession := cbUseRigorousPrecession.IsChecked;
end;

procedure TFormStarEphemeris.btnH150AlmanacClick(Sender: TObject);
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
          aSL.Add( FormatStrWithSpaces(aStar.Name,15)                      +' '+
               FormatStrWithSpaces(R2GMD(aSHA,aDummy,' -'),10)         +' '+
               FormatStrWithSpaces(R2GMD(aStar.fDecl,aDummy,'NS'),10)  );
        end;
    end;

  aSL.Sort;

  aSL.Insert(0,'H150 star Almanac for '+ FormatDateTime('dd/mmm/yyyy hh:nn:ss', aGMT) +' UT' );  //GMT = Universal Time
  aSL.Insert(1,'                   SHA        Decl');      //table header

  Memo1.Lines.Assign(aSL);
  aSL.Free;
end;

procedure TFormStarEphemeris.btnCalcStarClick(Sender: TObject);
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
      Memo1.Lines.Add('H150 star ------------------------');
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
      Memo1.Lines.Add('NavStar -------------------------------');
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

end.
