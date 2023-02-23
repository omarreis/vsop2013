unit fTestMoonCalculations;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    Memo1: TMemo;
    btnCalc: TButton;
    procedure btnCalcClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HeaderFooterForm: THeaderFooterForm;

implementation

uses
  System.DateUtils,
  Ah.Moon,
  Om.AstronomicalAlgorithms;

{$R *.fmx}

procedure THeaderFooterForm.btnCalcClick(Sender: TObject);
var aCoord:t_coord; T:TDatetime;
begin
  // T := Now;
  // T := TTimeZone.Local.ToUniversalTime( T );   //apply time zone

  T := EncodeDate(1992,04,12);     // AA ex 45.a --> Moon 12/apr/1992 0 TD

  aCoord := moon_coordinate( T );  // T in UT
  Memo1.Lines.Add( 'rad: '+Format('%9.2f',[aCoord.radius   ]) );
  Memo1.Lines.Add( 'lat: '+floatToGMSD(aCoord.latitude ) );
  Memo1.Lines.Add( 'lon: '+floatToGMSD(aCoord.longitude) );

  Memo1.Lines.Add( 'RA:  '+ floatToHHMMSS(aCoord.rektaszension) );
  Memo1.Lines.Add( 'Decl:'+ floatToGMSD(aCoord.declination) );

end;

end.
