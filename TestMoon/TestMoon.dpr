program TestMoon;

uses
  System.StartUpCopy,
  FMX.Forms,
  fTestMoonCalculations in 'fTestMoonCalculations.pas' {HeaderFooterForm},
  Om.Moon in '..\Om.Moon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THeaderFooterForm, HeaderFooterForm);
  Application.Run;
end.
