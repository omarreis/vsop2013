program PlanetFun;

uses
  System.StartUpCopy,
  FMX.Forms,
  fPlanetFun in 'fPlanetFun.pas' {FormPlanetFun},
  PlanetData in 'PlanetData.pas',
  vsop2013 in '..\vsop2013.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPlanetFun, FormPlanetFun);
  Application.Run;
end.
