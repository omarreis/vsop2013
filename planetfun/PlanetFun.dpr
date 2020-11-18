program PlanetFun;  // PlanetFun 4d simulation ( space+time )
// Using serious astronomical ephemeris

uses
  System.StartUpCopy,
  FMX.Forms,
  fPlanetFun in 'fPlanetFun.pas' {FormPlanetFun},
  PlanetData in 'PlanetData.pas',
  vsop2013 in '..\vsop2013.pas',
  CameraMovementToolbar in 'CameraMovementToolbar.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPlanetFun, FormPlanetFun);
  Application.Run;
end.
