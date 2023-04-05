program PlanetFun;  // PlanetFun 4d simulation ( space+time ) --------\
 //----------------// Almanac query module - for Win32, Android, iOS   \
// Using serious astronomical algorithms                                \
//   by oMAR                                                            /
// See: github.com/omarreis/vsop2013                                   /
//--------------------------------------------------------------------/
// History:

uses
  System.StartUpCopy,
  FMX.Forms,
  fPlanetFun in 'fPlanetFun.pas' {FormPlanetFun},
  PlanetData in 'PlanetData.pas',
  StarData in '..\StarData.pas',
  vsop2013 in '..\vsop2013.pas',
  fPlanetFunAlmanac in '..\PlanetFunAlmanac\fPlanetFunAlmanac.pas',
  CameraMovementToolbar in 'CameraMovementToolbar.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPlanetFun, FormPlanetFun);
  Application.CreateForm(TFrmPlanetFunAlmanac, FrmPlanetFunAlmanac);
  Application.Run;
end.

