program PlanetFunAlmanac;

uses
  System.StartUpCopy,
  FMX.Forms,
  fPlanetFunAlmanac in 'fPlanetFunAlmanac.pas' {FrmPlanetFunAlmanac};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmPlanetFunAlmanac, FrmPlanetFunAlmanac);
  Application.Run;
end.
