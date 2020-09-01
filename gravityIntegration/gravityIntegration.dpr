program gravityIntegration;

uses
  System.StartUpCopy,
  FMX.Forms,
  fGravityIntegration in 'fGravityIntegration.pas' {FormGravityIntegration},
  doubleVector3D in '..\doubleVector3D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGravityIntegration, FormGravityIntegration);
  Application.Run;
end.
