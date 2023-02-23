program TestH150Stars;

uses
  System.StartUpCopy,
  FMX.Forms,
  fTestH150Stars in 'fTestH150Stars.pas' {FormStarEphemeris};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormStarEphemeris, FormStarEphemeris);
  Application.Run;
end.
