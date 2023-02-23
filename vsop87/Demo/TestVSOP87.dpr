program TestVSOP87;

uses
  System.StartUpCopy,
  FMX.Forms,
  fTestVSOP87 in 'fTestVSOP87.pas' {FormTestVSOP87},
  Om.Trigonometry in '..\..\Om.Trigonometry.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestVSOP87, FormTestVSOP87);
  Application.Run;
end.
