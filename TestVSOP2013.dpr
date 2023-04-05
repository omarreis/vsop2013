program TestVSOP2013;

uses
  System.StartUpCopy,
  FMX.Forms,
  fTestVSOP2013 in 'fTestVSOP2013.pas' {FormVSOP2013Tests},
  vsop2013 in 'vsop2013.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormVSOP2013Tests, FormVSOP2013Tests);
  Application.Run;
end.
