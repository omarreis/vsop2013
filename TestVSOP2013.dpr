program TestVSOP2013;

uses
  System.StartUpCopy,
  FMX.Forms,
  fTestVSOP2013 in 'fTestVSOP2013.pas' {Form2},
  vsop2013 in 'vsop2013.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
