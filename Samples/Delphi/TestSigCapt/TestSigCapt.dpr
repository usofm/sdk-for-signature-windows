program TestSigCapt;

uses
  Vcl.Forms,
  TestSigCaptForm in 'TestSigCaptForm.pas' {Form1},
  Device.Licence in 'Device.Licence.pas',
  Device.Command in 'Device.Command.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
