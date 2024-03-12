program CryptDecryptEventsSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  Olf.RTL.CryptDecrypt in '..\..\..\src\Olf.RTL.CryptDecrypt.pas',
  Olf.RTL.Params in '..\..\..\src\Olf.RTL.Params.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
