program ShiftCryptDecryptSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  Olf.RTL.CryptDecrypt in '..\..\..\src\Olf.RTL.CryptDecrypt.pas',
  Olf.RTL.Maths.Conversions in '..\..\..\src\Olf.RTL.Maths.Conversions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
