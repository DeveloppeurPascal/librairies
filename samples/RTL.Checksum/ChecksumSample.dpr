program ChecksumSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  Olf.RTL.Checksum in '..\..\src\Olf.RTL.Checksum.pas',
  u_md5 in '..\..\src\u_md5.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
