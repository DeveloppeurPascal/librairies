program DProjReaderSample;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form1},
  Olf.RTL.DPROJReader in '..\..\src\Olf.RTL.DPROJReader.pas',
  Olf.RTL.PathAliases in '..\..\src\Olf.RTL.PathAliases.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
