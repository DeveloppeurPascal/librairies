program GetSystemLanguageISOCode_VCL;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form1},
  Olf.RTL.Language in '..\..\..\src\Olf.RTL.Language.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
