program GetSystemLanguageISOCode_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form2},
  Olf.RTL.Language in '..\..\..\src\Olf.RTL.Language.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
