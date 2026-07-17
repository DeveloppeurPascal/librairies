program RandomPasswordSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form12},
  Olf.RTL.GenRandomID in '..\..\..\src\Olf.RTL.GenRandomID.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
