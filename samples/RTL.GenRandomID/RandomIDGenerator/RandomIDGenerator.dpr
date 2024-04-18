program RandomIDGenerator;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  Olf.RTL.GenRandomID in '..\..\..\src\Olf.RTL.GenRandomID.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
