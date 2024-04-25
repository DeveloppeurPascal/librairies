program SHL_SHR_Sample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form2},
  Olf.RTL.Maths.Conversions in '..\..\..\src\Olf.RTL.Maths.Conversions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
