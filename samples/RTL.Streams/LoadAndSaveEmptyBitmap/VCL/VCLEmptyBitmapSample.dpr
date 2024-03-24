program VCLEmptyBitmapSample;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form2},
  Olf.VCL.Streams in '..\..\..\..\src\Olf.VCL.Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
