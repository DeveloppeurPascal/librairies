program VCLSVGToBitmapSample;

uses
  Vcl.Forms,
  VCLForm in 'VCLForm.pas' {Form1},
  Olf.Skia.SVGToBitmap in '..\..\..\src\Olf.Skia.SVGToBitmap.pas',
  USVGCursorSVGSamples in '..\_CursorSVGSamples\USVGCursorSVGSamples.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
