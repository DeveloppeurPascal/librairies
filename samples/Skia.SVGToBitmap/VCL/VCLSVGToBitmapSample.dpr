program VCLSVGToBitmapSample;

uses
  Vcl.Forms,
  VCLForm in 'VCLForm.pas' {Form1},
  USVGSampleImages in '..\SampleImages\USVGSampleImages.pas',
  Olf.Skia.SVGToBitmap in '..\..\..\src\Olf.Skia.SVGToBitmap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
