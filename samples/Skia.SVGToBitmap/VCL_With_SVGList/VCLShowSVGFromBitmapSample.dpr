program VCLShowSVGFromBitmapSample;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form3},
  Olf.Skia.SVGToBitmap in '..\..\..\src\Olf.Skia.SVGToBitmap.pas',
  USVGSampleImages in '..\SampleImages\USVGSampleImages.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
