program FMXSVGToBitmapSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  FMXForm in 'FMXForm.pas' {Form2},
  USVGSampleImages in '..\SampleImages\USVGSampleImages.pas',
  Olf.Skia.SVGToBitmap in '..\..\..\src\Olf.Skia.SVGToBitmap.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
