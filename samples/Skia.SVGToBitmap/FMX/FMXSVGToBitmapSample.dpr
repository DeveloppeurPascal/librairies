program FMXSVGToBitmapSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  FMXForm in 'FMXForm.pas' {Form2},
  Olf.Skia.SVGToBitmap in '..\..\..\src\Olf.Skia.SVGToBitmap.pas',
  USVGCursorSVGSamples in '..\_CursorSVGSamples\USVGCursorSVGSamples.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
