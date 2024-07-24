program FMXAutoRegisterAndSVGMarginsSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {Form6},
  USVGCursorSVGSamples in '..\_CursorSVGSamples\USVGCursorSVGSamples.pas',
  USVGPipesSVGSamples in '..\_PipesSVGSamples\USVGPipesSVGSamples.pas',
  Olf.Skia.SVGToBitmap in '..\..\..\src\Olf.Skia.SVGToBitmap.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
