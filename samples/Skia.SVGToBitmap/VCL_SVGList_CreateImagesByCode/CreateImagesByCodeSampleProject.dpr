program CreateImagesByCodeSampleProject;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form7},
  USVGCursorSVGSamples in '..\_CursorSVGSamples\USVGCursorSVGSamples.pas',
  USVGGameControllerSVGSamples in '..\_GameControllerSVGSamples\USVGGameControllerSVGSamples.pas',
  USVGPipesSVGSamples in '..\_PipesSVGSamples\USVGPipesSVGSamples.pas',
  Olf.Skia.SVGToBitmap in '..\..\..\src\Olf.Skia.SVGToBitmap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
