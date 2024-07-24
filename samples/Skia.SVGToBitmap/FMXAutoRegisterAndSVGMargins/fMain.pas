unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects;

type
  TForm6 = class(TForm)
    btnRegisterSVGLists: TButton;
    btnShowCursors: TButton;
    FlowLayout1: TFlowLayout;
    btnShowPipes: TButton;
    Image1: TImage;
    procedure btnRegisterSVGListsClick(Sender: TObject);
    procedure btnShowCursorsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnShowPipesClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap,
  USVGCursorSVGSamples,
  USVGPipesSVGSamples;

procedure TForm6.btnRegisterSVGListsClick(Sender: TObject);
begin
  tSVGPipesSVGSamples.Tag := TOlfSVGBitmapList.Additem(SVGPipesSVGSamples);
  tSVGCursorSVGSamples.Tag := TOlfSVGBitmapList.Additem(SVGCursorSVGSamples);
  btnRegisterSVGLists.Visible := false;
  btnShowCursors.Visible := true;
  btnShowPipes.Visible := true;
end;

procedure TForm6.btnShowCursorsClick(Sender: TObject);
var
  SVGIndex: integer;
  img: TImage;
begin
  for SVGIndex := 0 to length(SVGCursorSVGSamples) - 1 do
  // TODO : replace length by TSVGCursorSVGSamples.Count() method
  begin
    img := TImage.Create(self);
    img.Parent := FlowLayout1;
    img.Width := 64;
    img.height := 64;
    img.WrapMode := TImageWrapMode.Fit;
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(SVGIndex +
      tSVGCursorSVGSamples.Tag, round(img.Width), round(img.height),
      Image1.Bitmap.BitmapScale));
  end;
end;

procedure TForm6.btnShowPipesClick(Sender: TObject);
var
  SVGIndex: integer;
  img: TImage;
begin
  for SVGIndex := 0 to length(SVGPipesSVGSamples) - 1 do
  // TODO : replace length by TSVGPipesSVGSamples.Count() method
  begin
    img := TImage.Create(self);
    img.Parent := FlowLayout1;
    img.Width := 64;
    img.height := 64;
    img.WrapMode := TImageWrapMode.Fit;
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(SVGIndex +
      tSVGPipesSVGSamples.Tag, round(img.Width), round(img.height),
      Image1.Bitmap.BitmapScale));
  end;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  Image1.Visible := false;
  btnRegisterSVGLists.Visible := true;
  btnShowCursors.Visible := false;
  btnShowPipes.Visible := false;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
