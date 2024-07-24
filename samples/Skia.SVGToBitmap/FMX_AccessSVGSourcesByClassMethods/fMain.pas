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
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm5 = class(TForm)
    FlowLayout1: TFlowLayout;
    Image1: TImage;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap,
  USVGCursorSVGSamples;

procedure TForm5.Button1Click(Sender: TObject);
var
  SVGIndex: TSVGCursorSVGSamplesIndex;
  img: TImage;
  // bmp: TBitmap;
begin
  Button1.visible := false;

  // register SVG images to the bitmap from SVG class
  // ***** do it only one time in a unit 'initialization' or main form creation *****
  for SVGIndex := low(TSVGCursorSVGSamplesIndex)
    to high(TSVGCursorSVGSamplesIndex) do
    TOlfSVGBitmapList.AddItemAt(ord(SVGIndex),
      TSVGCursorSVGSamples.SVG(SVGIndex));

  // add all image on screen
  for SVGIndex := low(TSVGCursorSVGSamplesIndex)
    to high(TSVGCursorSVGSamplesIndex) do
  begin
    img := TImage.Create(self);
    img.Parent := FlowLayout1;
    img.Width := 64;
    img.height := 64;
    img.WrapMode := TImageWrapMode.Fit;
    // bmp := TOlfSVGBitmapList.Bitmap(ord(SVGIndex), round(img.Width),
    // round(img.height), Image1.Bitmap.BitmapScale);
    // img.Bitmap.Assign(bmp);
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(ord(SVGIndex), round(img.Width),
      round(img.height), Image1.Bitmap.BitmapScale));
  end;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  Image1.visible := false;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
