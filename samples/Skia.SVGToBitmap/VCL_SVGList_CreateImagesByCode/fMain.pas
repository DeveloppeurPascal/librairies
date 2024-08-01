unit fMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TForm7 = class(TForm)
    Button1: TButton;
    ScrollBox1: TScrollBox;
    FlowPanel1: TFlowPanel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

uses
  Olf.Skia.SVGToBitmap,
  USVGCursorSVGSamples,
  USVGGameControllerSVGSamples,
  USVGPipesSVGSamples;

procedure TForm7.Button1Click(Sender: TObject);
var
  i: integer;
  img: TImage;
begin
  for i := 0 to TOlfSVGBitmapList.Count - 1 do
  begin
    img := TImage.Create(self);
    img.parent := FlowPanel1;
    img.width := 64;
    img.height := 64;
    img.picture.bitmap.Assign(TOlfSVGBitmapList.bitmap(i, img.width,
      img.height));

    FlowPanel1.height := img.Top + img.height + 10;

    Image1.picture.bitmap.Assign(TOlfSVGBitmapList.bitmap(i, Image1.width,
      Image1.height));
  end;
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  TOlfSVGBitmapList.AddItem(SVGCursorSVGSamples);
  TOlfSVGBitmapList.AddItem(SVGGameControllerSVGSamples);
  TOlfSVGBitmapList.AddItem(SVGPipesSVGSamples);
end;

end.
