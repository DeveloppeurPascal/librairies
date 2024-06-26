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
  Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    Timer1: TTimer;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses
  Olf.Skia.SVGToBitmap,
  USVGSampleImages;

procedure TForm3.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to length(SVGSampleImages) - 1 do
    TOlfSVGBitmapList.AddItemAt(i, SVGSampleImages[i]);

  Timer1.tag := -1;
end;

procedure TForm3.FormResize(Sender: TObject);
begin
  TOlfSVGBitmapList.ClearCache;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Timer1.tag := Timer1.tag + 1;
  if (Timer1.tag >= length(SVGSampleImages)) then
    Timer1.tag := 0;

  Image1.picture.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(Timer1.tag,
    trunc(Image1.Width), trunc(Image1.Height)));
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
