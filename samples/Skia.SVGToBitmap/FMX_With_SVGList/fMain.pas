unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TForm4 = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses Olf.Skia.SVGToBitmap, USVGSampleImages;

procedure TForm4.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to length(SVGSampleImages) - 1 do
    TOlfSVGBitmapList.AddItemAt(i, SVGSampleImages[i]);

  Timer1.tag := -1;
end;

procedure TForm4.FormResize(Sender: TObject);
begin
  TOlfSVGBitmapList.ClearCache;
end;

procedure TForm4.Timer1Timer(Sender: TObject);
begin
  Timer1.tag := Timer1.tag + 1;
  if (Timer1.tag >= length(SVGSampleImages)) then
    Timer1.tag := 0;

  Image1.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(Timer1.tag, trunc(Image1.Width),
    trunc(Image1.Height), Image1.Bitmap.BitmapScale));
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
