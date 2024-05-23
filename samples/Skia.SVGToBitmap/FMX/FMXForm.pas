unit FMXForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TForm2 = class(TForm)
    Timer1: TTimer;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  USVGSampleImages,
  Olf.Skia.SVGToBitmap;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Timer1.tag := -1;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
var
  bmp: tbitmap;
begin
  Timer1.tag := Timer1.tag + 1;
  if (Timer1.tag >= length(SVGSampleImages)) then
    Timer1.tag := 0;

  bmp := SVGToBitmap(trunc(Image1.Width), trunc(Image1.Height),
    SVGSampleImages[Timer1.tag], Image1.Bitmap.BitmapScale);
  try
    Image1.Bitmap.Assign(bmp);
  finally
    bmp.free;
  end;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
