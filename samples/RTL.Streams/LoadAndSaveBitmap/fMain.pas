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
  FMX.StdCtrls,
  FMX.Objects;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    btnSaveImagesToStream: TButton;
    btnClearImagesBitmap: TButton;
    btnLoadImagesFromStream: TButton;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelLayout2: TGridPanelLayout;
    procedure btnSaveImagesToStreamClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearImagesBitmapClick(Sender: TObject);
    procedure btnLoadImagesFromStreamClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    MesImages: TStream;

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.FMX.Streams;

procedure TForm1.btnClearImagesBitmapClick(Sender: TObject);
begin
  Image1.bitmap.Clear(TAlphaColors.Antiquewhite);
  Image2.bitmap.Clear(TAlphaColors.Beige);
end;

procedure TForm1.btnLoadImagesFromStreamClick(Sender: TObject);
var
  bmp: tbitmap;
begin
  MesImages.Position := 0;
  bmp := LoadBitmapFromStream(MesImages);
  try
    Image2.bitmap.assign(bmp);
  finally
    bmp.free;
  end;
  bmp := LoadBitmapFromStream(MesImages);
  try
    Image1.bitmap.assign(bmp);
  finally
    bmp.free;
  end;
end;

procedure TForm1.btnSaveImagesToStreamClick(Sender: TObject);
begin
  MesImages.Position := 0;
  SaveBitmapToStream(Image1.bitmap, MesImages);
  SaveBitmapToStream(Image2.bitmap, MesImages);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MesImages := tmemorystream.create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MesImages.free;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
