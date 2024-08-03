/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This repository contains functions, procedures and classes to use in
/// Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.
///
/// The units to be used in your projects can be found in the "src" folder.
/// Some features are explained on my blog or have been coded live on Twitch.
///
/// Examples of use in the form of VCL or FireMonkey projects are available in
/// the "samples" subfolder.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
///      https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:15
/// Signature : 67299733e54e8c2f81cfe134104d1687095e11f9
/// ***************************************************************************
/// </summary>

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
