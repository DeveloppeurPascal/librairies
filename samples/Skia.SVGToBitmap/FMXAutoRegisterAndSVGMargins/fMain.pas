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
/// File last update : 28/07/2024 14:37:16
/// Signature : c872b6a628daaa38b2e9c8c61d3e4f77d903da0f
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
  for SVGIndex := 0 to tSVGCursorSVGSamples.Count - 1 do
  begin
    img := TImage.Create(self);
    img.Parent := FlowLayout1;
    img.Width := 64;
    img.height := 64;
    img.WrapMode := TImageWrapMode.Original;
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(SVGIndex +
      tSVGCursorSVGSamples.Tag, round(img.Width), round(img.height),
      Image1.Bitmap.BitmapScale));
  end;
end;

procedure TForm6.btnShowPipesClick(Sender: TObject);
var
  SVGIndex: integer;
  img: TImage;
  MargeHaut, MargeBas, MargeGauche, MargeDroite: single;
begin
  for SVGIndex := 0 to tSVGPipesSVGSamples.Count - 1 do
  begin
    MargeHaut := 0;
    MargeDroite := 0;
    MargeBas := 0;
    MargeGauche := 0;
    case SVGIndex + tSVGPipesSVGSamples.Tag of
      tSVGPipesSVGSamples.PipeDb:
        begin
          MargeHaut := 100 * ((117.55 - 87.9) / 117.55);
          MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
        end;
      tSVGPipesSVGSamples.PipeGb:
        begin
          MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
          MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
        end;
      tSVGPipesSVGSamples.PipeGd:
        begin
          MargeHaut := 100 * ((117.55 - 58.6) / 117.55) / 2;
          MargeBas := 100 * ((117.55 - 58.6) / 117.55) / 2;
        end;
      tSVGPipesSVGSamples.PipeGdb:
        MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
      tSVGPipesSVGSamples.PipeHb:
        begin
          MargeDroite := 100 * ((117.55 - 58.6) / 117.55) / 2;
          MargeGauche := 100 * ((117.55 - 58.6) / 117.55) / 2;
        end;
      tSVGPipesSVGSamples.PipeHd:
        begin
          MargeBas := 100 * ((117.55 - 87.9) / 117.55);
          MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
        end;
      tSVGPipesSVGSamples.PipeHdb:
        MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
      tSVGPipesSVGSamples.PipeHg:
        begin
          MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
          MargeBas := 100 * ((117.55 - 88.05) / 117.55);
        end;
      tSVGPipesSVGSamples.PipeHgb:
        MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
      tSVGPipesSVGSamples.PipeHgd:
        MargeBas := 100 * ((117.55 - 87.9) / 117.55);
    end;

    img := TImage.Create(self);
    img.Parent := FlowLayout1;
    img.Width := 64;
    img.height := 64;
    img.WrapMode := TImageWrapMode.Original;
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(SVGIndex +
      tSVGPipesSVGSamples.Tag, round(img.Width), round(img.height), MargeHaut,
      MargeDroite, MargeBas, MargeGauche, Image1.Bitmap.BitmapScale));
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
