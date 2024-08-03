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
/// File last update : 01/08/2024 10:49:42
/// Signature : b2ebff54743f3bccc9f7e593263c81c3a373d6df
/// ***************************************************************************
/// </summary>

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
