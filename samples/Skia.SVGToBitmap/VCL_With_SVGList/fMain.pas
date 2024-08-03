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
/// File last update : 01/08/2024 10:43:10
/// Signature : d8dc5400e99690fab1226bdd4da07e1af71967e9
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
  USVGCursorSVGSamples;

procedure TForm3.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to length(SVGCursorSVGSamples) - 1 do
    TOlfSVGBitmapList.AddItemAt(i, SVGCursorSVGSamples[i]);

  Timer1.tag := -1;
end;

procedure TForm3.FormResize(Sender: TObject);
begin
  TOlfSVGBitmapList.ClearCache;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Timer1.tag := Timer1.tag + 1;
  if (Timer1.tag >= length(SVGCursorSVGSamples)) then
    Timer1.tag := 0;

  // clear the bitmap to avoid previous image on it
  Image1.Picture.Bitmap.SetSize(0, 0);
  // copy the non transparent part of the SVG to current bitmap
  Image1.Picture.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(Timer1.tag,
    trunc(Image1.Width), trunc(Image1.Height)));
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
