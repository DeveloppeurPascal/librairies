/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Pr�martin under AGPL 3.0 license.
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
/// Signature : 5bd05775151d0927e9c1b64946cade1a6f215f51
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
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  Olf.FMX.Streams;

procedure TForm1.Button1Click(Sender: TObject);
var
  bmp: tbitmap;
  fs: tfilestream;
  filename: string;
begin
  filename := tpath.GetTempFileName;
  try
    fs := tfilestream.Create(filename, fmCreate);
    try
      SaveBitmapToStream(nil, fs);
      bmp := tbitmap.Create(0, 0);
      try
        SaveBitmapToStream(bmp, fs);
      finally
        bmp.free;
      end;
    finally
      fs.free;
    end;
    fs := tfilestream.Create(filename, fmOpenRead);
    try
      bmp := LoadBitmapFromStream(fs);
      try
        if assigned(bmp) then
          showmessage('BMP Nil :' + bmp.Width.tostring + ',' +
            bmp.Height.tostring)
        else
          showmessage('nil');
      finally
        bmp.free;
      end;
      bmp := LoadBitmapFromStream(fs);
      try
        if assigned(bmp) then
          showmessage('BMP vide :' + bmp.Width.tostring + ',' +
            bmp.Height.tostring)
        else
          showmessage('nil');
      finally
        bmp.free;
      end;
    finally
      fs.free;
    end;
  finally
    tfile.Delete(filename);
  end;
end;

end.
