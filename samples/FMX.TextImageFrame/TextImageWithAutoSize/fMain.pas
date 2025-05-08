/// <summary>
/// ***************************************************************************
///
/// My libraries for Delphi
///
/// Copyright 1990-2025 Patrick Prémartin under AGPL 3.0 license.
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
/// Patrick PREMARTIN
///
/// Site :
/// https://librairies.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 2025-05-08T18:06:46.000+02:00
/// Signature : a879c5e1fa3f815798f5dec7baeaac500bda4ea5
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
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Olf.FMX.TextImageFrame;

type
  TForm1 = class(TForm)
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    btnRefresh: TButton;
    OlfFMXTextImageFrame1: TOlfFMXTextImageFrame;
    OlfFMXTextImageFrame2: TOlfFMXTextImageFrame;
    OlfFMXTextImageFrame3: TOlfFMXTextImageFrame;
    OlfFMXTextImageFrame4: TOlfFMXTextImageFrame;
    btnReverseAutosize: TButton;
    procedure Rectangle1Resized(Sender: TObject);
    procedure Rectangle2Resized(Sender: TObject);
    procedure Rectangle3Resized(Sender: TObject);
    procedure Rectangle4Resized(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnReverseAutosizeClick(Sender: TObject);
  private
  public
    procedure ChangeText;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  udm_CharacterImages;

procedure TForm1.btnRefreshClick(Sender: TObject);
begin
  ChangeText;
end;

procedure TForm1.btnReverseAutosizeClick(Sender: TObject);
begin
  OlfFMXTextImageFrame1.AutoSize := not OlfFMXTextImageFrame1.AutoSize;
  OlfFMXTextImageFrame2.AutoSize := not OlfFMXTextImageFrame2.AutoSize;
  OlfFMXTextImageFrame3.AutoSize := not OlfFMXTextImageFrame3.AutoSize;
  OlfFMXTextImageFrame4.AutoSize := not OlfFMXTextImageFrame4.AutoSize;
end;

procedure TForm1.ChangeText;
var
  S: String;
  I: Integer;
begin
  I := random(20) + 5;
  S := '';
  while (I > 0) do
  begin
    case random(9) of
      0:
        S := S + 'a';
      1:
        S := S + 'b';
      2:
        S := S + 'c';
      3:
        S := S + 'A';
      4:
        S := S + 'B';
      5:
        S := S + 'C';
      6:
        S := S + '1';
      7:
        S := S + '2';
    else
      S := S + '3';
    end;
    dec(I);
  end;

  OlfFMXTextImageFrame1.Text := S;
  OlfFMXTextImageFrame2.Text := S;
  OlfFMXTextImageFrame3.Text := S;
  OlfFMXTextImageFrame4.Text := S;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OlfFMXTextImageFrame1.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame2.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame3.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame4.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame1.AutoSize := True;
  OlfFMXTextImageFrame2.AutoSize := True;
  OlfFMXTextImageFrame3.AutoSize := True;
  OlfFMXTextImageFrame4.AutoSize := True;
end;

procedure TForm1.Rectangle1Resized(Sender: TObject);
begin
  OlfFMXTextImageFrame1.Refresh;
end;

procedure TForm1.Rectangle2Resized(Sender: TObject);
begin
  OlfFMXTextImageFrame2.Refresh;
end;

procedure TForm1.Rectangle3Resized(Sender: TObject);
begin
  OlfFMXTextImageFrame3.Refresh;
end;

procedure TForm1.Rectangle4Resized(Sender: TObject);
begin
  OlfFMXTextImageFrame4.Refresh;
end;

initialization

randomize;

end.
