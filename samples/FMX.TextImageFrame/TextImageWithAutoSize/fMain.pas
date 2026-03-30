(* C2PP
  ***************************************************************************

  My libraries for Delphi
  Copyright (c) 1990-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  This repository contains functions, procedures and classes to use in
  Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.

  The units to be used in your projects can be found in the "src" folder.
  Some features are explained on my blog or have been coded live on Twitch.

  Examples of use in the form of VCL or FireMonkey projects are available in
  the "samples" subfolder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://librairies.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/librairies

  ***************************************************************************
  File last update : 2026-03-30T17:16:22.000+02:00
  Signature : e8d1a14daeecce04dc961bf8647ff58116080d2b
  ***************************************************************************
*)

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
  Olf.FMX.TextImageFrame,
  FMX.Layouts;

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
    VertScrollBox1: TVertScrollBox;
    OlfFMXTextImageFrame5: TOlfFMXTextImageFrame;
    GridPanelLayout1: TGridPanelLayout;
    OlfFMXTextImageFrame6: TOlfFMXTextImageFrame;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Rectangle1Resized(Sender: TObject);
    procedure Rectangle2Resized(Sender: TObject);
    procedure Rectangle3Resized(Sender: TObject);
    procedure Rectangle4Resized(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnReverseAutosizeClick(Sender: TObject);
    procedure VertScrollBox1Resized(Sender: TObject);
    procedure GridPanelLayout1Resized(Sender: TObject);
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
  OlfFMXTextImageFrame5.AutoSize := not OlfFMXTextImageFrame5.AutoSize;
  OlfFMXTextImageFrame6.AutoSize := not OlfFMXTextImageFrame6.AutoSize;
end;

procedure TForm1.ChangeText;
var
  S: string;
  I: Integer;
begin
  I := random(20) + 5;
  S := '';
  while (I > 0) do
  begin
    case random(10) of
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
      8:
        S := S + ' ';
    else
      S := S + '3';
    end;
    dec(I);
  end;

  OlfFMXTextImageFrame1.Text := S;
  OlfFMXTextImageFrame2.Text := S;
  OlfFMXTextImageFrame3.Text := S;
  OlfFMXTextImageFrame4.Text := S;
  OlfFMXTextImageFrame5.Text := S;
  OlfFMXTextImageFrame6.Text := S;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OlfFMXTextImageFrame1.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame2.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame3.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame4.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame5.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame6.font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame1.AutoSize := True;
  OlfFMXTextImageFrame2.AutoSize := True;
  OlfFMXTextImageFrame3.AutoSize := True;
  OlfFMXTextImageFrame4.AutoSize := True;
  OlfFMXTextImageFrame5.AutoSize := True;
  OlfFMXTextImageFrame6.AutoSize := True;
end;

procedure TForm1.GridPanelLayout1Resized(Sender: TObject);
begin
  OlfFMXTextImageFrame6.Refresh;
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

procedure TForm1.VertScrollBox1Resized(Sender: TObject);
begin
  OlfFMXTextImageFrame5.Refresh;
end;

initialization

  randomize;

end.

