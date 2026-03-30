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
  File last update : 2026-03-30T17:19:44.000+02:00
  Signature : 86a286a7ccc7ed436b41b2572b566d9085940691
  ***************************************************************************
*)

unit fDisplayTextByCode;

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
  Olf.FMX.TextImageFrame,
  FMX.ImgList;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    function ConvertUnknownChars(Sender: TOlfFMXTextImageFrame;
      AChar: char): integer;
    function CreateTextImage(AFont: tcustomimagelist; Atext: string)
      : TOlfFMXTextImageFrame;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  udm_CharacterImages;

function TForm1.ConvertUnknownChars(Sender: TOlfFMXTextImageFrame;
  AChar: char): integer;
begin
  result := -1;
  if charinset(AChar, ['a'..'z']) then // _a, _b, _c in source images
    result := Sender.getImageIndexOfChar('_' + AChar);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateTextImage(dm_CharacterImages.ImageList, '123abcABC');
  with CreateTextImage(dm_CharacterImages.ImageList, '') do
  begin
    align := talignlayout.Center;
    OnGetImageIndexOfUnknowChar := ConvertUnknownChars;
    LetterSpacing := -20;
    text := '123 abc ABC';
  end;
end;

function TForm1.CreateTextImage(AFont: tcustomimagelist; Atext: string)
  : TOlfFMXTextImageFrame;
begin
  result := TOlfFMXTextImageFrame.Create(self);
  with result do
  begin
    parent := self;
    align := talignlayout.top;
    height := 100;
    Font := AFont;
    text := Atext;
  end;
end;

end.

