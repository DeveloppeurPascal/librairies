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
  File last update : 2026-07-17T19:02:58.000+02:00
  Signature : 3d7ec22b7cf51562aeda47c8cc98ea96c5d002be
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
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.EditBox,
  FMX.NumberBox,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation;

type
  TForm12 = class(TForm)
    cbNumbers: TCheckBox;
    cbLowerCaseLetters: TCheckBox;
    cbUpperCaseLetters: TCheckBox;
    edtSymbols: TEdit;
    btnGenerateWithSymbolsAsString: TButton;
    nbPasswordSize: TNumberBox;
    mmoPasswords: TMemo;
    btnGenerateWithSymbolsAsArray: TButton;
    procedure btnGenerateWithSymbolsAsStringClick(Sender: TObject);
    procedure btnGenerateWithSymbolsAsArrayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

uses
  Olf.RTL.GenRandomID;

procedure TForm12.btnGenerateWithSymbolsAsArrayClick(Sender: TObject);
  var
    Tab: TArray<Char>;
    i: Integer;
begin
  setlength(tab, edtSymbols.Text.Length);
  for i := 0 to length(tab) - 1 do
    tab[i] := edtSymbols.Text.Chars[i];
  mmoPasswords.lines.clear;
  for i := 1 to 10 do
    mmoPasswords.lines.Add(i.tostring + ': "' + TOlfRandomIDGenerator.getPassword(cbNumbers.IsChecked, cbLowerCaseLetters.IsChecked, cbUpperCaseLetters.IsChecked, tab, trunc(nbPasswordSize.Value)) + '"');
end;

procedure TForm12.btnGenerateWithSymbolsAsStringClick(Sender: TObject);
var
  i: Integer;
begin
  mmoPasswords.lines.clear;
  for i := 1 to 10 do
    mmoPasswords.lines.Add(i.tostring + ': "' + TOlfRandomIDGenerator.getPassword(cbNumbers.IsChecked, cbLowerCaseLetters.IsChecked, cbUpperCaseLetters.IsChecked, edtSymbols.Text, trunc(nbPasswordSize.Value)) + '"');
end;

      procedure TForm12.FormCreate(Sender: TObject);
begin
  nbPasswordSize.Value := 15;
end;

end.


