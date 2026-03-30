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
  File last update : 2026-03-30T16:35:19.567+02:00
  Signature : a5478b33e3cc2d42297638f96d2febe0f75bea00
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
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts;

type
  TForm2 = class(TForm)
    btnSHL: TButton;
    btnSHR: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    procedure btnSHLClick(Sender: TObject);
    procedure btnSHRClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Olf.RTL.Maths.Conversions;

procedure TForm2.btnSHLClick(Sender: TObject);
var
  s, d: word;
begin
  s := TOlfNumberConversion.BinaryToDecimal(Edit1.Text);
  d := s shl Edit2.Text.ToInteger;
  Memo1.lines.Add(Edit1.Text + ' shl ' + Edit2.Text + ' = ' +
    TOlfNumberConversion.DecimalToBinary(d));
  Edit1.setfocus;
end;

procedure TForm2.btnSHRClick(Sender: TObject);
var
  s, d: word;
begin
  s := TOlfNumberConversion.BinaryToDecimal(Edit1.Text);
  d := s shr Edit2.Text.ToInteger;
  Memo1.lines.Add(Edit1.Text + ' shr ' + Edit2.Text + ' = ' +
    TOlfNumberConversion.DecimalToBinary(d));
  Edit1.setfocus;
end;

end.
