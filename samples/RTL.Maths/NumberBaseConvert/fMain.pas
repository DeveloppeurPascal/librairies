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
  File last update : 2026-03-30T18:45:48.000+02:00
  Signature : 36e6838c83ea2e98baafbd244af1db3aa6a44b85
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
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.StdCtrls,
  System.Generics.Collections;

type
  TForm1 = class(TForm)
    lblBase2: TLabel;
    edtBase2: TEdit;
    lblBase62: TLabel;
    edtBase62: TEdit;
    lblBase57: TLabel;
    edtBase57: TEdit;
    lblBase36: TLabel;
    edtBase36: TEdit;
    lblBase27: TLabel;
    edtBase27: TEdit;
    lblBase16: TLabel;
    edtBase16: TEdit;
    lblBase10: TLabel;
    edtBase10: TEdit;
    lblBase8: TLabel;
    edtBase8: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    ChangeInProgress: boolean;
  protected
    NumberEditList: TList<TEdit>;
    procedure edtBaseXXChange(Sender: TObject);
    procedure edtBaseXXEnter(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.RTL.Maths.Conversions;

procedure TForm1.edtBaseXXChange(Sender: TObject);
var
  nb: uint64;
begin
  if ChangeInProgress then
    exit;
  ChangeInProgress := true;
  nb := 0;
  try
    if (Sender is TEdit) and ((Sender as TEdit).tag > 0) then
    begin
      nb := TOlfNumberConversion.ToDecimalFromBaseXX((Sender as TEdit).text,
        (Sender as TEdit).tag);
      for var edt in NumberEditList do
        edt.text := TOlfNumberConversion.FromDecimalToBaseXX(nb, edt.tag);
    end;
  finally
    ChangeInProgress := false;
  end;

  assert(edtBase2.text = TOlfNumberConversion.DecimalToBinary(nb),
    'Conversion decimal vers binaire erronée.');
  assert(edtBase8.text = TOlfNumberConversion.DecimalToOctal(nb),
    'Conversion decimal vers octal erronée.');
  assert(edtBase10.text = TOlfNumberConversion.DecimalToDecimal(nb),
    'Conversion decimal vers décimal erronée.');
  assert(edtBase16.text = TOlfNumberConversion.DecimalToHexadecimal(nb),
    'Conversion decimal vers hexadecimal erronée.');
  assert(edtBase36.text = TOlfNumberConversion.DecimalToBase36(nb),
    'Conversion decimal vers base36 erronée.');
  assert(edtBase62.text = TOlfNumberConversion.DecimalToBase62(nb),
    'Conversion decimal vers base62 erronée.');
end;

procedure TForm1.edtBaseXXEnter(Sender: TObject);
begin
  (Sender as TEdit).SelectAll;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  base: byte;
  i: integer;
  c: tcomponent;
begin
  NumberEditList := TList<TEdit>.create;

  ChangeInProgress := true;
  try
    for i := 0 to ComponentCount - 1 do
    begin
      c := components[i];
      if c is TLabel then
        with c as TLabel do
        begin
          base := string(name).Substring('lblBase'.length).tointeger;
          text := 'Base ' + base.tostring;
        end
      else if c is TEdit then
        with c as TEdit do
        begin
          base := string(name).Substring('edtBase'.length).tointeger;
          text := '';
          textprompt := 'base ' + base.tostring;
          tag := base;
          onChange := edtBaseXXChange;
          onEnter := edtBaseXXEnter;
          NumberEditList.add(c as TEdit);
        end;
    end;
  finally
    ChangeInProgress := false;
  end;
end;

end.
