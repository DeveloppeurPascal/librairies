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
  File last update : 2026-03-30T16:35:19.535+02:00
  Signature : e23a85e55046d87dff3d2eea554d3f1e0c4b07ee
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Edit, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    btnCalculateChecksumAndCheckIt: TButton;
    Memo1: TMemo;
    edtKey5: TEdit;
    edtKey4: TEdit;
    edtParam: TEdit;
    edtKey3: TEdit;
    edtKey2: TEdit;
    edtKey1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnCalculateChecksumAndCheckItClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.RTL.Checksum;

procedure TForm1.btnCalculateChecksumAndCheckItClick(Sender: TObject);
var
  cs: string;
begin
  cs := TOlfChecksumVerif.get(edtParam.Text);
  if TOlfChecksumVerif.check(cs, edtParam.Text) then
    Memo1.lines.Add('"' + cs + '" ok for 0 key')
  else
    Memo1.lines.Add('"' + cs + '" KO for 0 key');

  cs := TOlfChecksumVerif.get(edtParam.Text, edtKey1.Text);
  if TOlfChecksumVerif.check(cs, edtParam.Text, edtKey1.Text) then
    Memo1.lines.Add('"' + cs + '" ok for 1 key')
  else
    Memo1.lines.Add('"' + cs + '" KO for 1 key');

  cs := TOlfChecksumVerif.get(edtParam.Text, edtKey1.Text, edtKey2.Text);
  if TOlfChecksumVerif.check(cs, edtParam.Text, edtKey1.Text, edtKey2.Text) then
    Memo1.lines.Add('"' + cs + '" ok for 2 key')
  else
    Memo1.lines.Add('"' + cs + '" KO for 2 key');

  cs := TOlfChecksumVerif.get(edtParam.Text, edtKey1.Text, edtKey2.Text,
    edtKey3.Text);
  if TOlfChecksumVerif.check(cs, edtParam.Text, edtKey1.Text, edtKey2.Text,
    edtKey3.Text) then
    Memo1.lines.Add('"' + cs + '" ok for 3 key')
  else
    Memo1.lines.Add('"' + cs + '" KO for 3 key');

  cs := TOlfChecksumVerif.get(edtParam.Text, edtKey1.Text, edtKey2.Text,
    edtKey3.Text, edtKey4.Text);
  if TOlfChecksumVerif.check(cs, edtParam.Text, edtKey1.Text, edtKey2.Text,
    edtKey3.Text, edtKey4.Text) then
    Memo1.lines.Add('"' + cs + '" ok for 4 key')
  else
    Memo1.lines.Add('"' + cs + '" KO for 4 key');

  cs := TOlfChecksumVerif.get(edtParam.Text, edtKey1.Text, edtKey2.Text,
    edtKey3.Text, edtKey4.Text, edtKey5.Text);
  if TOlfChecksumVerif.check(cs, edtParam.Text, edtKey1.Text, edtKey2.Text,
    edtKey3.Text, edtKey4.Text, edtKey5.Text) then
    Memo1.lines.Add('"' + cs + '" ok for 5 key')
  else
    Memo1.lines.Add('"' + cs + '" KO for 5 key');

  cs := '';
  if TOlfChecksumVerif.check(cs, edtParam.Text, edtKey1.Text, edtKey2.Text,
    edtKey3.Text, edtKey4.Text, edtKey5.Text) then
    Memo1.lines.Add('"' + cs + '" ok for 5 key')
  else
    Memo1.lines.Add('"' + cs + '" KO for 5 key');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edtParam.Text :=
    'J''suis défoncé comme un enfant-soldat qui ride comme BMX. J''suis dans la matrice, j''suis dans la XXX. J''roule un stick et j''la fiste, en naviguant sur les rivières du Styx. J''y vais fort pour mon fils, j''contrôle le terrain comme Ryan Giggs.';
  edtKey1.Text := tguid.NewGuid.ToString;
  edtKey2.Text := tguid.NewGuid.ToString;
  edtKey3.Text := tguid.NewGuid.ToString;
  edtKey4.Text := tguid.NewGuid.ToString;
  edtKey5.Text := tguid.NewGuid.ToString;
end;

end.
