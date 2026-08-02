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
  File last update : 2026-08-02T21:10:40.000+02:00
  Signature : e819bcb1325cc0b9749e214b2c8b30b3cca0e248
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
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit;

type
  TfrmMain = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  Olf.RTL.CryptDecrypt;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  keyB: TByteDynArray;
  keyI: TIntegerDynArray;
  s: string;
begin
  Memo1.lines.Add(Edit1.Text);
  Memo1.lines.Add(string.Create('-', 20));

  Memo1.lines.Add('IDB :');
  keyI := TOlfCryptDecrypt.GenIDBKey(30);
  s := TOlfCryptDecrypt.IDBCrypt(Edit1.Text, keyI);
  Memo1.lines.Add(s);
  Memo1.lines.Add('');
  Memo1.lines.Add(TOlfCryptDecrypt.IDBDecrypt(s, keyI));
  Memo1.lines.Add(string.Create('-', 20));

  Memo1.lines.Add('Shift :');
  keyI := TOlfCryptDecrypt.GenShiftKey(30);
  s := TOlfCryptDecrypt.ShiftCrypt(Edit1.Text, keyI);
  Memo1.lines.Add(s);
  Memo1.lines.Add('');
  Memo1.lines.Add(TOlfCryptDecrypt.ShiftDecrypt(s, keyI));
  Memo1.lines.Add(string.Create('-', 20));

  Memo1.lines.Add('Swap :');
  keyB := TOlfCryptDecrypt.GenSwapKey;
  s := TOlfCryptDecrypt.SwapCrypt(Edit1.Text, keyB);
  Memo1.lines.Add(s);
  Memo1.lines.Add('');
  Memo1.lines.Add(TOlfCryptDecrypt.SwapDecrypt(s, keyB));
  Memo1.lines.Add(string.Create('-', 20));

  Memo1.lines.Add('XOR :');
  keyB := TOlfCryptDecrypt.GenXORKey(30);
  s := TOlfCryptDecrypt.XORCrypt(Edit1.Text, keyB);
  Memo1.lines.Add(s);
  Memo1.lines.Add('');
  Memo1.lines.Add(TOlfCryptDecrypt.XORDecrypt(s, keyB));
  Memo1.lines.Add(string.Create('-', 20));
end;

initialization
  ReportMemoryLeaksOnShutdown := true;
end.

