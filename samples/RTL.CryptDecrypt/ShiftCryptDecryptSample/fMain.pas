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
  File last update : 2026-03-30T18:47:02.000+02:00
  Signature : 0957bc0c10855cd69573ea92b6a1192519cd4aba
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
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    procedure LogStream(const Txt: string; const Stream: TStream);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.RTL.CryptDecrypt,
  Olf.RTL.Maths.Conversions;

procedure TForm1.Button1Click(Sender: TObject);
var
  key: TIntegerDynArray;
  i: integer;
  s: string;
  ss: TStringStream;
  ms1, ms2: TStream;
begin
  key := TOlfCryptDecrypt.GenShiftKey(random(21) + 5);
  s := '';
  for i := 0 to length(key) - 1 do
    s := s + ' ' + key[i].tostring;
  Memo1.Lines.Add('Key : ' + s);

  ss := TStringStream.Create(Edit1.Text);
  try
    Memo1.Lines.Add(ss.DataString);
    LogStream('Original', ss);

    ms1 := TOlfCryptDecrypt.ShiftCrypt(ss, key);
    LogStream('Crypted', ms1);
  finally
    ss.free;
  end;

  ms2 := TOlfCryptDecrypt.ShiftDecrypt(ms1, key);
  try
    LogStream('Uncrypted', ms2);

    ss := TStringStream.Create;
    try
      ms2.Position := 0;
      ss.CopyFrom(ms2);
      Memo1.Lines.Add(ss.DataString);
    finally
      ss.free;
    end;
  finally
    ms2.free;
  end;
  ms1.free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text :=
    'This template is distributed under the MIT license. The AGPL license is for the projects I create from this code repository template. Use whatever license you want if you use this template.';
end;

procedure TForm1.LogStream(const Txt: string; const Stream: TStream);
var
  i: integer;
  s: string;
  o: byte;
begin
  // TODO : if not assigned(astream) then raise
  s := Txt + ' :';
  Stream.Position := 0;
  for i := 1 to Stream.size do
  begin
    Stream.read(o, 1);
    s := s + ' ' + o.tostring;
  end;
  Memo1.Lines.Add(s);
end;

initialization

  ReportMemoryLeaksOnShutdown := true;

end.

