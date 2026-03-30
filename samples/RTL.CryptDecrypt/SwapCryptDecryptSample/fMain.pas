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
  File last update : 2026-03-30T18:47:04.000+02:00
  Signature : a9b52e5de7d1608074807a7af5f4844f5fd0d806
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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ShowStreamContent(const AName: string; const AStream: TStream);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Generics.Collections,
  Olf.RTL.CryptDecrypt,
  Olf.RTL.Maths.Conversions;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: string;
  ss: TStringStream;
  cs, dcs: TStream;
  i: integer;
  key: TByteDynArray;
  KeyToString: string;
begin
  s := Edit1.text;
  if s.isempty then
    raise exception.create('Please give me a string to crypt/decrypt.');

  // Init the private key
  key := TOlfCryptDecrypt.GenSwapKey;
  // setlength(key, 256);
  // Bytes := TList<byte>.create;
  // try
  // for i := 0 to 255 do
  // Bytes.add(i);
  // for i := 0 to 255 do
  // begin
  // nb := random(Bytes.count);
  // key[i] := Bytes[nb];
  // Bytes.Delete(nb);
  // end;
  // finally
  // Bytes.free;
  // end;

  KeyToString := '';
  for i := 0 to length(key) - 1 do
    KeyToString := KeyToString + ' ' + key[i].tostring;
  Memo1.lines.add('key : ' + KeyToString);

  Memo1.lines.add(s);
  Memo1.lines.add('s.length=' + s.length.tostring);

  // Crypt the string
  ss := TStringStream.create(s);
  try
    Memo1.lines.add('ss.size=' + ss.size.tostring);
    ShowStreamContent('ss', ss);
    cs := TOlfCryptDecrypt.SwapCrypt(ss, key);
    Memo1.lines.add('cs.size=' + cs.size.tostring);
    ShowStreamContent('cs', cs);
  finally
    ss.free;
  end;

  // Uncrypt the string
  dcs := TOlfCryptDecrypt.Swapdecrypt(cs, key);
  Memo1.lines.add('dcs.size=' + dcs.size.tostring);
  ShowStreamContent('dcs', dcs);
  cs.free;
  ss := TStringStream.create;
  try
    dcs.position := 0;
    ss.CopyFrom(dcs);
    ShowStreamContent('ss', ss);
    Memo1.lines.add('ss.size=' + ss.size.tostring);
    dcs.free;
    s := ss.DataString;
    Memo1.lines.add('s.length=' + s.length.tostring);
  finally
    ss.free;
  end;
  Memo1.lines.add(s);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.text :=
    'Cette température estivale de fin mars se paiera sur les cultures de fruits d''ici juin.';
end;

procedure TForm1.ShowStreamContent(const AName: string; const AStream: TStream);
var
  i: integer;
  s: string;
  o: byte;
begin
  // TODO : if not assigned(astream) then raise
  s := AName + ':';
  AStream.position := 0;
  for i := 1 to AStream.size do
  begin
    AStream.read(o, 1);
    s := s + ' ' + o.tostring;
  end;
  Memo1.lines.add(s);
end;

end.

