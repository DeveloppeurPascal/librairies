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
  File last update : 2026-03-30T18:47:10.000+02:00
  Signature : ee1ce52cc329b17997053215e63ea043daebfd16
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
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  Olf.RTL.Streams;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: tmemorystream;
  size: int64;
begin
  if Edit1.text.isEmpty then
    exit;

  s := tmemorystream.create;
  try
    SaveStringToStream(Edit1.text, s);
    size := s.size;
    s.write(size, sizeof(size));
    SaveStringToStream(Edit1.text, s);
    //
    s.position := 0;
    Memo1.lines.LoadFromStream(s, TEncoding.UTF8);
    //
    Memo1.lines.add('----------');
    Memo1.lines.add(DumpStream(s));
    Memo1.lines.add('----------');
    Memo1.lines.add(DumpStream(s, false));
  finally
    s.free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s: tmemorystream;
  size: int64;
begin
  if Edit1.text.isEmpty then
    exit;

  s := tmemorystream.create;
  try
    SaveStringToStream(Edit1.text, s);
    size := s.size;
    s.write(size, sizeof(size));
    SaveStringToStream(Edit1.text, s);
    //
    Memo1.lines.add('----------');
    //
    s.position := 0;
    Memo1.lines.add(LoadStringFromStream(s));
    s.read(size, sizeof(size));
    Memo1.lines.add(size.tostring);
    Memo1.lines.add(LoadStringFromStream(s));
    //
    Memo1.lines.add('----------');
    Memo1.lines.add(DumpStream(s));
    Memo1.lines.add('----------');
    Memo1.lines.add(DumpStream(s, false));
  finally
    s.free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  ch: string;
begin
  randomize;
  ch := '';
  for i := 0 to 50 do
    ch := ch + chr(ord('a') + random(26));
  Edit1.text := ch;
end;

end.

