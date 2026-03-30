(* C2PP
  ***************************************************************************

  My libraries for Delphi

  Copyright 1990-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

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
  File last update : 2025-02-09T11:03:59.095+01:00
  Signature : c55af0c7a929bdce5a30b7c581611ae0cc668558
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
    procedure LogStream(const Txt: string; const Stream: TStream);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.RTL.CryptDecrypt;

procedure TForm1.Button1Click(Sender: TObject);
var
  key: TIntegerDynArray;
  i: integer;
  s: string;
  ss: TStringStream;
  ms1, ms2: TStream;
begin
  key := TOlfCryptDecrypt.GenIDBKey(random(21) + 5);
  s := '';
  for i := 0 to length(key) - 1 do
    s := s + ' ' + key[i].tostring;
  Memo1.Lines.Add('Key : ' + s);

  ss := TStringStream.Create(Edit1.Text);
  try
    Memo1.Lines.Add(ss.DataString);
    LogStream('Original', ss);

    ms1 := TOlfCryptDecrypt.IDBCrypt(ss, key);
    LogStream('Crypted', ms1);
  finally
    ss.free;
  end;

  ms2 := TOlfCryptDecrypt.IDBDecrypt(ms1, key);
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

