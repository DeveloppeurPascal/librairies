/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This repository contains functions, procedures and classes to use in
/// Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.
///
/// The units to be used in your projects can be found in the "src" folder.
/// Some features are explained on my blog or have been coded live on Twitch.
///
/// Examples of use in the form of VCL or FireMonkey projects are available in
/// the "samples" subfolder.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
///      https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:15
/// Signature : ef29caf4c69a5aadd1bd5c40b02c117eaab4243c
/// ***************************************************************************
/// </summary>

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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ShowStreamContent(Const AName: string; Const AStream: TStream);
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
  s: string;
  ss: TStringStream;
  cs, dcs: TStream;
  crypt: TOlfCryptDecrypt;
  nb, i: integer;
  key: TByteDynArray;
  KeyToString: string;
begin
  s := Edit1.text;
  if s.isempty then
    raise exception.create('Please give me a string to crypt/decrypt.');

  // Init the private key
  key := TOlfCryptDecrypt.GenXORKey(random(length(s) + 1));
  // nb := random(length(s) + 1);
  // setlength(key, nb);
  // for i := 0 to nb - 1 do
  // begin
  // key[i] := random(255);
  // if (i = 0) then
  // while (key[0] in [0, 255]) do
  // key[0] := random(255);
  // KeyToString := KeyToString + ' ' + key[i].tostring;
  // end;
  KeyToString := '';
  for i := 0 to length(key) - 1 do
    KeyToString := KeyToString + ' ' + key[i].tostring;
  Memo1.lines.add('key : ' + KeyToString);

  crypt := TOlfCryptDecrypt.create(key);
  try
    Memo1.lines.add(s);
    Memo1.lines.add('s.length=' + s.length.tostring);

    // Crypt the string
    ss := TStringStream.create(s);
    try
      Memo1.lines.add('ss.size=' + ss.size.tostring);
      ShowStreamContent('ss', ss);
      cs := crypt.XORCrypt(ss);
      Memo1.lines.add('cs.size=' + cs.size.tostring);
      ShowStreamContent('cs', cs);
    finally
      ss.free;
    end;

    // Uncrypt the string
    dcs := crypt.XORDecrypt(cs);
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
  finally
    crypt.free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s: string;
  ss: TStringStream;
  cs, dcs: TStream;
  nb, i: integer;
  key: TByteDynArray;
  KeyToString: string;
begin
  s := Edit1.text;
  if s.isempty then
    raise exception.create('Please give me a string to crypt/decrypt.');

  key := TOlfCryptDecrypt.GenXORKey(random(length(s) + 1));
  // nb := random(length(s) + 1);
  // setlength(key, nb);
  // for i := 0 to nb - 1 do
  // begin
  // key[i] := random(255);
  // if (i = 0) then
  // while (key[0] in [0, 255]) do
  // key[0] := random(255);
  // KeyToString := KeyToString + ' ' + key[i].tostring;
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
    cs := TOlfCryptDecrypt.XORCrypt(ss, key);
    Memo1.lines.add('cs.size=' + cs.size.tostring);
    ShowStreamContent('cs', cs);
  finally
    ss.free;
  end;

  // Uncrypt the string
  dcs := TOlfCryptDecrypt.XORDecrypt(cs, key);
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
  Edit1.text := 'add a CryptDecrypt unit with a simple reversible XOR method';
  Memo1.lines.Clear;
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

initialization

randomize;
ReportMemoryLeaksOnShutdown := true;

end.
