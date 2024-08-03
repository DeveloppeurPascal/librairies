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
/// Signature : 2ff5dd158ad77f3ad60a755b49a529858f2c50c7
/// ***************************************************************************
/// </summary>

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit;

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
  ch: string;
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
