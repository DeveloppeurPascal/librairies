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
