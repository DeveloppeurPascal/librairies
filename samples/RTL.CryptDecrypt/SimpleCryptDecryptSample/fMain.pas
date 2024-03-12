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
  KeyToString := 'Key :';
  nb := random(length(s) + 1);
  setlength(key, nb);
  for i := 0 to nb - 1 do
  begin
    key[i] := random(255);
    if (i = 0) then
      while (key[0] in [0, 255]) do
        key[0] := random(255);
    KeyToString := KeyToString + ' ' + key[i].tostring;
  end;

  crypt := TOlfCryptDecrypt.create(key);
  try
    Memo1.lines.add(s);
    Memo1.lines.add('s.length=' + s.length.tostring);

    // Crypt the string
    ss := TStringStream.create(s);
    try
      Memo1.lines.add('ss.size=' + ss.size.tostring);
      ShowStreamContent('ss', ss);
      cs := crypt.crypt(ss);
      Memo1.lines.add('cs.size=' + cs.size.tostring);
      ShowStreamContent('cs', cs);
    finally
      ss.free;
    end;

    // Uncrypt the string
    dcs := crypt.Decrypt(cs);
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

  // Init the private key
  KeyToString := 'Key :';
  nb := random(length(s) + 1);
  setlength(key, nb);
  for i := 0 to nb - 1 do
  begin
    key[i] := random(255);
    if (i = 0) then
      while (key[0] in [0, 255]) do
        key[0] := random(255);
    KeyToString := KeyToString + ' ' + key[i].tostring;
  end;

  Memo1.lines.add(s);
  Memo1.lines.add('s.length=' + s.length.tostring);

  // Crypt the string
  ss := TStringStream.create(s);
  try
    Memo1.lines.add('ss.size=' + ss.size.tostring);
    ShowStreamContent('ss', ss);
    cs := TOlfCryptDecrypt.crypt(ss, key);
    Memo1.lines.add('cs.size=' + cs.size.tostring);
    ShowStreamContent('cs', cs);
  finally
    ss.free;
  end;

  // Uncrypt the string
  dcs := TOlfCryptDecrypt.Decrypt(cs, key);
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
