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
    procedure ShowStreamContent(Const AName: string; Const AStream: TStream);
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
  nb, i: integer;
  key: TByteDynArray;
  KeyToString: string;
  Bytes: TList<byte>;
begin
  s := Edit1.text;
  if s.isempty then
    raise exception.create('Please give me a string to crypt/decrypt.');

  // Init the private key
  setlength(key, 256);
  Bytes := TList<byte>.create;
  try
    for i := 0 to 255 do
      Bytes.add(i);
    for i := 0 to 255 do
    begin
      nb := random(Bytes.count);
      key[i] := Bytes[nb];
      Bytes.Delete(nb);
    end;
  finally
    Bytes.free;
  end;

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
