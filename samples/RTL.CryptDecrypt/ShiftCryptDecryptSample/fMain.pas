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
    procedure LogStream(Const Txt: string; Const Stream: TStream);
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
  ms1 := nil;

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
