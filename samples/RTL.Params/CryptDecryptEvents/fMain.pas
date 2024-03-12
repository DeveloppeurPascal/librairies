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
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    Key: TByteDynArray;
    procedure KeyInit;
    procedure ShowParamsValues;
  public
    function DoCryptParams(Const AParams: string): TStream;
    function DoDecryptParams(Const AStream: TStream): string;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.RTL.CryptDecrypt,
  Olf.RTL.Params;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  tparams.setValue('key' + random(10).tostring, random(maxint));
  tparams.Save;
  ShowParamsValues;
end;

function TForm1.DoCryptParams(const AParams: string): TStream;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.create(AParams);
  try
    result := TOlfCryptDecrypt.Crypt(StringStream, Key);
  finally
    StringStream.free;
  end;
end;

function TForm1.DoDecryptParams(const AStream: TStream): string;
var
  StringStream: TStringStream;
  Stream: TStream;
begin
  Stream := TOlfCryptDecrypt.deCrypt(AStream, Key);
  try
    StringStream := TStringStream.create;
    try
      Stream.position := 0;
      StringStream.CopyFrom(Stream);
      result := StringStream.DataString;
    finally
      StringStream.free;
    end;
  finally
    Stream.free;
  end;
end;

procedure TForm1.ShowParamsValues;
var
  i: Integer;
  v: Integer;
begin
  Memo1.lines.add('----------');
  for i := 0 to 9 do
  begin
    v := tparams.getvalue('key' + i.tostring, -1);
    Memo1.lines.add('key' + i.tostring + '=' + v.tostring);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  KeyInit;
  //
  tparams.InitDefaultFileNameV2('OlfSoftwareSamples',
    'CryptDecryptParamsSample', false);
  tparams.onCryptEvent := DoCryptParams;
  tparams.onDecryptEvent := DoDecryptParams;
  tparams.load;
  //
  Memo1.lines.add('Params filename : ' + tparams.getFilePath);
  ShowParamsValues;
end;

procedure TForm1.KeyInit;
var
  nb: byte;
  i: byte;
begin
  setlength(Key, 5);
  Key[0] := 1;
  Key[1] := 2;
  Key[2] := 3;
  Key[3] := 4;
  Key[4] := 5;
  exit;

  nb := random(250) + 5;
  setlength(Key, nb);
  for i := 0 to nb - 1 do
    Key[i] := random(255);
  while (Key[0] in [0, 255]) do
    Key[0] := random(255);
end;

initialization

// randomize;
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
