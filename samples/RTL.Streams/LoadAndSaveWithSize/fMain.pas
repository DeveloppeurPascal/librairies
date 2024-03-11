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
  Olf.RTL.Streams, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TForm1 = class(TForm, IOlfLoadSaveStreamWithSize)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
  public
    procedure LoadFromStreamWithSize(const AStream: TStream);
    procedure SaveToStreamWithSize(const AStream: TStream);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  ms: tmemorystream;
begin
  ms := tmemorystream.create;
  try
    SaveToStreamWithSize(ms);
    ms.Position := 0;
    LoadFromStreamWithSize(ms);
  finally
    ms.free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := tguid.NewGuid.ToString;
end;

procedure TForm1.LoadFromStreamWithSize(const AStream: TStream);
var
  s: string;
begin
  s := LoadStringFromStream(AStream);
  Memo1.lines.add(s);
end;

procedure TForm1.SaveToStreamWithSize(const AStream: TStream);
begin
  SaveStringToStream(Edit1.Text, AStream);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
