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
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  IdHashMessageDigest,
  u_md5;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.lines.Add(Edit1.text);
  Memo1.lines.Add(md5(Edit1.text));
  Memo1.lines.Add('--------------------');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.text := 'A record that implements the MD5 hash type.';
end;

end.
