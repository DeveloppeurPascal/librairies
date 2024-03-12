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
  FMX.StdCtrls,
  FMX.Edit;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure ShowParamsValues;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Olf.RTL.Params;

procedure TForm2.Button1Click(Sender: TObject);
begin
  tparams.setValue('key' + random(10).tostring, random(maxint));
  tparams.Save;
  ShowParamsValues;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  tparams.Remove(Edit1.Text);
  tparams.Save;
  ShowParamsValues;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  tparams.InitDefaultFileNameV2('OlfSoftwareSamples', 'RemoveKeySample');
  tparams.load;
  ShowParamsValues;
end;

procedure TForm2.ShowParamsValues;
var
  i: Integer;
  v: Integer;
begin
  Memo1.lines.add('----------');
  for i := 0 to 9 do
  begin
    v := tparams.getvalue('key' + i.tostring, -1);
    Memo1.lines.add('key' + i.tostring + '=' + v.tostring);
    if (v > -1) then
      Edit1.Text := 'key' + i.tostring;
  end;
  Memo1.GoToTextEnd;
end;

end.
