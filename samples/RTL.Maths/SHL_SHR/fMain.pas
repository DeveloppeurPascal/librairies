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
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts;

type
  TForm2 = class(TForm)
    btnSHL: TButton;
    btnSHR: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    procedure btnSHLClick(Sender: TObject);
    procedure btnSHRClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Olf.RTL.Maths.Conversions;

procedure TForm2.btnSHLClick(Sender: TObject);
var
  s, d: word;
begin
  s := TOlfNumberConversion.BinaryToDecimal(Edit1.Text);
  d := s shl Edit2.Text.ToInteger;
  Memo1.lines.Add(Edit1.Text + ' shl ' + Edit2.Text + ' = ' +
    TOlfNumberConversion.DecimalToBinary(d));
  Edit1.setfocus;
end;

procedure TForm2.btnSHRClick(Sender: TObject);
var
  s, d: word;
begin
  s := TOlfNumberConversion.BinaryToDecimal(Edit1.Text);
  d := s shr Edit2.Text.ToInteger;
  Memo1.lines.Add(Edit1.Text + ' shr ' + Edit2.Text + ' = ' +
    TOlfNumberConversion.DecimalToBinary(d));
  Edit1.setfocus;
end;

end.
