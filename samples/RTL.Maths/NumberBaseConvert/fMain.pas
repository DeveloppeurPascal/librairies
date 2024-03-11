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
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.StdCtrls,
  System.Generics.Collections;

type
  TForm1 = class(TForm)
    lblBase2: TLabel;
    edtBase2: TEdit;
    lblBase62: TLabel;
    edtBase62: TEdit;
    lblBase57: TLabel;
    edtBase57: TEdit;
    lblBase36: TLabel;
    edtBase36: TEdit;
    lblBase27: TLabel;
    edtBase27: TEdit;
    lblBase16: TLabel;
    edtBase16: TEdit;
    lblBase10: TLabel;
    edtBase10: TEdit;
    lblBase8: TLabel;
    edtBase8: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    ChangeInProgress: boolean;
  protected
    NumberEditList: TList<TEdit>;
    procedure edtBaseXXChange(Sender: TObject);
    procedure edtBaseXXEnter(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.RTL.Maths.Conversions;

procedure TForm1.edtBaseXXChange(Sender: TObject);
var
  nb: uint64;
begin
  if ChangeInProgress then
    exit;
  ChangeInProgress := true;
  try
    if (Sender is TEdit) and ((Sender as TEdit).tag > 0) then
    begin
      nb := TOlfNumberConversion.ToDecimalFromBaseXX((Sender as TEdit).text,
        (Sender as TEdit).tag);
      for var edt in NumberEditList do
        edt.text := TOlfNumberConversion.FromDecimalToBaseXX(nb, edt.tag);
    end;
  finally
    ChangeInProgress := false;
  end;

  assert(edtBase2.text = TOlfNumberConversion.DecimalToBinary(nb),
    'Conversion decimal vers binaire erronée.');
  assert(edtBase8.text = TOlfNumberConversion.DecimalToOctal(nb),
    'Conversion decimal vers octal erronée.');
  assert(edtBase10.text = TOlfNumberConversion.DecimalToDecimal(nb),
    'Conversion decimal vers décimal erronée.');
  assert(edtBase16.text = TOlfNumberConversion.DecimalToHexadecimal(nb),
    'Conversion decimal vers hexadecimal erronée.');
  assert(edtBase36.text = TOlfNumberConversion.DecimalToBase36(nb),
    'Conversion decimal vers base36 erronée.');
  assert(edtBase62.text = TOlfNumberConversion.DecimalToBase62(nb),
    'Conversion decimal vers base62 erronée.');
end;

procedure TForm1.edtBaseXXEnter(Sender: TObject);
begin
  (Sender as TEdit).SelectAll;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  base: byte;
  i: integer;
  c: tcomponent;
begin
  NumberEditList := TList<TEdit>.create;

  ChangeInProgress := true;
  try
    for i := 0 to ComponentCount - 1 do
    begin
      c := components[i];
      if c is TLabel then
        with c as TLabel do
        begin
          base := string(name).Substring('lblBase'.length).tointeger;
          text := 'Base ' + base.tostring;
        end
      else if c is TEdit then
        with c as TEdit do
        begin
          base := string(name).Substring('edtBase'.length).tointeger;
          text := '';
          textprompt := 'base ' + base.tostring;
          tag := base;
          onChange := edtBaseXXChange;
          onEnter := edtBaseXXEnter;
          NumberEditList.add(c as TEdit);
        end;
    end;
  finally
    ChangeInProgress := false;
  end;
end;

end.
