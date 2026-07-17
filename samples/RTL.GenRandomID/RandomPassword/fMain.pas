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
  FMX.EditBox,
  FMX.NumberBox,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation;

type
  TForm12 = class(TForm)
    cbNumbers: TCheckBox;
    cbLowerCaseLetters: TCheckBox;
    cbUpperCaseLetters: TCheckBox;
    edtSymbols: TEdit;
    btnGenerateWithSymbolsAsString: TButton;
    nbPasswordSize: TNumberBox;
    mmoPasswords: TMemo;
    btnGenerateWithSymbolsAsArray: TButton;
    procedure btnGenerateWithSymbolsAsStringClick(Sender: TObject);
    procedure btnGenerateWithSymbolsAsArrayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

uses
  Olf.RTL.GenRandomID;

procedure TForm12.btnGenerateWithSymbolsAsArrayClick(Sender: TObject);
  var
    Tab: TArray<Char>;
    i: Integer;
begin
  setlength(tab, edtSymbols.Text.Length);
  for i := 0 to length(tab) - 1 do
    tab[i] := edtSymbols.Text.Chars[i];
  mmoPasswords.lines.clear;
  for i := 1 to 10 do
    mmoPasswords.lines.Add(i.tostring + ': "' + TOlfRandomIDGenerator.getPassword(cbNumbers.IsChecked, cbLowerCaseLetters.IsChecked, cbUpperCaseLetters.IsChecked, tab, trunc(nbPasswordSize.Value)) + '"');
end;

procedure TForm12.btnGenerateWithSymbolsAsStringClick(Sender: TObject);
var
  i: Integer;
begin
  mmoPasswords.lines.clear;
  for i := 1 to 10 do
    mmoPasswords.lines.Add(i.tostring + ': "' + TOlfRandomIDGenerator.getPassword(cbNumbers.IsChecked, cbLowerCaseLetters.IsChecked, cbUpperCaseLetters.IsChecked, edtSymbols.Text, trunc(nbPasswordSize.Value)) + '"');
end;

      procedure TForm12.FormCreate(Sender: TObject);
begin
  nbPasswordSize.Value := 15;
end;

end.


