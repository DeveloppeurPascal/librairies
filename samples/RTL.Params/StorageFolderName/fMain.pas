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
  TfrmMain = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    btnDefaultFolder: TButton;
    btnDefaultFolderV2: TButton;
    Memo1: TMemo;
    procedure btnDefaultFolderClick(Sender: TObject);
    procedure btnDefaultFolderV2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  Olf.RTL.Params;

procedure TfrmMain.btnDefaultFolderClick(Sender: TObject);
begin
  tparams.setFilePath('');
  Memo1.lines.add(tparams.getFilePath);
end;

procedure TfrmMain.btnDefaultFolderV2Click(Sender: TObject);
begin
  tparams.InitDefaultFileNameV2(Edit1.Text, Edit2.Text);
  Memo1.lines.add(tparams.getFilePath);
end;

end.
