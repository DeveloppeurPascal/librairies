unit fParamsSample;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
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

{$R *.dfm}

uses System.IOUtils, Olf.RTL.Params;

procedure TForm1.Button1Click(Sender: TObject);
begin
  tParams.setValue('Edit1Text', Edit1.Text);
  Memo1.Lines.Add('Edit1Text=' + Edit1.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := tParams.getValue('Edit1Text', 'Default Value');
  Memo1.Lines.Add('Edit1Text=' + Edit1.Text);
end;

procedure initParamsFile;
var
  NewFolder, Oldfolder, FileName: string;
begin
  Oldfolder := tpath.Combine(tpath.GetPicturesPath, 'temp');
  NewFolder := tpath.Combine(tpath.GetDocumentsPath, 'temp');
  FileName := tpath.GetFileName(tParams.getFilePath);

  // Move actual Pictures/Temp parameter file to new Documents/Temp parameter file
  if tfile.Exists(tpath.Combine(Oldfolder, FileName)) then
  begin
    tParams.setFolderName(Oldfolder);
    tParams.MoveToFilePath(tpath.Combine(NewFolder, FileName), true, true);
  end
  else
    tParams.setFolderName(NewFolder);
end;

initialization

initParamsFile;

end.
