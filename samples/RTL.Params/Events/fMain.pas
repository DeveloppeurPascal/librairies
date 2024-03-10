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
  FMX.Layouts,
  FMX.Edit,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Olf.RTL.Params;

type
  TForm1 = class(TForm)
    btnLoad: TButton;
    btnSave: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    GridPanelLayout1: TGridPanelLayout;
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure BeforeLoad(Const AParamsFile: TParamsFile);
    procedure AfterLoad(Const AParamsFile: TParamsFile);
    procedure BeforeSave(Const AParamsFile: TParamsFile);
    procedure AfterSave(Const AParamsFile: TParamsFile);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.AfterLoad(const AParamsFile: TParamsFile);
begin
  Memo1.lines.add('AfterLoadEvent : ' + AParamsFile.ToJSON);
end;

procedure TForm1.AfterSave(const AParamsFile: TParamsFile);
begin
  if not assigned(self.owner) then
    exit;
  Memo1.lines.add('AfterSaveEvent : ' + AParamsFile.ToJSON);
end;

procedure TForm1.BeforeLoad(const AParamsFile: TParamsFile);
begin
  Memo1.lines.add('BeforeLoadEvent : ' + AParamsFile.ToJSON);
end;

procedure TForm1.BeforeSave(const AParamsFile: TParamsFile);
begin
  if not assigned(self.owner) then
    exit;
  Memo1.lines.add('BeforeSaveEvent : ' + AParamsFile.ToJSON);
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  tparams.load;
  Memo1.lines.add(tparams.ToJSON);
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  tparams.setValue('val' + random(20).ToString, random(maxint));
  tparams.save;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := tparams.getFilePath;
  tparams.onBeforeLoadEvent := BeforeLoad;
  tparams.onAfterLoadEvent := AfterLoad;
  tparams.onBeforeSaveEvent := BeforeSave;
  tparams.onAfterSaveEvent := AfterSave;
end;

initialization

finalization

tparams.onbeforesaveproc := procedure(const AParamsFile: TParamsFile)
  begin
    AParamsFile.onBeforeSaveEvent := nil;
    AParamsFile.onAfterSaveEvent := nil;
  end;

end.
