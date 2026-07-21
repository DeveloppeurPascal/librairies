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
  FMX.StdCtrls,
  FMX.DateTimeCtrls,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo;

type
  TForm14 = class(TForm)
    edtInteger: TEdit;
    edtSingle: TEdit;
    edtDate: TDateEdit;
    btnInitialize: TButton;
    btnLoadSettings: TButton;
    btnSaveSettings: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnInitializeClick(Sender: TObject);
    procedure btnLoadSettingsClick(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
  private
    FSettingsFilePath: string;
  protected
    procedure SettingsInit;
    procedure SettingsLoad;
    procedure SettingsSave;
    procedure ShowFile;
  public
  end;

var
  Form14: TForm14;

implementation

{$R *.fmx}

uses
  Olf.RTL.Params,
  System.IOUtils;

procedure TForm14.btnInitializeClick(Sender: TObject);
begin
  SettingsInit;
end;

procedure TForm14.btnLoadSettingsClick(Sender: TObject);
begin
  SettingsLoad;
  ShowFile;
end;

procedure TForm14.btnSaveSettingsClick(Sender: TObject);
begin
  SettingsSave;
  ShowFile;
end;

procedure TForm14.FormCreate(Sender: TObject);
begin
  FSettingsFilePath := tpath.GetTempFileName;
  memo1.lines.Clear;
  memo1.lines.add('DecimalSeparator: "' + FormatSettings.DecimalSeparator +
    '"');
end;

procedure TForm14.FormDestroy(Sender: TObject);
begin
  if TFile.Exists(FSettingsFilePath) then
    TFile.Delete(FSettingsFilePath);
  FSettingsFilePath := '';
end;

procedure TForm14.SettingsInit;
begin
  edtInteger.Text := '';
  edtSingle.Text := '';
  edtDate.Date := now;
end;

procedure TForm14.SettingsLoad;
var
  Params: TParamsFile;
  i: integer;
  s: single;
  dt: tdate;
begin
  Params := TParamsFile.Create(FSettingsFilePath);
  try
    i := params.getValue('i', Integer(0));
    edtInteger.Text := i.ToString;
    s := params.getValue('s', Single(0.0));
    //s := params.GetValue<single>('s', 0);
    edtSingle.Text := s.ToString;
    dt := params.getValue('dt', TDateTime(Now));
    edtDate.Date := dt;

    memo1.lines.add('');
    memo1.lines.Add(string.Create('-', 20));
    memo1.lines.add('');
    dt := params.getValue('dtDelphi', TDateTimeStorrageFormat.Delphi, Now);
    memo1.lines.add('dtDelphi=' + DateTimeToStr(dt));
    dt := params.getValue('dtDelphi', Now);
    memo1.lines.add('dtDelphi=' + DateTimeToStr(dt));
    dt := params.getValue('dtISO8601', TDateTimeStorrageFormat.ISO8601, Now);
    memo1.lines.add('dtISO8601=' + DateTimeToStr(dt));
    dt := params.getValue('dtISO8601', Now);
    memo1.lines.add('dtISO8601=' + DateTimeToStr(dt));
    dt := params.getValue('dtRFC822', TDateTimeStorrageFormat.RFC822, Now);
    memo1.lines.add('dtRFC822=' + DateTimeToStr(dt));
    dt := params.getValue('dtRFC822', Now);
    memo1.lines.add('dtRFC822=' + DateTimeToStr(dt));

  finally
    Params.Free;
  end;
end;

procedure TForm14.SettingsSave;
var
  Params: TParamsFile;
  i: integer;
  s: single;
  dt: tdate;
begin
  Params := TParamsFile.Create(FSettingsFilePath);
  try
    i := edtInteger.Text.ToInteger;
    params.setValue('i', i);
    s := edtSingle.Text.ToSingle;
    // Perte de précision sur la conversion String To Single
    params.setValue('s', s);
    dt := edtDate.Date;
    params.setValue('dt', dt);
    params.setValue('dtDelphi', dt, TDateTimeStorrageFormat.Delphi);
    params.setValue('dtISO8601', dt, TDateTimeStorrageFormat.ISO8601);
    params.setValue('dtRFC822', dt, TDateTimeStorrageFormat.RFC822);
  finally
    Params.Free;
  end;
end;

procedure TForm14.ShowFile;
begin
  memo1.lines.add('');
  memo1.lines.Add(string.Create('-', 20));
  memo1.lines.add('');
  if tfile.Exists(FSettingsFilePath) then
    memo1.lines.Add(tfile.ReadAllText(FSettingsFilePath))
  else
    memo1.lines.add('File "' + FSettingsFilePath + '" doesn''t exist !');
  memo1.GoToTextEnd;
end;

end.

