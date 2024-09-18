unit fMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
  protected
    procedure LoadDeployement(const DPROJFilePath: string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  Olf.RTL.DPROJReader;

procedure TForm1.Button1Click(Sender: TObject);
var
  DPROJFilePath: string;
begin
  if OpenDialog1.Execute then
  begin
    DPROJFilePath := OpenDialog1.FileName;
    if not tfile.Exists(DPROJFilePath) then
      raise exception.create(DPROJFilePath + ' doesn''t exist!');
    LoadDeployement(DPROJFilePath);
  end;
end;

procedure TForm1.LoadDeployement(const DPROJFilePath: string);
var
  Reader: TOlfDPROJReader;
  Files: TOlfFilesToDeployList;
  i: integer;
begin
  Memo1.Clear;
  Reader := TOlfDPROJReader.create(DPROJFilePath);
  try
    Memo1.Lines.Add('Win32 DEBUG : ' + Reader.GetProjectExecutable('Win32',
      'Debug'));
    Files := Reader.GetFilesToDeploy('Win32', 'Debug');
    if assigned(Files) then
      try
        for i := 0 to Files.Count - 1 do
          Memo1.Lines.Add(' => ' + Files[i].FromPath + '\' +
            Files[i].FromFileName + ' || ' + Files[i].ToPath + '\' +
            Files[i].ToFileName);
      finally
        Files.Free;
      end;
    Memo1.Lines.Add('--------------------');
    Memo1.Lines.Add('Win64 DEBUG : ' + Reader.GetProjectExecutable('Win64',
      'Debug'));
    Files := Reader.GetFilesToDeploy('Win32', 'Debug');
    if assigned(Files) then
      try
        for i := 0 to Files.Count - 1 do
          Memo1.Lines.Add(' => ' + Files[i].FromPath + '\' +
            Files[i].FromFileName + ' || ' + Files[i].ToPath + '\' +
            Files[i].ToFileName);
      finally
        Files.Free;
      end;
    Memo1.Lines.Add('--------------------');
    Memo1.Lines.Add('Win32 RELEASE : ' + Reader.GetProjectExecutable('Win32',
      'Release'));
    Files := Reader.GetFilesToDeploy('Win32', 'Debug');
    if assigned(Files) then
      try
        for i := 0 to Files.Count - 1 do
          Memo1.Lines.Add(' => ' + Files[i].FromPath + '\' +
            Files[i].FromFileName + ' || ' + Files[i].ToPath + '\' +
            Files[i].ToFileName);
      finally
        Files.Free;
      end;
    Memo1.Lines.Add('--------------------');
    Memo1.Lines.Add('Win64 RELEASE : ' + Reader.GetProjectExecutable('Win64',
      'Release'));
    Files := Reader.GetFilesToDeploy('Win32', 'Debug');
    if assigned(Files) then
      try
        for i := 0 to Files.Count - 1 do
          Memo1.Lines.Add(' => ' + Files[i].FromPath + '\' +
            Files[i].FromFileName + ' || ' + Files[i].ToPath + '\' +
            Files[i].ToFileName);
      finally
        Files.Free;
      end;
    Memo1.Lines.Add('--------------------');
  finally
    Reader.Free;
  end;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
