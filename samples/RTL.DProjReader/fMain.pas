/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This repository contains functions, procedures and classes to use in
/// Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.
///
/// The units to be used in your projects can be found in the "src" folder.
/// Some features are explained on my blog or have been coded live on Twitch.
///
/// Examples of use in the form of VCL or FireMonkey projects are available in
/// the "samples" subfolder.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://librairies.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 2024-09-18T09:54:34.000+02:00
/// Signature : 8d0031d1dd19ba4bf53c3a7ab0ce49558b39dd1f
/// ***************************************************************************
/// </summary>

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
