(* C2PP
  ***************************************************************************

  My libraries for Delphi
  Copyright (c) 1990-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  This repository contains functions, procedures and classes to use in
  Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.

  The units to be used in your projects can be found in the "src" folder.
  Some features are explained on my blog or have been coded live on Twitch.

  Examples of use in the form of VCL or FireMonkey projects are available in
  the "samples" subfolder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://librairies.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/librairies

  ***************************************************************************
  File last update : 2026-03-30T16:35:19.563+02:00
  Signature : 31d69223c458105819da0836b4324fbcc2d1c1ab
  ***************************************************************************
*)

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
