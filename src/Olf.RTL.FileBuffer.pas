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
  File last update : 2026-03-30T16:35:19.662+02:00
  Signature : cf3ad6ca9cc75fd16797c32927f5b75dbe579adb
  ***************************************************************************
*)

unit Olf.RTL.FileBuffer;

interface

uses
  System.Classes;

type
  TOlfFileBuffer = class
  private
    FFileInMemory: TMemoryStream;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(Const AFileName: string);
    procedure LoadFromFile(Const AFileName: string);
    procedure SaveToStream(Const AStream: TStream);
    procedure LoadFromStream(Const AStream: TStream);
    function Size: int64;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils;

{ TOlfFileBuffer }

constructor TOlfFileBuffer.Create;
begin
  inherited;
  FFileInMemory := TMemoryStream.Create;
end;

destructor TOlfFileBuffer.Destroy;
begin
  FFileInMemory.free;
  inherited;
end;

procedure TOlfFileBuffer.LoadFromFile(const AFileName: string);
begin
  FFileInMemory.Clear;
  if AFileName.IsEmpty then
    raise Exception.Create('Empty file name.');
  if not tfile.Exists(AFileName) then
    raise Exception.Create('This file doesn''t exist.');
  FFileInMemory.LoadFromFile(AFileName);
end;

procedure TOlfFileBuffer.LoadFromStream(const AStream: TStream);
var
  Size: Int64;
begin
  FFileInMemory.Clear;
  if not assigned(AStream) then
    raise Exception.Create('No stream to copy from.');
  if (sizeof(Size) <> AStream.Read(Size, sizeof(Size))) then
    raise Exception.Create('Wrong stream format.');
  if (Size > 0) and (Size <> FFileInMemory.CopyFrom(AStream, Size)) then
    raise Exception.Create('Copy incomplete.');
end;

procedure TOlfFileBuffer.SaveToFile(const AFileName: string);
begin
  if AFileName.IsEmpty then
    raise Exception.Create('Empty file name.');
  FFileInMemory.SaveToFile(AFileName);
end;

procedure TOlfFileBuffer.SaveToStream(const AStream: TStream);
var
  Size: Int64;
begin
  if not assigned(AStream) then
    raise Exception.Create('No stream to copy to.');
  Size := FFileInMemory.Size;
  AStream.Write(Size, sizeof(Size));
  if (Size > 0) then
  begin
    FFileInMemory.Position := 0;
    if (Size <> AStream.CopyFrom(FFileInMemory)) then
      raise Exception.Create('Copy incomplete.');
  end;
end;

function TOlfFileBuffer.Size: int64;
begin
  result := FFileInMemory.Size;
end;

end.
