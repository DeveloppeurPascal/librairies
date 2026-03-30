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
  File last update : 2026-03-30T16:35:19.664+02:00
  Signature : 2ce31ad92d01bdebd6aca95d57986752d672e94e
  ***************************************************************************
*)

unit Olf.RTL.Streams;

interface

uses
  System.SysUtils,
  System.Classes;

procedure SaveStringToStream(AString: string; AStream: TStream); overload;
procedure SaveStringToStream(AString: string; AStream: TStream;
  AEncoding: TEncoding); overload;

function LoadStringFromStream(AStream: TStream): string; overload;
function LoadStringFromStream(AStream: TStream; AEncoding: TEncoding)
  : string; overload;

type
  IOlfLoadSaveStreamWithSize = interface
    procedure SaveToStreamWithSize(Const AStream: TStream);
    procedure LoadFromStreamWithSize(Const AStream: TStream);
  end;

function LoadSubStreamFromStream(const AFromStream, AToSubStream
  : TStream): boolean;
procedure SaveSubStreamToStream(const AFromSubStream, AToStream: TStream);

function DumpStream(const Stream: TStream; HexaDump: boolean = true): string;

implementation

uses
  Olf.RTL.Maths.Conversions;

procedure SaveStringToStream(AString: string; AStream: TStream);
begin
  SaveStringToStream(AString, AStream, TEncoding.UTF8);
end;

procedure SaveStringToStream(AString: string; AStream: TStream;
  AEncoding: TEncoding);
var
  StrLen: int64; // typeof(System.Classes.TStream.size)
  StrStream: TStringStream;
begin
  StrStream := TStringStream.Create(AString, AEncoding);
  try
    StrLen := StrStream.Size;
    AStream.write(StrLen, sizeof(StrLen));
    if (StrLen > 0) then
    begin
      StrStream.Position := 0;
      AStream.CopyFrom(StrStream);
    end;
  finally
    StrStream.Free;
  end;
end;

function LoadStringFromStream(AStream: TStream): string;
begin
  Result := LoadStringFromStream(AStream, TEncoding.UTF8);
end;

function LoadStringFromStream(AStream: TStream; AEncoding: TEncoding): string;
var
  StrLen: int64; // typeof(System.Classes.TStream.size)
  StrStream: TStringStream;
begin
  AStream.Read(StrLen, sizeof(StrLen));
  if (StrLen > 0) then
  begin
    StrStream := TStringStream.Create('', AEncoding);
    try
      StrStream.CopyFrom(AStream, StrLen);
      Result := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  end
  else
    Result := '';
end;

function LoadSubStreamFromStream(const AFromStream, AToSubStream
  : TStream): boolean;
var
  Size: int64;
begin
  if not assigned(AFromStream) then
    raise exception.Create('Need a FromStream instance !');
  if not assigned(AToSubStream) then
    raise exception.Create('Need a ToStream instance !');

  Result := (AFromStream.ReadData(Size) = sizeof(Size));
  if Result then
    AToSubStream.CopyFrom(AFromStream, Size);
end;

procedure SaveSubStreamToStream(const AFromSubStream, AToStream: TStream);
var
  Size: int64;
begin
  if not assigned(AFromSubStream) then
    raise exception.Create('Need a FromStream instance !');
  if not assigned(AToStream) then
    raise exception.Create('Need a ToStream instance !');

  Size := AFromSubStream.Size;
  AToStream.WriteData(Size);
  if (Size > 0) then
  begin
    AFromSubStream.Position := 0;
    AToStream.CopyFrom(AFromSubStream, Size);
  end;
end;

function DumpStream(const Stream: TStream; HexaDump: boolean): string;
var
  o: byte;
  Sortie: boolean;
begin
  Result := '';
  if assigned(Stream) then
  begin
    Stream.Position := 0;
    Sortie := false;
    while (Stream.Position < Stream.Size) and (not Sortie) do
    begin
      Sortie := sizeof(o) <> Stream.Read(o, sizeof(o));
      if not Sortie then
      begin
        if not Result.IsEmpty then
          Result := Result + ', ';
        if HexaDump then
          Result := Result + TOlfNumberConversion.DecimalToHexadecimal(o, true)
        else
          Result := Result + o.tostring;
      end;
    end;
  end;
end;

end.
