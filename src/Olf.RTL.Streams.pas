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
