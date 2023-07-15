unit Olf.RTL.Streams;

interface

uses
  System.SysUtils,
  System.Classes;

procedure SaveStringToStream(AString: string; AStream: TStream); overload;
procedure SaveStringToStream(AString: string; AStream: TStream; AEncoding: TEncoding); overload;

function LoadStringFromStream(AStream: TStream): string; overload;
function LoadStringFromStream(AStream: TStream; AEncoding: TEncoding): string; overload;

implementation

procedure SaveStringToStream(AString: string; AStream: TStream);
begin
  SaveStringToStream(AString, AStream, TEncoding.UTF8);
end;

procedure SaveStringToStream(AString: string; AStream: TStream; AEncoding: TEncoding);
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
      result := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  end
  else
    result := '';
end;

end.
