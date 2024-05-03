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
  if (Size > 0) then
    FFileInMemory.CopyFrom(AStream, Size);
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
    AStream.CopyFrom(FFileInMemory);
  end;
end;

end.
