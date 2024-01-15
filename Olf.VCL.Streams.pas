unit Olf.VCL.Streams;

interface

uses
  VCL.Graphics,
  System.SysUtils,
  System.Classes;

procedure SaveBitmapToStream(ABitmap: TBitmap; AToStream: TStream);
function LoadBitmapFromStream(AFromStream: TStream): TBitmap;

implementation

procedure SaveBitmapToStream(ABitmap: TBitmap; AToStream: TStream);
var
  ms: TMemoryStream;
  size: int64;
begin
  if not assigned(AToStream) then
    raise exception.create('Need an existing stream to save the bitmap !');

  ms := TMemoryStream.create;
  try
    ABitmap.SaveToStream(ms);
    size := ms.size;
    AToStream.WriteData(size);
    if (size > 0) then
    begin
      ms.Position := 0;
      AToStream.CopyFrom(ms, size);
    end;
  finally
    ms.free;
  end;
end;

function LoadBitmapFromStream(AFromStream: TStream): TBitmap;
var
  ms: TMemoryStream;
  size: int64;
begin
  if not assigned(AFromStream) then
    raise exception.create('Need an existing stream to load the bitmap !');

  ms := TMemoryStream.create;
  try
    if (AFromStream.ReadData(size) <> sizeof(size)) then
      result := nil
    else
    begin
      ms.CopyFrom(AFromStream, size);
      ms.Position := 0;
      result := TBitmap.create;
      result.LoadFromStream(ms);
    end;
  finally
    ms.free;
  end;
end;

end.
