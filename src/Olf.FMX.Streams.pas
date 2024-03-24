unit Olf.FMX.Streams;

interface

uses
  FMX.Graphics,
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

  if not assigned(ABitmap) or (ABitmap.Width = 0) or (ABitmap.Height = 0) then
  begin
    size := 0;
    AToStream.WriteData(size);
  end
  else
  begin
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
end;

function LoadBitmapFromStream(AFromStream: TStream): TBitmap;
var
  ms: TMemoryStream;
  size: int64;
begin
  if not assigned(AFromStream) then
    raise exception.create('Need an existing stream to load the bitmap !');

  if (AFromStream.ReadData(size) <> sizeof(size)) then
    result := nil
  else if (size < 1) then
    result := nil
  else
  begin
    ms := TMemoryStream.create;
    try
      ms.CopyFrom(AFromStream, size);
      ms.Position := 0;
      result := TBitmap.create;
      result.LoadFromStream(ms);
    finally
      ms.free;
    end;
  end;
end;

end.
