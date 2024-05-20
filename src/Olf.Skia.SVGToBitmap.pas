unit Olf.Skia.SVGToBitmap;

interface

{$IF Defined(FRAMEWORK_VCL)}

uses
  VCL.Graphics;
{$ELSE IF Defined(FRAMEWORK_FMX)}

uses
  FMX.Graphics;
{$ELSE}
{$MESSAGE FATAL 'Is it a VCL or FMX program ?'}
{$ENDIF}
function SVGToBitmap(Const Width, Height: integer;
  Const SVGSource: string{$IF Defined(FRAMEWORK_FMX)};
  Const BitmapScale: single = 1{$ENDIF}): TBitmap;

implementation

uses
{$IF Defined(FRAMEWORK_VCL)}
  VCL.Skia,
{$ELSE IF Defined(FRAMEWORK_FMX)}
  FMX.Skia,
{$ENDIF}
  System.Skia,
  System.Types;

function SVGToBitmap(Const Width, Height: integer;
  Const SVGSource: string{$IF Defined(FRAMEWORK_FMX)};
  Const BitmapScale: single{$ENDIF}): TBitmap;
begin {$IF Defined(FRAMEWORK_FMX)}
  result := TBitmap.Create(trunc(Width * BitmapScale),
    trunc(Height * BitmapScale)); {$ELSE}
  result := TBitmap.Create(trunc(Width), trunc(Height)); {$ENDIF}
  try
{$IF Defined(FRAMEWORK_FMX)}
    result.BitmapScale := BitmapScale;
{$ENDIF}
    result.SkiaDraw(
      procedure(const ACanvas: ISKCanvas)
      var
        LSvgBrush: TSkSvgBrush;
      begin
        LSvgBrush := TSkSvgBrush.Create;
        try
          LSvgBrush.Source := SVGSource;
          LSvgBrush.Render(ACanvas, RectF(0, 0, Width, Height), 1);
        finally
          LSvgBrush.Free;
        end;
      end);
  except
    result.Free;
    result := nil;
  end;
end;

end.
