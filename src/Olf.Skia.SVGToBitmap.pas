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
///      Patrick PREMARTIN
///
/// Site :
///      https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
///      https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 01/08/2024 10:47:00
/// Signature : 2e545670f696ebf375fed52e96432b79a3144f1c
/// ***************************************************************************
/// </summary>

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

type
  TOlfSVGBitmapList = class
  private
  protected
  public
    /// <summary>
    /// Add a list in the global container
    /// </summary>
    class function AddAList: word;

    /// <summary>
    /// Remove the list at "index" from the global container
    /// </summary>
    /// <remarks>
    /// It doesn't change others lists indexes, it's internally stored in a dictionary, not a list.
    /// </remarks>
    class procedure DeleteList(Const Index: word);

    /// <summary>
    /// Add a SVG source to the list "ToList" at index "AtIndex"
    /// </summary>
    /// <remarks>
    /// It doesn't change others SVG indexes, it's internally stored in a dictionary, not a list.
    /// If an item exists at this position, the AddItemAt() is refused.
    /// </remarks>
    class function AddItemAt(Const ToList, AtIndex: word; const SVG: String)
      : boolean; overload;

    /// <summary>
    /// Add a SVG source to the default list at index "AtIndex"
    /// </summary>
    /// <remarks>
    /// It doesn't change others SVG indexes, it's internally stored in a dictionary, not a list.
    /// If an item exists at this position, the AddItemAt() is refused.
    /// </remarks>
    class function AddItemAt(AtIndex: word; const SVG: string)
      : boolean; overload;

    /// <summary>
    /// Add a SVG source to the list "ToList" and return it's index
    /// </summary>
    class function AddItem(Const ToList: word; const SVG: string)
      : word; overload;

    /// <summary>
    /// Add all items from an array of SVG sources to the list "ToList" and
    /// return the index of the first added item
    /// </summary>
    class function AddItem(Const ToList: word; const SVGArray: array of String)
      : word; overload;

    /// <summary>
    /// Add a SVG source to the default list and return it's index
    /// </summary>
    class function AddItem(const SVG: string): word; overload;

    /// <summary>
    /// Add all items from an array of SVG sources to the default list and
    /// return the index of the first added item
    /// </summary>
    class function AddItem(const SVGArray: array of string): word; overload;

    /// <summary>
    /// Remove the SVG "AtIndex" from the "FromList" list
    /// </summary>
    /// <remarks>
    /// It doesn't change others SVG indexes, it's internally stored in a dictionary, not a list.
    /// </remarks>
    class procedure DeleteItem(Const FromList, AtIndex: word); overload;

    /// <summary>
    /// Remove the SVG "AtIndex" from the default list
    /// </summary>
    /// <remarks>
    /// It doesn't change others SVG indexes, it's internally stored in a dictionary, not a list.
    /// </remarks>
    class procedure DeleteItem(Const AtIndex: word); overload;

    /// <summary>
    /// Get a bitmap from the SVG in "FromList" list at "AtIndex" with specified sizes.
    /// </summary>
    /// <remarks>
    /// If the bitmap doesn't exist in the cache, it's added to it after drawing.
    /// If the bitmap already exists in the cache, you get a reference to it.
    /// </remarks>
    class function Bitmap(Const FromList, AtIndex: word;
      const Width, Height: integer; const BitmapScale: single = 1)
      : TBitmap; overload;

    /// <summary>
    /// Get a bitmap from the SVG in "FromList" list at "AtIndex" with specified sizes.
    /// </summary>
    /// <remarks>
    /// If the bitmap doesn't exist in the cache, it's added to it after drawing.
    /// If the bitmap already exists in the cache, you get a reference to it.
    ///
    /// The margins values must be decimal values between 0 and 100 (as a percent).
    /// </remarks>
    class function Bitmap(Const FromList, AtIndex: word;
      const Width, Height: integer; const MarginTop: single;
      const MarginRight: single; const MarginBottom: single;
      const MarginLeft: single; const BitmapScale: single = 1)
      : TBitmap; overload;

    /// <summary>
    /// Get a bitmap from the SVG in default list at "AtIndex" with specified sizes.
    /// </summary>
    /// <remarks>
    /// If the bitmap doesn't exist in the cache, it's added to it after drawing.
    /// If the bitmap already exists in the cache, you get a reference to it.
    /// </remarks>
    class function Bitmap(Const AtIndex: word; const Width, Height: integer;
      const BitmapScale: single = 1): TBitmap; overload;

    /// <summary>
    /// Get a bitmap from the SVG in default list at "AtIndex" with specified sizes.
    /// </summary>
    /// <remarks>
    /// If the bitmap doesn't exist in the cache, it's added to it after drawing.
    /// If the bitmap already exists in the cache, you get a reference to it.
    ///
    /// The margins values must be decimal values between 0 and 100 (as a percent).
    /// </remarks>
    class function Bitmap(Const AtIndex: word; const Width, Height: integer;
      const MarginTop: single; const MarginRight: single;
      const MarginBottom: single; const MarginLeft: single;
      const BitmapScale: single = 1): TBitmap; overload;

    /// <summary>
    /// Get a bitmap from the SVG in "FromList" list at "AtIndex" with specified sizes.
    /// No cache is used, the bitmap is drawn each time you call this function.
    /// </summary>
    class function BitmapWithNoCache(const FromList, AtIndex: word;
      const Width, Height: integer; const BitmapScale: single = 1)
      : TBitmap; overload;

    /// <summary>
    /// Get a bitmap from the SVG in "FromList" list at "AtIndex" with specified sizes.
    /// No cache is used, the bitmap is drawn each time you call this function.
    /// </summary>
    /// <remarks>
    /// The margins values must be decimal values between 0 and 100 (as a percent).
    /// </remarks>
    class function BitmapWithNoCache(const FromList, AtIndex: word;
      const Width, Height: integer; const MarginTop: single;
      const MarginRight: single; const MarginBottom: single;
      const MarginLeft: single; const BitmapScale: single = 1)
      : TBitmap; overload;

    /// <summary>
    /// Get a bitmap from the SVG in default list at "AtIndex" with specified sizes.
    /// No cache is used, the bitmap is drawn each time you call this function.
    /// </summary>
    class function BitmapWithNoCache(const AtIndex: word;
      const Width, Height: integer; const BitmapScale: single = 1)
      : TBitmap; overload;

    /// <summary>
    /// Get a bitmap from the SVG in default list at "AtIndex" with specified sizes.
    /// No cache is used, the bitmap is drawn each time you call this function.
    /// </summary>
    /// <remarks>
    /// The margins values must be decimal values between 0 and 100 (as a percent).
    /// </remarks>
    class function BitmapWithNoCache(const AtIndex: word;
      const Width, Height: integer; const MarginTop: single;
      const MarginRight: single; const MarginBottom: single;
      const MarginLeft: single; const BitmapScale: single = 1)
      : TBitmap; overload;

    /// <summary>
    /// Anything you added is removed.
    /// Delete the cache, the items and the lists
    /// </summary>
    class procedure ClearAll;

    /// <summary>
    /// Delete the bitmap's cache.
    /// </summary>
    class procedure ClearCache;

    /// <summary>
    /// Return the number of items in the specified list or the default list
    /// </summary>
    class function Count(const FromList: word = 0): NativeInt;
  end;

  /// <summary>
  /// Returns a transparent bitmap drawn by Skia4Delphi library from a SVG source.
  /// </summary>
  /// <param name="Width">
  /// Logical width of the target bitmap. Its real width will be calculated ith the BitmapScale (in a FireMonkey project).
  /// </param>
  /// <param name="Height">
  /// Logical height of the target bitmap. Its real height will be calculated ith the BitmapScale (in a FireMonkey project).
  /// </param>
  /// <param name="SVGSource">
  /// XML source code of the SVG  as string.
  /// No CSS content allowed. Check default Adobe Illustrator settings if you have exports problems.
  /// </param>
  /// <param name="BitmapScale">
  /// Only used in FireMonkey projects. The BitmapScale is used to calculate the real bitmap size from logical given size.
  /// Default value is 1.
  /// </param>
  /// <param name="MarginTop">
  /// Margin top used to calculate the real SVG size and position in the final bitmap.
  /// It's a decimal value between 0 and 100, used as a percent of the bitmap height.
  /// </param>
  /// <param name="MarginRight">
  /// Margin right used to calculate the real SVG size and position in the final bitmap.
  /// It's a decimal value between 0 and 100, used as a percent of the bitmap width.
  /// </param>
  /// <param name="MarginBottom">
  /// Margin bottom used to calculate the real SVG size and position in the final bitmap.
  /// It's a decimal value between 0 and 100, used as a percent of the bitmap height.
  /// </param>
  /// <param name="MarginLeft">
  /// Margin left used to calculate the real SVG size and position in the final bitmap.
  /// It's a decimal value between 0 and 100, used as a percent of the bitmap width.
  /// </param>
function SVGToBitmap(Const Width, Height: integer;
  Const SVGSource: string{$IF Defined(FRAMEWORK_FMX)};
  Const BitmapScale: single = 1{$ENDIF}; const MarginTop: single = 0;
  const MarginRight: single = 0; const MarginBottom: single = 0;
  const MarginLeft: single = 0): TBitmap;

implementation

uses
{$IF Defined(FRAMEWORK_VCL)}
  VCL.Skia,
{$ELSE IF Defined(FRAMEWORK_FMX)}
  FMX.Skia,
{$ENDIF}
  System.Skia,
  System.Types,
  System.SysUtils,
  System.Generics.Collections;

const
  CDefaultListIndex = 0;

type
  TBitmapScaleList = class(TObjectDictionary<single, TBitmap>)
  private
  protected
  public
    constructor Create;
  end;

  THeightList = class(TObjectDictionary<integer, TBitmapScaleList>)
  private
  protected
  public
    constructor Create;
  end;

  TWithList = class(TObjectDictionary<integer, THeightList>)
  private
  protected
  public
    constructor Create;
  end;

  TBMPCache = class
  private
    FWithList: TWithList;
  protected
  public
    constructor Create;
    function Bitmap(const Width, Height: integer; const BitmapScale: single;
      const SVG: string; const MarginTop: single; const MarginRight: single;
      const MarginBottom: single; const MarginLeft: single): TBitmap;
    destructor Destroy; override;
  end;

  TItem = class
  private
    FSVG: string;
    FBMPCache: TBMPCache;
  protected
  public
    constructor Create(const SVG: string);
    function Bitmap(const Width, Height: integer; const BitmapScale: single;
      const WithCache: boolean = true): TBitmap; overload;
    function Bitmap(const Width, Height: integer; const MarginTop: single;
      const MarginRight: single; const MarginBottom: single;
      const MarginLeft: single; const BitmapScale: single;
      const WithCache: boolean = true): TBitmap; overload;
    destructor Destroy; override;
    procedure ClearCache;
  end;

  TItemsList = class(TObjectDictionary<word, TItem>)
  private
  protected
  public
    constructor Create;
    procedure ClearCache;
  end;

  TListsList = class(TObjectDictionary<word, TItemsList>)
  private
  protected
  public
    constructor Create;
  end;

var
  SVGList: TListsList;

function SVGToBitmap(Const Width, Height: integer;
  Const SVGSource: string{$IF Defined(FRAMEWORK_FMX)};
  Const BitmapScale: single{$ENDIF}; const MarginTop: single;
  const MarginRight: single; const MarginBottom: single;
  const MarginLeft: single): TBitmap;
var
  SVGWidth, SVGHeight: integer;
  bmp: TBitmap;
begin
  result := nil;
  SVGWidth := round(Width * (100 - MarginLeft - MarginRight) / 100);
  SVGHeight := round(Height * (100 - MarginTop - MarginBottom) / 100);
{$IF Defined(FRAMEWORK_FMX)}
  bmp := TBitmap.Create(trunc(SVGWidth * BitmapScale),
    trunc(SVGHeight * BitmapScale));
{$ELSE}
  bmp := TBitmap.Create(trunc(SVGWidth), trunc(SVGHeight));
  bmp.transparent := true;
{$ENDIF}
  try
{$IF Defined(FRAMEWORK_FMX)}
    bmp.BitmapScale := BitmapScale;
{$ENDIF}
    bmp.SkiaDraw(
      procedure(const ACanvas: ISKCanvas)
      var
        LSvgBrush: TSkSvgBrush;
      begin
        LSvgBrush := TSkSvgBrush.Create;
        try
          LSvgBrush.Source := SVGSource;
          LSvgBrush.WrapMode := TSkSvgWrapMode.Fit;
          LSvgBrush.Render(ACanvas, RectF(0, 0, SVGWidth, SVGHeight), 1);
        finally
          LSvgBrush.Free;
        end;
      end, true);

    if (MarginTop = 0) and (MarginRight = 0) and (MarginBottom = 0) and
      (MarginLeft = 0) then
      result := bmp
    else
    begin
{$IF Defined(FRAMEWORK_FMX)}
      result := TBitmap.Create(round(Width * BitmapScale),
        round(Height * BitmapScale));
      try
        result.BitmapScale := BitmapScale;
        result.Clear(0);
        result.canvas.BeginScene;
        try
          result.canvas.DrawBitmap(bmp, bmp.BoundsF,
            trectf.Create((result.Width * MarginLeft / 100) / BitmapScale,
            (result.Height * MarginTop / 100) / BitmapScale,
            (bmp.Width + result.Width * MarginLeft / 100) / BitmapScale,
            (bmp.Height + result.Height * MarginTop / 100) / BitmapScale), 1);
        finally
          result.canvas.EndScene;
        end;
      except
        result.Free;
        result := nil;
      end;
{$ELSE}
      result := TBitmap.Create(round(Width), round(Height));
      try
        result.transparent := true;
        result.canvas.CopyRect
          (trect.Create(round(result.Width * MarginLeft / 100),
          round(result.Height * MarginTop / 100),
          round(bmp.Width + result.Width * MarginLeft / 100),
          round(bmp.Height + result.Height * MarginTop / 100)), bmp.canvas,
          bmp.canvas.ClipRect);
      except
        result.Free;
        result := nil;
      end;
{$ENDIF}
    end;
  finally
    if result <> bmp then
      bmp.Free;
  end;
end;

{ TOlfSVGBitmapList }

class function TOlfSVGBitmapList.AddAList: word;
var
  i: word;
begin
  result := CDefaultListIndex;
  for i in SVGList.Keys do
    if (i >= result) then
      result := i + 1;
  SVGList.Add(result, TItemsList.Create);
end;

class function TOlfSVGBitmapList.Bitmap(const FromList, AtIndex: word;
const Width, Height: integer; const BitmapScale: single): TBitmap;
begin
  result := Bitmap(FromList, AtIndex, Width, Height, 0, 0, 0, 0, BitmapScale);
end;

class function TOlfSVGBitmapList.BitmapWithNoCache(const FromList,
  AtIndex: word; const Width, Height: integer;
const BitmapScale: single): TBitmap;
begin
  result := BitmapWithNoCache(FromList, AtIndex, Width, Height, 0, 0, 0, 0,
    BitmapScale);
end;

class procedure TOlfSVGBitmapList.ClearAll;
begin
  SVGList.Free;
  SVGList := TListsList.Create;
end;

class procedure TOlfSVGBitmapList.ClearCache;
var
  ItemsList: TItemsList;
begin
  for ItemsList in SVGList.Values do
    ItemsList.ClearCache;
end;

class function TOlfSVGBitmapList.Count(const FromList: word): NativeInt;
begin
  if not SVGList.ContainsKey(FromList) then
    raise exception.Create('This list doesn''t exist.');

  result := SVGList[FromList].Count;
end;

class procedure TOlfSVGBitmapList.DeleteItem(const FromList, AtIndex: word);
begin
  if not SVGList.ContainsKey(FromList) then
    raise exception.Create('This list doesn''t exist.');

  if SVGList[FromList].ContainsKey(AtIndex) then
    SVGList[FromList].Remove(AtIndex);
end;

class procedure TOlfSVGBitmapList.DeleteItem(const AtIndex: word);
begin
  DeleteItem(CDefaultListIndex, AtIndex);
end;

class procedure TOlfSVGBitmapList.DeleteList(const Index: word);
begin
  if (index <> CDefaultListIndex) and SVGList.ContainsKey(index) then
    SVGList.Remove(index);
end;

class function TOlfSVGBitmapList.AddItem(const ToList: word;
const SVG: string): word;
var
  i: word;
begin
  if not SVGList.ContainsKey(ToList) then
    raise exception.Create('This list doesn''t exist.');

  result := 0;
  for i in SVGList[ToList].Keys do
    if (i >= result) then
      result := i + 1;

  if not AddItemAt(ToList, result, SVG) then
    raise exception.Create('Can''t add an item to this list.');
end;

class function TOlfSVGBitmapList.AddItem(const SVG: string): word;
begin
  result := AddItem(CDefaultListIndex, SVG);
end;

class function TOlfSVGBitmapList.AddItem(const SVGArray: array of string): word;
begin
  result := AddItem(CDefaultListIndex, SVGArray);
end;

class function TOlfSVGBitmapList.AddItem(const ToList: word;
const SVGArray: array of String): word;
var
  i: integer;
begin
  result := 0;
  for i := 0 to length(SVGArray) - 1 do
    if i = 0 then
      result := AddItem(ToList, SVGArray[i])
    else
      AddItem(ToList, SVGArray[i]);
end;

class function TOlfSVGBitmapList.AddItemAt(AtIndex: word;
const SVG: string): boolean;
begin
  result := AddItemAt(CDefaultListIndex, AtIndex, SVG);
end;

class function TOlfSVGBitmapList.AddItemAt(const ToList, AtIndex: word;
const SVG: string): boolean;
begin
  if not SVGList.ContainsKey(ToList) then
    raise exception.Create('This list doesn''t exist.');

  result := not SVGList[ToList].ContainsKey(AtIndex);

  if result then
    SVGList[ToList].Add(AtIndex, TItem.Create(SVG));
end;

class function TOlfSVGBitmapList.Bitmap(const AtIndex: word;
const Width, Height: integer; const BitmapScale: single): TBitmap;
begin
  result := Bitmap(CDefaultListIndex, AtIndex, Width, Height, BitmapScale);
end;

class function TOlfSVGBitmapList.BitmapWithNoCache(const AtIndex: word;
const Width, Height: integer; const BitmapScale: single): TBitmap;
begin
  result := BitmapWithNoCache(CDefaultListIndex, AtIndex, Width, Height,
    BitmapScale);
end;

class function TOlfSVGBitmapList.Bitmap(const FromList, AtIndex: word;
const Width, Height: integer; const MarginTop, MarginRight, MarginBottom,
  MarginLeft, BitmapScale: single): TBitmap;
begin
  if not SVGList.ContainsKey(FromList) then
    raise exception.Create('This list doesn''t exist.');

  if not SVGList[FromList].ContainsKey(AtIndex) then
    raise exception.Create('This item doesn''t exist.');

  result := SVGList[FromList][AtIndex].Bitmap(Width, Height, MarginTop,
    MarginRight, MarginBottom, MarginLeft, BitmapScale, true);
end;

class function TOlfSVGBitmapList.Bitmap(const AtIndex: word;
const Width, Height: integer; const MarginTop, MarginRight, MarginBottom,
  MarginLeft, BitmapScale: single): TBitmap;
begin
  result := Bitmap(CDefaultListIndex, AtIndex, Width, Height, MarginTop,
    MarginRight, MarginBottom, MarginLeft, BitmapScale);
end;

class function TOlfSVGBitmapList.BitmapWithNoCache(const FromList,
  AtIndex: word; const Width, Height: integer; const MarginTop, MarginRight,
  MarginBottom, MarginLeft, BitmapScale: single): TBitmap;
begin
  if not SVGList.ContainsKey(FromList) then
    raise exception.Create('This list doesn''t exist.');

  if not SVGList[FromList].ContainsKey(AtIndex) then
    raise exception.Create('This item doesn''t exist.');

  result := SVGList[FromList][AtIndex].Bitmap(Width, Height, MarginTop,
    MarginRight, MarginBottom, MarginLeft, BitmapScale, false);
end;

class function TOlfSVGBitmapList.BitmapWithNoCache(const AtIndex: word;
const Width, Height: integer; const MarginTop, MarginRight, MarginBottom,
  MarginLeft, BitmapScale: single): TBitmap;
begin
  result := BitmapWithNoCache(CDefaultListIndex, AtIndex, Width, Height,
    MarginTop, MarginRight, MarginBottom, MarginLeft, BitmapScale);
end;

{ TBitmapScaleList }

constructor TBitmapScaleList.Create;
begin
  inherited Create([TDictionaryOwnership.doOwnsValues]);
end;

{ THeightList }

constructor THeightList.Create;
begin
  inherited Create([TDictionaryOwnership.doOwnsValues]);
end;

{ TWithList }

constructor TWithList.Create;
begin
  inherited Create([TDictionaryOwnership.doOwnsValues]);
end;

{ TBMPCache }

function TBMPCache.Bitmap(const Width, Height: integer;
const BitmapScale: single; const SVG: string; const MarginTop: single;
const MarginRight: single; const MarginBottom: single;
const MarginLeft: single): TBitmap;
begin
  if not FWithList.ContainsKey(Width) then
    FWithList.Add(Width, THeightList.Create);

  if not FWithList[Width].ContainsKey(Height) then
    FWithList[Width].Add(Height, TBitmapScaleList.Create);

  if not FWithList[Width][Height].ContainsKey(BitmapScale) then
    FWithList[Width][Height].Add(BitmapScale, nil);

  if not assigned(FWithList[Width][Height][BitmapScale]) then
{$IF Defined(FRAMEWORK_VCL)}
    FWithList[Width][Height][BitmapScale] := SVGToBitmap(Width, Height, SVG,
      MarginTop, MarginRight, MarginBottom, MarginLeft)
{$ELSEIF Defined(FRAMEWORK_FMX)}
    FWithList[Width][Height][BitmapScale] := SVGToBitmap(Width, Height, SVG,
      BitmapScale, MarginTop, MarginRight, MarginBottom, MarginLeft)
{$ENDIF};

  result := FWithList[Width][Height][BitmapScale];
end;

constructor TBMPCache.Create;
begin
  inherited;
  FWithList := TWithList.Create;
end;

destructor TBMPCache.Destroy;
begin
  FWithList.Free;
  inherited;
end;

{ TItem }

function TItem.Bitmap(const Width, Height: integer; const BitmapScale: single;
const WithCache: boolean): TBitmap;
begin
  result := Bitmap(Width, Height, 0, 0, 0, 0, BitmapScale, WithCache);
end;

function TItem.Bitmap(const Width, Height: integer;
const MarginTop, MarginRight, MarginBottom, MarginLeft, BitmapScale: single;
const WithCache: boolean): TBitmap;
begin
  if WithCache then
    result := FBMPCache.Bitmap(Width, Height, BitmapScale, FSVG, MarginTop,
      MarginRight, MarginBottom, MarginLeft)
  else
{$IF Defined(FRAMEWORK_VCL)}
    result := SVGToBitmap(Width, Height, FSVG, MarginTop, MarginRight,
      MarginBottom, MarginLeft)
{$ELSEIF Defined(FRAMEWORK_FMX)}
      result := SVGToBitmap(Width, Height, FSVG, BitmapScale, MarginTop,
      MarginRight, MarginBottom, MarginLeft)
{$ENDIF};
end;

procedure TItem.ClearCache;
begin
  FBMPCache.Free;
  FBMPCache := TBMPCache.Create;
end;

constructor TItem.Create(const SVG: string);
begin
  inherited Create;
  FSVG := SVG;
  FBMPCache := TBMPCache.Create;
end;

destructor TItem.Destroy;
begin
  FBMPCache.Free;
  inherited;
end;

{ TItemsList }

procedure TItemsList.ClearCache;
var
  Item: TItem;
begin
  for Item in Values do
    Item.ClearCache;
end;

constructor TItemsList.Create;
begin
  inherited Create([TDictionaryOwnership.doOwnsValues]);
end;

{ TListsList }

constructor TListsList.Create;
begin
  inherited Create([TDictionaryOwnership.doOwnsValues]);
end;

initialization

SVGList := TListsList.Create;
SVGList.Add(CDefaultListIndex, TItemsList.Create);

finalization

SVGList.Free;

end.
