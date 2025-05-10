/// <summary>
/// ***************************************************************************
///
/// My libraries for Delphi
///
/// Copyright 1990-2025 Patrick Prémartin under AGPL 3.0 license.
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
/// Patrick PREMARTIN
///
/// Site :
/// https://librairies.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 2025-05-10T09:42:46.000+02:00
/// Signature : 7a3a133c3e0231443ab3230d11931360c0b99651
/// ***************************************************************************
/// </summary>

unit Olf.FMX.TextImageFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  System.ImageList,
  FMX.ImgList;

type
  TOlfFMXTextImageFrame = class;

  /// <summary>
  /// Type to use for the "get image index" method event
  /// </summary>
  TOlfFMXTIFOnGetImageIndexOfUnknowChar = function
    (Sender: TOlfFMXTextImageFrame; AChar: char): integer of object;

  /// <summary>
  /// Display a text with a bitmap font.
  /// </summary>
  TOlfFMXTextImageFrame = class(TFrame)
  private
    FText: string;
    FFont: TCustomImageList;
    FOnGetImageIndexOfUnknowChar: TOlfFMXTIFOnGetImageIndexOfUnknowChar;
    FLetterSpacing: single;
    FSpaceWidth, FRealSpaceWidth: single;
    FHasPendingRefresh: boolean;
    FAutoSize: boolean;
    procedure SetFont(const Value: TCustomImageList);
    procedure SetText(const Value: string);
    procedure SetOnGetImageIndexOfUnknowChar(const Value
      : TOlfFMXTIFOnGetImageIndexOfUnknowChar);
    procedure SetLetterSpacing(const Value: single);
    procedure SetSpaceWidth(const Value: single);
    procedure SetHasPendingRefresh(const Value: boolean);
    procedure SetAutoSize(const Value: boolean);
    procedure DoRefresh;
  protected
    /// <summary>
    /// Use to delay a Refresh operation during a BeginUpdate/EndUpdate bloc
    /// </summary>
    property HasPendingRefresh: boolean read FHasPendingRefresh
      write SetHasPendingRefresh;
    /// <summary>
    /// Add an image (= a character as bitmap) and returns its width.
    /// </summary>
    function AjoutImageEtRetourneLargeur(AImages: TCustomImageList;
      AImageIndex: TImageIndex; AX: single; AChar: char): single;
    /// <summary>
    /// Called by GetImageIndexOfChar() when no index for a char has been found
    /// after calling OnGetImageIndexOfUnknowChar event.
    /// </summary>
    function DefaultOnGetImageIndexOfUnknowChar(AChar: char): integer; virtual;
    /// <summary>
    /// Called at the end of a BeginUpdate/EndUpdate bloc. It calls the Refresh
    /// method if it has been asked in the bloc.
    /// </summary>
    procedure DoEndUpdate; override;
  public
    /// <summary>
    /// If True, the text size is reduced or increased to fill its parent.
    /// If False, the text size depends only on its Height property, the Width
    /// is adapted after a Refresh.
    /// </summary>
    property AutoSize: boolean read FAutoSize write SetAutoSize;
    /// <summary>
    /// Font to use (an image list with characters as bitmaps)
    /// </summary>
    property Font: TCustomImageList read FFont write SetFont;
    /// <summary>
    /// Text to display
    /// </summary>
    property Text: string read FText write SetText;
    /// <summary>
    /// Width to use when a text contains a space character.
    /// </summary>
    property SpaceWidth: single read FSpaceWidth write SetSpaceWidth;
    /// <summary>
    /// Width to use between two letters.
    /// </summary>
    property LetterSpacing: single read FLetterSpacing write SetLetterSpacing;
    /// <summary>
    /// Event used to get the index of an unknow character in the font image list.
    /// </summary>
    property OnGetImageIndexOfUnknowChar: TOlfFMXTIFOnGetImageIndexOfUnknowChar
      read FOnGetImageIndexOfUnknowChar write SetOnGetImageIndexOfUnknowChar;
    /// <summary>
    /// Create an instance of this class
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    /// Returns the width of a character image with current drawing height of
    /// the text.
    /// </summary>
    function RetourneLargeur(AImages: TCustomImageList;
      AImageIndex: TImageIndex): single;
    /// <summary>
    /// Returns the index of the char in the image list.
    /// If it's not found this function can call the
    /// OnGetImageIndexOfUnknowCharIfNotFound event or do a default character
    /// swap to find the good one.
    /// </summary>
    /// <remarks>
    /// Don't use True for CallOnGetImageIndexOfUnknowCharIfNotFound in your
    /// OnGetImageIndexOfUnknowCharIfNotFound events or you'll have infinite
    /// loops.
    /// </remarks>
    function getImageIndexOfChar(AChar: String;
      CallOnGetImageIndexOfUnknowCharIfNotFound: boolean = false): integer;
    /// <summary>
    /// Repaint the text
    /// </summary>
    procedure Refresh;
  end;

implementation

{$R *.fmx}

function TOlfFMXTextImageFrame.AjoutImageEtRetourneLargeur
  (AImages: TCustomImageList; AImageIndex: TImageIndex; AX: single;
  AChar: char): single;
var
  g: TGlyph;
  wi, hi: single;
begin
  if (not assigned(AImages)) or (AImageIndex < 0) or
    (AImageIndex >= AImages.Count) then
    result := 0
  else
  begin
    g := TGlyph.Create(self);
    g.Parent := self;
    wi := AImages.Destination[AImageIndex].Layers[0].MultiResBitmap[0].Width;
    hi := AImages.Destination[AImageIndex].Layers[0].MultiResBitmap[0].Height;
    g.Height := Height;
    g.Width := g.Height * wi / hi;
    g.Images := AImages;
    g.ImageIndex := AImageIndex;
    g.Position.x := AX;
    g.Position.y := 0;
    g.TagString := AChar;
    result := g.Width;
  end;
end;

constructor TOlfFMXTextImageFrame.Create(AOwner: TComponent);
begin
  inherited;
  name := '';
  FFont := nil;
  FText := '';
  FLetterSpacing := 0;
  FSpaceWidth := 0;
  FRealSpaceWidth := 0;
  FOnGetImageIndexOfUnknowChar := nil;
  FHasPendingRefresh := false;
end;

function TOlfFMXTextImageFrame.DefaultOnGetImageIndexOfUnknowChar
  (AChar: char): integer;
begin
  result := -1;
  if CharInset(AChar, ['a' .. 'z']) then
  begin
    result := getImageIndexOfChar('_' + AChar);
    if (result < 0) then
      result := getImageIndexOfChar(AChar + '_');
    if (result < 0) then
      result := getImageIndexOfChar(uppercase(AChar));
  end;

  if (result < 0) and (AChar = 'à') then
  begin
    result := getImageIndexOfChar('_agrave');
    if result < 0 then
      result := getImageIndexOfChar('agrave');
  end;
  if (result < 0) and (AChar = 'ã') then
  begin
    result := getImageIndexOfChar('_atilde');
    if result < 0 then
      result := getImageIndexOfChar('atilde');
  end;
  if (result < 0) and CharInset(AChar, ['à', 'ã']) then
    result := getImageIndexOfChar('a', true);

  if (result < 0) and (AChar = 'ç') then
  begin
    result := getImageIndexOfChar('_ccedille');
    if (result < 0) then
      result := getImageIndexOfChar('c');
  end;

  if (result < 0) and (AChar = 'ď') then
  begin
    result := getImageIndexOfChar('_dprime');
    if (result < 0) then
      result := getImageIndexOfChar('dprime');
    if (result < 0) then
      result := getImageIndexOfChar('d');
  end;

  if (result < 0) and (AChar = 'é') then
  begin
    result := getImageIndexOfChar('_eaigu');
    if result < 0 then
      result := getImageIndexOfChar('eaigu');
  end;
  if (result < 0) and (AChar = 'è') then
  begin
    result := getImageIndexOfChar('_egrave');
    if result < 0 then
      result := getImageIndexOfChar('egrave');
  end;
  if (result < 0) and (AChar = 'ê') then
  begin
    result := getImageIndexOfChar('_ecirconflexe');
    if result < 0 then
      result := getImageIndexOfChar('ecirconflexe');
  end;
  if (result < 0) and (AChar = 'ë') then
  begin
    result := getImageIndexOfChar('_etrema');
    if result < 0 then
      result := getImageIndexOfChar('etrema');
  end;
  if (result < 0) and CharInset(AChar, ['é', 'è', 'ê', 'ë']) then
    result := getImageIndexOfChar('e', true);

  if (result < 0) and (AChar = 'ı') then
  begin
    result := getImageIndexOfChar('_iturc');
    if result < 0 then
      result := getImageIndexOfChar('iturc');
  end;
  if (result < 0) and (AChar = 'î') then
  begin
    result := getImageIndexOfChar('_icirconflexe');
    if result < 0 then
      result := getImageIndexOfChar('icirconflexe');
  end;
  if (result < 0) and (AChar = 'ï') then
  begin
    result := getImageIndexOfChar('_itrema');
    if result < 0 then
      result := getImageIndexOfChar('itrema');
  end;
  if (result < 0) and (AChar = 'í') then
  begin
    result := getImageIndexOfChar('_iaigu');
    if result < 0 then
      result := getImageIndexOfChar('iaigu');
  end;
  if (result < 0) and CharInset(AChar, ['î', 'ï', 'í', 'ı']) then
    result := getImageIndexOfChar('i', true);

  if (result < 0) and (AChar = 'ñ') then
  begin
    result := getImageIndexOfChar('_ntilde');
    if result < 0 then
      result := getImageIndexOfChar('ntilde');
    if (result < 0) then
      result := getImageIndexOfChar('n');
  end;

  if (result < 0) and (AChar = 'ô') then
  begin
    result := getImageIndexOfChar('_ocirconflexe');
    if result < 0 then
      result := getImageIndexOfChar('ocirconflexe');
  end;
  if (result < 0) and (AChar = 'ö') then
  begin
    result := getImageIndexOfChar('_otrema');
    if result < 0 then
      result := getImageIndexOfChar('otrema');
  end;
  if (result < 0) and (AChar = 'ó') then
  begin
    result := getImageIndexOfChar('_oaigu');
    if result < 0 then
      result := getImageIndexOfChar('oaigu');
  end;
  if (result < 0) and CharInset(AChar, ['ô', 'ö', 'ó']) then
    result := getImageIndexOfChar('o', true);

  // TODO : cf https://github.com/DeveloppeurPascal/librairies/issues/128
  if (result < 0) and (AChar = 'oe') then
    result := getImageIndexOfChar('_oe');
  // TODO : récupérer "oe" en minuscules
  if (result < 0) and (AChar = 'OE') then
    result := getImageIndexOfChar('OE');
  // TODO : récupérer "oe" en majuscules

  if (result < 0) and (AChar = 'ť') then
  begin
    result := getImageIndexOfChar('_tprime');
    if (result < 0) then
      result := getImageIndexOfChar('tprime');
    if (result < 0) then
      result := getImageIndexOfChar('t');
  end;

  if (result < 0) and (AChar = 'û') then
  begin
    result := getImageIndexOfChar('_ucirconflexe');
    if result < 0 then
      result := getImageIndexOfChar('ucirconflexe');
  end;
  if (result < 0) and (AChar = 'ü') then
  begin
    result := getImageIndexOfChar('_utrema');
    if result < 0 then
      result := getImageIndexOfChar('_utrema');
  end;
  if (result < 0) and (AChar = 'ù') then
  begin
    result := getImageIndexOfChar('_ugrave');
    if result < 0 then
      result := getImageIndexOfChar('ugrave');
  end;
  // if (result < 0) and (AChar = '') then // TODO : ajouter u aigu
  // result := getImageIndexOfChar('_uaigu');
  if (result < 0) and CharInset(AChar, ['û', 'ü', 'ù']) then
    // TODO : ajouter u aigu
    result := getImageIndexOfChar('u', true);

  if (result < 0) and (AChar = '?') then
    result := getImageIndexOfChar('interrogation');
  if (result < 0) and (AChar = '$') then
    result := getImageIndexOfChar('dollar');
  if (result < 0) and (AChar = '!') then
    result := getImageIndexOfChar('exclamation');
  if (result < 0) and (AChar = '&') then
    result := getImageIndexOfChar('et');
  if (result < 0) and (AChar = '%') then
    result := getImageIndexOfChar('pourcent');
  if (result < 0) and (AChar = '''') then
    result := getImageIndexOfChar('apostrophe');
  if (result < 0) and (AChar = ',') then
    result := getImageIndexOfChar('virgule');
  if (result < 0) and (AChar = '=') then
    result := getImageIndexOfChar('egale');
  if (result < 0) and (AChar = '-') then
    result := getImageIndexOfChar('moins');
  if (result < 0) and (AChar = '+') then
    result := getImageIndexOfChar('plus');
  // if (result < 0) and (AChar = '...') then
  // TODO : récupérer points de suspensions en 1 caractère
  // result := getImageIndexOfChar('suspension'); // TODO
  if (result < 0) and (AChar = '.') then
    result := getImageIndexOfChar('point');
  if (result < 0) and (AChar = ':') then
  begin
    result := getImageIndexOfChar('deuxpoint');
    if (result < 0) then
      result := getImageIndexOfChar('deuxpoints');
    if (result < 0) then
      result := getImageIndexOfChar('deux-point');
    if (result < 0) then
      result := getImageIndexOfChar('deux-points');
  end;
  if (result < 0) and (AChar = ';') then
  begin
    result := getImageIndexOfChar('pointvirgule');
    if (result < 0) then
      result := getImageIndexOfChar('point-virgule');
  end;
end;

procedure TOlfFMXTextImageFrame.DoEndUpdate;
begin
  inherited;
  if (not IsUpdating) and HasPendingRefresh then
    Refresh;
end;

procedure TOlfFMXTextImageFrame.DoRefresh;
var
  i: integer;
  x: single;
  idx: integer;
begin
  if IsUpdating then
  begin
    FHasPendingRefresh := true;
    exit;
  end
  else
    FHasPendingRefresh := false;

  for i := childrencount - 1 downto 0 do
    if (children[i] is TGlyph) then
      children[i].Free;

  x := 0;
  try
    if not assigned(FFont) then
      exit;

    if FText.IsEmpty then
      exit;

    for i := 0 to FText.Length - 1 do
    begin
      idx := getImageIndexOfChar(FText.Chars[i], true);
      if (idx >= 0) then
        x := x + AjoutImageEtRetourneLargeur(FFont, idx, x, FText.Chars[i]) +
          FLetterSpacing
      else if (FText.Chars[i] = ' ') then
      begin
        if (FRealSpaceWidth < 1) then
        begin
          idx := getImageIndexOfChar('.', false);
          if (idx >= 0) then
            FRealSpaceWidth := RetourneLargeur(FFont, idx);

          idx := getImageIndexOfChar('i', false);
          if (idx >= 0) then
            FRealSpaceWidth := RetourneLargeur(FFont, idx);

          idx := getImageIndexOfChar('I', false);
          if (idx >= 0) then
            FRealSpaceWidth := RetourneLargeur(FFont, idx);

          idx := getImageIndexOfChar('1', false);
          if (idx >= 0) then
            FRealSpaceWidth := RetourneLargeur(FFont, idx);
        end;
        x := x + FRealSpaceWidth;
      end;
    end;
  finally
    Width := x;
  end;
end;

function TOlfFMXTextImageFrame.getImageIndexOfChar(AChar: String;
  CallOnGetImageIndexOfUnknowCharIfNotFound: boolean): integer;
begin
  result := 0;
  while (result < FFont.Count) and
    (FFont.Destination[result].Layers[0].Name <> AChar) do
    inc(result);

  if (result >= FFont.Count) then
    if CallOnGetImageIndexOfUnknowCharIfNotFound then
    begin
      result := -1;
      if assigned(OnGetImageIndexOfUnknowChar) then
        result := OnGetImageIndexOfUnknowChar(self, AChar.Chars[0]);
      if result < 0 then
        result := DefaultOnGetImageIndexOfUnknowChar(AChar.Chars[0]);
    end
    else
      result := -1;
end;

procedure TOlfFMXTextImageFrame.Refresh;
var
  C: TControl;
  ParentWidth, ParentHeight: single;
begin
  if AutoSize then
  begin
    if assigned(Parent) and (Parent is TControl) then
    begin
      C := (Parent as TControl);
      ParentWidth := C.Width - C.Padding.Left - C.Padding.Right;
      ParentHeight := C.Height - C.Padding.Top - C.Padding.Bottom;
      Height := ParentHeight;
      DoRefresh;
      if (Width > ParentWidth) then
        Height := ParentHeight * ParentWidth / Width;
      DoRefresh;
    end
    else
      Raise Exception.Create('AutoSize not allowed on this parent.');
  end
  else
    DoRefresh;
end;

function TOlfFMXTextImageFrame.RetourneLargeur(AImages: TCustomImageList;
  AImageIndex: TImageIndex): single;
var
  wi, hi: single;
begin
  if (not assigned(AImages)) or (AImageIndex < 0) or
    (AImageIndex >= AImages.Count) then
    result := 0
  else
  begin
    wi := AImages.Destination[AImageIndex].Layers[0].MultiResBitmap[0].Width;
    hi := AImages.Destination[AImageIndex].Layers[0].MultiResBitmap[0].Height;
    result := Height * wi / hi;
  end;
end;

procedure TOlfFMXTextImageFrame.SetAutoSize(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    Refresh;
  end;
end;

procedure TOlfFMXTextImageFrame.SetFont(const Value: TCustomImageList);
begin
  if (FFont <> Value) then
  begin
    FFont := Value;
    FRealSpaceWidth := FSpaceWidth;
    Refresh;
  end;
end;

procedure TOlfFMXTextImageFrame.SetHasPendingRefresh(const Value: boolean);
begin
  FHasPendingRefresh := Value;
end;

procedure TOlfFMXTextImageFrame.SetLetterSpacing(const Value: single);
begin
  if (FLetterSpacing <> Value) then
  begin
    FLetterSpacing := Value;
    Refresh;
  end;
end;

procedure TOlfFMXTextImageFrame.SetOnGetImageIndexOfUnknowChar
  (const Value: TOlfFMXTIFOnGetImageIndexOfUnknowChar);
begin
  if (@FOnGetImageIndexOfUnknowChar <> @Value) then
  begin
    FOnGetImageIndexOfUnknowChar := Value;
    Refresh;
  end;
end;

procedure TOlfFMXTextImageFrame.SetSpaceWidth(const Value: single);
begin
  if (FSpaceWidth <> Value) then
  begin
    FSpaceWidth := Value;
    FRealSpaceWidth := FSpaceWidth;
    Refresh;
  end;
end;

procedure TOlfFMXTextImageFrame.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Refresh;
  end;
end;

// TODO : gérer changement de taille des chiffres en cas de resize de la zone

end.
