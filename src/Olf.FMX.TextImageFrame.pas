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
/// Patrick PREMARTIN
///
/// Site :
/// https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:15
/// Signature : 391f3042364d77f2433eaabadf097f8049612985
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

  TOlfFMXTIFOnGetImageIndexOfUnknowChar = function
    (Sender: TOlfFMXTextImageFrame; AChar: char): integer of object;

  TOlfFMXTextImageFrame = class(TFrame)
  private
    FText: string;
    FFont: TCustomImageList;
    FOnGetImageIndexOfUnknowChar: TOlfFMXTIFOnGetImageIndexOfUnknowChar;
    FLetterSpacing: single;
    FSpaceWidth, FRealSpaceWidth: single;
    procedure SetFont(const Value: TCustomImageList);
    procedure SetText(const Value: string);
    procedure SetOnGetImageIndexOfUnknowChar(const Value
      : TOlfFMXTIFOnGetImageIndexOfUnknowChar);
    procedure SetLetterSpacing(const Value: single);
    procedure SetSpaceWidth(const Value: single);
  protected
    function AjoutImageEtRetourneLargeur(AImages: TCustomImageList;
      AImageIndex: TImageIndex; AX: single): single;
    procedure RefreshTexte;
  public
    property Font: TCustomImageList read FFont write SetFont;
    property Text: string read FText write SetText;
    property SpaceWidth: single read FSpaceWidth write SetSpaceWidth;
    property LetterSpacing: single read FLetterSpacing write SetLetterSpacing;
    property OnGetImageIndexOfUnknowChar: TOlfFMXTIFOnGetImageIndexOfUnknowChar
      read FOnGetImageIndexOfUnknowChar write SetOnGetImageIndexOfUnknowChar;
    constructor Create(AOwner: TComponent); override;
    function RetourneLargeur(AImages: TCustomImageList;
      AImageIndex: TImageIndex): single;
    function getImageIndexOfChar(AChar: string): integer;
  end;

implementation

{$R *.fmx}

const
  CPosChiffres = 0;
  CPosMajuscules = CPosChiffres + 10;
  CPosMinuscules = CPosMajuscules;
  // Pas de minuscules dans les fontes prises sur ce jeu
  CPosPonctuation = CPosMajuscules + 26;
  // TODO : à modifier si nécessaire selon les fontes

  { TcadAffichageTexteGraphique }

function TOlfFMXTextImageFrame.AjoutImageEtRetourneLargeur
  (AImages: TCustomImageList; AImageIndex: TImageIndex; AX: single): single;
var
  g: tglyph;
  wi, hi: single;
begin
  if (not assigned(AImages)) or (AImageIndex < 0) or
    (AImageIndex >= AImages.Count) then
    result := 0
  else
  begin
    g := tglyph.Create(self);
    g.Parent := self;
    wi := AImages.Destination[AImageIndex].Layers[0].MultiResBitmap[0].Width;
    hi := AImages.Destination[AImageIndex].Layers[0].MultiResBitmap[0].height;
    g.height := height;
    g.Width := g.height * wi / hi;
    g.Images := AImages;
    g.ImageIndex := AImageIndex;
    g.Position.x := AX;
    g.Position.y := 0;
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
end;

function TOlfFMXTextImageFrame.getImageIndexOfChar(AChar: string): integer;
begin
  result := 0;
  while (result < FFont.Count) and
    (FFont.Destination[result].Layers[0].Name <> AChar) do
    inc(result);
  if (result >= FFont.Count) then
    result := -1;
end;

procedure TOlfFMXTextImageFrame.RefreshTexte;
var
  i: integer;
  x: single;
  idx: integer;
begin
  for i := childrencount - 1 downto 0 do
    if (children[i] is tglyph) then
      children[i].Free;

  x := 0;
  if assigned(FFont) and (FText.Length > 0) then
    for i := 0 to FText.Length - 1 do
    begin
      idx := getImageIndexOfChar(FText.Chars[i]);
      if (idx < 0) and assigned(FOnGetImageIndexOfUnknowChar) then
        idx := FOnGetImageIndexOfUnknowChar(self, FText.Chars[i]);
      if (idx >= 0) then
        x := x + AjoutImageEtRetourneLargeur(FFont, idx, x) + FLetterSpacing
      else if (FText.Chars[i] = ' ') then
      begin
        if (FRealSpaceWidth < 1) then
        begin
          idx := getImageIndexOfChar('.');
          if (idx < 0) and assigned(FOnGetImageIndexOfUnknowChar) then
            idx := FOnGetImageIndexOfUnknowChar(self, '.');
          if (idx >= 0) then
            FRealSpaceWidth := RetourneLargeur(FFont, idx);

          idx := getImageIndexOfChar('i');
          if (idx < 0) and assigned(FOnGetImageIndexOfUnknowChar) then
            idx := FOnGetImageIndexOfUnknowChar(self, 'i');
          if (idx >= 0) then
            FRealSpaceWidth := RetourneLargeur(FFont, idx);

          idx := getImageIndexOfChar('I');
          if (idx < 0) and assigned(FOnGetImageIndexOfUnknowChar) then
            idx := FOnGetImageIndexOfUnknowChar(self, 'I');
          if (idx >= 0) then
            FRealSpaceWidth := RetourneLargeur(FFont, idx);

          idx := getImageIndexOfChar('1');
          if (idx < 0) and assigned(FOnGetImageIndexOfUnknowChar) then
            idx := FOnGetImageIndexOfUnknowChar(self, '1');
          if (idx >= 0) then
            FRealSpaceWidth := RetourneLargeur(FFont, idx);
        end;
        x := x + FRealSpaceWidth;
      end;
    end;

  Width := x;
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
    hi := AImages.Destination[AImageIndex].Layers[0].MultiResBitmap[0].height;
    result := height * wi / hi;
  end;
end;

procedure TOlfFMXTextImageFrame.SetFont(const Value: TCustomImageList);
begin
  FFont := Value;
  FRealSpaceWidth := FSpaceWidth;
  if (FText.Length > 0) then
    RefreshTexte;
end;

procedure TOlfFMXTextImageFrame.SetLetterSpacing(const Value: single);
begin
  FLetterSpacing := Value;
end;

procedure TOlfFMXTextImageFrame.SetOnGetImageIndexOfUnknowChar
  (const Value: TOlfFMXTIFOnGetImageIndexOfUnknowChar);
begin
  FOnGetImageIndexOfUnknowChar := Value;
end;

procedure TOlfFMXTextImageFrame.SetSpaceWidth(const Value: single);
begin
  FSpaceWidth := Value;
  FRealSpaceWidth := FSpaceWidth;
end;

procedure TOlfFMXTextImageFrame.SetText(const Value: string);
begin
  FText := Value;
  if not assigned(FFont) then
    exit;
  RefreshTexte;
end;

// TODO : gérer changement de taille des chiffres en cas de resize de la zone

end.
