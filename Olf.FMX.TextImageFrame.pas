unit Olf.FMX.TextImageFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.ImageList, FMX.ImgList;

type
  TOlfFMXTextImageFrame = class;

  TOlfFMXTIFOnGetImageIndexOfUnknowChar = function
    (Sender: TOlfFMXTextImageFrame; AChar: char): integer of object;

  TOlfFMXTextImageFrame = class(TFrame)
  private
    FTexte: string;
    FFonte: TCustomImageList;
    FOnGetImageIndexOfUnknowChar: TOlfFMXTIFOnGetImageIndexOfUnknowChar;
    procedure SetFonte(const Value: TCustomImageList);
    procedure SetTexte(const Value: string);
    procedure SetOnGetImageIndexOfUnknowChar(const Value
      : TOlfFMXTIFOnGetImageIndexOfUnknowChar);
  protected
    function AjoutImageEtRetourneLargeur(AImages: TCustomImageList;
      AImageIndex: TImageIndex; AX: single): single;
    procedure RefreshTexte;
  public
    property Fonte: TCustomImageList read FFonte write SetFonte;
    property Texte: string read FTexte write SetTexte;
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
  FFonte := nil;
  FTexte := '';
  FOnGetImageIndexOfUnknowChar := nil;
end;

function TOlfFMXTextImageFrame.getImageIndexOfChar(AChar: string): integer;
begin
  result := 0;
  while (result < FFonte.Count) and
    (FFonte.Destination[result].Layers[0].Name <> AChar) do
    inc(result);
  if (result >= FFonte.Count) then
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
  if assigned(FFonte) and (FTexte.Length > 0) then
    for i := 0 to FTexte.Length - 1 do
    begin
      idx := getImageIndexOfChar(FTexte.Chars[i]);
      if (idx < 0) and assigned(FOnGetImageIndexOfUnknowChar) then
        idx := FOnGetImageIndexOfUnknowChar(self, FTexte.Chars[i]);
      x := x + AjoutImageEtRetourneLargeur(FFonte, idx, x);
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

procedure TOlfFMXTextImageFrame.SetFonte(const Value: TCustomImageList);
begin
  FFonte := Value;
  if (FTexte.Length > 0) then
    RefreshTexte;
end;

procedure TOlfFMXTextImageFrame.SetOnGetImageIndexOfUnknowChar
  (const Value: TOlfFMXTIFOnGetImageIndexOfUnknowChar);
begin
  FOnGetImageIndexOfUnknowChar := Value;
end;

procedure TOlfFMXTextImageFrame.SetTexte(const Value: string);
begin
  FTexte := Value;
  if not assigned(FFonte) then
    exit;
  RefreshTexte;
end;

// TODO : gérer changement de taille des chiffres en cas de resize de la zone

end.
