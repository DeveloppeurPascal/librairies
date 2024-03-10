unit fDisplayTextByCode;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Olf.FMX.TextImageFrame, FMX.ImgList;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    function ConvertUnknownChars(Sender: TOlfFMXTextImageFrame;
      AChar: char): integer;
    function CreateTextImage(AFont: tcustomimagelist; Atext: string)
      : TOlfFMXTextImageFrame;
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses udm_CharacterImages;

function TForm1.ConvertUnknownChars(Sender: TOlfFMXTextImageFrame;
  AChar: char): integer;
begin
  result := -1;
  if charinset(AChar, ['a' .. 'z']) then // _a, _b, _c in source images
    result := Sender.getImageIndexOfChar('_' + AChar);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateTextImage(dm_CharacterImages.ImageList, '012abcABC');
  with CreateTextImage(dm_CharacterImages.ImageList, '') do
  begin
    align := talignlayout.Center;
    OnGetImageIndexOfUnknowChar := ConvertUnknownChars;
    LetterSpacing:=-20;
    text := '012 abc ABC';
  end;
end;

function TForm1.CreateTextImage(AFont: tcustomimagelist; Atext: string)
  : TOlfFMXTextImageFrame;
begin
  result := TOlfFMXTextImageFrame.Create(self);
  with result do
  begin
    parent := self;
    align := talignlayout.top;
    height := 100;
    Font := AFont;
    text := Atext;
  end;
end;

end.
