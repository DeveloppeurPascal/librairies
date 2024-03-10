unit fUseLibraryAsTFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Olf.FMX.TextImageFrame;

type
  TForm1 = class(TForm)
    OlfFMXTextImageFrame1: TOlfFMXTextImageFrame;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function ConvertUnknownChars(Sender: TOlfFMXTextImageFrame;
      AChar: char): integer;
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
  OlfFMXTextImageFrame1.OnGetImageIndexOfUnknowChar := ConvertUnknownChars;
  OlfFMXTextImageFrame1.Font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame1.Text := '012 abc ABC';
end;

end.
