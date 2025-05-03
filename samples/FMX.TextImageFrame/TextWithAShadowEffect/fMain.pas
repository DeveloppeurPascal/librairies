unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  Olf.FMX.TextImageFrame,
  FMX.Effects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts;

type
  TForm1 = class(TForm)
    OlfFMXTextImageFrame1: TOlfFMXTextImageFrame;
    ShadowEffect1: TShadowEffect;
    Button1: TButton;
    Rectangle1: TRectangle;
    FlowLayout1: TFlowLayout;
    FlowLayoutBreak1: TFlowLayoutBreak;
    Rectangle2: TRectangle;
    OlfFMXTextImageFrame2: TOlfFMXTextImageFrame;
    ShadowEffect2: TShadowEffect;
    FlowLayoutBreak2: TFlowLayoutBreak;
    OlfFMXTextImageFrame3: TOlfFMXTextImageFrame;
    ShadowEffect3: TShadowEffect;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FlowLayout1Resized(Sender: TObject);
  private
  protected
    function GetImageIndexOfUnknowChar(Sender: TOlfFMXTextImageFrame;
      AChar: char): integer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  udm_CharacterImages;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: string;
  nb: integer;
begin
  nb := random(6) + 5;
  s := '';
  while (nb > 0) do
  begin
    case random(9) of
      0:
        s := s + '0';
      1:
        s := s + '1';
      2:
        s := s + '2';
      3:
        s := s + 'a';
      4:
        s := s + 'b';
      5:
        s := s + 'c';
      6:
        s := s + 'A';
      7:
        s := s + 'B';
    else
      s := s + 'C';
    end;
    dec(nb);
  end;

  // case random(6) of
  // 0:
  // s := 'Play';
  // 1:
  // s := 'Continue';
  // 2:
  // s := 'Quitter';
  // 3:
  // s := 'Scores';
  // 4:
  // s := 'Credits';
  // 5:
  // s := 'Options';
  // end;

  if OlfFMXTextImageFrame1.Text = s then
  begin
    Button1Click(Sender);
    exit;
  end;

  OlfFMXTextImageFrame1.Text := s;
  OlfFMXTextImageFrame2.Text := s;
  OlfFMXTextImageFrame3.Text := s;
end;

procedure TForm1.FlowLayout1Resized(Sender: TObject);
begin
  Rectangle1.width := FlowLayout1.width - 10;
  Rectangle2.width := Rectangle1.width;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  tthread.ForceQueue(nil,
    procedure
    begin
      OlfFMXTextImageFrame1.Font := dm_CharacterImages.ImageList;
      OlfFMXTextImageFrame1.OnGetImageIndexOfUnknowChar :=
        GetImageIndexOfUnknowChar;

      OlfFMXTextImageFrame2.Font := OlfFMXTextImageFrame1.Font;
      OlfFMXTextImageFrame2.OnGetImageIndexOfUnknowChar :=
        OlfFMXTextImageFrame1.OnGetImageIndexOfUnknowChar;

      OlfFMXTextImageFrame3.Font := OlfFMXTextImageFrame1.Font;
      OlfFMXTextImageFrame3.OnGetImageIndexOfUnknowChar :=
        OlfFMXTextImageFrame1.OnGetImageIndexOfUnknowChar;
    end);
end;

function TForm1.GetImageIndexOfUnknowChar(Sender: TOlfFMXTextImageFrame;
AChar: char): integer;
begin
  result := -1;
  if charinset(AChar, ['a' .. 'z']) then // _a, _b, _c in source images
  begin
    result := Sender.getImageIndexOfChar('_' + AChar);
    if (result < 0) then
      result := Sender.getImageIndexOfChar(uppercase(AChar));
  end;
end;

initialization

randomize;
{$IFDEF DEBUG}
ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
