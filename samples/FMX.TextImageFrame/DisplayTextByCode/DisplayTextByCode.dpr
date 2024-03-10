program DisplayTextByCode;

uses
  System.StartUpCopy,
  FMX.Forms,
  fDisplayTextByCode in 'fDisplayTextByCode.pas' {Form1},
  udm_CharacterImages in '..\_CharacterImages\udm_CharacterImages.pas' {dm_CharacterImages: TDataModule},
  Olf.FMX.TextImageFrame in '..\..\..\src\Olf.FMX.TextImageFrame.pas' {OlfFMXTextImageFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tdm_CharacterImages, dm_CharacterImages);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
