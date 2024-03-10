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
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.RTL.SystemAppearance;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if isSystemThemeInLightMode then
    showmessage('Light mode')
  else if isSystemThemeInDarkMode then
    showmessage('Dark mode')
  else
    showmessage('j''sais pas, et toi ?');
end;

end.
