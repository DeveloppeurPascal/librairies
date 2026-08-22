unit fShowDeviceName;

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
  FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm15 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form15: TForm15;

implementation

{$R *.fmx}

uses
  uGetDeviceName;

procedure TForm15.Button1Click(Sender: TObject);
begin
  Label1.Visible := true;
  Button1.Visible := not Label1.Visible;
  Label1.Text := getDeviceName;
end;

procedure TForm15.FormCreate(Sender: TObject);
begin
  Label1.Visible := false;
  Button1.Visible := not Label1.Visible;
end;

end.
