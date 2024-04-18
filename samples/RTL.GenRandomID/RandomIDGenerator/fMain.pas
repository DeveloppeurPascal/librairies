unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses Olf.RTL.GenRandomID;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.lines.add('Base 2 => ' + TOlfRandomIDGenerator.getidbase2
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.lines.add('Base 10 => ' + TOlfRandomIDGenerator.getidbase10
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.lines.add('Base 36 => ' + TOlfRandomIDGenerator.getIDBase36
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.lines.add('Base 62 => ' + TOlfRandomIDGenerator.getIDBase62
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

end.
