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
  System.IOUtils,
  Olf.FMX.Streams;

procedure TForm1.Button1Click(Sender: TObject);
var
  bmp: tbitmap;
  fs: tfilestream;
  filename: string;
begin
  filename := tpath.GetTempFileName;
  try
    fs := tfilestream.Create(filename, fmCreate);
    try
      SaveBitmapToStream(nil, fs);
      bmp := tbitmap.Create(0, 0);
      try
        SaveBitmapToStream(bmp, fs);
      finally
        bmp.free;
      end;
    finally
      fs.free;
    end;
    fs := tfilestream.Create(filename, fmOpenRead);
    try
      bmp := LoadBitmapFromStream(fs);
      try
        if assigned(bmp) then
          showmessage('BMP Nil :' + bmp.Width.tostring + ',' +
            bmp.Height.tostring)
        else
          showmessage('nil');
      finally
        bmp.free;
      end;
      bmp := LoadBitmapFromStream(fs);
      try
        if assigned(bmp) then
          showmessage('BMP vide :' + bmp.Width.tostring + ',' +
            bmp.Height.tostring)
        else
          showmessage('nil');
      finally
        bmp.free;
      end;
    finally
      fs.free;
    end;
  finally
    tfile.Delete(filename);
  end;
end;

end.
