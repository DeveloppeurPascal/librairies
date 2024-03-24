unit fMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  Olf.Vcl.Streams;

procedure TForm2.Button1Click(Sender: TObject);
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

initialization

ReportMemoryLeaksOnShutdown := true;

end.
