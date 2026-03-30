(* C2PP
  ***************************************************************************

  My libraries for Delphi
  Copyright (c) 1990-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  This repository contains functions, procedures and classes to use in
  Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.

  The units to be used in your projects can be found in the "src" folder.
  Some features are explained on my blog or have been coded live on Twitch.

  Examples of use in the form of VCL or FireMonkey projects are available in
  the "samples" subfolder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://librairies.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/librairies

  ***************************************************************************
  File last update : 2026-03-30T16:35:19.617+02:00
  Signature : 985abdb151e0de7c45fe66f38e87091a2f280755
  ***************************************************************************
*)

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
