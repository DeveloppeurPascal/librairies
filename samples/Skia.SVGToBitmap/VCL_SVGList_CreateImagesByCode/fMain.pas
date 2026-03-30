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
  File last update : 2026-03-30T16:35:19.638+02:00
  Signature : e0f3bef76bddc460fddc09b59e9867dc4cd3ff9d
  ***************************************************************************
*)

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TForm7 = class(TForm)
    Button1: TButton;
    ScrollBox1: TScrollBox;
    FlowPanel1: TFlowPanel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

uses
  Olf.Skia.SVGToBitmap,
  USVGCursorSVGSamples,
  USVGGameControllerSVGSamples,
  USVGPipesSVGSamples;

procedure TForm7.Button1Click(Sender: TObject);
var
  i: integer;
  img: TImage;
begin
  for i := 0 to TOlfSVGBitmapList.Count - 1 do
  begin
    img := TImage.Create(self);
    img.parent := FlowPanel1;
    img.width := 64;
    img.height := 64;
    img.picture.bitmap.Assign(TOlfSVGBitmapList.bitmap(i, img.width,
      img.height));

    FlowPanel1.height := img.Top + img.height + 10;

    Image1.picture.bitmap.Assign(TOlfSVGBitmapList.bitmap(i, Image1.width,
      Image1.height));
  end;
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  TOlfSVGBitmapList.AddItem(SVGCursorSVGSamples);
  TOlfSVGBitmapList.AddItem(SVGGameControllerSVGSamples);
  TOlfSVGBitmapList.AddItem(SVGPipesSVGSamples);
end;

end.
