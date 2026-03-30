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
  File last update : 2026-03-30T16:35:19.599+02:00
  Signature : 137da3fca97a2b5663f1919a6e48146f3a8ed25a
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
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    btnSaveImagesToStream: TButton;
    btnClearImagesBitmap: TButton;
    btnLoadImagesFromStream: TButton;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelLayout2: TGridPanelLayout;
    procedure btnSaveImagesToStreamClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearImagesBitmapClick(Sender: TObject);
    procedure btnLoadImagesFromStreamClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    MesImages: TStream;

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Olf.FMX.Streams;

procedure TForm1.btnClearImagesBitmapClick(Sender: TObject);
begin
  Image1.bitmap.Clear(TAlphaColors.Antiquewhite);
  Image2.bitmap.Clear(TAlphaColors.Beige);
end;

procedure TForm1.btnLoadImagesFromStreamClick(Sender: TObject);
var
  bmp: tbitmap;
begin
  MesImages.Position := 0;
  bmp := LoadBitmapFromStream(MesImages);
  try
    Image2.bitmap.assign(bmp);
  finally
    bmp.free;
  end;
  bmp := LoadBitmapFromStream(MesImages);
  try
    Image1.bitmap.assign(bmp);
  finally
    bmp.free;
  end;
end;

procedure TForm1.btnSaveImagesToStreamClick(Sender: TObject);
begin
  MesImages.Position := 0;
  SaveBitmapToStream(Image1.bitmap, MesImages);
  SaveBitmapToStream(Image2.bitmap, MesImages);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MesImages := tmemorystream.create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MesImages.free;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
