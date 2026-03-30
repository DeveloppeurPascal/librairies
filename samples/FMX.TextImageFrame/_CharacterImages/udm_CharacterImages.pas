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
  File last update : 2026-03-30T16:35:19.519+02:00
  Signature : cafd778d0c09a2ee8f31195438ac05c650486a3a
  ***************************************************************************
*)

unit udm_CharacterImages;

// ****************************************
// * Images from folder :
// * C:\Dev\___lib\librairies\samples\FMX.TextImageFrame\_CharacterImages
// ****************************************
//
// This file contains a TDataModule with a 
// TImageList to use in a FireMonkey project.
//
// ****************************************
// File generator : Folder to FMX Image List v1.2
// Website : https://folder2fmximagelist.olfsoftware.fr/
// Generation date : 2025-05-08T14:50:09.956Z
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

uses
  System.SysUtils, 
  System.Classes, 
  System.ImageList,
  FMX.ImgList;

type
  Tdm_CharacterImages = class(TDataModule)
    ImageList: TImageList;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  dm_CharacterImages: Tdm_CharacterImages;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
