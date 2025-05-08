/// <summary>
/// ***************************************************************************
///
/// My libraries for Delphi
///
/// Copyright 1990-2025 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This repository contains functions, procedures and classes to use in
/// Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.
///
/// The units to be used in your projects can be found in the "src" folder.
/// Some features are explained on my blog or have been coded live on Twitch.
///
/// Examples of use in the form of VCL or FireMonkey projects are available in
/// the "samples" subfolder.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://librairies.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 2025-05-08T14:50:09.969+02:00
/// Signature : e03f957194849cc6809ab71e6629d0dba4474a0b
/// ***************************************************************************
/// </summary>

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
