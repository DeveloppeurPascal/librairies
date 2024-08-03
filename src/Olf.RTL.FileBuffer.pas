/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Prémartin under AGPL 3.0 license.
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
///      Patrick PREMARTIN
///
/// Site :
///      https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
///      https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:15
/// Signature : 21a2aa3cbee6ef4896c01298de4dab4506ef67fa
/// ***************************************************************************
/// </summary>

unit Olf.RTL.FileBuffer;

interface

uses
  System.Classes;

type
  TOlfFileBuffer = class
  private
    FFileInMemory: TMemoryStream;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(Const AFileName: string);
    procedure LoadFromFile(Const AFileName: string);
    procedure SaveToStream(Const AStream: TStream);
    procedure LoadFromStream(Const AStream: TStream);
    function Size: int64;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils;

{ TOlfFileBuffer }

constructor TOlfFileBuffer.Create;
begin
  inherited;
  FFileInMemory := TMemoryStream.Create;
end;

destructor TOlfFileBuffer.Destroy;
begin
  FFileInMemory.free;
  inherited;
end;

procedure TOlfFileBuffer.LoadFromFile(const AFileName: string);
begin
  FFileInMemory.Clear;
  if AFileName.IsEmpty then
    raise Exception.Create('Empty file name.');
  if not tfile.Exists(AFileName) then
    raise Exception.Create('This file doesn''t exist.');
  FFileInMemory.LoadFromFile(AFileName);
end;

procedure TOlfFileBuffer.LoadFromStream(const AStream: TStream);
var
  Size: Int64;
begin
  FFileInMemory.Clear;
  if not assigned(AStream) then
    raise Exception.Create('No stream to copy from.');
  if (sizeof(Size) <> AStream.Read(Size, sizeof(Size))) then
    raise Exception.Create('Wrong stream format.');
  if (Size > 0) and (Size <> FFileInMemory.CopyFrom(AStream, Size)) then
    raise Exception.Create('Copy incomplete.');
end;

procedure TOlfFileBuffer.SaveToFile(const AFileName: string);
begin
  if AFileName.IsEmpty then
    raise Exception.Create('Empty file name.');
  FFileInMemory.SaveToFile(AFileName);
end;

procedure TOlfFileBuffer.SaveToStream(const AStream: TStream);
var
  Size: Int64;
begin
  if not assigned(AStream) then
    raise Exception.Create('No stream to copy to.');
  Size := FFileInMemory.Size;
  AStream.Write(Size, sizeof(Size));
  if (Size > 0) then
  begin
    FFileInMemory.Position := 0;
    if (Size <> AStream.CopyFrom(FFileInMemory)) then
      raise Exception.Create('Copy incomplete.');
  end;
end;

function TOlfFileBuffer.Size: int64;
begin
  result := FFileInMemory.Size;
end;

end.
