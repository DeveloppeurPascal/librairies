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
  File last update : 2026-03-30T16:35:19.666+02:00
  Signature : 228503897cdcc4f538889f2752116eba0b806c18
  ***************************************************************************
*)

unit XML_as_List;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, MSXML_TLB;

type
  TXML_as_List = class(TComponent)
  private
    { Déclarations privées }
    fActive: Boolean;
    fFileName: WideString;
    fDocument: TDOMDocument;
    fNoeud_Actuel : IXMLDOMNode;
    procedure Set_Active(const Value: Boolean);
    procedure Set_FileName(const Value: WideString);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    function Bof: boolean;
    procedure Close;
    constructor Create (AOwner: TComponent); override;
    function Eof: boolean;
    function FieldByName_AsString (FieldName: WideString): WideString;
    function FieldByName_AsInteger (FieldName: WideString): Int64;
    function FieldByName_AsFloat (FieldName: WideString): Extended;
    procedure First;
    procedure Last;
    procedure Next;
    function Open (FileName: WideString): boolean;
    procedure Previous;
  published
    { Déclarations publiées }
    property Active : Boolean read fActive write Set_Active default false;
    property FileName : WideString read fFileName write Set_FileName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OlfSoftware', [TXML_as_List]);
end;

{ TXML_as_List }

function TXML_as_List.Bof: boolean;
begin
  Result := (fNoeud_Actuel = nil);
end;

procedure TXML_as_List.Close;
begin
  Active := False;
end;

constructor TXML_as_List.Create(AOwner: TComponent);
begin
  inherited;
  fDocument := TDOMDocument.Create (Self);
  fNoeud_Actuel := nil;
end;

function TXML_as_List.Eof: boolean;
begin
  Result := (fNoeud_Actuel = nil);
end;

function TXML_as_List.FieldByName_AsFloat(FieldName: WideString): Extended;
begin
  try
    Result := StrToFloat (FieldByName_AsString (FieldName));
  except
    Result := 0;
  end;
end;

function TXML_as_List.FieldByName_AsInteger(
  FieldName: WideString): Int64;
begin
  try
    Result := StrToInt64 (FieldByName_AsString (FieldName));
  except
    Result := 0;
  end;
end;

function TXML_as_List.FieldByName_AsString(
  FieldName: WideString): WideString;
var
   noeud: IXMLDOMNode;
begin
  if (fNoeud_Actuel <> nil) then begin
    noeud := fNoeud_Actuel.firstChild;
    while ((noeud <> nil) and (LowerCase (noeud.baseName) <> LowerCase (FieldName))) do
      noeud := noeud.nextSibling;
    {endwhile}
    if (noeud <> nil) then
      Result := noeud.text
    else
      Result := '';
    {endif}
  end else
    Result := '';
  {endif}
end;

procedure TXML_as_List.First;
begin
  if (fActive) then
    fNoeud_Actuel := fDocument.documentElement.firstChild;
  {endif}
end;

procedure TXML_as_List.Last;
begin
  if (fActive) then
    fNoeud_Actuel := fDocument.documentElement.lastChild;
  {endif}
end;

procedure TXML_as_List.Next;
begin
  fNoeud_Actuel := fNoeud_Actuel.nextSibling;
end;

function TXML_as_List.Open(FileName: WideString): boolean;
begin
  if fActive then
    fActive := False;
  {endif}
  if (fDocument.load (FileName)) then begin
      fFileName := FileName;
      fActive := True;
      First;
  end;
  Result := fActive;
end;

procedure TXML_as_List.Previous;
begin
  fNoeud_Actuel := fNoeud_Actuel.previousSibling;
end;

procedure TXML_as_List.Set_Active(const Value: Boolean);
begin
  if (Value <> fActive) then begin
    fActive := Value;
    if (Value) then
      Open (fFileName)
    else
      fNoeud_Actuel := nil;
    {endif}
  end;
end;

procedure TXML_as_List.Set_FileName(const Value: WideString);
begin
  fFileName := Value;
end;

end.
