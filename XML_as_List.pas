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
