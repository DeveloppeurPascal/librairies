unit u_compteur_reg;
{*******************************************************************************
Unit: U_Compteur_Reg
Author: Patrick Prémartin (pprem@lenet.net or pprem@infonie.fr)
Date: 29 March 1998
Comment: This unit use Windows registry for counters.
*******************************************************************************}
interface

type
    TString80 = String [80];

function compteur_add (Application, Code: TString80): Integer;

implementation

uses
    Windows,
    Registry;

const
     NomDeLaCle : string = '\Software\Olf Software\Counter';

var
   BaseDeRegistres: TRegistry;

function compteur_add (Application, Code: TString80): Integer;
var
   ch : string;
begin
  ch := Application+' * '+Code;
  try
    Result := BaseDeRegistres.ReadInteger (ch);
  except
    Result := 0;
  end;
  inc (Result);
  BaseDeRegistres.WriteInteger (ch, Result);
end; { Compteur_Add }

initialization
  BaseDeRegistres := TRegistry.Create;
  BaseDeRegistres.RootKey := HKEY_Local_Machine;
  BaseDeRegistres.OpenKey (NomDeLaCle, not BaseDeRegistres.KeyExists (NomDeLaCle));

finalization
  BaseDeRegistres.Free;

end.
