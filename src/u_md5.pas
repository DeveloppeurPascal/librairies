unit u_md5;

interface

/// <summary>Get MD5 code from a string</summary>
/// <param name="AString">String to encode</param>
/// <returns>The function return the MD5 of the AString string.</returns>
/// <remarks>
/// Before Delphi 10 Seattle this function uses IdHashMessageDigest from Iny.
/// Since Delphi 10 Seattle it uses System.Hash.THashMD5 from Embarcadero.
/// </remarks>
function MD5(const AString: String): String;

implementation

uses
{$IF CompilerVersion>=30.0}
  System.Hash,
{$ELSE}
  IdHashMessageDigest,
{$ENDIF}
  System.SysUtils;

function MD5(const AString: String): String;
{$IF CompilerVersion>=30.0}
{$ELSE}
var
  ch: string;
{$ENDIF}
begin
{$IF CompilerVersion>=30.0}
  result := THashMD5.GetHashString(AString).ToLower;
{$ELSE}
  with TIdHashMessageDigest5.Create do
  begin
    ch := HashStringAsHex(AString);
    Free;
  end;
  result := ch.ToLower;
{$ENDIF}
end;

end.
