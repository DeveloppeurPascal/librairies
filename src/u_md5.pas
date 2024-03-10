unit u_md5;

interface

/// <summary>Get MD5 code from a string</summary>
/// <param name="S">String to encode</param>
/// <returns>The function return the MD5 of the S string.</returns>
function MD5(S: String): String;

implementation

uses
  IdHashMessageDigest, System.SysUtils;

function MD5(S: String): String;
var
  ch: string;
begin
  with TIdHashMessageDigest5.Create do
  begin
    ch := HashStringAsHex(S);
    DisposeOf;
  end;
  result := ch.ToLower;
end;

end.
