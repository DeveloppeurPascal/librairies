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
/// Signature : 09cad8d9b4796d5bc9d0b29b5d7119b47c06550e
/// ***************************************************************************
/// </summary>

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
