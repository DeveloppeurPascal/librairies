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
/// Signature : 32a84c98d916b2b29a8f60f8a1a7be612a01bc6e
/// ***************************************************************************
/// </summary>

unit u_util;

interface

function sans_decimal_separator (ch: string): string;

function sans_les_blancs (ch : string): string;

function convert_date (date: string): string;

function convert_heure (heure: string): string;

implementation

uses
    SysUtils;

function sans_decimal_separator (ch: string): string;
var
   n : integer;
begin
  if (DecimalSeparator <> '.') then begin
    n := pos (DecimalSeparator, ch);
    while (n > 0) do begin
      delete (ch, n, Length (DecimalSeparator));
      insert ('.', ch, n);
      n := pos (DecimalSeparator, ch);
    end;
  end;
  Result := ch;
end;

function sans_les_blancs (ch : string): string;
var
   ch2 : string;
begin
  ch2 := '';
  while (ch > '') do begin
    if (ch[1] in ['0'..'9', ',', '.']) then
      ch2 := ch2+ch[1];
    {endif}
    delete (ch, 1, 1);
  end;
  Result := ch2;
end;

function convert_date (date: string): string;
begin
  Result := copy (date, 7, 2)+'/'+copy (date, 5, 2)+'/'+copy (date, 1, 4);
end;

function convert_heure (heure: string): string;
begin
  Result := copy (heure, 1, 2)+':'+copy (heure, 3, 2)+':'+copy (heure, 5, 2);
end;

end.
