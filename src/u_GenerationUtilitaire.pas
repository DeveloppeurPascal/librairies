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
  File last update : 2026-03-30T16:35:19.678+02:00
  Signature : 945a62154efb0917320116e61a943e1087d039b0
  ***************************************************************************
*)

unit u_GenerationUtilitaire;

// (c) Patrick Prémartin / Olf Software 12/2000 - 2024

interface

uses
  db;

function changer_accents(ch: string): string;
function compression_blancs(ch: string): string;
function remplacer_marqueur_fichier(var ch: string; tab: tdataset;
  marqueur, nom_zone: string): boolean;
function modifier_texte_javascript(ch: string): string;
function URL_Encode(url: string): string;
function ReverseString(From: string): string;

implementation

uses
  SysUtils;

function URL_Encode(url: string): string;
var
  n: longint;
  ch: string;
begin
  ch := '';
  for n := 1 to length(url) do
    case url[n] of
      #32:
        ch := ch + '+';
      #33 .. #47, #58 .. #64, #91 .. #96, #123 .. #255:
        ch := ch + '%' + format('%x', [ord(url[n])]);
    else
      ch := ch + url[n];
    end;
  { endfor }
  result := ch;
end;

function changer_accents(ch: string): string;
var
  n: longint;
begin
  n := pos('& ', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&amp;' + copy(ch, succ(n), length(ch) - n);
    n := pos('& ', ch);
  end;
  { endwhile }
  n := pos('<', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&lt;' + copy(ch, succ(n), length(ch) - n);
    n := pos('<', ch);
  end;
  { endwhile }
  n := pos('>', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&gt;' + copy(ch, succ(n), length(ch) - n);
    n := pos('>', ch);
  end;
  { endwhile }
  n := pos(#$d, ch);
  while (n > 0) do
  begin
    delete(ch, n, 1);
    n := pos(#$d, ch);
  end;
  { endwhile }
  n := pos(#$a#$a, ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '<BR>&nbsp;' +
      copy(ch, succ(n), length(ch) - n);
    n := pos(#$a, ch);
  end;
  { endwhile }
  n := pos(#$a, ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '<BR>' + copy(ch, succ(n), length(ch) - n);
    n := pos(#$a, ch);
  end;
  { endwhile }
  n := pos('â', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&acirc;' + copy(ch, succ(n), length(ch) - n);
    n := pos('â', ch);
  end;
  { endwhile }
  n := pos('à', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&agrave;' + copy(ch, succ(n), length(ch) - n);
    n := pos('à', ch);
  end;
  { endwhile }
  n := pos('ä', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&auml;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ä', ch);
  end;
  { endwhile }
  n := pos('é', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&eacute;' + copy(ch, succ(n), length(ch) - n);
    n := pos('é', ch);
  end;
  { endwhile }
  n := pos('ê', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&ecirc;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ê', ch);
  end;
  { endwhile }
  n := pos('è', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&egrave;' + copy(ch, succ(n), length(ch) - n);
    n := pos('è', ch);
  end;
  { endwhile }
  n := pos('ë', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&euml;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ë', ch);
  end;
  { endwhile }
  n := pos('î', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&icirc;' + copy(ch, succ(n), length(ch) - n);
    n := pos('î', ch);
  end;
  { endwhile }
  n := pos('ï', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&iuml;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ï', ch);
  end;
  { endwhile }
  n := pos('ô', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&ocirc;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ô', ch);
  end;
  { endwhile }
  n := pos('ö', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&ouml;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ö', ch);
  end;
  { endwhile }
  n := pos('û', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&ucirc;' + copy(ch, succ(n), length(ch) - n);
    n := pos('û', ch);
  end;
  { endwhile }
  n := pos('ù', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&ugrave;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ù', ch);
  end;
  { endwhile }
  n := pos('ü', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&uuml;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ü', ch);
  end;
  { endwhile }
  n := pos('ç', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&ccedil;' + copy(ch, succ(n), length(ch) - n);
    n := pos('ç', ch);
  end;
  { endwhile }
  n := pos('"', ch);
  while (n > 0) do
  begin
    ch := copy(ch, 1, pred(n)) + '&quot;' + copy(ch, succ(n), length(ch) - n);
    n := pos('"', ch);
  end;
  { endwhile }
  result := ch;
end; { Changer_Accents }

function compression_blancs(ch: string): string;
begin
  while ((length(ch) > 0) and (ch[length(ch)] = ' ')) do
    delete(ch, length(ch), 1);
  { endwhile }
  result := ch;
end; { Compression_Blancs }

function remplacer_marqueur_fichier(var ch: string; tab: tdataset;
  marqueur, nom_zone: string): boolean;
var
  n: Integer;
  res: string;
begin
  // writeln (marqueur, ':', nom_zone, '<br>');
  result := false;
  n := pos(UpperCase(marqueur), UpperCase(ch));
  if (n > 0) then
  begin
    result := true;
    delete(ch, n, length(marqueur));
    if (tab.FieldByName(nom_zone).DataType = ftCurrency) then
      res := FormatFloat('#####0.00', tab.FieldByName(nom_zone).AsFloat)
    else
      res := compression_blancs(tab.FieldByName(nom_zone).AsString);
    { endif }
    if ((copy(marqueur, length(marqueur) - 4, 3) = ' JS') or
      (copy(marqueur, length(marqueur) - 2, 3) = ' JS')) then
      ch := copy(ch, 1, pred(n)) + modifier_texte_javascript
        (changer_accents(res)) + copy(ch, n, succ(length(ch) - n))
    else
      ch := copy(ch, 1, pred(n)) + changer_accents(res) +
        copy(ch, n, succ(length(ch) - n));
    { endif }
  end;
  { endif }
end; { Remplacer_Marqueur_Fichier }

function modifier_texte_javascript(ch: string): string;
var
  res: string;
  i: longint;
begin
  res := '';
  for i := 1 to length(ch) do
    case ch[i] of
      '''':
        res := res + '\''';
      '\':
        res := res + '\\';
    else
      res := res + ch[i];
    end;
  { endfor }
  result := res;
end;

function ReverseString(From: string): string;
begin
  result := '';
  for var i := length(From) - 1 downto 0 do
    result := result + From.Chars[i];
end;

end.
