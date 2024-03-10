unit u_GenerationUtilitaire;

// (c) Patrick Prémartin / Olf Software 12/2000

interface

uses
    db;

function changer_accents (ch : string) : string;
function compression_blancs (ch : string) : string;
function remplacer_marqueur_fichier (var ch : string; tab: tdataset; marqueur, nom_zone: string): boolean;
function modifier_texte_javascript (ch : string) : string;
function URL_Encode (url: string): string;

implementation

uses
    SysUtils;

function URL_Encode (url: string): string;
var
   n : longint;
   ch : string;
begin
  ch := '';
  for n := 1 to length (url) do
    case url [n] of
      #32: ch := ch + '+';
      #33..#47, #58..#64, #91..#96, #123..#255: ch := ch + '%' + format ('%x', [ord (url [n])]);
    else
      ch := ch + url [n];
    end;
  {endfor}
  result := ch;
end;

function changer_accents (ch : string) : string;
var
   n : longint;
begin
  n := pos ('& ', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&amp;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('& ', ch);
    end;
  {endwhile}
  n := pos ('<', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&lt;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('<', ch);
    end;
  {endwhile}
  n := pos ('>', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&gt;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('>', ch);
    end;
  {endwhile}
  n := pos (#$d, ch);
  while (n > 0) do
    begin
      delete (ch, n, 1);
      n := pos (#$d, ch);
    end;
  {endwhile}
  n := pos (#$a#$a, ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'<BR>&nbsp;'+copy (ch, succ (n), length (ch)-n);
      n := pos (#$a, ch);
    end;
  {endwhile}
  n := pos (#$a, ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'<BR>'+copy (ch, succ (n), length (ch)-n);
      n := pos (#$a, ch);
    end;
  {endwhile}
  n := pos ('â', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&acirc;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('â', ch);
    end;
  {endwhile}
  n := pos ('à', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&agrave;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('à', ch);
    end;
  {endwhile}
  n := pos ('ä', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&auml;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ä', ch);
    end;
  {endwhile}
  n := pos ('é', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&eacute;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('é', ch);
    end;
  {endwhile}
  n := pos ('ê', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&ecirc;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ê', ch);
    end;
  {endwhile}
  n := pos ('è', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&egrave;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('è', ch);
    end;
  {endwhile}
  n := pos ('ë', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&euml;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ë', ch);
    end;
  {endwhile}
  n := pos ('î', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&icirc;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('î', ch);
    end;
  {endwhile}
  n := pos ('ï', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&iuml;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ï', ch);
    end;
  {endwhile}
  n := pos ('ô', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&ocirc;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ô', ch);
    end;
  {endwhile}
  n := pos ('ö', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&ouml;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ö', ch);
    end;
  {endwhile}
  n := pos ('û', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&ucirc;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('û', ch);
    end;
  {endwhile}
  n := pos ('ù', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&ugrave;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ù', ch);
    end;
  {endwhile}
  n := pos ('ü', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&uuml;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ü', ch);
    end;
  {endwhile}
  n := pos ('ç', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&ccedil;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('ç', ch);
    end;
  {endwhile}
  n := pos ('"', ch);
  while (n > 0) do
    begin
      ch := copy (ch, 1, pred (n))+'&quot;'+copy (ch, succ (n), length (ch)-n);
      n := pos ('"', ch);
    end;
  {endwhile}
  Result := ch;
end; { Changer_Accents }

function compression_blancs (ch : string) : string;
begin
  while ((length (ch) > 0) and (ch [length (ch)] = ' ')) do
    delete (ch, length (ch), 1);
  {endwhile}
  Result := ch;
end; { Compression_Blancs }

function remplacer_marqueur_fichier (var ch : string; tab: tdataset; marqueur, nom_zone: string): boolean;
var
   n : Integer;
   res: string;
begin
//  writeln (marqueur, ':', nom_zone, '<br>');
  result := false;
  n := pos (UpperCase (marqueur), UpperCase (ch));
  if (n > 0)
  then
    begin
      result := true;
      delete (ch, n, length (marqueur));
      if (Tab.FieldByName (Nom_Zone).DataType = ftCurrency)
      then
        res := FormatFloat ('#####0.00', Tab.FieldByName (Nom_Zone).AsFloat)
      else
        res := Compression_blancs (Tab.FieldByName (Nom_Zone).AsString);
      {endif}
      if ((copy (marqueur, length (marqueur) - 4, 3) = ' JS') or (copy (marqueur, length (marqueur) - 2, 3) = ' JS'))
      then
        ch := copy (ch, 1, pred (n))+modifier_texte_javascript (Changer_Accents (res))+copy (ch, n, succ (length (ch)-n))
      else
        ch := copy (ch, 1, pred (n))+Changer_Accents (res)+copy (ch, n, succ (length (ch)-n));
      {endif}
    end;
  {endif}
end; { Remplacer_Marqueur_Fichier }

function modifier_texte_javascript (ch : string) : string;
var
   Res : string;
   i : longint;
begin
  res := '';
  for i := 1 to length (ch) do
    case ch [i] of
      '''' : res := res + '\''';
      '\' : res := res + '\\';
    else
      res := res + ch [i];
    end;
  {endfor}
  Result := res;
end;

end.
