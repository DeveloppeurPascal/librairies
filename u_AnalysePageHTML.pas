unit u_AnalysePageHTML;
{
Cette unité fourni des fonctions et procédures permettant d'interpréter une page
HTML et d'y modifier les éventuels tags.

(c) Patrick Prémartin 21/05/2000

Site : https://developpeur-pascal.fr
}

// (c) Patrick Prémartin / Olf Software 06/2002

interface

type
    TTag_Action = (rien, effacer, remplacer);
    Ttag_traitement_proc = procedure (tag, instruction_html: string; var action: TTag_Action; var nouveau_tag: string);

procedure tag_traiter_fichier (nom_fichier_html: string; proc: Ttag_traitement_proc); overload;

procedure tag_traiter_page (var contenu_fichier: string; proc: Ttag_traitement_proc); overload;

function tag_nom_instruction (tag: string): string;

function tag_valeur (tag, nom_param: string): string;

implementation

uses
    Classes, SysUtils;

procedure tag_traiter_fichier (nom_fichier_html: string; proc: Ttag_traitement_proc);
var
   contenu_fichier: string;
   liste: TSTringList;
begin
  contenu_fichier := '';
  liste := TStringList.Create;
  try
    liste.LoadFromFile (nom_fichier_html);
    contenu_fichier := liste.Text;
    tag_traiter_page (contenu_fichier, proc);
    liste.Text := contenu_fichier;
    liste.SaveToFile (nom_fichier_html);
  finally
    liste.Free;
  end;
end;

procedure tag_traiter(var contenu_fichier: string; tag: string; tag_debut, tag_fin: integer; var position_dans_source: integer; var proc: Ttag_traitement_proc);
var
   i: integer;
   sortie : boolean;
   action_a_faire: TTag_Action;
   Nouveau_Tag : string;
begin
  tag := UpperCase (tag);

  // on enlève les espaces éventuels de début de tag
  sortie := false;
  i := 1;
  while not ((i = length (tag)) or (sortie)) do
    if (tag [i] = ' ') then
      delete (tag, i, 1)
    else if (tag [i] in ['0'..'9', 'A'..'Z']) then
      sortie := true
    else
      inc (i);
    {endif}
  {endwhile}

  if ((copy (tag, 2, 1) >= '0') { tag de début ou seul }
     or ((copy (tag, 2, 1) = '/') and (copy (tag, 3, 1) >= '0'))) {tag de fin } then begin

    if (assigned (proc)) then begin
      proc (tag, tag_nom_instruction (tag), action_a_faire, nouveau_tag);
      case action_a_faire of
        effacer: begin
          position_dans_source := pred (tag_debut);
          while ((contenu_fichier[tag_fin+1]=#13) or (contenu_fichier[tag_fin+1]=#10)) do
            inc (tag_fin);
          {endwhile}
          delete (contenu_fichier, tag_debut, succ (tag_fin - tag_debut));
        end;
        remplacer: begin
          delete (contenu_fichier, tag_debut, succ (tag_fin - tag_debut));
          insert (nouveau_tag, contenu_fichier, tag_debut);
          position_dans_source := pred (tag_debut+length (nouveau_tag));
        end;
      end;
    end;
  end;
end;

procedure tag_traiter_page (var contenu_fichier: string; proc: Ttag_traitement_proc);
var
   i : integer;
   tag: string;
   c: char;
   tag_on: boolean;
   tag_debut, tag_fin : integer;
begin
  tag := '';
  tag_on := false;
  i := 0;
  while (i < length (contenu_fichier)) do begin
    inc (i);
    c := contenu_fichier [i];
    if tag_on then begin
      if (c = '>') then begin
        tag := tag + '>';
        tag_fin := i;
        tag_traiter (contenu_fichier, tag, tag_debut, tag_fin, i, proc);
        tag_on := false;
      end else if (c >= ' ') then begin
        tag := tag + c;
      end else
        tag := tag + ' ';
      {endif}
    end else if (c = '<') then begin
      tag_on := true;
      tag_debut := i;
      tag := '<';
    end;
  end;
  tag := 'Ce texte est exclusivement r&eacute;serv&eacute; aux lecteurs adultes consentants';
  i := pos (tag, contenu_fichier);
  if (i > 0) then begin
    delete (contenu_fichier, i, length (tag));
    insert ('<img src="../boutons/avertissement.gif" alt="Ce texte est exclusivement réservé aux lecteurs adultes consentants">', contenu_fichier, i);
  end;
end;

function tag_nom_instruction(tag: string): string;
var
   i : integer;
   ch : string;
   en_cours: boolean;
   sortie: boolean;
   fin_de_tag : boolean;
begin
  tag := UpperCase (tag);
  en_cours := false;
  sortie:= false;
  fin_de_tag := false;
  i := 0;
  while ((i < length (tag)) and (not sortie)) do begin
    inc (i);
    if ((tag [i] = '/') and (not en_cours)) then
      fin_de_tag := true;
    {endif}
    if (tag [i] in ['0'..'9', 'A'..'Z']) then
      if (en_cours) then
        ch := ch + tag [i]
      else begin
        en_cours := true;
        ch := tag [i];
      end
    else
      sortie := en_cours;
    {endif}
  end;
  if fin_de_tag then
    ch := '/'+ch;
  {endif}
  result := ch;
end;

function tag_valeur(tag, nom_param: string): string;
var
   ch : string;
   n : integer;
   i : integer;
   fin : set of char;
begin
  tag := UpperCase (tag);
  nom_param := UpperCase (nom_param);
  ch := '';
  n := pos (nom_param, tag);
  if (n > 0) then begin
    i := n + length (nom_param);
    while ((i < length (tag)) and (tag [i] in [' ', '='])) do
      inc (i);
    {endwhile}
    if (tag [i] = '"') then begin
      fin := ['"'];
      inc (i);
    end else
      fin := [' ', '>'];
    {endif}
    while ((i < length (tag)) and not (tag [i] in fin)) do begin
      ch := ch + tag [i];
      inc (i);
    end;
  end;
  result := ch;
end;

end.
