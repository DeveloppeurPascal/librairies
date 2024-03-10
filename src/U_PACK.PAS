unit u_pack;
{
Auteur: Patrick Premartin
Date: 28/12/1991. 25/06/1992.
Langage: Turbo Pascal 6.0
But: Proc‚dures de compactage/d‚compactage de donn‚es.
}
interface
procedure pack (deb : pointer; nb_deb : word; var res : pointer; var nb_res : word);
procedure unpack (deb : pointer; var res : pointer; var nb_res : word);

implementation
type
    prec = ^trec;
    trec = array [1..65535] of byte;

procedure pack (deb : pointer; nb_deb : word; var res : pointer; var nb_res : word);
var
   tampon,
   page : prec;
   i,
   j : word;
   k : byte;
begin
     page := deb;
     getmem (tampon, nb_deb);
     if (tampon <> nil)
     then
         begin
              tampon^ [1] := nb_deb div 256;
              tampon^ [2] := nb_deb mod 256;
              i := 2; { Index dans donn‚es de d‚part … compacter. } {page}
              j := 3; { Index dans donn‚es compact‚es d'arriv‚e. } {tampon}
              k := 0; { Nombre d'it‚ration d'une mˆme valeur. }
              while not ((i > nb_deb) or (j > nb_deb)) do
                    begin
                         if ((page^ [i] = page^ [pred (i)]) and (k < 255))
                         then
                             inc (k)
                         else
                             if ((page^ [pred (i)] = 27) or (k > 1))
                             then
                                 begin
                                      tampon^ [j] := 27;
                                      inc (j);
                                      tampon^ [j] := page^ [pred (i)];
                                      inc (j);
                                      tampon^ [j] := k;
                                      inc (j);
                                      k := 0;
                                 end
                             else
                                 if (k = 1)
                                 then
                                     begin
                                          tampon^ [j] := page^ [pred (i)];
                                          inc (j);
                                          tampon^ [j] := page^ [pred (i)];
                                          inc (j);
                                          k := 0;
                                     end
                                 else
                                     begin
                                          tampon^ [j] := page^ [pred (i)];
                                          inc (j);
                                     end;
                                 {endif}
                             {endif}
                         {endif}
                         inc (i);
                    end;
              {endwhile}
              if ((page^ [pred (i)] = 27) or (k > 1))
              then
                  begin
                       tampon^ [j] := 27;
                       inc (j);
                       tampon^ [j] := page^ [pred (i)];
                       inc (j);
                       tampon^ [j] := k;
                       inc (j);
                       k := 0;
                  end
              else
                  if (k = 1)
                  then
                      begin
                           tampon^ [j] := page^ [pred (i)];
                           inc (j);
                           tampon^ [j] := page^ [pred (i)];
                           inc (j);
                           k := 0;
                      end
                  else
                      begin
                           tampon^ [j] := page^ [pred (i)];
                           inc (j);
                      end;
                  {endif}
              {endif}
         end;
     {endif}
     nb_res := pred (j);
     if (nb_res < nb_deb)
     then
         begin
              getmem (res, nb_res);
              if ((res <> nil) and (tampon <> nil))
              then
                  move (tampon^, res^, nb_res);
              {endif}
              if (tampon <> nil)
              then
                  freemem (tampon, nb_deb);
              {endif}
         end
     else
         begin
              if (tampon <> nil)
              then
                  move (deb^, tampon^, nb_deb);
              {endif}
              res := tampon;
              nb_res := nb_deb;
         end;
     {endif}
end; { Pack }

procedure unpack (deb : pointer; var res : pointer; var nb_res : word);
var
   tampon,
   page : prec;
   i,
   j,
   l_max : word;
   k : word;
begin
     page := deb;
     l_max := page^ [1]*256+page^ [2];
     getmem (tampon, l_max);
     if (tampon <> nil)
     then
         begin
              i := 1;
              j := 3;
              while (i <= l_max) do
                    begin
                         if (page^ [j] = 27) {ESC}
                         then
                             begin
                                  inc (j);
                                  k := page^ [succ (j)];
                                  k := succ (k);
                                  repeat
                                        tampon^ [i] := page^ [j];
                                        inc (i);
                                        dec (k);
                                  until (k < 1);
                                  inc (j);
                             end
                         else
                             begin
                                  tampon^ [i] := page^ [j];
                                  inc (i);
                             end;
                         {endif}
                         inc (j);
                    end;
              {endwhile}
         end;
     {endif}
     res := tampon;
     nb_res := l_max;
end; { Unpack }

end. { Fin de l'unit‚ 'U_Pack'. }
