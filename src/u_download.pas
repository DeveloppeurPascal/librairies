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
  Signature : f5e0ba863084dc54383b613caaa585340d9b8935
  ***************************************************************************
*)

unit u_download;

interface

uses system.SysUtils;

type
  tdownload_file = class(tobject)
    class procedure download(from_url, to_filename: string;
      success: tproc = nil; error: tproc = nil);
    class function temporaryFileName(appName: string): string;
  end;

implementation

{ download_file }

uses system.ioutils, system.Net.HttpClient, system.Classes;

class procedure tdownload_file.download(from_url, to_filename: string;
  success: tproc = nil; error: tproc = nil);
begin
  tthread.CreateAnonymousThread(
    procedure
    var
      serveur: THTTPClient;
      serveur_reponse: IHTTPResponse;
      fichier: tfilestream;
    begin
      try
        serveur := THTTPClient.Create;
        try
          serveur_reponse := serveur.Get(from_url);
          if serveur_reponse.StatusCode = 200 then
          begin
            fichier := tfilestream.Create(to_filename,
              fmCreate or fmOpenWrite or fmShareDenyWrite);
            try
              fichier.CopyFrom(serveur_reponse.ContentStream,
                serveur_reponse.ContentStream.Size);
            finally
              fichier.Free;
            end;
            if assigned(success) then
              tthread.queue(nil,
                procedure
                begin
                  success;
                end);
          end
          else
          begin
            raise Exception.CreateFmt
              ('Cannot get distant file. Please contact us or retry later. HTTP %d - %s',
              [serveur_reponse.StatusCode, serveur_reponse.StatusText]);
          end;
        finally
          serveur.Free;
        end;
      except
        if assigned(error) then
          tthread.queue(nil,
            procedure
            begin
              error;
            end);
      end;
    end).Start;
end;

class function tdownload_file.temporaryFileName(appName: string): string;
begin
  result := tpath.Combine(tpath.gettempPath,
    appName + '-' + datetimetotimestamp(now).Time.ToString + '.tmp')
end;

end.
