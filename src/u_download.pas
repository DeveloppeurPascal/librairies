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
/// Signature : 630170f29491a7b7c73b302d6a5dfed7ba5e2aad
/// ***************************************************************************
/// </summary>

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
