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
/// Patrick PREMARTIN
///
/// Site :
/// https://librairies.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 2024-09-18T09:30:16.000+02:00
/// Signature : afbfa1d133884e028b049ad88cd22e266a451028
/// ***************************************************************************
/// </summary>

unit Olf.RTL.DPROJReader;

interface

uses
  System.Generics.Collections,
  Xml.XMLIntf,
  Olf.RTL.PathAliases;

type
  TOlfFileToDeploy = class
  private
    FToFileName: string;
    FToPath: string;
    FOverwrite: boolean;
    FFromFileName: string;
    FFromPath: string;
    procedure SetFromFileName(const Value: string);
    procedure SetFromPath(const Value: string);
    procedure SetOverwrite(const Value: boolean);
    procedure SetToFileName(const Value: string);
    procedure SetToPath(const Value: string);
  protected
  public
    property FromPath: string read FFromPath write SetFromPath;
    property FromFileName: string read FFromFileName write SetFromFileName;
    property ToPath: string read FToPath write SetToPath;
    property ToFileName: string read FToFileName write SetToFileName;
    property Overwrite: boolean read FOverwrite write SetOverwrite;
    constructor Create;
  end;

  TOlfFilesToDeployList = class(TObjectList<TOlfFileToDeploy>)
  private
  protected
  public
  end;

  TOlfDPROJReader = class
  private
    FXMLDocument: IXMLDocument;
    FBDSVersion: string;
    FonGetPathForAliasFunc: TGetPathForAliasFunc;
    function GetAsXMLNode: IXMLNode;
    function GetAsString: string;
    function GetFileName: string;
    procedure SetBDSVersion(const Value: string);
    procedure SetonGetPathForAliasFunc(const Value: TGetPathForAliasFunc);
  protected
  public
    property FileName: string read GetFileName;
    property AsString: string read GetAsString;
    property AsXMLNode: IXMLNode read GetAsXMLNode;
    property BDSVersion: string read FBDSVersion write SetBDSVersion;
    property onGetPathForAliasFunc: TGetPathForAliasFunc
      read FonGetPathForAliasFunc write SetonGetPathForAliasFunc;
    constructor Create(Const ADPROJFileName: string);
    destructor Destroy; override;
    function GetProjectExecutable(Const APlatform: string;
      Const AConfiguration: string = 'Release'): string;
    function HasPlatform(Const APlatform: string): boolean;
    function GetFilesToDeploy(Const APlatform: string;
      Const AConfiguration: string = 'Release'): TOlfFilesToDeployList;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  Xml.XMLDoc;

{ TOlfFileToDeploy }

constructor TOlfFileToDeploy.Create;
begin
  inherited;
  FToFileName := '';
  FToPath := '';
  FOverwrite := true;
  FFromFileName := '';
  FFromPath := '';
end;

procedure TOlfFileToDeploy.SetFromFileName(const Value: string);
begin
  FFromFileName := Value;
end;

procedure TOlfFileToDeploy.SetFromPath(const Value: string);
begin
  FFromPath := Value;
end;

procedure TOlfFileToDeploy.SetOverwrite(const Value: boolean);
begin
  FOverwrite := Value;
end;

procedure TOlfFileToDeploy.SetToFileName(const Value: string);
begin
  FToFileName := Value;
end;

procedure TOlfFileToDeploy.SetToPath(const Value: string);
begin
  FToPath := Value;
end;

{ TOlfDPROJReader }

constructor TOlfDPROJReader.Create(const ADPROJFileName: string);
begin
  inherited Create;
  if ADPROJFileName.isempty then
    raise exception.Create('Empty file name !');
  if not tfile.Exists(ADPROJFileName) then
    raise exception.Create('This file doesn''t exist !');

  FXMLDocument := txmldocument.Create(ADPROJFileName);
  if not FXMLDocument.Active then
    raise exception.Create('Wrong file format !');

  FBDSVersion := '';
  FonGetPathForAliasFunc := nil;
end;

destructor TOlfDPROJReader.Destroy;
begin
  // no free needed for FXMLDocument which is an interface
  inherited;
end;

function TOlfDPROJReader.GetAsString: string;
begin
  result := FXMLDocument.Xml.Text;
end;

function TOlfDPROJReader.GetAsXMLNode: IXMLNode;
begin
  result := FXMLDocument.ChildNodes.FindNode('Project');
end;

function TOlfDPROJReader.GetFileName: string;
begin
  result := FXMLDocument.FileName;
end;

function TOlfDPROJReader.GetFilesToDeploy(const APlatform, AConfiguration
  : string): TOlfFilesToDeployList;
var
  ProjectNode, ProjectExtensionsNode, BorlandProjectNode, DeploymentNode,
    DeployFileNode, PlatformNode, Node: IXMLNode;
  i, j: integer;
  AliasList: TKeyValueList;
  FileToDeploy: TOlfFileToDeploy;
  FilePath: string;
begin
  result := TOlfFilesToDeployList.Create;
  try
    AliasList := TKeyValueList.Create;
    try
      if tpath.IsRelativePath(tpath.GetDirectoryName(FileName)) then
        AliasList.Add('PROJECTDIR',
          tpath.GetFullPath(tpath.GetDirectoryName(FileName)))
      else
        AliasList.Add('PROJECTDIR', tpath.GetDirectoryName(FileName));

      ProjectNode := FXMLDocument.ChildNodes.FindNode('Project');
      if assigned(ProjectNode) then
      begin
        ProjectExtensionsNode := ProjectNode.ChildNodes.FindNode
          ('ProjectExtensions');
        if assigned(ProjectExtensionsNode) then
        begin
          BorlandProjectNode := ProjectExtensionsNode.ChildNodes.FindNode
            ('BorlandProject');
          if assigned(BorlandProjectNode) then
          begin
            DeploymentNode := BorlandProjectNode.ChildNodes.FindNode
              ('Deployment');
            if assigned(DeploymentNode) and DeploymentNode.HasAttribute
              ('Version') and ((DeploymentNode.Attributes['Version'] = 4) or
              (DeploymentNode.Attributes['Version']
              = 5 (* Delphi 12.2 Athens *) )) then
              // TODO : prendre en charge autres versions du déploiement en XE (bof, mais why not) et 10.x
              for i := 0 to DeploymentNode.ChildNodes.Count - 1 do
              begin
                DeployFileNode := DeploymentNode.ChildNodes[i];
                if (DeployFileNode.NodeName = 'DeployFile') and
                  DeployFileNode.HasAttribute('Configuration') and
                  (DeployFileNode.Attributes['Configuration'] = AConfiguration)
                  and DeployFileNode.HasAttribute('LocalName') then
                begin
                  PlatformNode := nil;
                  for j := 0 to DeployFileNode.ChildNodes.Count - 1 do
                  begin
                    Node := DeployFileNode.ChildNodes[j];
                    if (Node.NodeName = 'Platform') and
                      Node.HasAttribute('Name') and
                      (Node.Attributes['Name'] = APlatform) then
                    begin
                      PlatformNode := Node;
                      AliasList.AddOrSetValue('Platform',
                        Node.Attributes['Name']);
                      AliasList.AddOrSetValue('Configuration',
                        DeployFileNode.Attributes['Configuration']);
                      break;
                    end;
                  end;
                  if assigned(PlatformNode) then
                  begin
                    FileToDeploy := TOlfFileToDeploy.Create;
                    try
                      FilePath := ReplaceAliasesInPath
                        (DeployFileNode.Attributes['LocalName'], AliasList,
                        false, FBDSVersion,
                        function(const AAlias: string): string
                        begin
                          if assigned(onGetPathForAliasFunc) then
                            result := onGetPathForAliasFunc(AAlias)
                          else
                            result := '';
                        end);
                      FileToDeploy.FromPath := tpath.GetDirectoryName(FilePath);
                      FileToDeploy.FromFileName := tpath.GetFileName(FilePath);

                      Node := PlatformNode.ChildNodes.FindNode('RemoteDir');
                      if assigned(Node) and (Node.NodeValue <> '.\') and
                        (Node.NodeValue <> './') then
                        FileToDeploy.ToPath := Node.NodeValue
                      else
                        FileToDeploy.ToPath := '';

                      Node := PlatformNode.ChildNodes.FindNode('RemoteName');
                      if assigned(Node) then
                        FileToDeploy.ToFileName := Node.NodeValue
                      else
                        FileToDeploy.ToFileName :=
                          tpath.GetFileName
                          (DeployFileNode.Attributes['LocalName']);

                      Node := PlatformNode.ChildNodes.FindNode('Overwrite');
                      FileToDeploy.Overwrite := assigned(Node) and
                        (Node.NodeValue);

                      result.Add(FileToDeploy);
                    except
                      FileToDeploy.free;
                      raise;
                    end;
                  end;
                end;
              end;
          end;
        end;
      end;
    finally
      AliasList.free;
    end;
  except
    result.free;
    raise;
  end;
end;

function TOlfDPROJReader.GetProjectExecutable(const APlatform,
  AConfiguration: string): string;
var
  ProjectNode, ProjectExtensionsNode, BorlandProjectNode, DeploymentNode,
    DeployFileNode, Node: IXMLNode;
  i, j: integer;
  AliasList: TKeyValueList;
begin
  result := '';
  AliasList := TKeyValueList.Create;
  try
    if tpath.IsRelativePath(tpath.GetDirectoryName(FileName)) then
      AliasList.Add('PROJECTDIR',
        tpath.GetFullPath(tpath.GetDirectoryName(FileName)))
    else
      AliasList.Add('PROJECTDIR', tpath.GetDirectoryName(FileName));

    ProjectNode := FXMLDocument.ChildNodes.FindNode('Project');
    if assigned(ProjectNode) then
    begin
      ProjectExtensionsNode := ProjectNode.ChildNodes.FindNode
        ('ProjectExtensions');
      if assigned(ProjectExtensionsNode) then
      begin
        BorlandProjectNode := ProjectExtensionsNode.ChildNodes.FindNode
          ('BorlandProject');
        if assigned(BorlandProjectNode) then
        begin
          DeploymentNode := BorlandProjectNode.ChildNodes.FindNode
            ('Deployment');
          if assigned(DeploymentNode) and DeploymentNode.HasAttribute('Version')
            and ((DeploymentNode.Attributes['Version'] = 4) or
            (DeploymentNode.Attributes['Version'] = 5 (* Delphi 12.2 Athens *) ))
          then
            // TODO : prendre en charge autres versions du déploiement en XE (bof, mais why not) et 10.x
            for i := 0 to DeploymentNode.ChildNodes.Count - 1 do
            begin
              DeployFileNode := DeploymentNode.ChildNodes[i];
              if (DeployFileNode.NodeName = 'DeployFile') and
                DeployFileNode.HasAttribute('Configuration') and
                (DeployFileNode.Attributes['Configuration'] = AConfiguration)
                and DeployFileNode.HasAttribute('Class') and
                (DeployFileNode.Attributes['Class'] = 'ProjectOutput') and
                DeployFileNode.HasAttribute('LocalName') then
                for j := 0 to DeployFileNode.ChildNodes.Count - 1 do
                begin
                  Node := DeployFileNode.ChildNodes[j];
                  if (Node.NodeName = 'Platform') and Node.HasAttribute('Name')
                    and (Node.Attributes['Name'] = APlatform) then
                  begin
                    AliasList.AddOrSetValue('Configuration',
                      DeployFileNode.Attributes['Configuration']);
                    AliasList.AddOrSetValue('Platform',
                      Node.Attributes['Name']);
                    result := ReplaceAliasesInPath
                      (DeployFileNode.Attributes['LocalName'], AliasList, false,
                      FBDSVersion,
                      function(const AAlias: string): string
                      begin
                        if assigned(onGetPathForAliasFunc) then
                          result := onGetPathForAliasFunc(AAlias)
                        else
                          result := '';
                      end);
                  end;
                end;
            end;
        end;
      end;
    end;
  finally
    AliasList.free;
  end;
end;

function TOlfDPROJReader.HasPlatform(const APlatform: string): boolean;
var
  ProjectNode, ProjectExtensionsNode, BorlandProjectNode, PlatformsNode,
    Node: IXMLNode;
  i: integer;
begin
  result := false;
  ProjectNode := FXMLDocument.ChildNodes.FindNode('Project');
  if assigned(ProjectNode) then
  begin
    ProjectExtensionsNode := ProjectNode.ChildNodes.FindNode
      ('ProjectExtensions');
    if assigned(ProjectExtensionsNode) then
    begin
      BorlandProjectNode := ProjectExtensionsNode.ChildNodes.FindNode
        ('BorlandProject');
      if assigned(BorlandProjectNode) then
      begin
        PlatformsNode := BorlandProjectNode.ChildNodes.FindNode('Platforms');
        if assigned(PlatformsNode) then
          for i := 0 to PlatformsNode.ChildNodes.Count - 1 do
          begin
            Node := PlatformsNode.ChildNodes[i];
            if Node.HasAttribute('value') and
              (Node.Attributes['value'] = APlatform) then
            begin
              result := true;
              break;
            end;
          end;
      end;
    end;
  end;
end;

procedure TOlfDPROJReader.SetBDSVersion(const Value: string);
begin
  FBDSVersion := Value;
end;

procedure TOlfDPROJReader.SetonGetPathForAliasFunc
  (const Value: TGetPathForAliasFunc);
begin
  FonGetPathForAliasFunc := Value;
end;

end.
