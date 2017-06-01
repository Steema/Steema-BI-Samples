unit FindSampleData;

interface

// Find data files and databases suitable to be imported by TeeBI.

uses
  Vcl.ComCtrls,
  System.Classes;

procedure FillSampleData(const ATree:TTreeView);

function PathOf(const ANode:TTreeNode):String;

function BISamplesFolder:String;

implementation

{$IF CompilerVersion>26}
{$DEFINE HASFIREDAC}
{$ENDIF}

uses
  Winapi.Windows, System.Win.Registry, System.SysUtils,
  BI.DataSource, System.IOUtils, System.Types, BI.Persist,

  {$IFDEF HASFIREDAC}
  BI.DB.Fire.AllDrivers,
  {$ENDIF}

  BI.Dataset, BI.ClientDataset,
  BI.XMLData, BI.JSON, BI.Excel, BI.HTML, BI.DB;

function GetStudioDemosFolder(const AVersion:Integer; out AFolder:String):Boolean;
var R : TRegistry;
begin
  result:=False;

  R:=TRegistry.Create(KEY_READ);
  try
    if R.OpenKeyReadOnly('SOFTWARE\Embarcadero\BDS\'+IntToStr(AVersion)+'.0\Globals') then
    begin
      AFolder:=R.ReadString('InstalledDemosDir');

      result:=AFolder<>'';
    end;
  finally
    R.Free;
  end;
end;

// Try to find a RAD Studio installation, and its "Samples" folder
function FindStudioSamplesFolder:String;
var t : Integer;
begin
  for t:=20 downto 10 do
      if GetStudioDemosFolder(t,result) then
         Exit;

  result:='';
end;

// Get all files in RAD Studio Samples\Data folder that can be imported
procedure FindDataFiles(const Parent:TTreeNode; const AFolder:String);
var S : String;
    Tree : TTreeView;
begin
  if AFolder<>'' then
  begin
    Tree:=TTreeView(Parent.TreeView);

    for S in TDirectory.GetFiles(AFolder) do
        if TBIFileImporters.GuessExtension(TPath.GetExtension(S))<>nil then
           Tree.Items.AddChild(Parent,TPath.GetFileName(S));
  end;
end;

function BISamplesFolder:String;
begin
  if TStores.Exists('BISamples') then
     result:=TStore.PathOf('BISamples')
  else
     result:='';
end;

procedure FindBISamples(const ATree:TTreeView);
var s,
    Folder : String;

    SubNode,
    Node : TTreeNode;
begin
  Folder:=BISamplesFolder;

  if TDirectory.Exists(Folder) then
  begin
    Node:=ATree.Items.AddChild(nil,'TeeBI Samples');
    FindDataFiles(Node,Folder);

    for s in TDirectory.GetDirectories(Folder) do
    begin
      SubNode:=ATree.Items.AddChild(Node,TPath.GetFileName(s));
      FindDataFiles(SubNode,s);
    end;
  end;
end;

procedure FillSampleData(const ATree:TTreeView);
var Demos : String;
    Node : TTreeNode;
begin
  ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;

    FindBISamples(ATree);

    Demos:=FindStudioSamplesFolder;

    if Demos<>'' then
    begin
      Node:=ATree.Items.AddChild(nil,'Embarcadero');
      FindDataFiles(Node,Demos+'\Data');
    end;
  finally
    ATree.Items.EndUpdate;
  end;
end;

function FullPathOf(const ANode:TTreeNode):String;
begin
  result:=ANode.Text;

  if (ANode.Parent<>nil) and (ANode.Parent.Parent<>nil) then
     result:=TPath.Combine(FullPathOf(ANode.Parent),result);
end;

function PathOf(const ANode:TTreeNode):String;
begin
  if ANode.Parent=nil then
     result:=''
  else
  begin
    if SameText(ANode.Parent.Text,'Embarcadero') then
       result:=TPath.Combine(FindStudioSamplesFolder,'Data')
    else
       result:=BISamplesFolder;

    result:=TPath.Combine(result,FullPathOf(ANode));
  end
end;

end.
