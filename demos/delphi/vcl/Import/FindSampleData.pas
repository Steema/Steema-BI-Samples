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

uses
  Winapi.Windows, System.Win.Registry, System.SysUtils,
  BI.DataSource, System.IOUtils, System.Types, BI.Persist,

  BI.Data.DB.FireDAC.AllDrivers, BI.Data.Dataset, BI.Data.ClientDataset,
  BI.Data.XML, BI.Data.JSON, BI.Data.Excel, BI.Data.HTML, BI.Data.DB;

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

// Returns the top root node of ATree that has AText text
function FindNode(const AParent:TTreeNode; const AText:String):TTreeNode;
var t : Integer;
    Tree : TTreeView;
    Nodes : TTreeNodes;
begin
  Tree:=TTreeView(AParent.TreeView);
  Nodes:=Tree.Items;

  for t:=0 to Nodes.Count-1 do
      if Nodes[t].Parent=AParent then
         if SameText(Nodes[t].Text,AText) then
            Exit(Nodes[t]);

  result:=Nodes.AddChild(AParent,AText);
end;

// Get all files in RAD Studio Samples\Data folder that can be imported
procedure FindDataFiles(const ATree:TTreeView; const AParent,AFolder:String);
var Ext,
    S : String;
    Parent,
    Node : TTreeNode;
begin
  if AFolder<>'' then
  begin
    Parent:=ATree.Items.AddChild(nil,AParent);

    for S in TDirectory.GetFiles(AFolder) do
    begin
       Ext:=TPath.GetExtension(S);

       if TBIFileImporters.GuessExtension(Ext)<>nil then
       begin
         if SameText(Ext,'.sdb') then
            Node:=FindNode(Parent,'SQLite')
         else
         if SameText(Ext,'.xml') then
            Node:=FindNode(Parent,'XML')
         else
         if SameText(Ext,'.json') then
            Node:=FindNode(Parent,'JSON')
         else
         if SameText(Ext,'.csv') then
            Node:=FindNode(Parent,'CSV')
         else
         if SameText(Ext,'.cds') then
            Node:=FindNode(Parent,'TClientDataset')
         else
         if SameText(Ext,'.mdb') then
            Node:=FindNode(Parent,'Microsoft Access')
         else
//         if SameText(Ext,'.dbf') then
//            Node:=FindNode(Parent,'dBase')
//         else
//         if SameText(Ext,'.db') then
//            Node:=FindNode(Parent,'Paradox')
//         else
//         if SameText(Ext,'.gdb') then
//            Node:=FindNode(Parent,'Interbase')
//         else
            Node:=nil; //FindNode(Parent,'Other');

         if Node<>nil then
            ATree.Items.AddChild(Node,TPath.GetFileNameWithoutExtension(S));
       end;
     end;
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
var Folder : String;
begin
  Folder:=BISamplesFolder;

  if TDirectory.Exists(Folder) then
     FindDataFiles(ATree,'TeeBI Samples',Folder);
end;

procedure FillSampleData(const ATree:TTreeView);
var Demos : String;
begin
  ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;

    FindBISamples(ATree);

    Demos:=FindStudioSamplesFolder;

    if Demos<>'' then
       FindDataFiles(ATree,'Embarcadero',Demos+'\Data');
  finally
    ATree.Items.EndUpdate;
  end;
end;

function ExtensionOf(const ANode:TTreeNode):String;
begin
  if SameText(ANode.Text,'XML') then
     result:='.xml'
  else
  if SameText(ANode.Text,'JSON') then
     result:='.json'
  else
  if SameText(ANode.Text,'CSV') then
     result:='.csv'
  else
  if SameText(ANode.Text,'SQLite') then
     result:='.sdb'
  else
  if SameText(ANode.Text,'TClientDataset') then
     result:='.cds'
  else
  if SameText(ANode.Text,'dBase') then
     result:='.dbf'
  else
  if SameText(ANode.Text,'Microsoft Access') then
     result:='.mdb'
  else
     result:='';
end;

function PathOf(const ANode:TTreeNode):String;
begin
  if (ANode.Parent=nil) or (ANode.Parent.Parent=nil) then
     result:=''
  else
  if SameText(ANode.Parent.Parent.Text,'Embarcadero') then
     result:=FindStudioSamplesFolder+'\Data\'+ANode.Text+ExtensionOf(ANode.Parent)
  else
     result:=BISamplesFolder+'\'+ANode.Text+ExtensionOf(ANode.Parent)
end;

end.
