{*********************************************}
{  TeeBI Software Library                     }
{  Component Selector dialog                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.DataComponent;

interface

{
  This dialog can be used at design-time or runtime to select a TComponent
  that can be a provider of "Data".

  The TDataSelector dialog (VCLBI.DataSelect.pas unit) also uses this dialog.

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,

  {$IFDEF FPC}
  BI.FPC, FGL,
  {$ELSE}
  System.Generics.Collections,
  {$ENDIF}

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Grids, BI.DataItem, Vcl.Menus, Vcl.ExtCtrls;

type
  TFilterEvent=procedure(Sender: TComponent; var Valid:Boolean) of object;

  TDataComponent = class(TForm)
    Tree: TTreeView;
    PopupMenu1: TPopupMenu;
    ViewData1: TMenuItem;
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ViewData1Click(Sender: TObject);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    IComponents : {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TComponent>;

    FRoot : TDataItem;

    FOnFilter : TFilterEvent;
    FOnSelected : TNotifyEvent;

    function CalcName(const AComponent:TComponent; const AName:String):String;
    function CanAdd(const AComponent:TComponent):Boolean;
    procedure DoAddData(const AParent:TTreeNode; const AData:TDataItem);
    procedure FillTree;
  protected
    FCurrent : TObject;
    IEdited : TComponent;

    procedure Add(const AParent:TTreeNode; const AComponent:TComponent; const AName:String); overload;
    procedure Add(const AParent,AComponent:TComponent); overload;
    function NodeOf(const AObject:TObject):TTreeNode;
    function SelectedHasData:Boolean;
    procedure TryFreeData;
  public
    { Public declarations }

    type
      TBIAddComponent=procedure(const AParent:TTreeNode; const AComponent:TComponent; const AName:String) of object;

    class
       var OnGetDesignerNames : TProc<TBIAddComponent,TComponent>;

    function Data(const AOwner:TComponent):TDataItem;

    class function Import(const AOwner:TComponent; const AObject:TObject):TDataItem; static;

    class function Choose(const AOwner,AEdited:TComponent;
                          const ACurrent:TComponent=nil):TComponent; overload; static;

    // Select a data item from ARoot
    class function Choose(const AOwner:TComponent; const ARoot:TDataItem;
                          const ACurrent:TDataItem=nil):TDataItem; overload; static;

    // Multi Select an array of data items from ARoot
    class function ChooseMany(const AOwner:TComponent; const ARoot:TDataItem;
                              const ACurrent:TDataItem=nil;
                              const Compatible:Boolean=False):TDataArray; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const ARoot:TDataItem):TDataComponent; static;

    procedure Select(const AObject:TObject);

    function Selected:TComponent;
    function SelectedData:TDataItem;
    function SelectedItems:TDataArray;

    property OnFilter:TFilterEvent read FOnFilter write FOnFilter;
    property OnSelected:TNotifyEvent read FOnSelected write FOnSelected;
  end;

implementation

{$R *.dfm}

uses
  BI.DataSource,
  Data.DB, VCL.DBCtrls,
  // BDE.DBTables,
  BI.Dataset, BI.Store.Component, BI.Query,
  VCLBI.Component, VCLBI.DataViewer, VCLBI.Grid;

procedure TDataComponent.Add(const AParent, AComponent: TComponent);
begin
  Add(NodeOf(AParent),AComponent,CalcName(AComponent,AComponent.Name));
end;

function TDataComponent.CanAdd(const AComponent:TComponent):Boolean;

  function ValidFiltered(const AComponent:TComponent):Boolean;
  begin
    result:=True;

    if Assigned(FOnFilter) then
       FOnFilter(AComponent,result);
  end;

  function AlreadyAdded:Boolean;
  begin
    result:=(IComponents<>nil) and
            {$IFDEF FPC}
            (IComponents.IndexOf(AComponent)<>-1)
            {$ELSE}
            IComponents.Contains(AComponent)
            {$ENDIF}
            ;
  end;

begin
  result:= (AComponent<>Self) and
           (AComponent<>IEdited) and
           //(AComponent<>Dummy) and
           ValidFiltered(AComponent) and
           (not AlreadyAdded) and
           // Deprecated: (not (AComponent is TCloneProvider)) and
           (
              (AComponent is TCustomForm) or
              (AComponent is TDataModule) or
              TComponentImporter.IsSupported(AComponent)
           );
end;

class function TDataComponent.Choose(const AOwner: TComponent; const ARoot,
  ACurrent: TDataItem): TDataItem;
begin
  with TDataComponent.Create(AOwner) do
  try
    FCurrent:=ACurrent;
    FRoot:=ARoot;

    PanelButtons.Visible:=True;

    if ShowModal=mrOk then
       result:=SelectedData
    else
       result:=nil;
  finally
    Free;
  end;
end;

function TDataComponent.CalcName(const AComponent:TComponent; const AName:String):String;
begin
  if AComponent is TDataProvider then
     result:=TDataProvider(AComponent).Title
  else
     result:=AName;

  if result='' then
  begin
    result:=AComponent.Name;

    if result='' then
       result:=AComponent.ClassName
    else
       result:=result+' ('+AComponent.ClassName+')';
  end;
end;

procedure TDataComponent.DoAddData(const AParent:TTreeNode; const AData:TDataItem);

  procedure InternalAddData(const AParent:TTreeNode; const AData:TDataItem);
  var t : Integer;
      tmpParent : TTreeNode;
      tmp : TDataItem;
      tmpItems : TDataItems;
  begin
    try
      AData.Load;
    except
      on E:Exception do
      begin
        Tree.Items.AddChild(AParent,'Error: '+E.Message);
        Exit;
      end;
    end;

    tmpItems:=AData.Items;

    for t:=0 to tmpItems.Count-1 do
    begin
      tmp:=tmpItems[t];
      tmpParent:=Tree.Items.AddChildObject(AParent,tmp.Name,tmp);
      InternalAddData(tmpParent,tmp);
    end;
  end;

var tmpParent : TTreeNode;
    tmp : String;
begin
  if AData<>nil then
  begin
    AData.Load;

    if AData.Items.Count=1 then
       InternalAddData(AParent,AData.Items[0])
    else
    begin
      tmp:=AData.Name;

      if tmp='' then
         tmp:='Data';

      tmpParent:=Tree.Items.AddChildObject(AParent,tmp,AData);
      InternalAddData(tmpParent,AData);
    end;
  end;
end;

class function TDataComponent.Embedd(const AOwner: TComponent;
  const AParent:TWinControl; const ARoot: TDataItem): TDataComponent;
begin
  result:=TDataComponent.Create(AOwner);
  result.FRoot:=ARoot;
  TUICommon.AddForm(result,AParent);
end;

procedure TDataComponent.Add(const AParent:TTreeNode; const AComponent:TComponent; const AName:String);

  { Deprecated
  procedure AddItems(const AParent:TTreeNode; const AItems:TWorkflowItems);
  var t : Integer;
      tmp : TWorkflowItem;
  begin
    for t:=0 to AItems.Count-1 do
    begin
      tmp:=AItems[t];

      DoAddData(AParent,tmp.Data);
      AddItems(AParent,tmp.Items);
    end;
  end;
  }

  procedure AddDataset(const AParent:TTreeNode; const ADataset:TDataset);
  var tmp : TField;
  begin
    try
      ADataSet.FieldDefs.Update;

      if ADataset.Fields.Count>0 then
         for tmp in ADataset.Fields  do
             Add(AParent,tmp,tmp.DisplayName);
    except
      on E:Exception do
         Tree.Items.AddChild(AParent,E.Message);
    end;
  end;

  function AddTreeNode(const AParent:TTreeNode;
                       const AName:String;
                       const AComponent:TComponent):TTreeNode;
  begin
    result:=Tree.Items.AddChildObject(AParent,AName,AComponent);

    {
    if (Current<>nil) and (AComponent=Current) then
       result.Selected:=True;
    }
  end;

  function FindOrAddRoot(const AOwner:TComponent):TTreeNode;
  var t : Integer;
  begin
    if AOwner=nil then
       result:=nil
    else
    begin
      for t:=0 to Tree.Items.Count-1 do
          if Tree.Items[t].Data=AOwner then
             Exit(Tree.Items[t]);

      result:=AddTreeNode(nil,AOwner.Name,AOwner);
    end;
  end;

  // For Design-time GetComponentNames:
  function CalcParent:TTreeNode;
  begin
    result:=AParent;

    if result=nil then
       if not (AComponent is TCustomForm) then
          if not (AComponent is TDataModule) then
             result:=FindOrAddRoot(AComponent.Owner);
  end;

  procedure AddTheComponent(const AParent:TTreeNode; const AName:String);
  var tmp : TTreeNode;
      t : Integer;
      tmpData : TDataItem;
  begin
    tmp:=AddTreeNode(AParent,AName,AComponent);

    tmpData:=TControlImporter.DataOf(AComponent);

    if tmpData<>nil then
       DoAddData(tmp,tmpData)
    else
    {
      Deprecated 2025:
    if AComponent is TBIWorkflow then
       AddItems(tmp,TBIWorkflow(AComponent).Items)
    else
    }
    if AComponent is TDataset then
       AddDataset(tmp,TDataset(AComponent))
    else
    {
    if AComponent.ComponentCount=0 then
    begin
      Tree.Items.AddChildObject(tmp,'(dummy)',Dummy);
    end
    else
    }
    for t:=0 to AComponent.ComponentCount-1 do
        Add(tmp,AComponent.Components[t],'');
  end;

begin
  if CanAdd(AComponent) then
  begin
    if IComponents<>nil then
       IComponents.Add(AComponent);

    AddTheComponent(CalcParent,CalcName(AComponent,AName));
  end;
end;

procedure TDataComponent.FillTree;

  procedure AddRuntime;
  var t : Integer;
  begin
    for t:=0 to Screen.FormCount-1 do
        Add(nil,Screen.Forms[t],'');

    for t:=0 to Screen.DataModuleCount-1 do
        Add(nil,Screen.DataModules[t],'');
  end;

  procedure AddNodes;
  begin
    IComponents:={$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TComponent>.Create;
    try

// DEBUG:
//      if IEdited<>nil then
//         ShowMessage(IEdited.ClassName);
//      if Assigned(TDataComponent.OnGetDesignerNames) then
//         ShowMessage('assigned ongetdesignernames');

      if (IEdited<>nil) and Assigned(TDataComponent.OnGetDesignerNames) then
         TDataComponent.OnGetDesignerNames(Add,IEdited)
      else
         AddRuntime;
    finally
      IComponents.Free;
      IComponents:=nil;
    end;
  end;

begin
  Tree.Items.BeginUpdate;
  try
    Tree.Items.Clear;

    AddNodes;
  finally
    Tree.Items.EndUpdate;
  end;
end;

type
  TDataItemAccess=class(TDataItem);

procedure TDataComponent.TryFreeData;

  function ItemsToDelete:TDataArray;
  var tmpItems : TDataArray;

    function ExistsParent(const AData:TDataItem):Boolean;
    var t : Integer;
    begin
      for t:=0 to High(tmpItems) do
          if AData.IsChildOf(tmpItems[t]) then
             Exit(True);

      result:=False;
    end;

  var tmp : TTreeNode;
      tmpData : TDataItem;
  begin
    tmpItems:=nil;

    // Prepare items to delete
    tmp:=Tree.Items.GetFirstNode;

    while tmp<>nil do
    begin
      if TObject(tmp.Data) is TDataItem then
      begin
        tmpData:=TDataItem(tmp.Data);

        if TDataItemAccess(tmpData).GetProvider=nil then
           if not ExistsParent(tmpData) then
              tmpItems.Add(tmpData);
      end;

      tmp:=tmp.GetNext;
    end;

    result:=tmpItems;
  end;

begin
  // Now delete all root items
  ItemsToDelete.FreeAll;
end;

procedure TDataComponent.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TryFreeData;
  TUICommon.SavePosition(Self,'DataComponent');
end;

procedure TDataComponent.FormShow(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,'DataComponent');

  if FRoot=nil then
     FillTree
  else
     DoAddData(nil,FRoot);

  if FCurrent=nil then
  begin
    if Tree.Items.Count>0 then
       Tree.Items[0].Expand(False);
  end
  else
    Select(FCurrent);
end;

// To remove:
class function TDataComponent.Import(const AOwner:TComponent; const AObject: TObject): TDataItem;
begin
  result:=TControlImporter.From(AOwner,AObject as TComponent);
end;

procedure TDataComponent.PopupMenu1Popup(Sender: TObject);
begin
  ViewData1.Enabled:=(Selected<>nil) or (SelectedData<>nil);
end;

// Modal show this dialog, return selection (or nil if cancelled)
class function TDataComponent.Choose(const AOwner,AEdited,ACurrent: TComponent): TComponent;
begin
  with TDataComponent.Create(AOwner) do
  try
    IEdited:=AEdited;

    if ACurrent=nil then
    begin
      if IEdited is TComponentImporter then
         FCurrent:=TComponentImporter(IEdited).Source;
    end
    else
      FCurrent:=ACurrent;

    if ShowModal=mrOk then
       result:=Selected
    else
       result:=nil;
  finally
    Free;
  end;
end;

// Find the tree node that has AObject as node data
function TDataComponent.NodeOf(const AObject:TObject):TTreeNode;
var tmp : TTreeNode;
begin
  if AObject<>nil then
  begin
    tmp:=Tree.Items.GetFirstNode;

    while tmp<>nil do
          if TObject(tmp.Data)=AObject then
             Exit(tmp)
          else
             tmp:=tmp.GetNext;
  end;

  result:=nil;
end;

procedure TDataComponent.Select(const AObject:TObject);
var tmp : TTreeNode;
begin
  FCurrent:=AObject;

  if AObject=nil then
     Tree.ClearSelection
  else
  begin
    tmp:=NodeOf(AObject);

    if tmp<>nil then
       tmp.Selected:=True;
  end;
end;

function TDataComponent.Selected: TComponent;
begin
  if Tree.Selected=nil then
     result:=nil
  else
  if TObject(Tree.Selected.Data) is TComponent then
     result:=TComponent(Tree.Selected.Data)
  else
     result:=nil
end;

function TDataComponent.SelectedData: TDataItem;
begin
  if Tree.Selected=nil then
     result:=nil
  else
  if TObject(Tree.Selected.Data) is TDataItem then
     result:=TDataItem(Tree.Selected.Data)
  else
     result:=nil
end;

// Returns True when the current selected node has data or it can be imported
function TDataComponent.SelectedHasData: Boolean;
var tmp : TComponent;
begin
  tmp:=Selected;

  if tmp=nil then
     result:=SelectedData<>nil
  else
     result:=TComponentImporter.IsSupported(tmp);
end;

function TDataComponent.SelectedItems: TDataArray;
var t : Integer;
begin
  result:=nil;

  for t:=0 to Tree.SelectionCount-1 do
      if TObject(Tree.Selections[t].Data) is TDataItem then
         result.Add(TDataItem(Tree.Selections[t].Data));
end;

procedure TDataComponent.TreeChange(Sender: TObject; Node: TTreeNode);
var tmp : TObject;
begin
  tmp:=Selected;

  if tmp=nil then
     tmp:=SelectedData;

  if Assigned(FOnSelected) and (tmp<>nil) then
     FOnSelected(tmp);

  if PanelButtons.Visible then
     BOk.Enabled:=tmp<>nil;
end;

procedure TDataComponent.TreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  {
  if (Node.Count=1) and (Node.Item[0].Data=Dummy) then
  begin
    Node.Item[0].Free;
  end;
  }
end;

function TDataComponent.Data(const AOwner:TComponent):TDataItem;
begin
  if Selected=nil then
     result:=SelectedData
  else
     result:=TControlImporter.From(AOwner,Selected);
end;

// Show the DataViewer dialog for the current selected data item
procedure TDataComponent.ViewData1Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=Data(Self);

  if tmp<>nil then
     TDataViewer.View(Self,tmp);
end;

class function TDataComponent.ChooseMany(const AOwner: TComponent; const ARoot,
                                         ACurrent: TDataItem;
                                         const Compatible:Boolean): TDataArray;
begin
  with TDataComponent.Create(AOwner) do
  try
    FCurrent:=ACurrent;
    FRoot:=ARoot;

    Tree.MultiSelect:=True;

    PanelButtons.Visible:=True;

    if ShowModal=mrOk then
       result:=SelectedItems
    else
       result:=nil;
  finally
    Free;
  end;
end;

end.
