{*********************************************}
{  TeeBI Software Library                     }
{  DataViewer for Firemonkey                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.DataViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  FMX.Controls, FMX.Forms,

  {$IF CompilerVersion>25}
  FMX.Graphics,
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMXBI.Grid, FMX.Menus, BI.DataItem,
  BI.DataSet, FMX.TreeView, System.Rtti, FMX.Grid, FMXBI.Grid.Grid, FMX.ListBox,
  FMXBI.DataControl;

type
  TDataViewer = class(TForm)
    LayoutTree: TLayout;
    Layout2: TLayout;
    Splitter1: TSplitter;
    Layout3: TLayout;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    LName: TLabel;
    CBViewData: TCheckBox;
    PopupMenu2: TPopupMenu;
    MenuItem2: TMenuItem;
    Tree: TTreeView;
    SplitData: TSplitter;
    CBView: TComboBox;
    ItemsGrid: TBIGrid;
    DataGrid: TBIGrid;
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure TreeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBViewDataChange(Sender: TObject);
    procedure CBViewChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FData,
    DataMap,
    Items : TDataItem;

    function Selected:TDataItem;
    procedure ShowItems(const AData:TDataItem);
  public
    { Public declarations }

    class function Embedd(const AOwner: TComponent; const AParent: TControl; const AData: TDataItem): TDataViewer; static;

    procedure Select(const AData:TDataItem);
    class function View(const AOwner:TComponent; const AData:TDataItem):TModalResult; static;
  end;

  TDataTree=class
  public
    class procedure Fill(const AData:TDataItem; const ATree:TTreeView; const AddLeaves:Boolean=True); static;
  end;

implementation

{$R *.fmx}

uses
  BI.UI, FMXBI.GridForm, BI.DataSource, FMXBI.Editor.Column,
  BI.Arrays, BI.Languages.English, BI.Info;

procedure TDataViewer.TreeChange(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=Selected;

  if tmp=nil then
  begin
    ItemsGrid.BindTo(nil);
    DataGrid.BindTo(nil);
  end
  else
  begin
    ShowItems(tmp);

    if CBViewData.IsChecked then
       CBViewDataChange(Self);
  end;
end;

class function TDataViewer.Embedd(const AOwner: TComponent;
  const AParent: TControl; const AData: TDataItem): TDataViewer;
begin
  result:=TDataViewer.Create(AOwner);
  result.Select(AData);
  TUICommon.AddForm(result,AParent);
end;

procedure TDataViewer.ShowItems(const AData:TDataItem);
begin
  Items.Free;
  Items:=TDataItemsInfo.ItemsOf(AData);
  ItemsGrid.BindTo(Items);
end;

procedure TDataViewer.CBViewChange(Sender: TObject);
begin
  CBViewDataChange(Self);
end;

procedure TDataViewer.CBViewDataChange(Sender: TObject);
var Old : Boolean;
begin
  DataMap.Free;
  DataMap:=nil;

  if CBViewData.IsChecked and (Selected<>nil) then
  begin
    if CBView.ItemIndex=0 then
       DataGrid.BindTo(Selected)
    else
    begin
      DataMap:=TDataMapAsData.FromData(Selected);
      DataGrid.BindTo(DataMap);
    end;

    DataGrid.Visible:=True;
    SplitData.Visible:=True;

    Old:=ItemsGrid.Align=TUICommon.AlignClient;
    ItemsGrid.Align:=TUICommon.AlignTop;

    if Old then
       ItemsGrid.Height:=Height div 2;
  end
  else
  begin
    DataGrid.BindTo(nil);

    DataGrid.Visible:=False;
    SplitData.Visible:=False;

    ItemsGrid.Align:=TUICommon.AlignClient;
  end;
end;

procedure TDataViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,'DataViewer');
end;

procedure TDataViewer.FormCreate(Sender: TObject);
begin
  LName.Text:='';
end;

procedure TDataViewer.FormDestroy(Sender: TObject);
begin
  Items.Free;
  DataMap.Free;
end;

procedure TDataViewer.FormShow(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,'DataViewer');
  Select(FData);
end;

procedure TDataViewer.Select(const AData: TDataItem);
begin
  FData:=AData;

  if FData=nil then
  begin
    LName.Text:='';
    Tree.Clear;
    TreeChange(Self);
  end
  else
  begin
    LName.Text:=FData.Name;
    TDataTree.Fill(FData,Tree);
  end;
end;

function TDataViewer.Selected:TDataItem;
begin
  if Tree.Selected=nil then
     result:=nil
  else
     result:=Tree.Selected.TagObject as TDataItem;
end;

procedure TDataViewer.MenuItem1Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=Selected;

  if tmp<>nil then
     TBIGridForm.Present(Self,tmp);
end;

procedure TDataViewer.MenuItem2Click(Sender: TObject);
var Col : TDataItem;
begin
  Col:=Selected;

  if Col<>nil then
  begin
    TColumnEditor.Edit(Self,Col);

    // Pending: Refresh dataset and grid with new column settings after editing
  end;
end;

class function TDataViewer.View(const AOwner: TComponent; const AData: TDataItem): TModalResult;
begin
  with TDataViewer.Create(AOwner) do
  try
    FData:=AData;
    result:=ShowModal;
  finally
    Free;
  end;
end;

{ TDataTree }

class procedure TDataTree.Fill(const AData: TDataItem; const ATree: TTreeView;
  const AddLeaves: Boolean);

  function NewItem(const AParent:TTreeViewItem; const AData:TDataItem):TTreeViewItem;
  begin
    result:=TTreeViewItem.Create(ATree.Owner);

    if AData.Name='' then
       result.Text:=BIMsg_UnNamed
    else
       result.Text:=AData.Name;

    result.TagObject:=AData;

    if AParent=nil then
       result.Parent:=ATree
    else
       result.Parent:=AParent;
  end;

  procedure DoAddItems(const AParent:TTreeViewItem; const AData:TDataItem);
  var tmpItem : TDataItem;
      tmp : TTreeViewItem;
  begin
    tmp:=NewItem(AParent,AData);

    for tmpItem in AData.Items.AsArray do
        DoAddItems(tmp,tmpItem);
  end;

  function HasChildren(const AData:TDataItem):Boolean;
  var tmp : TDataItem;
  begin
    for tmp in AData.Parent.Items.AsArray do
        if tmp.ParentGroup=AData then
           Exit(True);

    result:=False;
  end;

  function MasterHas(const AMaster:TDataItem; const AMasterIndex:TInteger;
                     const ADetail:TDataItem; const ADetailIndex:TInteger):Boolean;
  begin
    result:=True;
  end;

var tmp : TDataItem;
begin
  ATree.BeginUpdate;
  try
    ATree.Clear;

    for tmp in AData.Items.AsArray do
        if tmp.ParentGroup=nil then
           if not HasChildren(tmp) then
              DoAddItems(nil,tmp);
  finally
    ATree.EndUpdate;
  end;
end;

end.
