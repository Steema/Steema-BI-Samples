{*********************************************}
{  TeeBI Software Library                     }
{  DataViewer VCL                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.DataViewer;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB,
  Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids, BI.DataItem, Vcl.StdCtrls, BI.Arrays,
  VCLBI.Grid, Vcl.Menus, BI.DataSet, Vcl.ComCtrls, Vcl.DBCtrls,
  VCLBI.DataControl, Vcl.Buttons;

type
  TDataViewer = class(TForm)
    PanelTop: TPanel;
    Label1: TLabel;
    LName: TLabel;
    Splitter1: TSplitter;
    DataSource2: TDataSource;
    PanelItems: TPanel;
    SplitterData: TSplitter;
    CBViewData: TCheckBox;
    PanelDatas: TPanel;
    DataTotals: TStringGrid;
    PopupMenu1: TPopupMenu;
    View1: TMenuItem;
    ItemsGrid: TBIGrid;
    Button2: TButton;
    PanelData: TPanel;
    PanelDataGrid: TPanel;
    DataGrid: TBIGrid;
    PanelNav: TPanel;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    LRow: TLabel;
    Items: TBIDataset;
    CBView: TComboBox;
    N1: TMenuItem;
    PanelItemsGrid: TPanel;
    DBNavigator2: TDBNavigator;
    Panel3: TPanel;
    Panel4: TPanel;
    CBRecord: TCheckBox;
    Panel5: TPanel;
    RecordView: TBIGrid;
    SplitterRecord: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBViewDataClick(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure CBViewChange(Sender: TObject);
    procedure ItemsAfterInsert(DataSet: TDataSet);
    procedure DataGridDataChange(Sender: TObject);
    procedure CBRecordClick(Sender: TObject);
  private
    { Private declarations }
    DataStats,
    DataMap : TDataItem;

    procedure CheckPanelDataAlign;
    procedure GetKind(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure SetKind(Sender: TField; const Text: string);

    procedure NewItems(const AData:TDataItem);

  protected
    FData : TDataItem;

    Tree : TTreeView;

    procedure FillData(const AData:TDataItem);
    procedure RefreshLabelName;
    function Selected:TDataItem;
    procedure SelectedChange(Sender: TObject);
    procedure TryAddInfoEditors(const AGrid:TObject); virtual;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl; const AData:TDataItem):TDataViewer; static;
    procedure Select(const AData:TDataItem);

    class procedure View(const AOwner:TComponent; const AData:TDataItem); static;
  end;

implementation

{$R *.dfm}

uses
  VCLBI.GridForm, BI.UI,
  BI.DataSource, BI.Persist, VCLBI.DataManager,
  VCLBI.DataTree, BI.Languages.English,
  VCLBI.Grid.DBGrid, BI.Info, BI.SingleRecord;

{ TDataViewer }

procedure TDataViewer.Button2Click(Sender: TObject);
begin
  TDataViewer.View(Self,TStores.GlobalCache);
end;

procedure TDataViewer.CBRecordClick(Sender: TObject);
begin
  RecordView.Visible:=CBRecord.Checked;
  SplitterRecord.Visible:=RecordView.Visible;

  FillData(Selected);

  if RecordView.Visible then
     DataGridDataChange(DataGrid);
end;

procedure TDataViewer.CBViewChange(Sender: TObject);
begin
  CBViewDataClick(Self);
end;

// Change the items and data panels alignment, depending on their visibility
procedure TDataViewer.CheckPanelDataAlign;
var Old : Boolean;
begin
  SplitterData.Visible:=PanelItemsGrid.Visible and PanelData.Visible;

  if PanelData.Visible then
     if PanelItemsGrid.Visible then
     begin
       Old:=PanelData.Align=alClient;
       PanelData.Align:=alBottom;

       if Old then
          PanelData.Height:=Height div 2;
     end
     else
        PanelData.Align:=alClient;

  if SplitterData.Visible then
  begin
    SplitterData.Align:=TAlign.alTop;
    SplitterData.Align:=TAlign.alBottom;
  end;
end;

procedure TDataViewer.DataGridDataChange(Sender: TObject);
var tmp : TDataSet;
    tmpSource : TDataSource;
begin
  tmpSource:=(Sender as TBIGrid).DataSource;

  if tmpSource=nil then
     Exit;

  tmp:=tmpSource.DataSet;
  LRow.Caption:=IntToStr(tmp.RecNo)+'/'+IntToStr(tmp.RecordCount);

  if CBRecord.Checked then
  begin
    if RecordView.Provider=nil then
    begin
      RecordView.Provider:=TSingleRecord.Create(Self);
      TSingleRecord(RecordView.Provider).Source:=(Sender as TBIGrid).Data;
    end;

    TSingleRecord(RecordView.Provider).Row:=tmp.RecNo-1;
  end;
end;

procedure TDataViewer.CBViewDataClick(Sender: TObject);
begin
  PanelData.Visible:=CBViewData.Checked;

  CheckPanelDataAlign;

  FillData(Selected);
end;

procedure TDataViewer.GetKind(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  Text:=TDataKind(Sender.AsInteger).ToString;
end;

type
  TDataAccess=class(TDataItem);

procedure TDataViewer.ItemsAfterInsert(DataSet: TDataSet);
var tmp : TDataItem;
    tmpItems : TDataItems;
begin
  tmpItems:=Selected.Items;

  // Create a new temporary empty item
  tmp:=tmpItems.New('',TDataKind.dkInt32);
  tmp.Resize(tmp.Parent.Count);

  TDataAccess(tmp).CheckEmptyName;

  tmp.Missing.Init(tmp.Count);

  // Insert new item
  tmpItems.Insert(tmp,DataSet.RecNo-1);

  // Set field values to show at grid
  DataSet.FieldByName('Name').AsString:=tmp.Name;
  DataSet.FieldByName('Count').AsLargeInt:=tmp.Count;
  DataSet.FieldByName('Missing').AsLargeInt:=tmp.Missing.MissingCount;
end;

procedure TDataViewer.TryAddInfoEditors(const AGrid:TObject);
begin
end;

procedure TDataViewer.NewItems(const AData:TDataItem);

  procedure SetHeaderKind(const AGrid:TObject);
  var tmp : TBIDBGrid;
      tmpCol : TColumn;
  begin
    if AGrid is TBIDBGrid then
    begin
      tmp:=TBIDBGrid(AGrid);

      tmpCol:=tmp.ColumnOf(Items.Data['Kind']);

      if tmpCol<>nil then
         tmpCol.Title.Alignment:=TAlignment.taLeftJustify;
    end;
  end;

var tmp : TField;
begin
  Items.Close;

  Items.Data.Free;
  Items.Data:=nil;

  if AData.Items.Count>0 then
  begin
    Items.Data:=TDataInfo.Create(AData);

    if Items.Data<>nil then
    begin
      Items.Open;

      tmp:=Items.FieldByName('Kind');
      tmp.Alignment:=TAlignment.taLeftJustify;
      SetHeaderKind(ItemsGrid.Plugin.GetObject);

      tmp.OnGetText:=GetKind;
      tmp.OnSetText:=SetKind;

      if not ItemsGrid.ReadOnly then
         TryAddInfoEditors(ItemsGrid.Plugin.GetObject);
    end;
  end;

  PanelItemsGrid.Visible:=Items.Active;

  CheckPanelDataAlign;
end;

class function TDataViewer.Embedd(const AOwner: TComponent;
  const AParent: TWinControl; const AData: TDataItem): TDataViewer;
begin
  result:=TDataViewer.Create(AOwner);
  result.FData:=AData;
  TUICommon.AddForm(result,AParent);
end;

procedure TDataViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,'DataViewer');
end;

procedure TDataViewer.FormCreate(Sender: TObject);
begin
  PanelData.Visible:=False;
  SplitterData.Visible:=False;
end;

procedure TDataViewer.FormDestroy(Sender: TObject);
begin
  Items.Data.Free;
  DataMap.Free;
  DataStats.Free;
end;

// Slow?
function Roots(const ATree:TTreeView):Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to ATree.Items.Count-1 do
      if ATree.Items[t].Parent=nil then
         Inc(result);
end;

function CheckName(const S:String):String;
begin
  if S='' then
     result:=BIMsg_UnNamed
  else
     result:=S;
end;

procedure TDataViewer.RefreshLabelName;
begin
  if FData=nil then
     LName.Caption:=''
  else
     LName.Caption:=CheckName(FData.Name);
end;

procedure TDataViewer.Select(const AData: TDataItem);

  function StoresVisible:Boolean;
  begin
    result:=(FData=nil) or (TStore.StoreOf(FData)<>nil);
  end;

var tmp : TDataManager;
begin
  FData:=AData;

  if Tree=nil then
  begin
    tmp:=TDataManager.Embed(Self,PanelDatas,TDataManagerEmbedMode.Choose,'',FData);
    tmp.Align:=TAlign.alClient;

    Tree:=tmp.Tree;

    tmp.OnSelect:=SelectedChange;
  end;

  RefreshLabelName;

  if FData=nil then
  begin
    Tree.Items.Clear;
    SelectedChange(Self);
  end
  else
  begin
    TDataTree.Fill(FData,Tree,True);

    if Roots(Tree)=1 then
    begin
      Tree.Items[0].Expanded:=True;
      Tree.Items[0].Selected:=True;
    end;
  end;

  // When viewing in-memory Data (not from a TStore), hide Stores panel
  (Tree.Owner as TDataManager).PanelStores.Visible:=StoresVisible;
end;

function TDataViewer.Selected: TDataItem;
begin
  if Tree.Selected=nil then
     result:=nil
  else
     result:=TDataItem(Tree.Selected.Data);
end;

procedure TDataViewer.SelectedChange(Sender: TObject);
begin
  CBViewData.Enabled:=Selected<>nil;

  if Selected=nil then
  begin
    Items.Close;
    DataMap.Free;
    DataStats.Free;
  end
  else
    NewItems(Selected);

  FillData(Selected);
end;

procedure TDataViewer.SetKind(Sender: TField; const Text: string);
var tmp : TDataKind;
begin
  if TDataKind.FromString(Text,tmp) then
     Sender.AsInteger:=Ord(tmp)
  else
     raise EBIException.Create('Error: Wrong data kind: '+Text);
end;

procedure TDataViewer.FillData(const AData:TDataItem);
var tmp : TDataItem;
begin
  RecordView.Provider.Free;
  RecordView.Provider:=nil;

  if CBViewData.Checked then
  begin
    case CBView.ItemIndex of
      0: tmp:=AData;

      1: begin
           DataMap.Free;
           DataMap:=TDataMapAsData.FromData(AData);
           tmp:=DataMap;
         end;
    else
      begin
        DataStats.Free;
        DataStats:=TDataItemsInfo.ItemsOf(AData);
        tmp:=DataStats;
      end;
    end;

    DataGrid.BindTo(tmp);

    if DBNavigator1.DataSource=nil then
       DBNavigator1.DataSource:=DataGrid.DataSource;

    DataGridDataChange(DataGrid);
  end
  else
    DataGrid.BindTo(nil);
end;

procedure TDataViewer.FormShow(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,'DataViewer');

  Select(FData);
end;

class procedure TDataViewer.View(const AOwner:TComponent; const AData: TDataItem);
begin
  with TDataViewer.Create(AOwner) do
  try
    FData:=AData;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TDataViewer.View1Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=Selected;

  if tmp<>nil then
     TBIGridForm.Present(Self,tmp);
end;

end.

