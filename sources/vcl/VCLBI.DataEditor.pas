{*********************************************}
{  TeeBI Software Library                     }
{  DataEditor VCL                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.DataEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.DataViewer, Data.DB, BI.Dataset,
  Vcl.Menus, Vcl.Grids, VCLBI.DataControl, VCLBI.Grid, Vcl.DBCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  BI.DataItem;

type
  TDataItemEditor = class(TDataViewer)
    PanelButtons: TPanel;
    Panel7: TPanel;
    BOK: TButton;
    BCancel: TButton;
    PopupNode: TPopupMenu;
    Rename1: TMenuItem;
    N2: TMenuItem;
    AddField1: TMenuItem;
    AddTable1: TMenuItem;
    Kind1: TMenuItem;
    Integer32bit1: TMenuItem;
    Integer64bit1: TMenuItem;
    FloatSingle1: TMenuItem;
    FloatDouble1: TMenuItem;
    FloatExtended1: TMenuItem;
    Text1: TMenuItem;
    DateTime1: TMenuItem;
    Boolean1: TMenuItem;
    AddFolder1: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Rename1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure PopupNodePopup(Sender: TObject);
    procedure AddTable1Click(Sender: TObject);
    procedure AddFolder1Click(Sender: TObject);
    procedure DataSource2UpdateData(Sender: TObject);
    procedure ItemsBeforeDelete(DataSet: TDataSet);
    procedure AddField1Click(Sender: TObject);
    procedure Boolean1Click(Sender: TObject);
  private
    { Private declarations }

    FOriginal : TDataItem;

    IModified : Boolean;

    procedure AskSave;
    function ConvertKind(const AItem:TDataItem; const AKind:TDataKind):Boolean;
    procedure EditButtonClick(Sender: TObject);
    procedure Modified;
    procedure SaveData;
    procedure SelectedEdited(Sender: TObject; Node: TTreeNode; var S: string);
    function SelectedItem:TDataItem;
    procedure UpdatedData(Sender: TObject);
    procedure TryAddNewItem(const ATitle,APrefix:String;
                            const IsTable:Boolean;
                            const AKind:TDataKind);
  protected
    procedure TryAddInfoEditors(const AGrid:TObject); override;
  public
    { Public declarations }

    class function Edit(const AOwner: TComponent; const AData: TDataItem):Boolean; static;
  end;

implementation

{$R *.dfm}

uses
  BI.DataSource, BI.Arrays, VCLBI.Grid.DBGrid, VCL.DBGrids, VCLBI.DataManager,
  BI.Convert, BI.UI;

procedure TDataItemEditor.AddField1Click(Sender: TObject);
begin
  inherited;

  TryAddNewItem('New Field','Field',False,TDataKind.dkInt32);
end;

procedure TDataItemEditor.AddFolder1Click(Sender: TObject);
begin
  TryAddNewItem('New Folder','Folder',False,TDataKind.dkUnknown);
end;

procedure TDataItemEditor.TryAddNewItem(const ATitle,APrefix:String;
                         const IsTable:Boolean; const AKind:TDataKind);
var tmpNew : TDataItem;
    tmpDefault,
    tmpName : String;
    tmpNode : TTreeNode;
    tmp : TDataItem;
begin
  tmp:=Selected;

  if (tmp=nil) or (tmp.Parent=nil) then
     tmpDefault:=APrefix
  else
     tmpDefault:=tmp.Items.UniqueName(APrefix);

  if TUICommon.Input(ATitle,'Name',tmpDefault,tmpName) then
  begin
    if AKind=TDataKind.dkUnknown then
    begin
      tmpNew:=TDataItem.Create(IsTable);
      tmpNew.Name:=tmpName;
    end
    else
      tmpNew:=TDataItem.Create(AKind,tmpName);

    tmp.Items.Add(tmpNew);

    Modified;

    tmpNode:=Tree.Items.AddChildObject(Tree.Selected,tmpName,tmpNew);

    Tree.Selected:=tmpNode;
  end;
end;

procedure TDataItemEditor.AddTable1Click(Sender: TObject);
begin
  TryAddNewItem('New Table','Table',True,TDataKind.dkUnknown);
end;

procedure TDataItemEditor.AskSave;
begin
  if TUICommon.YesNo('Data has been modified. Do you want to save it?') then
     SaveData;
end;

procedure TDataItemEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;

  if IModified then
     AskSave;

  CanClose:=True;
end;

procedure TDataItemEditor.FormDestroy(Sender: TObject);
begin
  FData.Free;
  inherited;
end;

function TDataItemEditor.SelectedItem:TDataItem;
var tmp : TDataItem;
begin
  tmp:=Selected;

  if tmp=nil then
     result:=nil
  else
     result:=tmp.Items[Items.RecNo-1];
end;

procedure TDataItemEditor.ItemsBeforeDelete(DataSet: TDataSet);
begin
  // Delete item
  SelectedItem.Free;

  // Delete Tree node
  Tree.Selected[DataSet.RecNo-1].Free;

  // Refresh data grid
  if CBViewData.Checked then
     FillData(Selected);
end;

procedure TDataItemEditor.Modified;
begin
  IModified:=True;
  BOK.Enabled:=True;
end;

procedure TDataItemEditor.PopupNodePopup(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=Selected;

  AddField1.Enabled:=tmp.AsTable;
  AddTable1.Enabled:=(tmp.Kind=TDataKind.dkUnknown) and (not tmp.AsTable);

  Kind1.Enabled:=tmp.Kind<>TDataKind.dkUnknown;

  case tmp.Kind of
      dkInt32: Integer32bit1.Checked:=True;
      dkInt64: Integer64bit1.Checked:=True;
     dkSingle: FloatSingle1.Checked:=True;
     dkDouble: FloatDouble1.Checked:=True;
   dkExtended: FloatExtended1.Checked:=True;
       dkText: Text1.Checked:=True;
   dkDateTime: DateTime1.Checked:=True;
    dkBoolean: Boolean1.Checked:=True;
  end;
end;

procedure TDataItemEditor.Rename1Click(Sender: TObject);
var tmp,
    tmpNew : String;
begin
  tmp:=Selected.Name;

  if TUICommon.Input('Rename item','Name',tmp,tmpNew) then
     if tmp<>tmpNew then
        SelectedEdited(Self,Tree.Selected,tmpNew);
end;

procedure TDataItemEditor.SelectedEdited(Sender: TObject; Node: TTreeNode;
  var S: string);

  function DuplicateName:Boolean;
  begin
    result:=(Selected.Parent<>nil) and
             Selected.Parent.Items.Exists(S);
  end;

begin
  if DuplicateName then
  begin
    Tree.Selected.Text:=Selected.Name;

    raise EBIException.Create('Error: Name '+S+' already exists');
    //TUICommon.Message('Duplicate name: '+S);
  end
  else
  begin
    Selected.Name:=S;
    Tree.Selected.Text:=S;

    Modified;
  end;
end;

procedure TDataItemEditor.SaveData;
begin
  if IModified then
  begin
    TDataClone.Clone(FData,FOriginal);
    IModified:=False;
  end;
end;

procedure TDataItemEditor.BOKClick(Sender: TObject);
begin
  SaveData;
end;

procedure TDataItemEditor.Boolean1Click(Sender: TObject);
var tmp : TMenuItem;
    tmpKind : TDataKind;
begin
  tmp:=(Sender as TMenuItem);

  tmpKind:=TDataKind(tmp.Tag);

  ConvertKind(Selected,tmpKind);

  tmp.Checked:=True;
end;

function TDataItemEditor.ConvertKind(const AItem:TDataItem; const AKind:TDataKind):Boolean;
begin
  result:=False;

  if AItem.Kind<>AKind then
  begin
    if TDataKindConvert.Convert(AItem,AKind) then
       result:=True
    else
       raise EBIException.Create('Error: Cannot convert data '+ AItem.FullName+' to: '+AKind.ToString);
  end;
end;

procedure TDataItemEditor.UpdatedData(Sender: TObject);
begin
  Modified;
end;

procedure TDataItemEditor.DataSource2UpdateData(Sender: TObject);
var tmpKind : TDataKind;
    tmp,
    tmpFind : TDataItem;
    tmpUpdate : Boolean;
    tmpS: String;
    tmpNode : TTreeNode;
begin
  tmp:=SelectedItem;

  // Check new name is not a duplicated of a existing item

  tmpS:=Items.FieldByName('Name').AsString;

  tmpFind:=tmp.Parent.Items.Find(tmpS);

  if (tmpFind<>nil) and (tmpFind<>tmp) then
     raise EBIException.Create('Error: Duplicate name: '+tmpS);

  // Try to convert item Kind
  tmpKind:=TDataKind(Items.FieldByName('Kind').AsInteger);

  tmpUpdate:=ConvertKind(tmp,tmpKind);

  // Try to rename the Tree node
  tmpNode:=Tree.Selected[Items.RecNo-1];

  if tmpNode.Text<>tmpS then
  begin
    tmpNode.Text:=tmpS;
    tmp.Name:=tmpS;
    tmpUpdate:=True;
  end;

  // Refresh data grid, if visible
  if tmpUpdate then
     if CBViewData.Checked then
        FillData(Selected);
end;

// Show user the data selection dialog
procedure TDataItemEditor.EditButtonClick(Sender: TObject);
var tmp : TBIDBGrid;
    tmpNew,
    tmpData : TDataItem;
begin
  tmp:=Sender as TBIDBGrid;

  tmpData:=Items.DataOf(tmp.SelectedField);

  if tmpData=Items.Data['Master'] then
  begin
    tmpNew:=TDataManager.Choose(Self,nil,True);

    if tmpNew<>nil then
    begin
      // Pending:
      // Try to evaluate the capability of tmpNew to be the Master of SelectedItem

      // Change master field
      Items.Edit;
      tmp.SelectedField.AsString:=tmpNew.Name;
      Items.Post;

      // Change item Master
      SelectedItem.Master:=tmpNew;

    end;
  end;
end;

procedure TDataItemEditor.TryAddInfoEditors(const AGrid:TObject);
var tmp : TBIDBGrid;
    tmpCol : TColumn;
begin
  if AGrid is TBIDBGrid then
  begin
    tmp:=TBIDBGrid(AGrid);

    tmpCol:=tmp.ColumnOf(Items.Data['Master']);

    if tmpCol<>nil then
    begin
      tmpCol.ButtonStyle:=TColumnButtonStyle.cbsEllipsis;
      tmp.OnEditButtonClick:=EditButtonClick;
    end;

    tmpCol:=tmp.ColumnOf(Items.Data['Kind']);

    if tmpCol<>nil then
       TCommonUI.AddKinds(tmpCol.PickList);
  end;
end;

class function TDataItemEditor.Edit(const AOwner: TComponent; const AData: TDataItem):Boolean;
begin
  with TDataItemEditor.Create(AOwner) do
  try
    FOriginal:=AData;

    Select(TDataClone.Clone(AData));

    PanelTop.Visible:=False;

    CBViewData.Checked:=True;

    // Prepare for edit mode
    ItemsGrid.ReadOnly:=False;
    DataGrid.ReadOnly:=False;

    DBNavigator2.VisibleButtons:=DBNavigator2.VisibleButtons+
                    [nbInsert, nbDelete, nbEdit, nbPost, nbCancel];

    Tree.ReadOnly:=False;
    Tree.OnEdited:=SelectedEdited;

    Tree.PopupMenu:=PopupNode;

    DataGrid.DataSource.OnUpdateData:=UpdatedData;

    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

end.
