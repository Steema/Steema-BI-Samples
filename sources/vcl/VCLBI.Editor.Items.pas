{*********************************************}
{  TeeBI Software Library                     }
{  TDataItems editor dialog (Table Structure) }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Items;

interface

{
  Editor dialog to modify the structure of a TDataItem that is in "Table" mode.

  Also a BIGrid in read-write mode to enable adding, modifying and removing
  rows.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  BI.DataItem, Vcl.ExtCtrls, VCLBI.DataControl, VCLBI.Grid,
  VCLBI.GridForm;

type
  TItemsEditor = class(TForm)
    PanelButtons: TPanel;
    Panel1: TPanel;
    BOK: TButton;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    LBFields: TListBox;
    PanelGrid: TPanel;
    Panel3: TPanel;
    Label24: TLabel;
    Label25: TLabel;
    LDuplicateName: TLabel;
    CBKind: TComboBox;
    EName: TEdit;
    Panel4: TPanel;
    SBAddField: TSpeedButton;
    SBRemoveField: TSpeedButton;
    Panel5: TPanel;
    SBUpField: TSpeedButton;
    SBDownField: TSpeedButton;
    Splitter1: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure CBKindChange(Sender: TObject);
    procedure ENameChange(Sender: TObject);
    procedure LBFieldsClick(Sender: TObject);
    procedure LBFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SBRemoveFieldClick(Sender: TObject);
    procedure SBUpFieldClick(Sender: TObject);
    procedure SBAddFieldClick(Sender: TObject);
    procedure SBDownFieldClick(Sender: TObject);
  private
    { Private declarations }

    IGrid : TBIGridForm;
    Items : TDataItems;

    function DataOf(const AIndex:Integer):TDataItem;
    function SelectedField:TDataItem;
    function SelectedItems:TDataItems;
    procedure SwapFields(const A,B:Integer);
    class function Valid(const AItems:TDataItems):Boolean; static;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AItems:TDataItems):TItemsEditor; static;

    class function Edit(const AOwner:TComponent;
                        const AItems:TDataItems):Boolean; static;

    procedure Refresh(const AItems:TDataItems);
  end;

implementation

{$R *.dfm}

uses
  BI.Arrays, BI.Convert, BI.Languages.English;

// Returns the Items property of the most "near" Parent of selected item
function TItemsEditor.SelectedItems:TDataItems;
var tmp : TDataItem;
begin
  tmp:=SelectedField;

  while tmp<>nil do
  begin
    tmp:=tmp.Parent;

    if tmp.Items=Items then
       break;
  end;

  if tmp=nil then
     result:=Items
  else
     result:=tmp.Items;
end;

procedure TItemsEditor.SBAddFieldClick(Sender: TObject);
var tmp : String;
    tmpItem : TDataItem;
    tmpItems : TDataItems;
begin
  tmp:=Items.UniqueName(BIMsg_Field);

  tmpItems:=SelectedItems;
  tmpItem:=tmpItems.Add(tmp,TDataKind.dkInt32);

  // Resize new item
  tmpItem.Resize(tmpItems.Parent.Count);

  // Make all new item rows "missing" (null)
  tmpItem.Missing.Init(tmpItem.Count);

  // Add item to listbox
  LBFields.ItemIndex:=LBFields.Items.AddObject(tmp,tmpItem);

  LBFieldsClick(Self);
end;

procedure TItemsEditor.SBDownFieldClick(Sender: TObject);
begin
  SwapFields(LBFields.ItemIndex,LBFields.ItemIndex+1);
end;

procedure TItemsEditor.SBRemoveFieldClick(Sender: TObject);
var tmp : Integer;
    tmpItem : TDataItem;
    tmpMessage : String;
begin
  tmp:=LBFields.ItemIndex;

  tmpItem:=SelectedField;

  if tmpItem.Kind=TDataKind.dkUnknown then
     if tmpItem.AsTable then
        tmpMessage:='Sure to delete table: '
     else
        tmpMessage:='Sure to delete group: '
  else
     tmpMessage:='Sure to delete field: ';

  if TUICommon.YesNo(tmpMessage+LBFields.Items[tmp]+'?') then
  begin
    if tmpItem.Kind=TDataKind.dkUnknown then
    begin
      // Remove all children items
    end;

    // Destroy item (and children, if any)
    tmpItem.Free;

    // Next: Remove from listbox
    LBFields.Items.Delete(tmp);

    // Reset current selected listbox item
    if LBFields.Items.Count>tmp then
       LBFields.ItemIndex:=tmp
    else
       LBFields.ItemIndex:=tmp-1;

    LBFieldsClick(Self);
  end;
end;

procedure TItemsEditor.SBUpFieldClick(Sender: TObject);
begin
  SwapFields(LBFields.ItemIndex,LBFields.ItemIndex-1);
end;

procedure TItemsEditor.SwapFields(const A,B:Integer);
begin
  // Pending: Subtable items
  Items.Exchange(A,B);
  LBFields.Items.Exchange(A,B);

  LBFields.ItemIndex:=B;
  LBFieldsClick(Self);
end;

function TItemsEditor.DataOf(const AIndex:Integer):TDataItem;
begin
  if AIndex=-1 then
     result:=nil
  else
     result:=TDataItem(LBFields.Items.Objects[AIndex]);
end;

function TItemsEditor.SelectedField:TDataItem;
begin
  result:=DataOf(LBFields.ItemIndex);
end;

procedure TItemsEditor.CBKindChange(Sender: TObject);
var tmp : TDataKind;
begin
  if SelectedField<>nil then
  begin
    tmp:=TDataKind(CBKind.ItemIndex);

    if not TDataKindConvert.Convert(SelectedField,tmp) then
    begin
      ShowMessage('Data cannot be converted to '+tmp.ToString);
      CBKind.ItemIndex:=Ord(SelectedField.Kind)
    end;
  end;
end;

class function TItemsEditor.Edit(const AOwner: TComponent;
  const AItems: TDataItems): Boolean;
begin
  if Valid(AItems) then
  begin
    with TItemsEditor.Create(AOwner) do
    try
      Items:=AItems;

      result:=ShowModal=mrOk;
    finally
      Free;
    end;
  end
  else
    result:=False;
end;

class function TItemsEditor.Valid(const AItems:TDataItems):Boolean;
begin
  if AItems=nil then
     raise EBIException.Create('Error: Items is nil')
  else
  if (AItems.Parent=nil) or  AItems.Parent.AsTable then
     result:=True
  else
     raise EBIException.Create('Error: Data: '+AItems.Parent.Name+' is not a table');
end;

class function TItemsEditor.Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AItems:TDataItems):TItemsEditor;
begin
  if Valid(AItems) then
  begin
    result:=TItemsEditor.Create(AOwner);
    result.Items:=AItems;

    result.PanelButtons.Visible:=False;

    TUICommon.AddForm(result,AParent);
  end
  else
    result:=nil;
end;

procedure TItemsEditor.ENameChange(Sender: TObject);
var tmp : String;
    tmpSelected,
    tmpField : TDataItem;
begin
  tmpSelected:=SelectedField;

  if tmpSelected<>nil then
  begin
    tmp:=EName.Text;

    tmpField:=SelectedItems.Find(tmp);

    if (tmpField<>nil) and (tmpField<>tmpSelected) then
       LDuplicateName.Visible:=True
    else
    begin
      LDuplicateName.Visible:=False;

      if tmp<>tmpSelected.Name then
      begin
        // Change item name
        tmpSelected.Name:=tmp;

        // Change listbox text
        LBFields.Items[LBFields.ItemIndex]:=tmp;
      end;
    end;
  end;
end;

procedure TItemsEditor.Refresh(const AItems:TDataItems);

  procedure AddItems(const AItems:TDataArray);
  var t : Integer;
      tmp : TDataItem;
  begin
    for t:=0 to AItems.Count-1 do
    begin
      tmp:=AItems[t];

      LBFields.Items.AddObject(tmp.Name,tmp);

      if tmp.Kind=TDataKind.dkUnknown then
         AddItems(tmp.Items.AsArray);
    end;
  end;

  procedure AddFields;
  begin
    LBFields.Items.BeginUpdate;
    try
      LBFields.Clear;

      AddItems(Items.AsArray);

      LBFieldsClick(Self);
    finally
      LBFields.Items.EndUpdate;
    end;
  end;

begin
  Items:=AItems;

  AddFields;

  if IGrid<>nil then
     IGrid.Grid.Data:=Items.Parent;
end;

procedure TItemsEditor.FormShow(Sender: TObject);
begin
  Refresh(Items);

  IGrid:=TBIGridForm.Embedd(Self,PanelGrid,Items.Parent);
  IGrid.MakeEditable;
end;

procedure TItemsEditor.LBFieldsClick(Sender: TObject);
var tmp : Boolean;
    tmpIndex : Integer;
    tmpItem : TDataItem;
begin
  tmpIndex:=LBFields.ItemIndex;
  tmp:=tmpIndex<>-1;

  tmpItem:=SelectedField;

  CBKind.Enabled:=tmp and (tmpItem.Kind<>TDataKind.dkUnknown);
  SBRemoveField.Enabled:=tmp;

  EName.Enabled:=tmp;

  if tmp then
  begin
    EName.Text:=tmpItem.Name;

    // Pending: subtables
    SBDownField.Enabled:=(tmpIndex<LBFields.Count-1);
    SBUpField.Enabled:=(tmpIndex>0);
  end
  else
  begin
    EName.Text:='';

    SBDownField.Enabled:=False;
    SBUpField.Enabled:=False;
  end;

  if CBKind.Enabled then
     CBKind.ItemIndex:=Ord(tmpItem.Kind)
  else
     CBKind.ItemIndex:=-1;
end;

procedure TItemsEditor.LBFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
var tmp : Integer;
begin
  tmp:=LBFields.ItemAtPos(TUICommon.Point(X,Y),True);

  SwapFields(LBFields.ItemIndex,tmp);
end;

procedure TItemsEditor.LBFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);

  function ParentOf(const AIndex:Integer):TDataItem;
  begin
    result:=DataOf(AIndex).Parent;
  end;

  function CanBeDragged(const AIndex:Integer):Boolean;
  var tmp : TDataItem;
  begin
    result:=AIndex<>LBFields.ItemIndex;

    if result then
    begin
      tmp:=SelectedField;

      if tmp.Kind<>TDataKind.dkUnknown then
         result:=ParentOf(AIndex)=ParentOf(LBFields.ItemIndex);
    end;
  end;

var tmp : Integer;
begin
  Accept:=(LBFields.Count>1) and (Sender=Source);

  if Accept then
  begin
    tmp:=LBFields.ItemAtPos(TUICommon.Point(X,Y),True);

    Accept:=CanBeDragged(tmp);
  end;
end;

end.
