{*********************************************}
{  TeeBI Software Library                     }
{  TDataItems editor dialog (Table Structure) }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Editor.Items;

interface

{
  Editor dialog to modify the structure of a TDataItem that is in "Table" mode.

  Also a BIGrid in read-write mode to enable adding, modifying and removing
  rows.
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Edit,
  BI.DataItem, FMXBI.GridForm;

type
  TItemsEditor = class(TForm)
    Layout1: TLayout;
    GroupBox1: TGroupBox;
    LayoutButtons: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    LBFields: TListBox;
    LayoutGrid: TLayout;
    SBAdd: TSpeedButton;
    SBRemove: TSpeedButton;
    Label1: TLabel;
    EName: TEdit;
    LDuplicateName: TText;
    Label2: TLabel;
    CBKind: TComboBox;
    Layout4: TLayout;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    Layout5: TLayout;
    BOK: TButton;
    procedure LBFieldsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBAddClick(Sender: TObject);
    procedure SBRemoveClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure ENameChangeTracking(Sender: TObject);
    procedure ENameChange(Sender: TObject);
    procedure CBKindChange(Sender: TObject);
  private
    { Private declarations }

    IGrid : TBIGridForm;
    Items : TDataItems;

    FOnChanged : TNotifyEvent;

    procedure Changed;

    procedure DataChanged(Sender: TObject);
    function DataOf(const AIndex:Integer): TDataItem;
    function SelectedField:TDataItem;
    function SelectedItems:TDataItems;
    procedure SwapFields(const A,B:Integer);

    class function Valid(const AItems:TDataItems):Boolean; static;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AItems:TDataItems):TItemsEditor; static;

    class function Edit(const AOwner:TComponent;
                        const AItems:TDataItems):TModalResult; static;

    procedure Refresh(const AItems:TDataItems);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.fmx}

uses
  BI.Arrays, FMXBI.Grid, BI.Convert, BI.Languages.English;

{ TItemsEditor }

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

procedure TItemsEditor.CBKindChange(Sender: TObject);
var tmp : TDataKind;
begin
  if SelectedField<>nil then
  begin
    tmp:=TDataKind(CBKind.ItemIndex);

    if TDataKindConvert.Convert(SelectedField,tmp) then
       Changed
    else
    begin
      ShowMessage('Data cannot be converted to '+tmp.ToString);
      CBKind.ItemIndex:=Ord(SelectedField.Kind)
    end;
  end;
end;

class function TItemsEditor.Edit(const AOwner: TComponent;
  const AItems: TDataItems): TModalResult;
begin
  if Valid(AItems) then
  begin
    with TItemsEditor.Create(AOwner) do
    try
      Items:=AItems;

      result:=ShowModal;
    finally
      Free;
    end;
  end
  else
    result:=mrCancel;
end;

class function TItemsEditor.Embedd(const AOwner: TComponent;
  const AParent: TControl; const AItems: TDataItems): TItemsEditor;
begin
  if Valid(AItems) then
  begin
    result:=TItemsEditor.Create(AOwner);
    result.Items:=AItems;

    result.LayoutButtons.Visible:=False;

    TUICommon.AddForm(result,AParent);

    result.FormShow(result);
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

    tmpField:=Items.Find(tmp);

    if (tmpField<>nil) and (tmpField<>tmpSelected) then
       LDuplicateName.Visible:=True
    else
    begin
      LDuplicateName.Visible:=False;

      if tmp<>tmpSelected.Name then
      begin
        tmpSelected.Name:=tmp;
        LBFields.Items[LBFields.ItemIndex]:=tmp;

        Changed;
      end;
    end;
  end;
end;

procedure TItemsEditor.ENameChangeTracking(Sender: TObject);
begin
  ENameChange(Self);
end;

procedure TItemsEditor.FormShow(Sender: TObject);
begin
  LDuplicateName.Visible:=False;

  Refresh(Items);

  IGrid:=TBIGridForm.Embedd(Self,LayoutGrid,Items.Parent);
  IGrid.MakeEditable;

  IGrid.OnDataChange:=DataChanged;
end;

procedure TItemsEditor.Changed;
begin
  if Assigned(FOnChanged) then
     FOnChanged(Self);
end;

procedure TItemsEditor.DataChanged(Sender: TObject);
begin
  Changed;
end;

procedure TItemsEditor.LBFieldsChange(Sender: TObject);
var tmp : Boolean;
    tmpIndex : Integer;
    tmpItem : TDataItem;
begin
  tmpIndex:=LBFields.ItemIndex;
  tmp:=tmpIndex<>-1;

  tmpItem:=SelectedField;

  CBKind.Enabled:=tmp and (tmpItem.Kind<>TDataKind.dkUnknown);
  SBRemove.Enabled:=tmp;

  EName.Enabled:=tmp;

  if tmp then
  begin
    EName.Text:=tmpItem.Name;

    // Pending: subtables
    SBDown.Enabled:=(tmpIndex<LBFields.Count-1);
    SBUp.Enabled:=(tmpIndex>0);
  end
  else
  begin
    EName.Text:='';

    SBDown.Enabled:=False;
    SBUp.Enabled:=False;
  end;

  if CBKind.Enabled then
     CBKind.ItemIndex:=Ord(tmpItem.Kind)
  else
     CBKind.ItemIndex:=-1;

  Changed;
end;

procedure TItemsEditor.Refresh(const AItems:TDataItems);

  procedure AddItems(const AItems:TDataArray);
  var t : Integer;
      tmp : TDataItem;
  begin
    for t:=0 to Items.Count-1 do
    begin
      tmp:=AItems[t];

      LBFields.Items.AddObject(tmp.Name,tmp);

      if tmp.Kind=TDataKind.dkUnknown then
         AddItems(tmp.Items.AsArray);
    end;
  end;

  procedure AddFields;
  begin
    LBFields.BeginUpdate;
    try
      LBFields.Clear;

      AddItems(Items.AsArray);

      LBFieldsChange(Self);
    finally
      LBFields.EndUpdate;
    end;
  end;

begin
  Items:=AItems;

  AddFields;

  if IGrid<>nil then
     IGrid.Grid.Data:=Items.Parent;
end;

function TItemsEditor.DataOf(const AIndex:Integer): TDataItem;
begin
  if AIndex=-1 then
     result:=nil
  else
     result:=TDataItem(LBFields.Items.Objects[AIndex]);
end;

function TItemsEditor.SelectedField: TDataItem;
begin
  result:=DataOf(LBFields.ItemIndex);
end;

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

procedure TItemsEditor.SBAddClick(Sender: TObject);
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

  LBFields.ItemIndex:=LBFields.Items.AddObject(tmp,tmpItem);

  Changed;
end;

procedure TItemsEditor.SBDownClick(Sender: TObject);
begin
  SwapFields(LBFields.ItemIndex,LBFields.ItemIndex+1);
end;

procedure TItemsEditor.SBRemoveClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBFields.ItemIndex;

  if TUICommon.YesNo('Sure to delete field: '+LBFields.Items[tmp]+'?') then
  begin
    Items[tmp].{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};

    LBFields.Items.Delete(tmp);

    if LBFields.Items.Count>tmp then
       LBFields.ItemIndex:=tmp
    else
       LBFields.ItemIndex:=tmp-1;

    Changed;
  end;
end;

procedure TItemsEditor.SBUpClick(Sender: TObject);
begin
  SwapFields(LBFields.ItemIndex,LBFields.ItemIndex-1);
end;

procedure TItemsEditor.SwapFields(const A, B: Integer);
begin
  Items.Exchange(A,B);

  LBFields.ItemsExchange(LBFields.ItemByIndex(A),LBFields.ItemByIndex(B));
  LBFields.ItemIndex:=B;

  LBFieldsChange(Self);
end;

end.
