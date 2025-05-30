{*********************************************}
{  TeeBI Software Library                     }
{  Data Selector dialog                       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.DataSelect;

interface

{
 This dialog enables selecting a "Data" object that can be used for example
 to change a BIGrid.Data property.

 "Data" can be selected from:

 - Any persisted data in a "Store" (disk cache)

 - Any TComponent that is supported, for example a BIQuery, or content from a
   TMemo that will be imported automatically.

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.DataItem, VCLBI.DataManager, VCLBI.Editor.DataComponent, Vcl.Menus,
  BI.Persist, VCLBI.DataControl;

type
  TDataSelector = class(TForm)
    PageControl1: TPageControl;
    TabStore: TTabSheet;
    TabComponent: TTabSheet;
    PanelButtons: TPanel;
    PanelAlignButtons: TPanel;
    BOK: TButton;
    BCancel: TButton;
    BClear: TButton;
    ImportMenu: TPopupMenu;
    Files1: TMenuItem;
    Database1: TMenuItem;
    Web1: TMenuItem;
    Import1: TMenuItem;
    Query1: TMenuItem;
    Panel1: TPanel;
    BNew: TButton;
    CustomData1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Query1Click(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure Database1Click(Sender: TObject);
    procedure Files1Click(Sender: TObject);
    procedure Web1Click(Sender: TObject);
    procedure CustomData1Click(Sender: TObject);
  private
    { Private declarations }

    IManager : TDataManager;
    IComp : TDataComponent;
    FOnSelect: TNotifyEvent;

    IFilterOwner,
    IEdited : TComponent;

    procedure FilterSelf(Sender: TComponent; var Valid:Boolean);
    procedure FinishAddNew(const AComponent:TComponent);
    function IsEmbedded:Boolean;
    function NewOwner:TComponent;
    function SelectedHasData:Boolean;
    procedure SelectedItem(Sender: TObject);
    procedure TryAddImport(const AKind:TDataDefinitionKind);
  protected
    procedure SetEdited(const AEdited:TComponent);
  public
    { Public declarations }

    class procedure Choose(const AOwner:TComponent;
                           const AEdited:TBIDataControl); overload; static;

    class function Choose(const AOwner:TComponent;
                          const AEdited:TComponent):TDataItem; overload; static;

    class function Choose(const AOwner:TComponent;
                          const AEdited:TComponent;
                          var AData:TDataItem;
                          const AFilterOwner:TComponent=nil):Boolean; overload; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AEdited: TComponent):TDataSelector; static;

    procedure RefreshTree(const AFilter:TDataManagerFilter);
    procedure Select(const AData:TDataItem);
    function Selected:TDataItem;

    property ComponentSelector:TDataComponent read IComp;
    property Manager:TDataManager read IManager;

    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation

{$R *.dfm}

uses
  VCLBI.Grid, VCLBI.Menus, BI.Store.Component, VCLBI.Editor.Data, BI.Query,
  VCLBI.Editor.Query, BI.UI;

function TDataSelector.Selected:TDataItem;
begin
  if PageControl1.ActivePage=TabStore then
     result:=IManager.SelectedData
  else
     result:=IComp.Data(Owner);
end;

procedure TDataSelector.BClearClick(Sender: TObject);
begin
  if PageControl1.ActivePage=TabStore then
     IManager.SelectData(nil)
  else
     IComp.Select(nil);

  BClear.Enabled:=False;
  BOK.Enabled:=True;
end;

type
  TDataAccess=class(TDataItem);
  TDataComponentAccess=class(TDataComponent);

procedure TDataSelector.Select(const AData:TDataItem);
var tmp : TDataProvider;
begin
  tmp:=TDataAccess(AData).GetProvider;

  if tmp is TDataDelayProvider then
     IManager.SelectData(AData)
  else
  begin
    IComp.Select(tmp);
    PageControl1.ActivePage:=TabComponent;
  end;
end;

procedure TDataSelector.BNewClick(Sender: TObject);
begin
  TBIMenu.Popup(ImportMenu,BNew);
end;

class function TDataSelector.Choose(const AOwner, AEdited: TComponent):TDataItem;
begin
  result:=nil;

  if not Choose(AOwner,AEdited,result) then
     result:=nil;
end;

class procedure TDataSelector.Choose(const AOwner: TComponent;
  const AEdited: TBIDataControl);
var tmp : TDataItem;
begin
  tmp:=AEdited.Data;

  if TDataSelector.Choose(AOwner,AEdited,tmp) then
     AEdited.Data:=tmp;
end;

procedure TDataSelector.SetEdited(const AEdited:TComponent);
begin
  IEdited:=AEdited;
  TDataComponentAccess(IComp).IEdited:=IEdited;
end;

class function TDataSelector.Choose(const AOwner: TComponent;
             const AEdited:TComponent;
             var AData:TDataItem;
             const AFilterOwner:TComponent=nil): Boolean;

begin
  with TDataSelector.Create(AOwner) do
  try
    SetEdited(AEdited);

    IFilterOwner:=AFilterOwner;

    BClear.Enabled:=AData<>nil;

    if AData<>nil then
       Select(AData);

    result:=ShowModal=mrOk;

    if result then
       AData:=Selected
    else
       AData:=nil;
  finally
    Free;
  end;
end;

procedure TDataSelector.CustomData1Click(Sender: TObject);
begin
  TryAddImport(TDataDefinitionKind.Manual);
end;

procedure TDataSelector.Database1Click(Sender: TObject);
begin
  TryAddImport(TDataDefinitionKind.Database);
end;

class function TDataSelector.Embedd(const AOwner: TComponent;
  const AParent: TWinControl;
  const AEdited: TComponent): TDataSelector;
begin
  result:=TDataSelector.Create(AOwner);
  result.SetEdited(AEdited);
  result.PanelButtons.Visible:=False;
  TUICommon.AddForm(result,AParent);
end;

procedure TDataSelector.Files1Click(Sender: TObject);
begin
  TryAddImport(TDataDefinitionKind.Files);
end;

function TDataSelector.IsEmbedded:Boolean;
begin
  result:=not PanelButtons.Visible;
end;

procedure TDataSelector.FilterSelf(Sender: TComponent; var Valid: Boolean);
begin
  Valid:=(Sender<>Self) and (Sender<>Manager) and
         (Sender<>IComp.Owner) and (Sender<>Manager.Owner) and
         (
           (not IsEmbedded)
           or
           (Sender<>Owner)
         )
         and
         // Extra "IFilterOwner" (ie: used by BIChart editor "Data" button)
         (
           (IFilterOwner=nil)
           or
           (
             (Sender.Owner<>IFilterOwner)
             and
             (Sender<>IFilterOwner)
           )
         );
end;

procedure TDataSelector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TDataComponentAccess(IComp).TryFreeData;

  TUICommon.SavePosition(Self,'DataSelector');
end;

function TDataSelector.SelectedHasData:Boolean;
begin
  if PageControl1.ActivePage=TabStore then
     result:=IManager.SelectedData<>nil
  else
     result:=TDataComponentAccess(IComp).SelectedHasData;
end;

procedure TDataSelector.SelectedItem(Sender: TObject);
begin
  BOK.Enabled:=SelectedHasData;

  if Assigned(FOnSelect) then
     FOnSelect(Self);
end;

procedure TDataSelector.Web1Click(Sender: TObject);
begin
  TryAddImport(TDataDefinitionKind.Web);
end;

function TDataSelector.NewOwner:TComponent;
begin
  if IEdited=nil then
     result:=Self
  else
     result:=IEdited.Owner;
end;

procedure TDataSelector.FinishAddNew(const AComponent:TComponent);
begin
  // Move UniqueName to an early place, where AComponent is created?
  if AComponent.Name='' then
     AComponent.Name:=TCommonUI.UniqueName(AComponent);

  TDataComponentAccess(IComp).Add(IEdited,AComponent);

  IComp.Select(AComponent);
end;

procedure TDataSelector.TryAddImport(const AKind:TDataDefinitionKind);
var tmp : TDataDefinition;
    tmpName : String;
begin
  tmp:=TDataEditor.NewDefinition(Self,NewOwner,AKind,tmpName);

  if tmp<>nil then
  begin
    tmp.Title:=tmpName;
    FinishAddNew(tmp);
  end;
end;

procedure TDataSelector.RefreshTree(const AFilter:TDataManagerFilter);
begin
  IManager.OnFilter:=AFilter;
end;

procedure TDataSelector.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabStore;

  IManager:=TDataManager.Embed(Self,TabStore);
  IManager.OnSelect:=SelectedItem;

  IComp:=TDataComponent.Create(Self);
  IComp.OnSelected:=SelectedItem;
  IComp.OnFilter:=FilterSelf;

  TUICommon.AddForm(IComp,TabComponent);
end;

procedure TDataSelector.FormShow(Sender: TObject);
begin
  SetEdited(IEdited);

  TUICommon.LoadPosition(Self,'DataSelector');
end;

procedure TDataSelector.PageControl1Change(Sender: TObject);
begin
  if Showing then
     BOK.Enabled:=SelectedHasData;
end;

procedure TDataSelector.Query1Click(Sender: TObject);
var tmp : TBIQuery;
begin
  tmp:=TBIQuery.Create(NewOwner);

  if TBIQueryEditor.Edit(Self,tmp) then
     FinishAddNew(tmp)
  else
     tmp.Free;
end;

end.
