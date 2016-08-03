{*********************************************}
{  TeeBI Software Library                     }
{  Dynamic Filter Editor                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.DynamicFilter;

interface

{
  This dialog fills a tree with all possible Data Values that can be accessed
  from a given AData parameter.

  The tree enables checking and unchecking individual items to build a Filter
  expression that can be then used in for example a BIGrid to display rows that
  match the selected items.

  Example 1:
  Using this editor embedded into a Form:

  uses
    BI.Persist, BI.VCL.Editor.DynamicFilter;

  var IEditor : TDynamicFilterEditor;

  procedure TFormTest.FilterChanged(Sender: TObject);
  begin
    BIGrid1.SetFilter(IEditor.Filter);
  end;

  procedure TFormTest.FormCreate(Sender: TObject);
  begin
    BIGrid1.Data:=TStore.load('SQLite_Demo')['"Order Details"'];

    IEditor:=TDynamicFilterEditor.Embedd(Self,Panel1,BIGrid1.Data);
    IEditor.OnChange:=FilterChanged;
  end;

  ----------------------

  Example 2:
  Using this editor as a modal dialog:

  procedure TForm23.Button2Click(Sender: TObject);
  begin
    BIGrid1.SetFilter(TDynamicFilterEditor.Choose(Self,BIGrid1.Data));
  end;

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.DataControl,
  BI.VCL.Tree, BI.Data, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, BI.Expression,
  BI.VCL.Editor.Expression, Vcl.Buttons,
  BI.Expression.Filter, Vcl.CheckLst, BI.Data.CollectionItem,
  BI.VCL.Editor.Filter.Item;

type
  TDynamicFilterEditor = class(TForm)
    PanelButtons: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabData: TTabSheet;
    TabItems: TTabSheet;
    BITree1: TBITree;
    CBItems: TCheckListBox;
    Panel3: TPanel;
    CBEnabled: TCheckBox;
    Panel4: TPanel;
    BAdd: TButton;
    Splitter1: TSplitter;
    BDelete: TButton;
    TabCustom: TTabSheet;
    PanelCustom: TPanel;
    SBCustom: TSpeedButton;
    LError: TLabel;
    Panel2: TPanel;
    CBCustom: TComboBox;
    ECustom: TEdit;
    PanelItem: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ECustomChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBCustomChange(Sender: TObject);
    procedure SBCustomClick(Sender: TObject);
    procedure PanelCustomResize(Sender: TObject);
    procedure BITree1Change(Sender: TObject);
    procedure CBItemsClick(Sender: TObject);
    procedure CBItemsClickCheck(Sender: TObject);
    procedure CBEnabledClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
  private
    { Private declarations }

    IChecking : Boolean;

    IFilter : TBIFilter;

    ICustom : TLogicalExpression;

    IData,
    IMainData : TDataItem;

    IEditor : TFilterItemEditor;

    FOnChange: TNotifyEvent;

    procedure AddFilter;
    procedure AddFilterItem(const AItem:TFilterItem);
    procedure AddMapValues(const AParent:TBITreeNode; const AData:TDataItem);
    function AddNewFilter(const AData:TDataItem):TFilterItem;
    function CanAddFilter(const AData:TDataItem):Boolean;
    procedure ChangedFilter(Sender: TObject);
    function Current:TFilterItem;
    function CurrentData:TDataItem;
    procedure DoChanged;
    procedure Expanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    function HasDummy(const ANode:TBITreeNode):Boolean;
    procedure RefreshProperties(const AItem:TFilterItem);
    function Resolver(const S:String; IsFunction:Boolean):TExpression;
    procedure TreeChecked(Sender: TObject);
    function TryAddFilter(const AData:TDataItem):TFilterItem;
    procedure TryChangeExpression(const AExp:TExpression);
  public
    { Public declarations }

    class function Choose(const AOwner:TComponent;
                          const AData:TDataItem;
                          const AMain:TDataItem=nil):TExpression; static;

    class function Edit(const AOwner:TComponent;
                        const AFilter:TBIFilter;
                        const AData:TDataItem=nil;
                        const AMain:TDataItem=nil):Boolean; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AFilter:TBIFilter;
                          const AData:TDataItem;
                          const AMain:TDataItem=nil):TDynamicFilterEditor; static;

    procedure Refresh(const AData:TDataItem; const AMain:TDataItem=nil); overload;
    procedure Refresh(const AFilter:TBIFilter; const AData:TDataItem; const AMain:TDataItem=nil); overload;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
