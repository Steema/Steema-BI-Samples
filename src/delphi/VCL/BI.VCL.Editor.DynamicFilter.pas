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
  BI.VCL.Editor.Expression, Vcl.Buttons, BI.VCL.Editor.DateTimeRange,
  BI.VCL.Editor.NumericFromTo, BI.VCL.Editor.SelectText,
  BI.Expression.Filter, Vcl.CheckLst;

type
  TDynamicFilterEditor = class(TForm)
    PanelCustom: TPanel;
    Panel2: TPanel;
    CBCustom: TComboBox;
    PanelButtons: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ECustom: TEdit;
    SBCustom: TSpeedButton;
    LError: TLabel;
    PageControl1: TPageControl;
    TabData: TTabSheet;
    TabItems: TTabSheet;
    BITree1: TBITree;
    CBItems: TCheckListBox;
    Panel3: TPanel;
    CBEnabled: TCheckBox;
    PageItem: TPageControl;
    TabDateTime: TTabSheet;
    TabBoolean: TTabSheet;
    CBTrue: TCheckBox;
    CBFalse: TCheckBox;
    TabNumeric: TTabSheet;
    TabText: TTabSheet;
    Panel4: TPanel;
    BAdd: TButton;
    PageControl2: TPageControl;
    TabIncluded: TTabSheet;
    TabExcluded: TTabSheet;
    Splitter1: TSplitter;
    BDelete: TButton;
    PageNumeric: TPageControl;
    TabNumericRange: TTabSheet;
    TabNumericSelected: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure ECustomChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBCustomChange(Sender: TObject);
    procedure SBCustomClick(Sender: TObject);
    procedure PanelCustomResize(Sender: TObject);
    procedure BITree1Change(Sender: TObject);
    procedure CBTrueClick(Sender: TObject);
    procedure CBFalseClick(Sender: TObject);
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

    IChanging : Boolean;

    IData,
    IMainData : TDataItem;

    INumericFromTo,
    INumericSelected : TNumericFromTo;

    ITextInclude,
    ITextExclude   : TSelectTextItems;
    IDateTimeRange : TDateTimeRangeEditor;

    FOnChange: TNotifyEvent;

    procedure AddFilter;
    procedure AddFilterItem(const AItem:TFilterItem);
    procedure AddMapValues(const AParent:TBITreeNode; const AData:TDataItem);
    function AddNewFilter(const AData:TDataItem):TFilterItem;
    function CanAddFilter(const AData:TDataItem):Boolean;
    procedure ChangedDateTime(Sender: TObject);
    procedure ChangedNumeric(Sender: TObject);
    procedure ChangedText(Sender: TObject);
    procedure CheckedText(const Sender: TObject; const AText:String; const IsChecked:Boolean);
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
                          const AMain:TDataItem=nil):TLogicalExpression; static;

    class function Edit(const AOwner:TComponent;
                        const AFilter:TBIFilter):Boolean; static;

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
