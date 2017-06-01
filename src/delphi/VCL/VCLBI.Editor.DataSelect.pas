{*********************************************}
{  TeeBI Software Library                     }
{  TDataSelect Editor                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.DataSelect;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Buttons,
  Vcl.ComCtrls, Vcl.Menus,
  BI.DataItem, BI.DataSource, BI.Expressions;

type
  TDataSelectEditor = class(TForm)
    PageControl1: TPageControl;
    TabItems: TTabSheet;
    Panel2: TPanel;
    BUp: TSpeedButton;
    BDown: TSpeedButton;
    BAdd: TButton;
    BRemoveItem: TButton;
    LItems: TCheckListBox;
    TabSort: TTabSheet;
    TabFilter: TTabSheet;
    LBSort: TCheckListBox;
    EFilter: TEdit;
    Panel3: TPanel;
    BUpSort: TSpeedButton;
    BDownSort: TSpeedButton;
    BAddSort: TButton;
    BRemoveSort: TButton;
    PopupSort: TPopupMenu;
    Data1: TMenuItem;
    Expression1: TMenuItem;
    Panel1: TPanel;
    CBDistinct: TCheckBox;
    LFilter: TLabel;
    Label1: TLabel;
    EMax: TEdit;
    Panel4: TPanel;
    LSortError: TLabel;
    ESortExpression: TEdit;
    LSortExpression: TLabel;
    CBIgnoreCase: TCheckBox;
    CBAscending: TCheckBox;
    Panel5: TPanel;
    Label2: TLabel;
    EItemExpression: TEdit;
    LItemError: TLabel;
    PopupItems: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    TabSQL: TTabSheet;
    MemoSQL: TMemo;
    Label3: TLabel;
    EItemName: TEdit;
    PanelButtons: TPanel;
    Panel9: TPanel;
    BOK: TButton;
    Button2: TButton;
    procedure CBDistinctClick(Sender: TObject);
    procedure LItemsClick(Sender: TObject);
    procedure LItemsClickCheck(Sender: TObject);
    procedure BRemoveItemClick(Sender: TObject);
    procedure BUpClick(Sender: TObject);
    procedure BDownClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure LBSortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BAddSortClick(Sender: TObject);
    procedure BRemoveSortClick(Sender: TObject);
    procedure BUpSortClick(Sender: TObject);
    procedure BDownSortClick(Sender: TObject);
    procedure Expression1Click(Sender: TObject);
    procedure Data1Click(Sender: TObject);
    procedure ESortExpressionChange(Sender: TObject);
    procedure EFilterChange(Sender: TObject);
    procedure CBIgnoreCaseClick(Sender: TObject);
    procedure CBAscendingClick(Sender: TObject);
    procedure EMaxChange(Sender: TObject);
    procedure LItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBSortDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBSortDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure EItemExpressionChange(Sender: TObject);
    procedure LBSortClickCheck(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure EItemNameChange(Sender: TObject);
  private
    { Private declarations }

    ISelect : TDataSelect;
    FOnChanged : TNotifyEvent;

    IChanging : Boolean;

    procedure AddItem(const AData:TDataItem);

    function ChangeExpression(const AData:TDataItem; const AError:TLabel;
                              const AText:String;
                              out AExpression:TExpressionColumn):Boolean;

    function ChooseSortItem:TDataItem;
    procedure DoChanged;

    procedure FillItems;
    procedure FillSortBy;

    procedure SetExpressionText(const AEdit:TEdit; const AData:TDataItem);
    procedure SwapItems(const A,B:Integer);
    procedure SwapSort(const A,B:Integer);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent;
                        const ASelect:TDataSelect;
                        const OnChanged:TNotifyEvent=nil):Boolean; static;

    procedure Refresh(const ASelect:TDataSelect);

    property Select:TDataSelect read ISelect;
    property OnChange:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation
