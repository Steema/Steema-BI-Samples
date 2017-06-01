{*********************************************}
{  TeeBI Software Library                     }
{  Query Editor Dialog                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Query;

interface

{
  This form is used to edit a TBIQuery (pivot-table) component.

  Data can be drag-dropped from the left tree to the Rows, Columns or Measures
  listboxes.

  The listboxes can also be drag-dropped to reorder elements or to move items
  from one list to another.

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.CheckLst, Vcl.Buttons,

  BI.DataItem, BI.Summary, BI.DataSource, VCLBI.DataSelect, BI.Query,
  VCLBI.Grid, VCLBI.DataControl, BI.Persist, VCLBI.Editor.DynamicFilter,
  BI.Expression, VCLBI.Editor.Sort, VCLBI.DataManager;

type
  TOnShowQueryEditor=procedure(const AParent:TWinControl;
                               const AProvider:TComponent;
                               const AData:TDataItem);

  TBIQueryEditor = class(TForm)
    PanelSelector: TPanel;
    PanelEdit: TPanel;
    OuterPanel: TPanel;
    SplitterPreview: TSplitter;
    SplitterSelector: TSplitter;
    PanelRows: TPanel;
    PanelColumns: TPanel;
    PanelMeasures: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ListRows: TCheckListBox;
    ListMeasures: TCheckListBox;
    ListColumns: TCheckListBox;
    Panel4: TPanel;
    CBDistinct: TCheckBox;
    Panel5: TPanel;
    Panel7: TPanel;
    BDeleteColumn: TButton;
    PageMeasures: TPageControl;
    TabMeasure: TTabSheet;
    CBAggregate: TComboBox;
    CBMissingAsZero: TCheckBox;
    TabCalc: TTabSheet;
    RGRunning: TRadioGroup;
    CBRunningRows: TCheckBox;
    RGPercentage: TRadioGroup;
    Label4: TLabel;
    LabelItemKind: TLabel;
    Label5: TLabel;
    LExpressionError: TLabel;
    EItemExpression: TEdit;
    CBRemoveRows: TCheckBox;
    CBRemoveCols: TCheckBox;
    PanelOptions: TPanel;
    PanelButtons: TPanel;
    BOK: TButton;
    Button1: TButton;
    LMax: TLabel;
    EMax: TEdit;
    Label1: TLabel;
    BDeleteMeasure: TButton;
    Label2: TLabel;
    BDeleteRow: TButton;
    PageOptions: TPageControl;
    TabItem: TTabSheet;
    TabMeasureOptions: TTabSheet;
    Label3: TLabel;
    CBDatePart: TComboBox;
    GBHistogram: TGroupBox;
    CBHistoActive: TCheckBox;
    SBMeasureUp: TSpeedButton;
    SBMeasureDown: TSpeedButton;
    Label6: TLabel;
    EBins: TEdit;
    UDBins: TUpDown;
    SBSwap: TSpeedButton;
    SBSelector: TSpeedButton;
    Label7: TLabel;
    ETitle: TEdit;
    SpeedButton1: TSpeedButton;
    TabItemData: TTabSheet;
    PagePreview: TPageControl;
    TabGrid: TTabSheet;
    BIGrid1: TBIGrid;
    TabChart: TTabSheet;
    PageData: TPageControl;
    TabData: TTabSheet;
    TabFilter: TTabSheet;
    LStart: TLabel;
    EStart: TEdit;
    TabSort: TTabSheet;
    BIQuery1: TBIQuery;
    TabSQL: TTabSheet;
    MemoSQL: TMemo;
    CBPreview: TCheckBox;
    Panel6: TPanel;
    SBRowUp: TSpeedButton;
    SBRowDown: TSpeedButton;
    Panel8: TPanel;
    SBColUp: TSpeedButton;
    SBColDown: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure ListRowsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListRowsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListRowsClick(Sender: TObject);
    procedure BDeleteRowClick(Sender: TObject);
    procedure CBDistinctClick(Sender: TObject);
    procedure BDeleteMeasureClick(Sender: TObject);
    procedure ListMeasuresClick(Sender: TObject);
    procedure ListColumnsClick(Sender: TObject);
    procedure BDeleteColumnClick(Sender: TObject);
    procedure CBAggregateChange(Sender: TObject);
    procedure CBMissingAsZeroClick(Sender: TObject);
    procedure RGPercentageClick(Sender: TObject);
    procedure RGRunningClick(Sender: TObject);
    procedure CBRunningRowsClick(Sender: TObject);
    procedure EItemExpressionChange(Sender: TObject);
    procedure CBRemoveRowsClick(Sender: TObject);
    procedure CBRemoveColsClick(Sender: TObject);
    procedure CBDatePartChange(Sender: TObject);
    procedure ListMeasuresDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListRowsClickCheck(Sender: TObject);
    procedure EMaxChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBHistoActiveClick(Sender: TObject);
    procedure SBRowUpClick(Sender: TObject);
    procedure SBRowDownClick(Sender: TObject);
    procedure SBMeasureDownClick(Sender: TObject);
    procedure SBMeasureUpClick(Sender: TObject);
    procedure SBColUpClick(Sender: TObject);
    procedure SBColDownClick(Sender: TObject);
    procedure EBinsChange(Sender: TObject);
    procedure SBSwapClick(Sender: TObject);
    procedure SBSelectorClick(Sender: TObject);
    procedure ETitleChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PageDataChange(Sender: TObject);
    procedure EStartChange(Sender: TObject);
    procedure PagePreviewChange(Sender: TObject);
    procedure CBPreviewClick(Sender: TObject);
  private
    { Private declarations }

    type
      TAddMasterProc=procedure(const AData:TDataItem) of object;

    var
    IQuery : TBIQuery;

    ISelector : TDataSelector;

    CompTree,
    DataTree : TTreeView;

    IFilter : TDynamicFilterEditor;
    ISort : TSortEditor;

    IChanging,
    ISettingPreview,
    IModified : Boolean;

    IMasterFilter : TMasterFilter;

    IDimension : TQueryDimension;
    IMeasure : TQueryMeasure;

    function AddData(const AList:TCheckListBox; const AData:TDataItem; const IsActive:Boolean=True):TQueryItem;
    procedure AddItem(const AList:TCheckListBox; const AItem:TQueryItem);
    procedure AdjustSelectorCaption;
    procedure ChangeCurrentBy;
    procedure ChangeCurrentMeasure;
    procedure ChangeDimension(const AList:TCheckListBox); overload;
    procedure ChangeDimension(const AItem:TQueryDimension); overload;
    procedure ChangeMeasure; overload;
    procedure ChangeMeasure(const AItem:TQueryMeasure); overload;
    procedure ChangedFilter(Sender: TObject);
    function ChangingQuery:Boolean;
    procedure DeleteDimension(const AList:TCheckListBox);
    procedure DoExchangeItem(const AList:TCheckListBox; const A,B:Integer); overload;
    procedure DoExchangeItem(const AList:TCheckListBox; const Delta:Integer); overload;

    function EditResolver(const S:String; IsFunction:Boolean):TExpression;

    procedure EnableHistogramControls;
    procedure EnableColumnButtons;
    procedure EnableMeasureButtons;
    procedure EnableRowButtons;
    procedure EnableRowSettings;
    procedure EnableSwap;
    procedure FilterComponent(Sender: TComponent; var Valid:Boolean);

    function GetPreview: Boolean;
    function ListOf(const ABy:TQueryDimension):TCheckListBox;
    function Measure:TQueryMeasure;
    procedure Modified;
    procedure RefreshFilter;
    procedure RefreshFilterAndSort;
    procedure RefreshQuery;
    procedure RefreshSelector;
    procedure RemoveFromList(const AList:TCheckListBox); overload;
    procedure RemoveFromList(const AList:TCheckListBox; const AIndex:Integer); overload;
    function ResolveData(const APos:Integer; const AMessage:String):Boolean;
    function Resolver(const S:String; IsFunction:Boolean):TExpression;

    procedure SetDataProperties(const AItem:TQueryDimension); overload;
    procedure SetDataProperties(const AItem:TQueryMeasure); overload;
    procedure SetDataProperties(const AData:TDataItem; const AExpression:TExpression); overload;

    procedure SetItemProperties(const ACurrent:TQueryDimension); overload;
    procedure SetItemProperties(const ACurrent:TQueryMeasure); overload;

    procedure SetPart(const ACombo:TComboBox; const APart:TDateTimePart);
    procedure SetPreview(const Value: Boolean);

    procedure ShowSQL;
    procedure SortChanged(Sender:TObject);
    procedure TryAddMasters(const AProc:TAddMasterProc);
    procedure TryShowPreviewGrid;
    procedure TryShowSQL;
  public
    { Public declarations }

    class
      // Event used by VCLBI.Editor.Chart to display a BIChart at Preview tab
      var OnShowEditor : TOnShowQueryEditor;

    function Selector:TDataSelector;

    class function Edit(const AOwner:TComponent; const AQuery:TBIQuery):Boolean; static;
    class function Embedd(const AOwner: TComponent; const AParent: TWinControl; const AQuery:TBIQuery): TBIQueryEditor; static;

    procedure Refresh(const AQuery:TBIQuery);
    procedure ShowSelector(const AShow:Boolean);

    // Show or hide the "Preview" tab
    property Preview:Boolean read GetPreview write SetPreview;
  end;

implementation
