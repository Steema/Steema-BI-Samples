{*********************************************}
{  TeeBI Software Library                     }
{  Workflow Component                         }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Workflow;

interface

{
  The TBIWorkflow component is a non-visual container of Data items.

  Each data item is associated with an "action", like for example adding a
  column, remove a column, perform a calculation etc.

  Items are organized in a hierarchical tree, so the "action" of an item uses
  its Parent data as the source.

  At design-time or run-time, any BI control or component can use a Workflow
  item output as data source.

  This means data will be calculated propagating each tree node level to obtain
  the final result.

  Example:

  Workflow:
  Data2 = Data1 -> Add Column -> Query -> Calculation -> Query ... etc

  BIGrid1.Data := Data2  // <-- process all steps above from Data1 to Data2

}

uses
  System.Classes, BI.Data, BI.Persist, BI.Query, BI.Arrays, BI.Arrays.Strings,
  BI.DataSource, BI.Summary, BI.Data.Expressions, BI.Algorithm,
  BI.Data.CollectionItem, BI.Data.Tools, BI.Expression.Filter, BI.Data.Rank,
  BI.Data.Merge;

type
  TWorkflowItems=class;

  // Collection Item
  TWorkflowItem=class(TDataCollectionItem)
  private
    FItems: TWorkflowItems;

    FParentItem : String;

    OwnsData : Boolean;

    // Temporary during loading
    IProvider : Integer;
    IOrigins : TDataOrigins;

    procedure FixOrigins(const AParent:TDataItem);
    function IsItemsStored: Boolean;
    procedure ReadProvider(Reader: TReader);
    procedure ReadOrigins(Reader: TReader);
    procedure SetItems(const Value: TWorkflowItems);
    procedure TrySetNeeds(const Value:TComponent);
    procedure TrySetSource(const Value:TDataItem); overload;
    procedure TrySetSource(const Value:TDataArray); overload;
    procedure WriteProvider(Writer: TWriter);
    procedure WriteOrigins(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function OriginsStored:Boolean;
    function Owner:TComponent; override;
    procedure SetData(const Value: TDataItem); override;
    procedure SetProvider(const Value: TComponent); override;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    function ParentData:TDataItem;
  published
    property Items:TWorkflowItems read FItems write SetItems stored IsItemsStored;
    property ParentItem:String read FParentItem write FParentItem;
  end;

  // Collection
  TWorkflowItems=class(TOwnedCollection)
  private
    function Get(const Index: Integer): TWorkflowItem;
    procedure Put(const Index: Integer; const Value: TWorkflowItem);
  public
    Constructor Create(AOwner: TPersistent);

    function Add: TWorkflowItem; overload;

    function Add(const AProvider:TDataProvider;
                 const AName:String):TWorkflowItem; overload;

    function Add(const ASource:TDataItem;
                 const AProvider:TDataProvider;
                 const AName:String):TWorkflowItem; overload;

    function Add(const ASources:TDataArray;
                 const AProvider:TDataProvider;
                 const AName:String):TWorkflowItem; overload;

    property Items[const Index:Integer]:TWorkflowItem read Get write Put; default;
  end;

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  {$ENDIF}
  TBIWorkflow=class(TBaseDataImporter)
  private
    FItems: TWorkflowItems;

    function IndexOfProvider(const AProvider:TDataProvider):Integer;
    procedure SetItems(const Value: TWorkflowItems);
  protected
    function GetChildOwner: TComponent; override;
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;

    procedure Load(const AData:TDataItem; const Children:Boolean); override;

    procedure Loaded; override;

    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
  published
    property Items:TWorkflowItems read FItems write SetItems;
  end;

  TConvertItem=class(TCloneProvider)
  private
    FKind : TDataKind;

    procedure SetKind(const Value: TDataKind);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;
  published
    property Kind:TDataKind read FKind write SetKind default TDataKind.dkUnknown;
  end;

  TReorderItem=class(TCloneProvider)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Items : TDataArray;
  end;

  TCompareItem=class(TDataProviderNeeds)
  private
    function GetA: TDataItem;
    function GetB: TDataItem;
    procedure SetA(const Value: TDataItem);
    procedure SetB(const Value: TDataItem);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;

    property A:TDataItem read GetA write SetA;
    property B:TDataItem read GetB write SetB;
  end;

  TMergeStyle=(Join,Subtract,Common,Different);

  TMergeItem=class(TCloneProvider)
  private
    FItems : TDataArray;
    FStyle : TMergeStyle;
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;
  published
    property Items:TDataArray read FItems write FItems;
    property Style:TMergeStyle read FStyle write FStyle default TMergeStyle.Join;
  end;

  TSplitItem=class(TSingleSourceProvider)
  private
    FBy : TSplitBy;
    FCount : Integer;
    FMode : TSplitMode;
    FPercent : Single;

    procedure SetBy(const Value:TSplitBy);
    procedure SetCount(const Value:Integer);
    procedure SetMode(const Value:TSplitMode);
    procedure SetPercent(const Value:Single);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;
  published
    property By:TSplitBy read FBy write SetBy default TSplitBy.Percent;
    property Count:Integer read FCount write SetCount default 0;
    property Mode:TSplitMode read FMode write SetMode default TSplitMode.Start;
    property Percent:Single read FPercent write SetPercent;
  end;

  TSortActionItem=class(TCloneProvider)
  private
    FItems : TSortItems;

    procedure SetItems(const Value: TSortItems);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    property SortItems:TSortItems read FItems write SetItems;
  published
  end;

  TDataRankItem=class(TCloneProvider)
  private
    FAscending: Boolean;

    function GetGroups: TDataArray;
    function GetValue: TDataItem;
    procedure SetAscending(const Value: Boolean);
    procedure SetGroups(const Value: TDataArray);
    procedure SetValue(const Value: TDataItem);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;

    property Groups:TDataArray read GetGroups write SetGroups;
    property Value:TDataItem read GetValue write SetValue;
  published
    property Ascending:Boolean read FAscending write SetAscending;
  end;

  TDataGridifyItem=class(TSingleSourceProvider)
  private
    function GetColumns: TDataArray;
    function GetRows: TDataArray;
    function GetValue: TDataItem;
    procedure SetColumns(const Value: TDataArray);
    procedure SetRows(const Value: TDataArray);
    procedure SetValue(const Value: TDataItem);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;

    property Columns:TDataArray read GetColumns write SetColumns;
    property Rows:TDataArray read GetRows write SetRows;
    property Value:TDataItem read GetValue write SetValue;
  end;

  TItemAction=class(TCloneProvider)
  private
    FItemName : String;

    procedure SetItemName(const Value: String);
  published
    property ItemName:String read FItemName write SetItemName;
  end;

  TNewItem=class(TItemAction)
  private
    FKind : TDataKind;

    function AddNewItem(const AData:TDataItem):TDataItem;
    procedure SetKind(const Value: TDataKind);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  published
    property Kind:TDataKind read FKind write SetKind;
  end;

  TItemsAction=class(TCloneProvider)
  private
    FItems : TStringArray;

    procedure SetItems(const Value: TStringArray);
  published
    property Items:TStringArray read FItems write SetItems;
  end;

  TDeleteItems=class(TItemsAction)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  end;

  TDuplicateItems=class(TItemsAction)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  end;

  TRenameItem=class(TItemAction)
  private
    FNewName : String;

    procedure SetNewName(const Value: String);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    property NewName:String read FNewName write SetNewName;
  end;

  TDataTranspose=class(TSingleSourceProvider)
  private
    procedure TransposeRow(const ADest:TDataItem; const ARow:TInteger; const AOffset:Integer);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  end;

  TNormalizeItem=class(TCloneProvider)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  end;

  TFilterAction=class(TSingleSourceProvider)
  private
    FQuery : TBIQuery;

    procedure SetFilter(const Value: TBIFilter);
    function GetFilter: TBIFilter;
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    property Filter:TBIFilter read GetFilter write SetFilter;
  end;

  TDataShuffle=class(TCloneProvider)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  end;

  TGroupByItem=class(TSingleSourceProvider)
  private
    function GetGroups: TDataArray;
    procedure SetGroups(const Value: TDataArray);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;

    property Groups:TDataArray read GetGroups write SetGroups;
  end;

  // Pending:
  // TDataMapReduce=class(TCloneProvider)
  // end;

  TBIProviderChooser=function(const AOwner:TComponent):TDataProvider;

var
  BIFunctionChooser:TBIProviderChooser=nil;

implementation
