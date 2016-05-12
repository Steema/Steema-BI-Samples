{*********************************************}
{  TeeBI Software Library                     }
{  Workflow Component                         }
{  Copyright (c) 2015-2016 by Steema Software }
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
  System.Classes, BI.Data, BI.Persist, BI.Query,
  BI.DataSource, BI.Summary, BI.Store.Component;

type
  TWorkflowAction=class(TBaseDataImporter)
  private
    FSource : TDataItem;
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    property Source:TDataItem read FSource write FSource;
  end;

  TWorkflowActions=class;

  TWorkflowItem=class(TDataCollectionItem)
  private
    FItems: TWorkflowActions;
    OwnsData : Boolean;

//    procedure ReadWorkAction(Reader: TReader);
    procedure SetItems(const Value: TWorkflowActions);
    procedure SetProvider(const AProvider:TDataProvider; const AName:String);
 //   procedure WriteWorkAction(Writer: TWriter);
    function IsItemsStored: Boolean;
    function GetWorkAction: TDataProvider;
    function IsWorkActionStored: Boolean;
  protected
    function Owner:TComponent; override;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    property Items:TWorkflowActions read FItems write SetItems stored IsItemsStored;
    property WorkAction:TDataProvider read GetWorkAction stored IsWorkActionStored;
  end;

  TWorkflowActions=class(TOwnedCollection)
  private
    function Get(const Index: Integer): TWorkflowItem;
    procedure Put(const Index: Integer; const Value: TWorkflowItem);
  public
    Constructor Create(AOwner: TPersistent);

    function Add: TWorkflowItem; overload;
    function Add(const AProvider:TDataProvider; const AName:String):TWorkflowItem; overload;

    property Items[const Index:Integer]:TWorkflowItem read Get write Put; default;
  end;

  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TBIWorkflow=class(TComponent)
  private
    FItems: TWorkflowActions;
    FSelected: TWorkflowItem;

    procedure SetItems(const Value: TWorkflowActions);
  protected
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    property Selected:TWorkflowItem read FSelected write FSelected;
  published
    property Items:TWorkflowActions read FItems write SetItems;
  end;

  TItemAction=class(TWorkflowAction)
  private
    FItemName : String;

    procedure SetItemName(const Value: String);
  published
    property ItemName:String read FItemName write SetItemName;
  end;

  TNewItem=class(TItemAction)
  private
    FKind : TDataKind;

    procedure SetKind(const Value: TDataKind);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  published
    property Kind:TDataKind read FKind write SetKind;
  end;

  TDeleteItem=class(TItemAction)
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

  TDataTranspose=class(TWorkflowAction)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  end;

  TQueryAction=class(TWorkflowAction)
  private
    FQuery: TBIQuery;
    FSQL: String;
    FOnError: TBIErrorProc;

    procedure SetSQL(const Value: String);
    procedure SetQuery(const Value: TBIQuery);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Constructor From(const AOwner:TComponent; const ASelect:TDataSelect); overload;
    Constructor From(const AOwner:TComponent; const ASummary:TSummary); overload;

    Destructor Destroy; override;

    function Add(const AData:TDataItem;
                 const AStyle:TQueryItemStyle=TQueryItemStyle.Automatic;
                 const IsActive:Boolean=True): TQueryItem; overload;

    function Add(const AData:TDataItem;
                 const AMeasure:TAggregate): TQueryItem; overload;

    property OnError:TBIErrorProc read FOnError write FOnError;
  published
    property Query:TBIQuery read FQuery write SetQuery;
    property SQL:String read FSQL write SetSQL;
 end;

implementation
