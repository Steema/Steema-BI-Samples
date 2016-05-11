unit BI.Data.Workflow;

interface

uses
  System.Classes, BI.Data, BI.Persist, BI.Query,
  BI.DataSource, BI.Summary;

type
  TWorkflowAction=class(TDataProvider)
  private
    FData : TDataItem;

    procedure Notify(const AEvent:TBIEvent);
    procedure SetData(const Value: TDataItem);
    procedure TryRemoveNotify;
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Destructor Destroy; override;
  published
    property Data:TDataItem read FData write SetData;
  end;

  TWorkflowActions=class;

  TWorkflowItem=class(TCollectionItem)
  private
    FData: TDataItem;
    FItems: TWorkflowActions;
    FOnChange: TNotifyEvent;

    OwnsData : Boolean;

    procedure Changed;
//    procedure ReadWorkAction(Reader: TReader);
    procedure SetData(const Value: TDataItem);
    procedure SetItems(const Value: TWorkflowActions);
    procedure SetProvider(const AProvider:TDataProvider; const AName:String);
    procedure TryRemoveNotify;
 //   procedure WriteWorkAction(Writer: TWriter);
    function IsItemsStored: Boolean;
    function GetWorkAction: TDataProvider;
    function IsWorkActionStored: Boolean;
  protected
    //procedure DefineProperties(Filer: TFiler); override;
    procedure Notify(const AEvent:TBIEvent);
    function Owner:TComponent;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    property Data:TDataItem read FData write SetData;
  published
    property Items:TWorkflowActions read FItems write SetItems stored IsItemsStored;

    property WorkAction:TDataProvider read GetWorkAction stored IsWorkActionStored;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
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
