unit BI.Data.Workflow;

interface

uses
  System.Classes, BI.Data, BI.Persist, BI.Query,
  BI.DataSource, BI.Summary;

type
  TWorkflowAction=class(TDataProvider)
  private
    FData : TDataItem;
    FOnChange: TNotifyEvent;

    procedure Changed;
    procedure Notify(const AEvent:TBIEvent);
    procedure SetData(const Value: TDataItem);
    procedure TryRemoveNotify;
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Destructor Destroy; override;
  published
    property Data:TDataItem read FData write SetData;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

  TWorkflowActions=class;

  TWorkflowActionItem=class(TCollectionItem)
  private
    FItems: TWorkflowActions;
    FData: TDataItem;
    FOnChange: TNotifyEvent;

    OwnsData : Boolean;

    procedure Changed;
    procedure SetItems(const Value: TWorkflowActions);
    procedure SetData(const Value: TDataItem);
    procedure TryRemoveNotify;
  protected
    procedure Notify(const AEvent:TBIEvent);
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    property Data:TDataItem read FData write SetData;
  published
    property Items:TWorkflowActions read FItems write SetItems;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

  TWorkflowActions=class(TOwnedCollection)
  private
    function Get(const Index: Integer): TWorkflowActionItem;
    procedure Put(const Index: Integer; const Value: TWorkflowActionItem);
  public
    Constructor Create(AOwner: TPersistent);

    function Add: TWorkflowActionItem; overload;
    function Add(const AProvider:TDataProvider; const AName:String):TWorkflowActionItem; overload;

    property Items[const Index:Integer]:TWorkflowActionItem read Get write Put; default;
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
    FSelected: TWorkflowActionItem;

    procedure SetItems(const Value: TWorkflowActions);
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    property Selected:TWorkflowActionItem read FSelected write FSelected;
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
