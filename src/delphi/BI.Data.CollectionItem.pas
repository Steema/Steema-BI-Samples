unit BI.Data.CollectionItem;

interface

uses
  System.Classes, BI.Data, BI.Expression;

type
  TDataCollectionItem=class(TCollectionItem)
  private
    FOnChange: TNotifyEvent;

    procedure AddNotify;
    function GetData: TDataItem;
    procedure InternalSetProvider(const Value: TComponent);
    function LoadOrigin:TDataItem;
    procedure Notify(const AEvent:TBIEvent);
    procedure NotifyDataDestroy(const AEvent:TBIEvent);
    function Origin:String;
    procedure ReadOrigin(Reader: TReader);
    procedure RemoveNotify;
    procedure SetDataDirect(const Value: TDataItem);
    procedure SetProvider(const Value: TComponent);
    procedure WriteOrigin(Writer: TWriter);
  protected
    FData : TDataItem;
    FProvider : TComponent;

    IOrigin : String;
    IUpdating : Integer;

    procedure BeginUpdate;
    procedure Changed; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure EndUpdate;
    procedure Loaded; virtual;
    function Owner:TComponent; virtual;
    procedure SetData(const Value: TDataItem); virtual;
    procedure ValidateData(const AData:TDataItem); virtual;
  public
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Data:TDataItem read GetData write SetData;
    property Provider:TComponent read FProvider write SetProvider;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
