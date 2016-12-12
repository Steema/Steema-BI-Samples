{*********************************************}
{  TeeBI Software Library                     }
{  TDataCollectionItem (Collection Item)      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.CollectionItem;

interface

{
  TDataCollectionItem is a derived class of TCollectionItem.

  It can be used to create custom TCollection classes where each item
  can reference to a TDataItem and / or a TDataProvider.

  For example, TBIQuery and TBIWorkflow components use this class for their
  collections.
}

uses
  System.Classes, BI.Data, BI.Expression;

type
  TDataCollectionItem=class(TCollectionItem)
  private
    FName : String;
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
    procedure WriteOrigin(Writer: TWriter);
  protected
    FData : TDataItem;
    FProvider : TComponent;

    IOrigin : String;
    IUpdating : Integer;

    procedure BeginUpdate; inline;
    procedure Changed; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure EndUpdate;
    function GetDisplayName:String; override;
    procedure Loaded; virtual;
    function Owner:TComponent; virtual;
    procedure SetData(const Value: TDataItem); virtual;
    procedure SetProvider(const Value: TComponent); virtual;
    procedure ValidateData(const AData:TDataItem); virtual;
  public
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Data:TDataItem read GetData write SetData;
    property Name:String read FName write FName;
    property Provider:TComponent read FProvider write SetProvider;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
