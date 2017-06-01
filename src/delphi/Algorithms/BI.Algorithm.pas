{*********************************************}
{  TeeBI Software Library                     }
{  Base abstract Algorithm class              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm;

interface

uses
  System.Classes, BI.DataItem, BI.Arrays.Strings;

type
  // Just a set of TDataKind
  TDataKinds=set of TDataKind;

  // "Numeric" set constant with all TDataKind that correspond to numbers
  TDataKindsHelper=record helper for TDataKinds
  public
    const
      Numeric:TDataKinds=[dkInt32,dkInt64,dkSingle,dkDouble,dkExtended,dkDateTime];
  end;

  TQuantityStyle=(Exact, Minimum, Maximum);

  // Describes an input parameter for algorithms
  TAlgorithmNeed=record
  private
    function AnyDataNot(const AData:TDataItem):Boolean;
    class procedure DoError(const AMessage:String; const Args:Array of Const); static;
  public
    Data : TDataArray;
    Kinds : TDataKinds;
    Name : String;
    Quantity : Integer;
    Style : TQuantityStyle;

    Constructor Create(const AName:String;
                       const AStyle:TQuantityStyle;
                       const AQuantity:Integer;
                       const AKinds:TDataKinds=[]);

    procedure CheckData;
    function First:TDataItem;
    function Verify(const Silent:Boolean=False):Boolean;
  end;

  // Describes all the input parameters an algorithm requires
  TAlgorithmNeeds=Array of TAlgorithmNeed;

  TAlgorithmNeedsHelper=record helper for TAlgorithmNeeds
  public
    procedure Add(const ANeed:TAlgorithmNeed);
    function AnyNeedNot(const AData:TDataItem):Boolean;
    procedure Clear; inline;
    function Count:Integer; inline;
    function Verify(const Silent:Boolean=False):Boolean;
  end;

  TDataOrigins=Array of TStringArray;

  TDataProviderNeeds=class(TDataProvider)
  protected
    IParent : TDataItem;
    IOrigins : TDataOrigins;

    procedure TryFixOrigins;
  public
    Needs : TAlgorithmNeeds;
  end;

  // Base class for all algorithms
  TBaseAlgorithm=class(TDataProviderNeeds)
  protected
    procedure AddOutput(const AData:TDataItem); virtual;
    procedure Load(const AData: TDataItem; const Children: Boolean); override;
  public
    procedure Calculate; virtual; abstract;
  end;

  TBaseAlgorithmClass=class of TBaseAlgorithm;

  // Simple helper class that defines a single "needed" data item
  TSingleSourceProvider=class(TDataProviderNeeds)
  private
    function GetSource: TDataItem;
    procedure TryRemoveNotify;
  protected
    procedure ClearSource; virtual;
    procedure Notify(const AEvent:TBIEvent);
    procedure SetSource(const Value: TDataItem); virtual;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    property Source:TDataItem read GetSource write SetSource;
  end;

  // Special provider that creates a clone copy of its single source data
  TCloneProvider=class(TSingleSourceProvider)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  end;

implementation
