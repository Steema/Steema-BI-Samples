unit BI.Algorithm;

interface

uses
  BI.Data;

type
  TQuantityStyle=(Exact, Minimum, Maximum);

  TDataKinds=set of TDataKind;

  TDataKindsHelper=record helper for TDataKinds
  public
    const Numeric:TDataKinds=[dkInt32,dkInt64,dkSingle,dkDouble,dkExtended];
  end;

  // Describes an input parameter for algorithms
  TAlgorithmNeed=record
  public
    Data : TDataArray;
    Kinds : TDataKinds;
    Quantity : Integer;
    Style : TQuantityStyle;

    Constructor Create(const AStyle:TQuantityStyle; const AQuantity:Integer; const AKinds:TDataKinds=[]);

    procedure CheckData;
    procedure Verify;
  end;

  // Describes all the input parameters an algorithm requires
  TAlgorithmNeeds=Array of TAlgorithmNeed;

  TAlgorithmNeedsHelper=record helper for TAlgorithmNeeds
  public
    procedure Add(const ANeed:TAlgorithmNeed);
    procedure Clear; inline;
    function Count:Integer; inline;
    procedure Verify;
  end;

  // Base class for all algorithms
  TBaseAlgorithm=class(TDataProvider)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Needs : TAlgorithmNeeds;

    procedure Calculate; virtual; abstract;
  end;

implementation
