{*********************************************}
{  TeeBI Software Library                     }
{  Base Model algorithm classes               }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Model;

interface

uses
  BI.Arrays, BI.Data;

type
  TQuantityStyle=(Exact, Minimum, Maximum);

  TDataKinds=set of TDataKind;

  TDataKindsHelper=record helper for TDataKinds
  public
    const Numeric:TDataKinds=[dkInt32,dkInt64,dkSingle,dkDouble,dkExtended];
  end;

  TAlgorithmNeed=record
  public
    Data : TDataArray;
    Kinds : TDataKinds;
    Quantity : Integer;
    Style : TQuantityStyle;

    procedure CheckData;
    procedure Verify;
  end;

  // Describes the input parameter an algorithm requires
  TAlgorithmNeeds=Array of TAlgorithmNeed;

  TAlgorithmNeedsHelper=record helper for TAlgorithmNeeds
  public
    procedure Add(const ANeed:TAlgorithmNeed);
    function Count:Integer; inline;
    procedure Verify;
  end;

  // Base class for all algorithms
  TBaseAlgorithm=class
  public
    Needs : TAlgorithmNeeds;

    Constructor Create; virtual;
    procedure Calculate; virtual; abstract;
  end;

  // Base class for machine-learning algorithms that require a "Target"
  // (or "Class" or "Label") data
  TModel=class(TBaseAlgorithm)
  private
    class procedure NormalizeInt32(const AData:TDataItem); static;
    class procedure NormalizeInt64(const AData:TDataItem); static;
    class procedure NormalizeSingle(const AData:TDataItem); static;
    class procedure NormalizeDouble(const AData:TDataItem); static;
    class procedure NormalizeExtended(const AData:TDataItem); static;
    class procedure NormalizeDateTime(const AData:TDataItem); static;
    class procedure NormalizeBoolean(const AData:TDataItem); static;
    class procedure NormalizeText(const AData:TDataItem); static;
  public
    Constructor Create; override;

    class procedure Normalize(const AData:TDataItem); overload; static;
    class procedure Normalize(const ADatas:TDataArray); overload; static;
  end;

  TSplitMode=(Random,Start);

  // Fills A and B arrays either using random values or sequential
  TDataSplit=record
    A,
    B : TInt64Array;

    procedure Random(const CountA, CountB:TInteger);
    procedure Sequence(const CountA, CountB:TInteger);
  end;

  // Special TDataItem containing 3 columns:
  // 0 : The index position in the original source data
  // 1 : The "real" value (Target) in the original source data
  // 2 : The "predicted" value calculated by the algorithm
  TPredictedData=class(TDataItem)
  private
    FConfusion : TDataItem;
    FPredicted : TDataItem;
    FReal : TDataItem;

    function GetConfusion:TDataItem;
    function GetCorrect:TInteger;
  public
    Constructor Create(const ATarget:TDataItem; const Indices:TInt64Array);
    Destructor Destroy; override;

    function IsEqual(const AIndex: TInteger):Boolean;

    property Confusion:TDataItem read GetConfusion;
    property Correct:TInteger read GetCorrect;
    property Predicted:TDataItem read FPredicted;
    property Real:TDataItem read FReal;
  end;

  TSupervisedModelClass=class of TSupervisedModel;

  TSupervisedModels=Array of TSupervisedModelClass;

  // Model class supporting "train" and "test" of an algorithm
  TSupervisedModel=class(TModel)
  private
    function GetTarget: TDataItem;
    procedure SetTarget(const Value: TDataItem);
    function GetAttributes: TDataArray;
    procedure SetAttributes(const Value: TDataArray);
  protected
    FPredicted : TPredictedData;

    function GetPredicted:TPredictedData;
  public
    class var
       Models : TSupervisedModels;

    Indices : TDataSplit;

    Destructor Destroy; override;

    class procedure RegisterModels(const AModels:Array of TSupervisedModelClass); static;

    procedure Split(const Train,Test:TInteger; const Mode:TSplitMode=TSplitMode.Random); overload;
    procedure Split(const Ratio:Single; const Mode:TSplitMode=TSplitMode.Random); overload;

    property Attributes:TDataArray read GetAttributes write SetAttributes;
    property Predicted:TPredictedData read GetPredicted;
    property Target:TDataItem read GetTarget write SetTarget;
  end;

  // Base class for interfaces to frameworks like R and Python
  TBIPlugin=class
  public
    class function ConvertToNumeric(const AData:TDataItem; out ANew:TDataItem):Boolean; static;

    class function DataToVector(const Indices:TInt64Array; const AData:TDataItem;
                                const UseMissing:Boolean=True;
                                const MissingValue:String=''):String; static;
  end;

implementation
