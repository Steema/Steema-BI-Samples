{*********************************************}
{  TeeBI Software Library                     }
{  Base Model algorithm classes               }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Model;

interface

uses
  System.Classes, BI.Arrays, BI.DataItem, BI.Algorithm, BI.Tools;

type
  TModelClass=class of TModel;

  TModels=Array of TModelClass;

  TModelsHelper=record helper for TModels
  public
    function Count:Integer; inline;
    function Exists(const AModel:TModelClass):Boolean; inline;
    function IndexOf(const AModel:TModelClass):Integer;

    procedure Register(const AModel:TModelClass); overload;
    procedure Register(const AModels:Array of TModelClass); overload;

    procedure UnRegister(const AModel:TModelClass); overload;
    procedure UnRegister(const AModels:Array of TModelClass); overload;
  end;

  // Base class for machine-learning algorithms that require a "Target"
  // (or "Class" or "Label") data
  TModel=class(TBaseAlgorithm)
  public
    class var
       Models : TModels;

    Constructor Create(AOwner:TComponent); override;

    class function Description:String;
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

    procedure AddItems(const AMap:TTextMap);
    procedure CheckNeeds(const A,B:TDataItem);
    procedure Fill(const AMap:TTextMap; const A,B:TDataItem);
    function GetConfusion:TDataItem;
    function GetCorrect:TInteger;
  public
    class var
       ConfusionClass : TBaseAlgorithmClass;

    Constructor Create(const ATarget:TDataItem; const Indices:TNativeIntArray);
    Destructor Destroy; override;

    function IsEqual(const AIndex: TInteger):Boolean;

    property Confusion:TDataItem read GetConfusion;
    property Correct:TInteger read GetCorrect;
    property Predicted:TDataItem read FPredicted;
    property Real:TDataItem read FReal;
  end;

  TModelSplit=class(TPersistent)
  private
    IProvider : TDataProvider;

    procedure Changed;
    function GetBy: TSplitBy;
    function GetCount: Integer;
    function GetMode: TSplitMode;
    function GetPercent: Single;
    function IsPercentStored: Boolean;
    procedure SetBy(const Value: TSplitBy);
    procedure SetCount(const Value: Integer);
    procedure SetMode(const Value: TSplitMode);
    procedure SetPercent(const Value: Single);
  public
    Split : TSplitOptions;

    Constructor Create(const AProvider:TDataProvider);

    procedure Assign(Source:TPersistent); override;
  published
    property By:TSplitBy read GetBy write SetBy default TSplitBy.Percent;
    property Count: Integer read GetCount write SetCount default 0;
    property Mode:TSplitMode read GetMode write SetMode default TSplitMode.Random;
    property Percent:Single read GetPercent write SetPercent stored IsPercentStored;
  end;

  // Model class supporting "train" and "test" of an algorithm
  TSupervisedModel=class(TModel)
  private
    FNormalize : Boolean;
    FSplit : TModelSplit;

    function GetAttributes: TDataArray;
    function GetTarget: TDataItem;
    procedure SetAttributes(const Value: TDataArray);
    procedure SetNormalize(const Value: Boolean);
    procedure SetSplit(const Value:TModelSplit);
    procedure SetTarget(const Value: TDataItem);
  protected
    FPredicted : TPredictedData;

    procedure AddOutput(const AData:TDataItem); override;
    function GetPredicted:TPredictedData;
  public
    Indices : TDataSplit;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    // procedure Assign

    procedure Calculate; override;

    property Predicted:TPredictedData read GetPredicted;
  published
    property Attributes:TDataArray read GetAttributes write SetAttributes;
    property Normalize:Boolean read FNormalize write SetNormalize default False;
    property Split:TModelSplit read FSplit write SetSplit;
    property Target:TDataItem read GetTarget write SetTarget;
  end;

  TSupervisedModelClass=class of TSupervisedModel;

  // Base class for interfaces to frameworks like R and Python
  TBIPlugin=class
  public
    class function ConvertToNumeric(const AData:TDataItem; out ANew:TDataItem):Boolean; static;

    class function DataToVector(const Indices:TNativeIntArray; const AData:TDataItem;
                                const UseMissing:Boolean=True;
                                const MissingValue:String=''):String; static;
  end;

implementation
