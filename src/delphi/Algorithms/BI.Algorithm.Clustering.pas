{*********************************************}
{  TeeBI Software Library                     }
{  Clustering algorithm                       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Clustering;

interface

uses
  BI.Data, BI.Arrays, BI.Algorithm.Model, BI.Algorithm.Clustering.Engine;

type
  TDistanceMethod=(Euclidean, SquaredEuclidean, Manhattan, Minkowski, Sorensen, Chebyshev);
  TClusteringMethod=(KMeans, Hierarchical, QT);

  TCalcDistanceFunc=function(const A,B:TClusterData):TClusterValue;

  TBIProvider=class(TClusterProvider)
  private
    FDistance : TDistanceMethod;
    FLambda   : Single;
    FLinkage  : TLinkageMethod;
    FMethod   : TClusteringMethod;
    FNumClusters : Integer;

    ILambda : Single;
    InternalDistance : TCalcDistanceFunc;

    ICluster : TBaseClustering;
    IData : TDataItem;

    Source  : Array of TClusterData;

    IDataLength : Integer;

    IModel : TSupervisedModel;

    function Execute:TCluster;
    procedure FillData;
    procedure Prepare;
    procedure SetDistance(const Value: TDistanceMethod);
    procedure SetLambda(const Value: Single);
    procedure SetLinkage(const Value: TLinkageMethod);
    procedure SetMethod(const Value: TClusteringMethod);
  protected
    function CalcDistanceData(const A,B:TClusterData):TClusterValue; overload;
    function CalcDistanceData(const A,B:Integer):TClusterValue; overload; override;
    function CalcDistanceData(const A,B:TCluster):TClusterValue; overload; override;
    function CalcDistanceData(const A:Integer; const B:TCluster):TClusterValue; overload; override;
    function CloneData(const AData:TClusterData):TClusterData; override;
    function GetData(const Index:Integer):TClusterData; override;
    procedure RecalcValues(const ACluster:TCluster); override;
    function SourceCount:Integer; override;
  public
    Destructor Destroy; override;

    property Distance:TDistanceMethod read FDistance write SetDistance default Euclidean;
    property Linkage:TLinkageMethod read FLinkage write SetLinkage default lmNone;
    property Method: TClusteringMethod read FMethod write SetMethod default KMeans;
    property MinkowskiLambda:Single read FLambda write SetLambda;
    property NumClusters:Integer read FNumClusters write FNumClusters;
  end;

  TBIClustering=class(TSupervisedModel)
  public
    Options : TBIProvider;
    Output : TCluster;

    Constructor Create(const AData:TDataItem); override;
    Destructor Destroy; override;

    procedure Calculate; override;

    function Evaluation:Double;
    function Summary:TDataItem;
  end;

implementation
