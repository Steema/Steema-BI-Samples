{*************************************}
{ Clustering algorithms               }
{                                     }
{ Copyright Steema Software 2012-2015 }
{ www.steema.com                      }
{*************************************}
unit BI.Algorithm.Clustering.Engine;

interface

{$IFDEF NEXTGEN}
// Not possible to use our TFastList with Android / iOS
{$DEFINE TEEGENERICS}
{$ELSE}
{$DEFINE TEEFASTLIST}
{$ENDIF}

{$DEFINE D8}
{$DEFINE D9}
{$DEFINE D11}
{$DEFINE D16}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF TEEGENERICS}
  System.Generics.Collections,
  {$ENDIF}
  Classes;

type
  {$IFNDEF TEEGENERICS}

  {$IFDEF D16}
  PPointerList=^TPointerList;
  TPointerList = Array[0..(Maxint div 16) - 1] of Pointer;
  {$ENDIF}

  {$IFNDEF TEEFASTLIST}
  TFastList=TList;
  {$ELSE}
  TFastList=class
  private
    FCapacity : Integer;
    FCount : Integer;
    List   : PPointerList;

    function Get(Index:Integer):Pointer; {$IFDEF D9}inline;{$ENDIF}
    procedure Grow;
    procedure SetCapacity(const NewCapacity: Integer);
  public
    Destructor Destroy; override;

    procedure Add(const Item:Pointer);
    procedure Clear; virtual;
    procedure Delete(const Index:Integer);

    property Count:Integer read FCount;
    property Items[Index:Integer]:Pointer read Get; default;
  end;
  {$ENDIF}
  {$ENDIF}

  TClusterValue=Single;
  TClusterData=Array of TClusterValue;

  TCluster=class;

  TCluster=class({$IFDEF TEEGENERICS}TList<TCluster>{$ELSE}TFastList{$ENDIF})
  private
    Flag : Boolean;

    function Get(Index:Integer):TCluster; {$IFDEF D9}inline;{$ENDIF}
  public
    Data  : TClusterData;
    Index : Integer;

    function AddItem(const AIndex:Integer; const AData:TClusterData):TCluster;
    procedure Clear; {$IFNDEF TEEGENERICS}override;{$ENDIF}

    property Item[Index:Integer]:TCluster read Get; default;
  end;

  TBaseClustering=class;

  // Abstract Cluster Provider

  TClusterProvider=class {$IFDEF D11}abstract{$ENDIF}
  protected
    procedure BeginClustering; virtual;
    function CalcDistanceData(const A,B:Integer):Single; overload; virtual; abstract;
    function CalcDistanceData(const A,B:TCluster):Single; overload; virtual; abstract;
    function CalcDistanceData(const A:Integer; const B:TCluster):Single; overload; virtual; abstract;
    function CloneData(const AData:TClusterData):TClusterData; virtual; abstract;
    procedure EndClustering; virtual;
    function GetData(const Index:Integer):TClusterData; virtual; abstract;
    procedure RecalcValues(const ACluster:TCluster); virtual; abstract;
    function SourceCount:Integer; virtual; abstract;
  end;

  TClusterEvaluation=record
    DaviesBouldin,
    Dunn : Double;
  end;

  TLinkageMethod=(lmNone, lmSingle, lmComplete, lmAverage, lmWard);

  // Abstract Cluster

  TBaseClustering=class(TPersistent)
  private
    FLinkage    : TLinkageMethod;
    FProvider   : TClusterProvider;

    SourceCount : Integer;
    Matrix      : Array of TClusterData;

    procedure BuildMatrix;
    procedure CalcMatrix(Index:Integer);
  protected
    function CalcDistance(const A,B:TCluster):Single;
  public
    Clusters : TCluster;

    Constructor Create(const AProvider:TClusterProvider); virtual;
    Destructor Destroy; override;

    function Scatter(const ACluster:TCluster):Double; overload;
    function Scatter(const A,B:TCluster):Double; overload;

    function Execute:TCluster; virtual;
    class function IsUndefined(const ACluster:TCluster):Boolean;
    function Evaluation(const ACluster:TCluster):TClusterEvaluation;
  published
    property Linkage:TLinkageMethod read FLinkage write FLinkage default lmNone;
    property Provider:TClusterProvider read FProvider;
  end;

{$DEFINE MATRICES}

{$IFDEF MATRICES}
  TFloatList=class
  private
  type
    PSingleList=^TSingleList;
    TSingleList = array[0..(MaxInt div 16) - 1] of Single;

  var
    FCapacity : Integer;
    FCount : Integer;
    List   : PSingleList;

    function Get(Index:Integer):Single; {$IFDEF D9}inline;{$ENDIF}
    procedure Grow;
    procedure SetCapacity(const NewCapacity: Integer);
  public
    Destructor Destroy; override;

    procedure Add(const Item:Single);
    procedure Clear; virtual;
    procedure Delete(const Index:Integer);

    property Count:Integer read FCount;
    property Items[Index:Integer]:Single read Get; default;
  end;

  PMatrixItem=^TMatrixItem;
  TMatrixItem=record
    Value : Single;
    Next  : PMatrixItem;
  end;

  TMatrix=class(TFloatList)
  private
    Min : Single;
    MinIndex : Integer;
    NeedsRecalcMin : Boolean;

    procedure RecalcMin;
  public
    procedure Clear; override;
  end;

  TMatrixList=class({$IFDEF TEEGENERICS}TList<TMatrix>{$ELSE}TFastList{$ENDIF})
  private
  public
    procedure Clear; {$IFNDEF TEEGENERICS}override;{$ENDIF}
  end;
{$ENDIF}

  THierarchicalClustering=class(TBaseClustering)
  private
    FCount : Integer;

    {$IFDEF MATRICES}
    Matrices : TMatrixList;
    {$ENDIF}

    procedure BuildMatrix(Index:Integer);
  public
    Constructor Create(const AProvider:TClusterProvider); override;

    function Execute:TCluster; override;
  published
    property NumClusters:Integer read FCount write FCount default 5;
  end;

  TKMeansClustering=class(TBaseClustering)
  private
    FKCount  : Integer;
    FMaxIter : Integer;
  public
    Constructor Create(const AProvider:TClusterProvider); override;

    function Execute:TCluster; override;
  published
    property NumClusters:Integer read FKCount write FKCount default 5;
    property MaxIterations:Integer read FMaxIter write FMaxIter default 1000;
  end;

  TDistance=record
    Index : Integer;
    Value : Single;
  end;

  TDistances=Array of TDistance;

  TQTClustering=class(TBaseClustering)
  private
    FMinCount    : Integer;
    FMaxDiameter : Double;

    Distances    : Array of TDistances;
    procedure SortedDists(n:Integer);
  public
    Constructor Create(const AProvider:TClusterProvider); override;

    function Execute:TCluster; override;
  published
    property MinCount:Integer read FMinCount write FMinCount default 1;
    property MaxDiameter:Double read FMaxDiameter write FMaxDiameter;
  end;

implementation
