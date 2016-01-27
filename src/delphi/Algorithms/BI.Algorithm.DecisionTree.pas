{*********************************************}
{  TeeBI Software Library                     }
{  DecisionTree algorithm                     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.DecisionTree;

// https://en.wikipedia.org/wiki/ID3_algorithm

interface

uses
  BI.Data, BI.Arrays, BI.Tree, BI.Algorithm.Model;

type
  TNodeData=record
  public
    Attribute : TDataItem;
    Index : TInteger;

    function ToString:String;
  end;

  TDecisionNode=TNode<TNodeData>;

  TDecisionTree=class;

  TGain=record
    FEntropy : TFloat;
    Tree : TDecisionTree;
  public
    Indices : TBooleanArray;
    Count : TInteger;

    function Calculate(const AData:TDataItem):TFloat;
    function Entropy(const Count:TInteger):TFloat;
  end;

  TColumnData=record
  public
    Continous : Boolean;
    Flag : TBooleanArray;
  end;

  TDecisionTree=class(TSupervisedModel)
  private
    IGain : TGain;
    IMap : TDataMap;

    procedure AddBranches(const Node:TDecisionNode; const Attributes:TDataArray;
                          const DataIndices:TBooleanArray);
    procedure CalcMap(const DataIndices:TBooleanArray);
    procedure CheckTextIndices(const AData:TDataItem);
    procedure FilterIndices(const AData:TDataItem; const Index:TInteger; var Indices:TBooleanArray);
    function GuessBest(const Attributes:TDataArray):TDataItem;
    function IsContinous(const AData:TDataItem):Boolean;
  public
    Root : TDecisionNode;
    ColumnData : Array of TColumnData;
    MaxData : TInteger; // 0=all data

    Constructor Create; override;
    Destructor Destroy; override;

    procedure Calculate; override;
  end;

implementation
