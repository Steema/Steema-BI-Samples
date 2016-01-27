{*********************************************}
{  TeeBI Software Library                     }
{  NearestNeighbour algorithm                 }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Classify;

interface

uses
  System.Classes, BI.Data, BI.Arrays, BI.Algorithm.Model, BI.Plugins.R;

type
  TBINearestNeighbour=class(TRSupervisedModel)
  private
    FK : Integer;
  protected
    procedure BuildScript; override;
  public
    Constructor Create; override;
    procedure Calculate; override;

    property K:Integer read FK write FK default 1;
  end;

  TBICrossTable=class(TRBaseAlgorithm)
  private
    FChiSquare : Boolean;
    function GetPredicted: TDataItem;
    function GetReal: TDataItem;
    procedure SetPredicted(const Value: TDataItem);
    procedure SetReal(const Value: TDataItem);
  protected
    procedure BuildScript; override;
  public
    Output : TDataItem;

    Constructor Create; override;
    Destructor Destroy; override;

    property ChiSquare:Boolean read FChiSquare write FChiSquare;
    property Predicted:TDataItem read GetPredicted write SetPredicted;
    property Real:TDataItem read GetReal write SetReal;
  end;

implementation
