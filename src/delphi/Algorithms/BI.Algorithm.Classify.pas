{*********************************************}
{  TeeBI Software Library                     }
{  NearestNeighbour algorithm                 }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Classify;

interface

uses
  System.Classes, BI.DataItem, BI.Arrays, BI.Algorithm, BI.Algorithm.Model,
  BI.Plugins.R;

type
  TBINearestNeighbour=class(TRSupervisedModel)
  private
    FK : Integer;

    procedure SetK(const Value: Integer);
  protected
    procedure BuildScript; override;
  public
    Constructor Create(AOwner:TComponent); override;
  published
    property K:Integer read FK write SetK default 1;
  end;

  TBICrossTable=class(TRBaseAlgorithm)
  private
    FChiSquare : Boolean;

    function GetPredicted: TDataItem;
    function GetReal: TDataItem;
    procedure SetPredicted(const Value: TDataItem);
    procedure SetReal(const Value: TDataItem);
    procedure SetChiSquare(const Value: Boolean);
  protected
    procedure AddOutput(const AData:TDataItem); override;
    procedure BuildScript; override;
  public
    Constructor Create(AOwner:TComponent); override;

    property Predicted:TDataItem read GetPredicted write SetPredicted;
    property Real:TDataItem read GetReal write SetReal;
  published
    property ChiSquare:Boolean read FChiSquare write SetChiSquare;
  end;

implementation
