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
  protected
    procedure BuildScript; override;
  public
    K : Integer;

    procedure Calculate; override;
  end;

  TBICrossTable=class(TRBaseAlgorithm)
  private
    FChiSquare : Boolean;
    FPredicted : TPredictedData;
  protected
    procedure BuildRScript; override;
  public
    Constructor Create(const AData:TDataItem); override;

    property ChiSquare:Boolean read FChiSquare write FChiSquare;
  end;

implementation
