{*********************************************}
{  TeeBI Software Library                     }
{  Bayesian algorithms                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Bayes;

interface

uses
  System.Classes, BI.Plugins.R;

type
  TBINaiveBayes=class(TRSupervisedModel)
  protected
    procedure BuildScript; override;
  public
    procedure Calculate; override;

    // property Laplace
  end;

implementation
