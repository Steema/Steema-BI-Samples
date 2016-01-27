{*********************************************}
{  TeeBI Software Library                     }
{  Ensemble algorithms                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Ensemble;

interface

uses
  BI.Plugins.R;

type
  TBIRandomForest=class(TRSupervisedModel)
  protected
    procedure BuildScript; override;
  end;

implementation
