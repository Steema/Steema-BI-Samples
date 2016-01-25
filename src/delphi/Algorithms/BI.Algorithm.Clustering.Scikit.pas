{*********************************************}
{  TeeBI Software Library                     }
{  Scikit-learn Clustering algorithm          }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Clustering.Scikit;

interface

uses
  System.Classes, BI.Plugins.Python, BI.Data;

type
  TBIScikitClustering=class(TScikitSupervisedModel)
  protected
    function BuildScript:TStrings; override;
  public
    procedure Calculate; override;
  end;

implementation
