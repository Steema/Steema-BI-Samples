{*********************************************}
{  TeeBI Software Library                     }
{  All algorithms                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Register;

interface

{$IFNDEF IOS}
{$IFNDEF ANDROID}
{$DEFINE USE_PYTHON}
{$ENDIF}
{$ENDIF}

uses
  System.Classes;

type
  TAllModels=record
  public
    class procedure AllModels(const ADest: TStrings); static;
  end;

implementation
