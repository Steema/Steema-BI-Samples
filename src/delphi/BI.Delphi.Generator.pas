{*********************************************}
{  TeeBI Software Library                     }
{  ORM "Object Relational Mapping"            }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Delphi.Generator;

interface

uses
  System.Classes, BI.DataItem;

type
  TBIDelphiGenerator=class
  public
    class function DelphiName(const AData:TDataItem; const ACount:Integer=0):String;
    class function FromData(const AData:TDataItem):TStrings; static;
  end;

implementation
