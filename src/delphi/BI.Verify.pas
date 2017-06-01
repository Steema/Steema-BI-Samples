{*********************************************}
{  TeeBI Software Library                     }
{  Data Verification                          }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Verify;

interface

uses
  System.Classes, BI.DataItem;

type
  TVerificator=class
  public
    Strings: TStrings;

    Constructor Create(const AStrings:TStrings);

    procedure Verify(const AData:TDataItem); overload;

    // Pending: Multi-colum master-detail
    //procedure Verify(const AData:TDataArray); overload;
  end;

implementation
