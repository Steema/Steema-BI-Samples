{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for TMS FlexCel data export         }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Excel.TMSFlexCel;

interface

uses
  BI.DataItem, BI.Excel;

type
  TBITMSFlexCel=class(TBIExcelEngine)
  public
    class procedure SaveToFile(const AData:TDataItem; const AFileName:String); override;
  end;

implementation
