{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for TMS FlexCel data export         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Excel.TMSFlexCel;

interface

uses
  BI.Data, BI.Data.Excel;

type
  TBITMSFlexCel=class(TBIExcelEngine)
  public
    class procedure SaveToFile(const AData:TDataItem; const AFileName:String); override;
  end;

implementation
