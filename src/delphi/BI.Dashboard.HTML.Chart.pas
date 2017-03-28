{*********************************************}
{  TeeBI Software Library                     }
{  HTML Charts Web Dashboard                  }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Dashboard.HTML.Chart;

interface

uses
  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Engine,
  {$ELSE}
  VCLTee.TeeConst, VCLTee.TeEngine,
  {$ENDIF}

  System.Classes;

{$IF (TeeVCLBuildVersionInteger >= 160309)}
{$DEFINE HASJSCRIPTGETSERIES} // New event at TJavascriptExportFormat
{$ENDIF}

implementation
