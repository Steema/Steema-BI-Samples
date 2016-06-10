{*********************************************}
{  TeeBI Software Library                     }
{  Summary Consolidations (Grand Totals)      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Summary.Totals;

interface

uses
  System.Classes, BI.Data, BI.Summary;

type
  // Creates one TDataItem for each active Group in ASummary with the subtotals
  // of all measures for that group, and finally the Grand Totals for all measures.

  // Usage example:
  // BIGrid1.Data := TDataItem.Create(TSummaryTotals.Create(MySummary));

  TSummaryTotals=class(TDataProvider)
  private
    FSummary : TSummary;

    procedure CheckSummary;
    procedure DestroySummaries;
    procedure SetSummary(const Value: TSummary);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Constructor CreateSummary(const AOwner:TComponent; const ASummary:TSummary);
    Destructor Destroy; override;

    procedure Calculate(const AData:TDataItem);
  published
    property Summary:TSummary read FSummary write SetSummary;
  end;

implementation
