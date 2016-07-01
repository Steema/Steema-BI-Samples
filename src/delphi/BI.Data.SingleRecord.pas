{*********************************************}
{  TeeBI Software Library                     }
{  Single Record from a TDataItem row         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.SingleRecord;

interface

uses
  System.Classes, BI.Arrays, BI.Data, BI.Persist;

type
  // Returns a TDataItem that is a record-view of another AData item,
  // for the specified ARow record
  TSingleRecord=class(TDataProvider)
  private
    FRow : TInteger;
    FData: TDataItem;

    procedure SetRow(const Value:TInteger);
    procedure SetData(const Value: TDataItem);
  protected
    procedure Changed; override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Notify(const AEvent:TBIEvent);
  public
    Constructor Create(AOwner:TComponent); override;

    class function From(const AData:TDataItem; const ARow:TInteger):TDataItem; static;
  published
    property Data:TDataItem read FData write SetData;
    property Row:TInteger read FRow write SetRow;
  end;

implementation
