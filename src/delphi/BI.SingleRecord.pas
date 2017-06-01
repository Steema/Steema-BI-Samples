{*********************************************}
{  TeeBI Software Library                     }
{  Single Record from a TDataItem row         }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.SingleRecord;

interface

uses
  System.Classes, BI.Arrays, BI.DataItem, BI.Persist, BI.Algorithm;

type
  // Returns a TDataItem that is a record-view of another AData item,
  // for the specified ARow record
  TSingleRecord=class(TSingleSourceProvider)
  private
    FRow : TInteger;

    procedure SetRow(const Value:TInteger);
  protected
    procedure ClearSource; override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure SetSource(const Value: TDataItem); override;
  public
    Constructor Create(AOwner:TComponent); override;

    class function From(const AData:TDataItem; const ARow:TInteger):TDataItem; static;
  published
    property Row:TInteger read FRow write SetRow;
  end;

implementation
