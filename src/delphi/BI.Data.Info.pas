{*********************************************}
{  TeeBI Software Library                     }
{  Structure Information                      }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Info;

interface

uses
  BI.Data;

type
  // TDataInfo returns the structure of a TDataItem (properties of all sub-items)
  // in the Data property.

  // GetMinMax returns the minimum and maximum value of AData, using the Stats
  // property. If Stats are already calculated, they are reused.

  TDataInfo=class(TDataItem)
  private
    FData : TDataItem;

    procedure Fill;
    procedure SetData(const Value: TDataItem);
  public
    Constructor Create(const AData:TDataItem); overload;

    class procedure GetMinMax(const AData:TDataItem; out AMin,AMax:Extended); static;

    property Data:TDataItem read FData write SetData;
  end;

  // ItemsOf return a more complex view of the structure of AData, together with
  // all possible statistics of AData sub-items

  TDataItemsInfo=record
  private
    class procedure AddItems(const AItems:TDataItems); static;
    class procedure FillItemsOf(const AData,ADest:TDataItem); static;
  public
    class function ItemsOf(const AData:TDataItem):TDataItem; overload; static;
  end;

implementation
