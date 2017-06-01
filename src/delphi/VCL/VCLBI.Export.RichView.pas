{*********************************************}
{  TeeBI Software Library                     }
{  TRichView Export                           }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Export.RichView;

(*

 TDataRichView record below provides functions to export a TeeBI TDataItem
 to a TRichView Table element.

 TRichView components:

 http://www.TRichView.com


 Basic usage:

  TDataRichView.Add(BIGrid1.Data,RichView1);
  RichView1.Format;

*)

interface

uses
  BI.DataItem, BI.DataSource, RichView, RVTable;

type
  TDataRichView=record
  private
    class procedure AddHeader(const ATable:TRVTableItemInfo; const AData:TDataArray); static;
    class function NewTable(const ARowCount, AColCount:Integer; const ARichView:TRichView): TRVTableItemInfo; static;
  public
    class function Add(const AData:TDataItem; const ARichView:TRichView):TRVTableItemInfo; overload; static;
    class function Add(const AData:TDataArray; const ARichView:TRichView):TRVTableItemInfo; overload; static;
    class function Add(const ACursor:TDataCursor; const ARichView:TRichView):TRVTableItemInfo; overload; static;

    class procedure DefaultStyle(const ATable:TRVTableItemInfo); static;
  end;

implementation
