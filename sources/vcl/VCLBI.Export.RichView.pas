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

uses
  BI.Arrays;

{ TDataRichView }

class procedure TDataRichView.AddHeader(const ATable:TRVTableItemInfo; const AData:TDataArray);
var col : Integer;
begin
  for col:=0 to AData.Count-1 do
      ATable.Cells[0,col].AddNL(AData[col].Name,1,0);
end;

class function TDataRichView.NewTable(const ARowCount, AColCount:Integer; const ARichView:TRichView): TRVTableItemInfo;
var row,
    col : Integer;
begin
  result:=TRVTableItemInfo.CreateEx(ARowCount,AColCount,ARichView.RVData);

  for row:=0 to ARowCount-1 do
      for col:=0 to AColCount-1 do
          result.Cells[row,col].Clear;
end;

class function TDataRichView.Add(const AData: TDataItem; const ARichView:TRichView): TRVTableItemInfo;

  function AddItems:TRVTableItemInfo;
  var row : Integer;
  begin
    result:=NewTable(AData.Items.Count+1,1,ARichView);

    result.Cells[0,0].AddNL(AData.Name,1,0);

    for row:=0 to AData.Items.Count-1 do
        result.Cells[row+1,0].AddNL(AData[row].Name,0,0);

    ARichView.AddItem('',result);
  end;

begin
  if AData.AsTable then
     result:=Add(AData.Items.AsArray,ARichView)
  else
  if AData.Kind=TDataKind.dkUnknown then
     result:=AddItems
  else
     result:=Add([AData],ARichView);
end;

class function TDataRichView.Add(const AData: TDataArray;
  const ARichView: TRichView): TRVTableItemInfo;

  function MaxRowCount:Integer;
  var t : Integer;
  begin
    result:=0;

    for t:=0 to AData.Count-1 do
        if AData[t].Count>result then
           result:=AData[t].Count;
  end;

var tmp,
    row,
    col : Integer;
begin
  tmp:=MaxRowCount;

  result:=NewTable(tmp+1,AData.Count,ARichView);

  AddHeader(result,AData);

  for row:=0 to tmp-1 do
      for col:=0 to AData.Count-1 do
          if AData[col].Count>row then
             result.Cells[row+1,col].AddNL(AData[col].DataToString(row),0,0);

  ARichView.AddItem('',result);
end;

class function TDataRichView.Add(const ACursor: TDataCursor;
  const ARichView: TRichView): TRVTableItemInfo;
var tmp : TDataArray;
    tmpRow,
    row,
    col : Integer;
begin
  tmp:=ACursor.DataItems;

  if ACursor.Index=nil then
     result:=Add(tmp,ARichView)
  else
  begin
    result:=NewTable(ACursor.Index.Count+1,tmp.Count,ARichView);

    AddHeader(result,tmp);

    for row:=0 to ACursor.Index.Count-1 do
    begin
      tmpRow:=ACursor.Index[row];

      for col:=0 to tmp.Count-1 do
          if tmp[col].Count>tmpRow then
             result.Cells[row+1,col].AddNL(tmp[col].DataToString(tmpRow),0,0);
    end;

    ARichView.AddItem('',result);
  end;
end;

class procedure TDataRichView.DefaultStyle(const ATable: TRVTableItemInfo);
const
  BorderColor=$E0E0E0;

begin
  ATable.BorderWidth:=0;
  ATable.CellBorderWidth:=1;

  ATable.CellBorderStyle:=TRVTableBorderStyle.rvtbColor;
  ATable.CellBorderColor:=BorderColor;
  ATable.BorderLightColor:=BorderColor;

  ATable.CellHSpacing:=0;
  ATable.CellVSpacing:=-1;
end;

end.
