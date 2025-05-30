{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for TMS FlexCel data export         }
{  Copyright (c) 2015-2025 by Steema Software }
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

uses
  BI.Arrays, FlexCel.Core, FlexCel.XlsAdapter;

class procedure TBITMSFlexCel.SaveToFile(const AData:TDataItem; const AFileName:String);
var x : TXlsFile;

  procedure AddNewSheet(const AName:String);
  begin
    x.AddSheet;

    if AName<>'' then
       x.SheetName:=AName;
  end;

  function MaxHeaderRows(const AData:TDataItem):Integer;

    function MaxRows(const AData:TDataItem):Integer;
    begin
      if AData.AsTable then
         result:=1+MaxHeaderRows(AData)
      else
         result:=1;
    end;

  var t,
      tmp : Integer;
  begin
    result:=0;

    for t:=0 to AData.Items.Count-1 do
    begin
      tmp:=MaxRows(AData.Items[t]);

      if tmp>result then
         result:=tmp;
    end;
  end;

  procedure SetCell(const ARow,ACol:Integer; const AData:TDataItem; const AIndex:TLoopInteger);
  var tmpDouble : Double;
  begin
    case AData.Kind of
        dkInt32: x.SetCellValue(ARow,ACol,AData.Int32Data[AIndex]);
        dkInt64: x.SetCellValue(ARow,ACol,AData.Int64Data[AIndex]);

       dkSingle: begin
                   tmpDouble:=AData.SingleData[AIndex];
                   x.SetCellValue(ARow,ACol,tmpDouble);
                 end;

       dkDouble: x.SetCellValue(ARow,ACol,AData.DoubleData[AIndex]);
     dkExtended: x.SetCellValue(ARow,ACol,AData.ExtendedData[AIndex]);
         dkText: x.SetCellValue(ARow,ACol,AData.TextData[AIndex]);
     dkDateTime: x.SetCellValue(ARow,ACol,AData.DateTimeData[AIndex]);
      dkBoolean: x.SetCellValue(ARow,ACol,AData.BooleanData[AIndex]);
    end;
  end;

  procedure AddTable(const AData:TDataItem);
  var
    tmpOffset : Integer;

    procedure DoAddTable(const Row,Column:Integer; const AData:TDataItem);
    var t : Integer;
        tmpCol : Integer;
        tt : TLoopInteger;
        tmp : TDataItem;
    begin
      for t:=1 to AData.Items.Count do
      begin
        tmp:=AData.Items[t-1];

        if tmp.AsTable or (Column<=1) then
           tmpCol:=Column+t-1
        else
           tmpCol:=Column+t-2;

        x.SetCellValue(Row,tmpCol,tmp.Name);

        if tmp.AsTable then
           DoAddTable(1+Row,1+Column,tmp)
        else
        begin
          for tt:=0 to tmp.Count-1 do
            if not tmp.Missing[tt] then
               SetCell(tmpOffset+tt,tmpCol,tmp,tt);
        end;
      end;
    end;

  begin
    AddNewSheet(AData.Name);

    tmpOffset:=1+MaxHeaderRows(AData);

    DoAddTable(1,1,AData);
  end;

  procedure AddColumn(const AData:TDataItem);
  var t : Integer;
  begin
    AddNewSheet(AData.Name);

    for t:=0 to AData.Count-1 do
        SetCell(1+t,1,AData,t);
  end;

  procedure AddData(const AData:TDataItem);
  var tmp : TDataItem;
  begin
    if AData.AsTable then
       AddTable(AData)
    else
    if AData.Kind=TDataKind.dkUnknown then
    begin
      for tmp in AData.Items.AsArray do
          AddData(tmp);
    end
    else
      AddColumn(AData);
  end;

begin
  x:=TXlsFile.Create;
  try
    x.NewFile;
    AddData(AData);

    x.AddSheet;

    //x.AllowOverwritingFiles:=True;

    x.Save(AFileName,TFileFormats.Automatic);
  finally
    x.Free;
  end;
end;

end.
