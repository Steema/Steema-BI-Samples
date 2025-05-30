{*********************************************}
{  TeeBI Software Library                     }
{  TClientDataSet data import and export      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.ClientDataset;

interface

uses
  System.Classes, System.SysUtils, Data.DB, DBClient,
  BI.DB.Dataset, BI.DataItem, BI.Summary, BI.Persist, BI.Expression,
  BI.DataSource;

type
  TBIClientDataset=class(TBIDatasetSource)
  private
    class procedure DoFillData(const DataSet: TClientDataSet;
                               const AData: TDataItem;
                               const ADataArray: TDataArray;
                               const Filter:TExpression=nil); static;
  protected
    function DoImportFile(const FileName:String):TDataArray; override;
  public
    class function FileFilter:TFileFilters; override;

    class procedure FillData(const DataSet:TClientDataSet; const AData:TDataItem); overload; static;
    class procedure FillData(const DataSet: TClientDataSet; const AData: TDataArray); overload; static;
    class procedure FillData(const DataSet:TClientDataSet; const AData:TDataItem; const Filter:TExpression=nil); overload; static;

    function Import(const Folder:String; Recursive:Boolean):TDataArray; overload;
    class function Supports(const Extension:String):Boolean; override;
  end;

  TClientDatasetExport=class(TBIExport)
  public
    Format : TDataPacketFormat;

    Constructor Create; override;

    class function FileFilter: TFileFilters; override;
    procedure SaveToFile(const AFileName:String); override;
    class function Supports(const Extension:String):Boolean; override;
  end;

implementation

{$IF COMPILERVERSION>28}
{$DEFINE HASINLINEARRAY}
{$ENDIF}

uses
  System.IOUtils, System.Types,
  BI.Arrays, BI.Expressions, BI.DB;

{ TBIClientDataset }

class procedure TBIClientDataset.FillData(const DataSet: TClientDataSet;
  const AData: TDataArray);
begin
  DoFillData(DataSet,nil,AData);
end;

class procedure TBIClientDataset.FillData(const DataSet: TClientDataSet;
  const AData: TDataItem);
{$IFNDEF HASINLINEARRAY}
var tmp : TDataArray;
{$ENDIF}
begin
  {$IFDEF HASINLINEARRAY}
  DoFillData(DataSet,nil,[AData]);
  {$ELSE}
  SetLength(tmp,1);
  tmp[0]:=AData;
  DoFillData(DataSet,nil,tmp);
  {$ENDIF}
end;

class procedure TBIClientDataset.DoFillData(const DataSet: TClientDataSet;
                           const AData: TDataItem; const ADataArray: TDataArray;
                           const Filter:TExpression=nil);

  function CreateHops:TDataHops;
  begin
    if Filter=nil then
       result:=nil
    else
    begin
      result:=TDataHops.Create;
      result.Main:=AData;
      result.Add(Filter,False);
      result.Init;
    end;
  end;

  procedure AddRow(const APos:Integer);
  var tt : Integer;
    Col : TDataItem;
    Field : TField;
  begin
    for tt:=0 to AData.Items.Count-1 do
    begin
      Col:=AData.Items[tt];

      if Col.Kind<>dkUnknown then
      begin
        Field:=DataSet.Fields[tt];

        if Col.Missing[APos] then
           Field.Clear
        else
        case Col.Kind of
            dkInt32: Field.AsInteger:=Col.Int32Data[APos];
            dkInt64: Field.AsLargeInt:=Col.Int64Data[APos];
           dkSingle: Field.AsSingle:=Col.SingleData[APos];
           dkDouble: Field.AsFloat:=Col.DoubleData[APos];
         dkExtended: Field.AsExtended:=Col.ExtendedData[APos];
             dkText: Field.AsString:=Col.TextData[APos];
         dkDateTime: Field.AsDateTime:=Col.DateTimeData[APos];
          dkBoolean: Field.AsBoolean:=Col.BooleanData[APos];
        else
          Field.Clear;
        end;
      end;
    end;
  end;

  procedure AddInt32(const Item:TDataItem);
  var t : Integer;
  begin
    if Item.Missing.Count=0 then
       for t:=0 to Item.Int32Data.Count-1 do
           DataSet.AppendRecord([Item.Int32Data[t]])
       else
       for t:=0 to Item.Int32Data.Count-1 do
           if Item.Missing[t] then
              DataSet.InsertRecord([nil])
           else
              DataSet.InsertRecord([Item.Int32Data[t]]);
  end;

  procedure AddInt64(const Item:TDataItem);
  var t : Integer;
  begin
    if Item.Missing.Count=0 then
       for t:=0 to Item.Int64Data.Count-1 do
           DataSet.AppendRecord([Item.Int64Data[t]])
       else
       for t:=0 to Item.Int64Data.Count-1 do
           if Item.Missing[t] then
              DataSet.InsertRecord([nil])
           else
              DataSet.InsertRecord([Item.Int64Data[t]]);
  end;

  procedure AddSingle(const Item:TDataItem);
  var t : Integer;
  begin
    if Item.Missing.Count=0 then
       for t:=0 to Item.SingleData.Count-1 do
           DataSet.AppendRecord([Item.SingleData[t]])
    else
       for t:=0 to Item.SingleData.Count-1 do
           if Item.Missing[t] then
              DataSet.InsertRecord([nil])
           else
              DataSet.InsertRecord([Item.SingleData[t]]);
  end;

  procedure AddDouble(const Item:TDataItem);
  var t : Integer;
  begin
    if Item.Missing.Count=0 then
       for t:=0 to Item.DoubleData.Count-1 do
           DataSet.AppendRecord([Item.DoubleData[t]])
    else
       for t:=0 to Item.DoubleData.Count-1 do
           if Item.Missing[t] then
              DataSet.InsertRecord([nil])
           else
              DataSet.InsertRecord([Item.DoubleData[t]]);
  end;

  procedure AddExtended(const Item:TDataItem);
  var t : Integer;
  begin
    if Item.Missing.Count=0 then
       for t:=0 to Item.ExtendedData.Count-1 do
           DataSet.AppendRecord([Item.ExtendedData[t]])
    else
       for t:=0 to Item.ExtendedData.Count-1 do
           if Item.Missing[t] then
              DataSet.InsertRecord([nil])
           else
              DataSet.InsertRecord([Item.ExtendedData[t]]);
  end;

  procedure AddText(const Item:TDataItem);
  var t : Integer;
  begin
    if Item.Missing.Count=0 then
       for t:=0 to Item.TextData.Count-1 do
           DataSet.AppendRecord([Item.TextData[t]])
    else
       for t:=0 to Item.TextData.Count-1 do
           if Item.Missing[t] then
              DataSet.InsertRecord([nil])
           else
              DataSet.InsertRecord([Item.TextData[t]]);
  end;

  procedure AddDateTime(const Item:TDataItem);
  var t : Integer;
  begin
    if Item.Missing.Count=0 then
       for t:=0 to Item.DateTimeData.Count-1 do
           DataSet.AppendRecord([Item.DateTimeData[t]])
    else
       for t:=0 to Item.DateTimeData.Count-1 do
           if Item.Missing[t] then
              DataSet.InsertRecord([nil])
           else
              DataSet.InsertRecord([Item.DateTimeData[t]]);
  end;

  procedure AddBoolean(const Item:TDataItem);
  var t : Integer;
  begin
    if Item.Missing.Count=0 then
       for t:=0 to Item.BooleanData.Count-1 do
           DataSet.AppendRecord([Item.BooleanData[t]])
    else
       for t:=0 to Item.BooleanData.Count-1 do
           if Item.Missing[t] then
              DataSet.InsertRecord([nil])
           else
              DataSet.InsertRecord([Item.BooleanData[t]]);
  end;

var t : Integer;
    Item : TDataItem;
    Hops : TDataHops;
begin
  if DataSet.Active then
     DataSet.EmptyDataSet;

  DataSet.Close;
  DataSet.FieldDefs.Clear;
  DataSet.IndexName:='';
  DataSet.IndexDefs.Clear;

  Item:=nil;

  if ADataArray=nil then
     Add(DataSet.FieldDefs,AData.Items.AsArray)
  else
  begin
    Add(DataSet.FieldDefs,ADataArray);

    // Temporary, use first data item only:
    Item:=ADataArray[0];
  end;

  if DataSet.FieldDefs.Count>0 then
  begin
    DataSet.CreateDataSet;
    DataSet.Open;

    DataSet.DisableControls;
    try
      if ADataArray=nil then
      begin
        AData.Load;

        Hops:=CreateHops;

        for t:=0 to AData.Count-1 do
        if (Filter=nil) or Filter.Value then
        begin
          DataSet.Append;
          AddRow(t);
          DataSet.Post;
        end;

        Hops.Free;
      end
      else
      if Item<>nil then
      begin
        Item.Load;

        case Item.Kind  of
         dkInt32: AddInt32(Item);
         dkInt64: AddInt64(Item);
        dkSingle: AddSingle(Item);
        dkDouble: AddDouble(Item);
      dkExtended: AddExtended(Item);
          dkText: AddText(Item);
      dkDateTime: AddDateTime(Item);
       dkBoolean: AddBoolean(Item);
        end;
      end;

      DataSet.First;
    finally
      DataSet.EnableControls;
    end;
  end;
end;

class procedure TBIClientDataset.FillData(const DataSet: TClientDataSet;
  const AData: TDataItem; const Filter:TExpression=nil);
begin
  DoFillData(DataSet,AData,nil,Filter);
end;

function TBIClientDataset.Import(const Folder: String; Recursive:Boolean): TDataArray;
begin
  result:=Import(Folder,'*.cds',ExcludePattern,Recursive);
end;

function TBIClientDataset.DoImportFile(const FileName:String): TDataArray;
var tmpClient : TClientDataSet;
begin
  tmpClient:=TClientDataSet.Create(nil);
  try
    tmpClient.ReadOnly:=True;
    tmpClient.LoadFromFile(FileName);
    tmpClient.Open;

    result.Add(Import(tmpClient));
    result[0].Name:=NameOfFile(FileName);

  finally
    tmpClient.Free;
  end;
end;

class function TBIClientDataset.FileFilter:TFileFilters;
begin
  result:=nil;
  result.Add('ClientDataset files','*.cds');
end;

class function TBIClientDataset.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.CDS');
end;

{ TClientDatasetExport }

Constructor TClientDatasetExport.Create;
begin
  inherited;
  BinaryOnly:=True;
end;

class function TClientDatasetExport.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('ClientDataset','*.cds');
end;

procedure TClientDatasetExport.SaveToFile(const AFileName: String);
var tmp : TClientDataSet;
begin
  tmp:=TClientDataSet.Create(nil);
  try
    TBIDataSetSource.Add(tmp.FieldDefs,Data.Items.AsArray);

    tmp.CreateDataSet;

    TBIDBExport.Add(tmp,Data);

    tmp.SaveToFile(AFileName,Format);
  finally
    tmp.Free;
  end;
end;

class function TClientDatasetExport.Supports(const Extension: String): Boolean;
begin
  result:=TBIClientDataset.Supports(Extension);
end;

initialization
  TBIFileImporters.RegisterClass(TBIClientDataSet);
  TBIExporters.RegisterClass(TClientDatasetExport);
finalization
  TBIExporters.UnRegisterClass(TClientDatasetExport);
  TBIFileImporters.UnRegisterClass(TBIClientDataSet);
end.
