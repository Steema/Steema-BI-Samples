{*********************************************}
{  TeeBI Software Library                     }
{  TDataSet data import                       }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.Dataset;

interface

uses
  {System.}Classes, {System.}SysUtils, {Data.}DB,
  BI.Arrays, BI.DataItem, BI.DataSource;

// Import any TDataset, TCustomConnection or one or more TField objects
// into a TDataItem

type
  TBIDataSetSource=class(TBIFileSource)
  private
    procedure AddField(const AData:TDataItem; const AField:TField);
    procedure AddFields(const AData:TDataItem; const AFields:TFields);
    function DataFromADT(const AField:TField):TDataItem;
    procedure DoLoadData(const ADataSet:TDataSet; const AData:TDataItem);
    procedure GuessFields(const ADataSet:TDataSet; const AData:TDataItem);
    procedure LoadData(const ADataSet:TDataSet; const AData:TDataItem);
  public
    class procedure Add(const AFields:TFieldDefs; const AItems:TDataArray); overload; static;
    class procedure Add(const AFields:TFieldDefs; const AData:TDataItem); overload; static;

    class function FieldKind(const AFieldType:TFieldType):TDataKind; static;
    class function FieldOf(const AData:TDataItem; const ADataSet:TDataSet):TField; static;

    class function From(const ADataSet:TDataSet; const AName:String=''):TDataItem; overload; static;
    class function From(const AField:TField; const AName:String=''):TDataItem; overload; static;
    class function From(const AFields:Array of TField; const AName:String=''):TDataItem; overload; static;
    class function From(const AConnection:TCustomConnection; const AName:String=''):TDataItem; overload; static;

    function Import(const AField:TField; const AName:String=''):TDataItem; overload;
    function Import(const ADataSet:TDataSet; const AName:String=''):TDataItem; overload;
    function Import(const AConnection:TCustomConnection):TDataArray; overload;
  end;

implementation

uses
  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.Diagnostics,
  {$ENDIF}
  BI.Expression;

function TBIDatasetSource.Import(const AConnection: TCustomConnection): TDataArray;
var t,n : Integer;
begin
  n:=AConnection.DataSetCount;

  SetLength(result,n);

  for t:=0 to n-1 do
      result[t]:=Import(AConnection.DataSets[t]);
end;

type
  TDataAccess=class(TDataItem);

procedure TBIDatasetSource.DoLoadData(const ADataSet:TDataSet; const AData:TDataItem);

  procedure AddAllItems(const AItems:TDataArray; const APos:TInteger);

    procedure AddRecordValue(const AData:TDataItem);
    var Field : TField;
        tmp : Boolean;
    begin
      Field:=TField(TDataAccess(AData).TagObject);

      if (not IgnoreMissing) and Field.IsNull then
      begin
        AData.Missing[APos]:=True;
        tmp:=True;
      end
      else
        tmp:=False;

      if not tmp then
      case AData.Kind of
         dkInt32 : AData.Int32Data[APos]:=Field.AsInteger;
         dkInt64 : AData.Int64Data[APos]:=Field.AsLargeInt;
        dkSingle : AData.SingleData[APos]:=Field.{$IFDEF FPC}AsFloat{$ELSE}AsSingle{$ENDIF};
        dkDouble : AData.DoubleData[APos]:=Field.AsFloat;
      dkExtended : AData.ExtendedData[APos]:=Field.{$IFDEF FPC}AsFloat{$ELSE}AsExtended{$ENDIF};
          dkText : AData.TextData[APos]:=Field.AsString;
      dkDateTime : AData.DateTimeData[APos]:=Field.AsDateTime;
       dkBoolean : AData.BooleanData[APos]:=Field.AsBoolean;
      end;
    end;

  var t : Integer;
  begin
    for t:=0 to High(AItems) do
        if AItems[t].AsTable then
           AddAllItems(AItems[t].Items.AsArray,APos)
        else
           AddRecordValue(AItems[t]);
  end;

var Delta : Integer;
    Pos,
    Capacity : TInteger;

    tmp : TDataArray; // <-- speed opt
begin
  Delta:=1024;

  Capacity:=AData.Count;

  Pos:=0;

  tmp:=AData.Items.AsArray;

  ADataSet.First;

  while not ADataSet.Eof do
  begin
    if Pos>=Capacity then
    begin
      Inc(Capacity,Delta);
      AData.Items.Resize(Capacity);

      // Increase capacity dynamically:
      if Capacity>16*Delta then
         Delta:=Delta*2;
    end;

    AddAllItems(tmp,Pos);

    ADataSet.Next;
    Inc(Pos);
  end;

  // Reset size to real record count:
  AData.Resize(Pos);
end;

procedure TBIDatasetSource.LoadData(const ADataSet:TDataSet; const AData:TDataItem);
var B : TBookmark;
    NumRecs : Integer;
begin
  B:=ADataSet.GetBookmark;
  ADataSet.DisableControls;
  try
    // Problem: dbExpress RecordCount wrongly adds quotes to table name (ie: MSSQL), and fails !
    try
      NumRecs:=ADataSet.RecordCount;
    except
      NumRecs:=0;
    end;

    if NumRecs>0 then
       AData.Resize(NumRecs)
    else
       AData.Resize(128);

    // Speed opt: separate method to reduce register pressure
    DoLoadData(ADataSet,AData);

  finally
    ADataSet.GotoBookmark(B);
    ADataset.FreeBookmark(B);
    ADataSet.EnableControls;
  end;
end;

class function TBIDatasetSource.FieldKind(const AFieldType:TFieldType):TDataKind;
begin
  case AFieldType of

     ftMemo,
     ftFmtMemo,
     ftFixedChar,
     ftWideString,
     ftFixedWideChar,
     ftWideMemo,
     ftString : result:=dkText;

     ftAutoInc,

     {$IFNDEF FPC}
     {$IF CompilerVersion>36}
     ftLargeUint,
     {$ENDIF}
     {$ENDIF}

     ftLargeint : result:=dkInt64;

     {$IFNDEF FPC}
     ftLongWord,
     ftShortint,
     ftByte,
     {$ENDIF}

     ftSmallint,
     ftInteger,
     ftWord,
     ftFMTBcd,
     ftBCD : result:=dkInt32;

     ftBoolean : result:=dkBoolean;

     {$IFNDEF FPC}
     ftSingle : result:=dkSingle;
     {$ENDIF}

     ftCurrency,
     ftFloat : result:=dkDouble;

     {$IFNDEF FPC}
     ftExtended : result:=dkExtended;
     {$ENDIF}

     ftTimeStamp,
     {$IFNDEF FPC}
     ftOraTimeStamp,
     {$ENDIF}
     ftDate,
     ftTime,
     ftDateTime: result:=dkDateTime;

  else
     (*
     ftUnknown,

     ftBytes,
     ftVarBytes,
     ftBlob,
     ftGraphic,

     ftParadoxOle,
     ftDBaseOle,
     ftTypedBinary,
     ftCursor,

     ftADT,  <-- TDataItem AsTable
     ftArray,
     ftReference,
     ftDataSet,  <-- TDataItem and Master.Data !
     ftOraBlob,
     ftOraClob,
     ftVariant,
     ftInterface,
     ftIDispatch,
     ftGuid,
     ftOraInterval,
     ftConnection,
     ftParams,
     ftStream,
     ftTimeStampOffset,
     ftObject:
     *)

     result:=dkUnknown; // ??
  end;
end;

procedure TBIDatasetSource.AddFields(const AData:TDataItem; const AFields:TFields);
var t : Integer;
begin
  for t:=0 to AFields.Count-1 do
      AddField(AData,AFields[t]);
end;

function TBIDatasetSource.DataFromADT(const AField:TField):TDataItem;
begin
  result:=TDataItem.Create(True);
  result.Name:=AField.FieldName;

  TDataAccess(result).TagObject:=AField;

  {$IFNDEF FPC}
  AddFields(result,TADTField(AField).Fields);
  {$ENDIF}
end;

procedure TBIDatasetSource.AddField(const AData:TDataItem; const AField:TField);
var tmpKind : TDataKind;
begin
  if AField<>nil then
  begin
    if AField.DataType=ftADT then
       AData.Items.Add(DataFromADT(AField))
    else
    begin
      tmpKind:=FieldKind(AField.DataType);

      if tmpKind<>dkUnknown then
         AData.Items.Add(AField.FieldName,tmpKind,AField);
    end;
  end;
end;

procedure TBIDatasetSource.GuessFields(const ADataSet:TDataSet; const AData:TDataItem);
begin
  ADataSet.FieldDefs.Update;
  AddFields(AData,ADataset.Fields);
end;

function TBIDataSetSource.Import(const AField: TField;
  const AName: String): TDataItem;

  function CalcName:String;
  begin
    result:=AName;

    if result='' then
    begin
      result:=AField.DisplayName;

      if result='' then
      begin
        result:=AField.FieldName;

        if result='' then
           result:=AField.Name;
      end;
    end;
  end;

var tmp : TDataKind;
begin
  if AField.DataSet=nil then
     result:=nil
  else
  if AField.DataType=ftADT then
     result:=DataFromADT(AField)
  else
  begin
    tmp:=FieldKind(AField.DataType);

    if tmp=dkUnknown then
       result:=nil
    else
    begin
      result:=TDataItem.Create(tmp);
      TDataAccess(result).TagObject:=AField;
    end;
  end;

  if result<>nil then
  begin
    LoadData(AField.DataSet,result);
    result.Name:=CalcName;
  end;
end;

function TBIDatasetSource.Import(const ADataSet:TDataSet; const AName:String=''):TDataItem;
var t1 : TStopwatch;
begin
  result:=TDataItem.Create(True);
  try
    if AName='' then
       result.Name:=ADataSet.Name
    else
       result.Name:=AName;

    t1:=TStopwatch.StartNew;
    try
      GuessFields(ADataSet,result);
    finally
      result.History.Times.LoadingInfo:=t1.ElapsedMilliseconds;
    end;

    t1:=TStopwatch.StartNew;
    try
      LoadData(ADataSet,result);
    finally
      result.History.Times.LoadingData:=t1.ElapsedMilliseconds;
    end;
  finally
    result.Finish;
  end;
end;

function UniqueFieldName(const AFields:TFieldDefs; const AFieldName:String):String;
var t : Integer;
begin
  result:=AFieldName;

  t:=1;

  while AFields.IndexOf(result)<>-1 do
  begin
    result:=AFieldName+'_'+IntToStr(t);
    Inc(t);
  end;
end;

function FieldNameOfData(const AFields:TFieldDefs; const AData:TDataItem):String;
begin
  result:=UniqueFieldName(AFields,AData.Name);
end;

class procedure TBIDataSetSource.Add(const AFields:TFieldDefs; const AData:TDataItem);
var s : String;
begin
  s:=FieldNameOfData(AFields,AData);

  case AData.Kind  of
         dkInt32: AFields.Add(s,ftInteger);
         dkInt64: AFields.Add(s,ftLargeint);  // option: ftLargeUint
        dkSingle: AFields.Add(s,{$IFDEF FPC}ftFloat{$ELSE}ftSingle{$ENDIF});
        dkDouble: AFields.Add(s,ftFloat);
      dkExtended: AFields.Add(s,{$IFDEF FPC}ftFloat{$ELSE}ftExtended{$ENDIF});
          dkText: AFields.Add(s,ftWideString,TDataAccess(AData).MaxTextLength);
      dkDateTime: AFields.Add(s,ftDateTime);
       dkBoolean: AFields.Add(s,ftBoolean);
    else
       //dkUnknown:
       AFields.Add(s,{$IFDEF FPC}ftInteger{$ELSE}ftByte{$ENDIF}); // Dummy
  end;
end;

class procedure TBIDataSetSource.Add(const AFields:TFieldDefs; const AItems:TDataArray);

  // Returns the maximum length of Name property of all AItems
  function MaxLengthNames(const AItems:TDataArray):Integer;
  var tmp : TDataItem;
  begin
    result:=0;

    for tmp in AItems do
        if Length(tmp.Name)>result then
           result:=Length(tmp.Name);

    if result=0 then
       result:=64; // ??
  end;

var tmpDef : TFieldDef;
    Item : TDataItem;
begin
  AFields.BeginUpdate;
  try
    for Item in AItems do
    begin
      TDataAccess(Item).CheckEmptyName;

      if Item.AsTable or (Item is TDetailData) then
      begin
        tmpDef:=AFields.AddFieldDef;
        tmpDef.Name:=FieldNameOfData(AFields,Item);

        if (Item is TDetailData) or (Item.Master<>nil) then
           tmpDef.DataType:=ftDataSet
        else
        begin
          tmpDef.DataType:=ftADT;

          {$IFNDEF FPC}
          Add(tmpDef.ChildDefs,Item.Items.AsArray);
          {$ENDIF}
        end;

      end
      else
      if (Item.Kind=dkUnknown) and (Item.Items.Count>0) then
      begin
        // Not possible. Dataset is global to all Col records ! (instead of different dataset for each Data.Item)
        //tmpDef.DataType:=ftDataSet;

        AFields.Add(FieldNameOfData(AFields,Item),ftWideString,MaxLengthNames(Item.Items.AsArray));
      end
      else
         Add(AFields,Item);
    end;
  finally
    AFields.EndUpdate;
  end;
end;

{ TBIDatasetHelper }

class function TBIDataSetSource.FieldOf(const AData: TDataItem; const ADataSet: TDataSet): TField;

  function FindInFields(const AFields:TFields):TField;
  var t : Integer;
      tmp : TField;
  begin
    for t:=0 to AFields.Count-1 do
    begin
      tmp:=AFields[t];

      if TObject(tmp.Tag)=AData then
         Exit(tmp)

      {$IFNDEF FPC}
      else
      if tmp is TObjectField then
      begin
        tmp:=FindInFields(TObjectField(tmp).Fields);

        if tmp<>nil then
           Exit(tmp);
      end;
      {$ENDIF}
    end;

    result:=nil;
  end;

begin
  if ADataSet<>nil then
     result:=FindInFields(ADataSet.Fields)
  else
     result:=nil;
end;

class function TBIDataSetSource.From(const AFields: array of TField;
  const AName: String): TDataItem;
var tmp : TDataArray;
    tmpField : TField;
begin
  with TBIDataSetSource.Create do
  try
    tmp:=nil;

    for tmpField in AFields do
        tmp.Add(Import(tmpField));

    result:=TDataItem.Create(tmp);
    result.Name:=AName;
  finally
    Free;
  end;
end;

// Import all rows of ADataSet
class function TBIDataSetSource.From(const ADataSet: TDataSet;
  const AName: String): TDataItem;
begin
  with TBIDataSetSource.Create do
  try
    result:=Import(ADataSet,AName);
  finally
    Free;
  end;
end;

// Import all rows in AField
class function TBIDataSetSource.From(const AField: TField;
  const AName: String): TDataItem;
var tmp : String;
begin
  with TBIDataSetSource.Create do
  try
    tmp:=AName;

    if tmp='' then
       tmp:=AField.DisplayName;

    result:=Import(AField,tmp);
  finally
    Free;
  end;
end;

// Import all TDataset in AConnection.Datasets property
class function TBIDataSetSource.From(const AConnection:TCustomConnection;
                                     const AName:String=''):TDataItem;
var tmp : String;
begin
  with TBIDataSetSource.Create do
  try
    result:=TDataItem.Create(Import(AConnection));

    tmp:=AName;

    if tmp='' then
       tmp:=AConnection.Name;

    result.Name:=tmp;
  finally
    Free;
  end;
end;

end.


