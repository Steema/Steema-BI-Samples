{*********************************************}
{  TeeBI Software Library                     }
{  Loads and Saves TSummary to streams        }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Summary.Persist;

interface

uses
  System.Classes, BI.Persist, BI.Streams, BI.Summary;

type
  TSummaryPersist=class
  private
    class procedure Load(const Reader: TBIReader; const Summary:TSummary); static;
  public
    class function LoadFrom(const Reader: TBIReader): TSummary; overload; static;
    class function LoadFrom(const Stream: TStream): TSummary; overload; static;

    class procedure Save(const Stream: TStream; const Summary:TSummary); overload; static;
    class procedure Save(const Writer: TBIWriter; const Summary:TSummary); overload; static;
  end;

  TNamedSummary=record
    Name : String;
    Summary : TSummary;
  end;

  TSummaries=class
  public
    Items : Array of TNamedSummary;

    Destructor Destroy; override;

    procedure Add(const ASummary:TSummary);
    procedure Clear;
    function Count:Integer;

    procedure Load(const FileName:String); overload;
    procedure Load(const Stream:TStream); overload;
    procedure Load(const Reader:TBIReader); overload;

    procedure Save(const FileName:String); overload;
    procedure Save(const Stream:TStream); overload;
    procedure Save(const Writer:TBIWriter); overload;
  end;

implementation

uses
  System.SysUtils, BI.DataItem, BI.Expression, BI.Expressions;

class function TSummaryPersist.LoadFrom(const Reader: TBIReader): TSummary;
begin
  result:=TSummary.Create(nil);
  Load(Reader,result);
end;

class function TSummaryPersist.LoadFrom(const Stream: TStream): TSummary;
var r : TBIReader;
begin
  r:=TBIReader.Create(Stream,1024);
  try
    result:=LoadFrom(r);
  finally
    r.Free;
  end;
end;

class procedure TSummaryPersist.Load(const Reader: TBIReader; const Summary:TSummary);

  function ReadExpression(const AData:TDataItem):TExpression;
  begin
    result:=TDataExpression.FromString(AData,Reader.ReadString);
  end;

  function ReadMeasure(const AData:TDataItem):TMeasure;
  begin
    result:=TMeasure.Create;
    result.Aggregate:=TAggregate(Reader.ReadInteger);
    result.Expression:=ReadExpression(AData);
  end;

  function ReadHistogram:THistogram;
  begin
    result:=THistogram.Create;
    result.NumBins:=Reader.ReadInteger;
    result.BinSize:=Reader.ReadFloat;

    result.AutoMinimum:=Reader.ReadBoolean;
    result.Minimum:=Reader.ReadFloat;

    result.AutoMaximum:=Reader.ReadBoolean;
    result.Maximum:=Reader.ReadFloat;
  end;

  function ReadBy(const AData:TDataItem):TGroupBy;
  begin
    result:=TGroupBy.Create;

    result.Expression:=ReadExpression(AData);
    result.DatePart:=TDateTimePart(Reader.ReadInteger);
    result.Layout:=TGroupByLayout(Reader.ReadInteger);

    if Reader.ReadBoolean then
       result.Histogram:=ReadHistogram;
  end;

  function ReadData:TDataItem;
  var Data : TDataItem;
  begin
    // Pending:
    // Store:=Reader.ReadString;

    Data:=TStore.Load(Reader.ReadString);

    if Data=nil then
    begin
      Reader.ReadString;
      Reader.ReadString;

      result:=nil;
    end
    else
    begin
      result:=Data.Items.Find(Reader.ReadString);

      if result=nil then
         Reader.ReadString; // skip
    end;
  end;

  function ReadFilter(const AData:TDataItem):TExpression;
  begin
    if Reader.ReadBoolean then
       result:=TDataFilter.FromString(AData,Reader.ReadString)
    else
       result:=nil;
  end;

var t : Integer;
    tmpMain : TDataItem;
begin
  tmpMain:=ReadData;

  Summary.Description:=Reader.ReadString;
  Summary.RemoveMissing.Rows:=Reader.ReadBoolean;
  Summary.RemoveMissing.Columns:=Reader.ReadBoolean;

  for t:=1 to Reader.ReadInteger do
      Summary.Measures.Append(ReadMeasure(tmpMain));

  for t:=1 to Reader.ReadInteger do
      Summary.By.Append(ReadBy(tmpMain));

  if Reader.ReadBoolean then
     Summary.Filter:=ReadFilter(tmpMain);

  if Reader.ReadBoolean then
     Summary.Having.Add(Reader.ReadString);
end;

class procedure TSummaryPersist.Save(const Stream: TStream; const Summary:TSummary);
var w : TBIWriter;
begin
  w:=TBIWriter.Create(Stream,1024,TPersistence.Raw);
  try
    Save(w,Summary);
  finally
    w.Free;
  end;
end;

type
  TSummaryAccess=class(TSummary);

class procedure TSummaryPersist.Save(const Writer: TBIWriter; const Summary:TSummary);
var
  tmpMain : TDataItem;

  procedure WriteExpression(const AExp:TExpression);
  var S : String;
      tmp : TDataItem;
  begin
    S:=AExp.ToString;

    if AExp is TDataItemExpression then
    begin
      tmp:=TDataItemExpression(AExp).Data.Parent;

      if tmp<>tmpMain then
         S:=tmp.Name+'.'+S;
    end;

    Writer.WriteString(S);
  end;

  procedure WriteMeasure(const Measure:TMeasure);
  begin
    Writer.WriteInteger(Ord(Measure.Aggregate));
    WriteExpression(Measure.Expression);
  end;

  procedure WriteHistogram(const Histogram:THistogram);
  begin
    Writer.WriteInteger(Histogram.NumBins);
    Writer.WriteFloat(Histogram.BinSize);

    Writer.WriteBoolean(Histogram.AutoMinimum);
    Writer.WriteFloat(Histogram.Minimum);

    Writer.WriteBoolean(Histogram.AutoMaximum);
    Writer.WriteFloat(Histogram.Maximum);
  end;

  procedure WriteBy(const By:TGroupBy);
  begin
    WriteExpression(By.Expression);
    Writer.WriteInteger(Ord(By.DatePart));
    Writer.WriteInteger(Ord(By.Layout));

    if By.Histogram.Active then
    begin
      Writer.WriteBoolean(True);
      WriteHistogram(By.Histogram);
    end
    else
      Writer.WriteBoolean(False);
  end;

  procedure WriteData(const AData:TDataItem);
  begin
    Writer.WriteString(AData.Parent.Name);
    Writer.WriteString(AData.Name);
  end;

  procedure WriteFilter(const Filter:TExpression);
  begin
    Writer.WriteString(Filter.ToString);
  end;

var L,t : Integer;
    tmp : String;
begin
  if TSummaryAccess(Summary).Hops=nil then
     Summary.Prepare;

  tmpMain:=TSummaryAccess(Summary).Hops.Main;
  WriteData(tmpMain);

  Writer.WriteString(Summary.Description);
  Writer.WriteBoolean(Summary.RemoveMissing.Rows);
  Writer.WriteBoolean(Summary.RemoveMissing.Columns);

  L:=Summary.Measures.Count;
  Writer.WriteInteger(L);

  for t:=0 to L-1 do
      WriteMeasure(Summary.Measures[t]);

  L:=Summary.By.Count;
  Writer.WriteInteger(L);

  for t:=0 to L-1 do
      WriteBy(Summary.By[t]);

  if Summary.Filter=nil then
     Writer.WriteBoolean(False)
  else
  begin
    Writer.WriteBoolean(True);
    WriteFilter(Summary.Filter);
  end;

  tmp:=Summary.Having.ToString;

  if tmp='' then
     Writer.WriteBoolean(False)
  else
  begin
    Writer.WriteBoolean(True);
    Writer.WriteString(tmp);
  end;
end;

{ TSummaries }

function TSummaries.Count: Integer;
begin
  result:=Length(Items);
end;

destructor TSummaries.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSummaries.Load(const FileName: String);
var f : TFileStream;
begin
  f:=TPersistence.OpenFile(FileName);
  try
    Load(f);
  finally
    f.Free;
  end;
end;

procedure TSummaries.Load(const Stream: TStream);
var R : TBIReader;
begin
  R:=TBIReader.Create(Stream,1024);
  try
    Load(R);
  finally
    R.Free;
  end;
end;

procedure TSummaries.Load(const Reader: TBIReader);

  function LoadOne:TNamedSummary;
  begin
    result.Name:=Reader.ReadString;
    result.Summary:=TSummaryPersist.LoadFrom(Reader);
  end;

var t, n : Integer;
begin
  Clear;

  n:=Reader.ReadInteger;

  SetLength(Items,n);

  for t:=0 to n-1 do
      Items[t]:=LoadOne;
end;

procedure TSummaries.Save(const Writer: TBIWriter);

  procedure WriteOne(const AItem:TNamedSummary);
  begin
    Writer.WriteString(AItem.Name);
    TSummaryPersist.Save(Writer,AItem.Summary);
  end;

var t, n : Integer;
begin
  n:=Length(Items);
  Writer.WriteInteger(n);

  for t:=0 to n-1 do
      WriteOne(Items[t]);
end;

procedure TSummaries.Add(const ASummary: TSummary);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);

  Items[L].Name:=ASummary.Description;
  Items[L].Summary:=ASummary;
end;

procedure TSummaries.Clear;
var t : Integer;
begin
  for t:=0 to High(Items) do
      Items[t].Summary.Free;

  Items:=nil;
end;

procedure TSummaries.Save(const FileName: String);
var f : TFileStream;
begin
  f:=TPersistence.CreateFile(FileName);
  try
    Save(f);
  finally
    f.Free;
  end;
end;

procedure TSummaries.Save(const Stream: TStream);
var W : TBIWriter;
begin
  W:=TBIWriter.Create(Stream,1024,False);
  try
    Save(W);
  finally
    W.Free;
  end;
end;

end.
