{*********************************************}
{  TeeBI Software Library                     }
{  Data Merge                                 }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Merge;

// TDataMerge methods return a single TDataItem that is the result of
// aggregating many DataItems that have the same structure.

// "Same structure" means the number of sub-items and their Kind are
// identical. For sub-tables, structure is checked recursively.

interface

uses
  BI.Arrays, BI.DataItem, BI.Persist;

type
  TDataMerge=record
  private
    class procedure AppendTo(const ASource,ADest:TDataItem); static;

    class function Has(const A,B:TDataArray; const IndexB:TInteger):TInteger; static;
  public
    // Returns a new Data item with the same structure as AData.
    // If AData has sub-tables, they are also cloned recursively
    class function CloneStructure(const AData:TDataItem):TDataItem; static;

    // Returns a copy of all identical rows in A and B
    class procedure Common(const ADest:TDataItem; const A,B:TDataArray); static;

    // Returns a copy of all different rows in A and B
    class procedure Different(const ADest:TDataItem; const A,B:TDataArray); static;

    // Merges all data files in folder, of given extension
    class function FromFolder(const AFolder,AExtension:String):TDataItem; static;

    // Merges all data files of a TStore (with same structure) into a single one
    class function FromStore(const AStore,AName:String):TDataItem; static;

    // Merges all AData items into a single one.
    // When FreeData is True, all AData items are destroyed
    // (except the first one, that is the returned result)

    class function FromData(const AData:TDataArray; const FreeData:Boolean=True):TDataItem; overload; static;

    // Merges a copy of all AData items into ADest
    class procedure FromData(const ADest:TDataItem; const AData:TDataArray); overload; static;

    // Returns True when A and B have the same structure
    class function SameStructure(const A,B:TDataItem):Boolean; overload; static;

    // Returns True when all AItems have the same structure
    class function SameStructure(const AItems:TDataArray):Boolean; overload; static;

    // Removes data in ADest that is also in ASource
    class procedure Subtract(const ADest,ASource:TDataItem); static;
  end;

implementation

uses
  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.IOUtils,
  {$ENDIF}

  System.Types, BI.DataSource, BI.Compare, BI.Languages.English;

{ TDataMerge }

// Scans AFolder for all TeeBI native files, and loads and merges them into a
// single TDataItem
class function TDataMerge.FromFolder(const AFolder,AExtension: String): TDataItem;

  // Load all TeeBI native files
  function LoadAll(const AFiles:TStringDynArray):TDataArray;
  var t,L : Integer;
  begin
    L:=Length(AFiles);
    SetLength(result,L);

    for t:=0 to L-1 do
        result[t]:=TDataItemPersistence.Load(AFiles[t]);
  end;

var tmp : TStringDynArray;
begin
  tmp:=TDirectory.GetFiles(AFolder,'*'+AExtension);

  if tmp=nil then
     result:=nil
  else
     result:=FromData(LoadAll(tmp));
end;

class function TDataMerge.FromStore(const AStore, AName: String): TDataItem;
begin
  result:=FromData(TStore.Load(AStore,AName).Items.AsArray,False);
end;

// Returns True when A and B have a "compatible" structure,
// where all A and B items are of the same Kind (when A and B are tables),
// or A and B Kinds are equal (when A and B are not tables)
class function TDataMerge.SameStructure(const A,B:TDataItem):Boolean;

  function AllEqual(const A,B:TDataArray):Boolean;
  var t : Integer;
  begin
    result:=A.Count=B.Count;

    if result then
       for t:=0 to A.Count-1 do
           if not SameStructure(A[t],B[t]) then
              Exit(False);
  end;

begin
  // Are both A and B tables or not tables?
  result:=(A.AsTable and B.AsTable) or
          ((not A.AsTable) and (not B.AsTable));

  if result then
     if A.AsTable then
        result:=AllEqual(A.Items.AsArray,B.Items.AsArray)
     else
        result:=A.Kind=B.Kind;
end;

// Returns True when all items in AItems have a compatible structure
class function TDataMerge.SameStructure(const AItems:TDataArray):Boolean;
var t,L : Integer;
begin
  L:=AItems.Count;

  if L>0 then
  begin
    for t:=1 to L-1 do
        if not SameStructure(AItems[t-1],AItems[t]) then
           Exit(False);

    result:=True;
  end
  else
    result:=False;
end;

type
  TDataItemAccess=class(TDataItem);

class function TDataMerge.CloneStructure(const AData: TDataItem): TDataItem;
begin
  result:=TDataClone.CloneStructure(AData);
end;

class function TDataMerge.Has(const A,B:TDataArray; const IndexB:TInteger):TInteger;
var t : Integer;
begin
  for t:=0 to A[0].Parent.Count-1 do
      if TDataCompare.Compare(A,t,B,IndexB) then
         Exit(t);

  result:=-1;
end;

// Removes data in ADest that is also in ASource
class procedure TDataMerge.Subtract(const ADest,ASource: TDataItem);

  procedure NotCompatible;
  begin
    raise EBIException.CreateFmt(BIMsg_NotCompatible,
                                [ADest.Name,ASource.Name]);
  end;

var tmpDest,
    tmpSource : TDataArray;
    t,
    tt : TLoopInteger;
    tmp : TInteger;
begin
  if SameStructure(ADest,ASource) then
  begin
    if ADest.AsTable then
    begin
      tmpDest:=ADest.Items.AsArray;
      tmpSource:=ASource.Items.AsArray;

      for t:=0 to ASource.Count-1 do
      begin
        tmp:=Has(tmpDest,tmpSource,t);

        if tmp<>-1 then
           ADest.Delete(tmp);
      end;
    end
    else
    begin
      for t:=0 to ASource.Count-1 do
      begin
        for tt:=0 to ADest.Count-1 do
            if TDataCompare.Compare(ADest,tt,ASource,t) then
            begin
              ADest.Delete(tt);
              break;
            end;
      end;
    end
  end
  else
    NotCompatible;
end;

class procedure TDataMerge.Common(const ADest: TDataItem; const A,B: TDataArray);
var tmpA,
    tmpB,
    tmpDest : TDataArray;
    t : TLoopInteger;
    tmp : TInteger;
begin
  if A.Count>B.Count then
  begin
    tmpA:=A;
    tmpB:=B;
  end
  else
  begin
    tmpA:=B;
    tmpB:=A;
  end;

  tmpDest:=ADest.Items.AsArray;

  tmp:=0;

  for t:=0 to tmpB.Count-1 do
      if Has(tmpA,tmpB,t)<>-1 then
      begin
        if ADest.Count<=tmp then
           ADest.Resize(tmp+64);

        TDataClone.CopyData(tmpB,tmpDest,t,tmp);
        Inc(tmp);
      end;

  ADest.Resize(tmp);
end;

class procedure TDataMerge.Different(const ADest: TDataItem; const A,B: TDataArray);
var
  tmp : TInteger;
  tmpDest : TDataArray;

  procedure Process(const A,B:TDataArray);
  var t : TLoopInteger;
  begin
    for t:=0 to A.Count-1 do
        if Has(B,A,t)=-1 then
        begin
          if ADest.Count<=tmp then
             ADest.Resize(tmp+64);

          TDataClone.CopyData(B,tmpDest,t,tmp);

          Inc(tmp);
        end;
  end;

begin
  tmpDest:=ADest.Items.AsArray;

  tmp:=0;

  Process(A,B);
  Process(B,A);

  ADest.Resize(tmp);
end;

// Add all data in ASource into ADest, appending all items arrays directly
class procedure TDataMerge.AppendTo(const ASource,ADest:TDataItem);

  // Appends all data from ASource into ADest, directly.
  // Assumes both ASource and ADest Kind are equal
  procedure AppendItem(const ASource,ADest:TDataItem);
  begin
    case ADest.Kind of
        dkInt32: ADest.Int32Data.Append(ASource.Int32Data);
        dkInt64: ADest.Int64Data.Append(ASource.Int64Data);
       dkSingle: ADest.SingleData.Append(ASource.SingleData);
       dkDouble: ADest.DoubleData.Append(ASource.DoubleData);
     dkExtended: ADest.ExtendedData.Append(ASource.ExtendedData);
         dkText: ADest.TextData.Append(ASource.TextData);
     dkDateTime: ADest.DateTimeData.Append(ASource.DateTimeData);
      dkBoolean: ADest.BooleanData.Append(ASource.BooleanData);
    end;

    Inc(TDataItemAccess(ADest).FCount,ASource.Count);
  end;

var t : TLoopInteger;
begin
  if ASource.Count>0 then
  begin
    ASource.Load;

    for t:=0 to ADest.Items.Count-1 do
        AppendItem(ASource.Items[t],ADest.Items[t]);

    Inc(TDataItemAccess(ADest).FCount,ASource.Count);
  end;
end;

class function TDataMerge.FromData(const AData:TDataArray;
                                   const FreeData:Boolean): TDataItem;
var t : Integer;
begin
  if AData=nil then
     result:=nil
  else
  if SameStructure(AData) then
  begin
    if FreeData then
       result:=AData[0]
    else
       result:=CloneStructure(AData[0]);

    for t:=1 to High(AData) do
    begin
      AppendTo(AData[t],result);

      if FreeData then
         AData[t].Free;
    end;
  end
  else
    result:=TDataItem.Create(AData); // Do not merge, simply group them
end;

class procedure TDataMerge.FromData(const ADest: TDataItem; const AData: TDataArray);
var t : Integer;
begin
  ADest.Clear;

  if AData<>nil then
  begin
    if SameStructure(AData) then
    begin
      TDataClone.Clone(AData[0],ADest);

      for t:=1 to High(AData) do
          AppendTo(AData[t],ADest);
    end
    else
      for t:=0 to AData.Count-1 do
          ADest.Items.Add(TDataClone.Clone(AData[t]));
  end;
end;

end.
