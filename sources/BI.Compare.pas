{*********************************************}
{  TeeBI Software Library                     }
{  Data comparison                            }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Compare;

interface

{
  TDataCompare methods
}

uses
  BI.DataItem, BI.Arrays;

type
  TBaseDifferences=class(TDataItem)
  public
    Added,
    Removed,
    Modified : TDataItem;

    Constructor Create;
  end;

  TItemDifference=class(TBaseDifferences)
  public
    Item : TDataItem;

    SameKind  : Boolean;
    SameTable : Boolean;
  end;

  TRenamedItem=record
  public
    Before,
    After : TDataItem;
  end;

  TItemDifferences=class(TBaseDifferences)
  public
    Renamed  : Array of TRenamedItem;
  end;

  TDifference=class(TDataItem)
  public
    Items : TItemDifferences;
    Rows  : TBaseDifferences;

    Constructor Create;
  end;

  TRenamedData=record
  public
    Before,
    After : TDataItem;
  end;

  TDataDifferences=class(TDataItem)
  public
    Added    : TDataArray;
    Removed  : TDataArray;
    Renamed  : Array of TRenamedData;
    Modified : Array of TDifference;
  end;

  // Compare two TDataItem structures (including nested children items),
  // and return the differences both in structure and data in the Diff parameter.
  TDataCompare=record
  private
    class function Same(const Before,After:TDataItem; var Diff:TItemDifference):Boolean; overload; static;
    class function Same(const Before,After:TDataItems; var Diff:TItemDifferences):Boolean; overload; static;

    class function Same(const Before,After:TDataItem; var Diff:TBaseDifferences):Boolean; overload; static;
  public
    class function Compare(const A:TDataItem; const ARow:TInteger;
                           const B:TDataItem; const BRow:TInteger): Boolean; overload; static;

    class function Compare(const Before, After: TDataItem; const ARow:TInteger): Boolean; overload; static;

    class function Compare(const A:TDataArray; const ARow:TInteger;
                           const B:TDataArray; const BRow:TInteger): Boolean; overload; static;

    class function Same(const Before,After:TDataItem):Boolean; overload; static;
    class function Same(const Before,After:TDataItem; var Diff:TDifference):Boolean; overload; static;
    class procedure Compare(const Before,After:TDataItem; const Dest:TDataItem); overload; static;
  end;

implementation

uses
  System.SysUtils;

{ TDataCompare }

(* Unused (yet)
class function TDataCompare.Same(const Before, After: TDataArray; const Diff:TDataDifferences):Boolean;

  procedure AddData(var AData:TDataArray; const Data:TDataItem);
  var L : Integer;
  begin
    L:=AData.Count;
    SetLength(AData,L+1);
    AData[L]:=Data;
  end;

  procedure AddModified(const ADiff:TDifference);
  var L : Integer;
  begin
    L:=Length(Diff.Modified);
    SetLength(Diff.Modified,L+1);
    Diff.Modified[L]:=ADiff;
  end;

  function DataOf(const AData:TDataArray; const AName:String):TDataItem;
  begin
    result:=AData.Find(AName);
  end;

var t : Integer;
    tmp : TDataItem;
    tmpDiff : TDifference;
begin
  for t:=0 to High(Before)-1 do
      if DataOf(After,Before[t].Name)=nil then
         AddData(Diff.Removed,Before[t]);

  for t:=0 to High(After)-1 do
      if DataOf(Before,After[t].Name)=nil then
         AddData(Diff.Added,After[t]);

  for t:=0 to High(Before)-1 do
  begin
    tmp:=DataOf(After,Before[t].Name);

    if tmp<>nil then
       if not Same(Before[t],tmp,tmpDiff) then
          AddModified(tmpDiff);
  end;

  result:=(Diff.Removed=nil) and (Diff.Added=nil) and (Diff.Modified=nil);
end;
*)

// Compare a Value

class function TDataCompare.Compare(const A:TDataItem; const ARow:TInteger;
                                    const B:TDataItem; const BRow:TInteger): Boolean;
begin
  result:=A.Missing[ARow]=B.Missing[BRow];

  if result then
  case A.Kind of
      dkInt32: result:=A.Int32Data[ARow]=B.Int32Data[BRow];
      dkInt64: result:=A.Int64Data[ARow]=B.Int64Data[BRow];
     dkSingle: result:=A.SingleData[ARow]=B.SingleData[BRow];
     dkDouble: result:=A.DoubleData[ARow]=B.DoubleData[BRow];
   dkExtended: result:=A.ExtendedData[ARow]=B.ExtendedData[BRow];
       dkText: result:=A.TextData[ARow]=B.TextData[BRow];
   dkDateTime: result:=A.DateTimeData[ARow]=B.DateTimeData[BRow];
    dkBoolean: result:=A.BooleanData[ARow]=B.BooleanData[BRow];
  else
    result:=False;
  end;
end;

class function TDataCompare.Compare(const Before, After: TDataItem; const ARow:TInteger): Boolean;
begin
  result:=Compare(Before,ARow,After,ARow);
end;

type
  TDataItemAccess=class(TDataItem);

class function TDataCompare.Same(const Before, After: TDataItem;
                                 var Diff: TBaseDifferences): Boolean;

   function Exists(const Origin:TDataItem; const AIndex:TInteger; const Dest:TDataItem):Boolean;
   var t : TLoopInteger;
       tt : Integer;
       tmpFound : Boolean;
   begin
     if Origin.AsTable then
     begin
       for t:=0 to Dest.Count-1 do
       begin
         tmpFound:=True;

         for tt:=0 to Origin.Items.Count-1 do
             if not Compare(Origin.Items[tt],AIndex,Dest.Items[tt],t) then
             begin
               tmpFound:=False;
               break;
             end;

         if tmpFound then
            Exit(True);
       end;

       result:=False;
     end
     else
     begin
       for t:=0 to Dest.Count-1 do
           if Compare(Origin,AIndex,Dest,t) then
              Exit(True);

       result:=False;
     end;
   end;

   procedure FindRows(const ADest,A,B:TDataItem);
   var t : TInteger;
   begin
     t:=0;

     while t<A.Count do
     begin
        if not Exists(A,t,B) then
        begin
          ADest.Int64Data.Append(t);
          TDataItemAccess(ADest).FCount:=ADest.Int64Data.Count;
        end;

        Inc(t);
     end;
   end;

var t : TInteger;
    tt : Integer;
begin
  result:=False;

  Before.Load;
  After.Load;

  if (Before.Count=After.Count) and (Before.Missing.Count=After.Missing.Count) then
  begin
    t:=0;

    while t<Before.Count do
    begin
      if Before.AsTable then
      begin
        for tt:=0 to Before.Items.Count-1 do
            if not Compare(Before.Items[tt],After.Items[tt],t) then
            begin
              Diff.Modified.Int64Data.Append(t);
              TDataItemAccess(Diff.Modified).FCount:=Diff.Modified.Int64Data.Count;
            end;
      end
      else
      if not Compare(Before,After,t) then
      begin
         Diff.Modified.Int64Data.Append(t);
         TDataItemAccess(Diff.Modified).FCount:=Diff.Modified.Int64Data.Count;
      end;

      Inc(t);
    end;

    result:=Diff.Modified.Count=0;
  end
  else
  if Before.Count<After.Count then
     FindRows(Diff.Added,After,Before)
  else
     FindRows(Diff.Removed,Before,After);
end;

class function TDataCompare.Same(const Before, After: TDataItem): Boolean;
var tmp : TDifference;
begin
  tmp:=TDifference.Create;
  try
    result:=Same(Before,After,tmp);
  finally
    tmp.Free;
  end;
end;

class procedure TDataCompare.Compare(const Before, After, Dest: TDataItem);
var tmp : TDifference;
begin
  tmp:=TDifference.Create;
  try
    Same(Before,After,tmp);

    Dest.Items.Add(tmp.Items);
    Dest.Items.Add(tmp.Rows);
  finally
    tmp.Free;
  end;
end;

class function TDataCompare.Compare(const A: TDataArray; const ARow: TInteger;
                                    const B: TDataArray; const BRow: TInteger): Boolean;
var t : Integer;
begin
  for t:=0 to A.Count-1 do
      if not Compare(A[t],ARow,B[t],BRow) then
         Exit(False);

  result:=True;
end;

class function TDataCompare.Same(const Before, After: TDataItem; var Diff: TDifference): Boolean;
begin
  result:=Same(Before.Items,After.Items,Diff.Items) and
          Same(Before,After,Diff.Rows);
end;

class function TDataCompare.Same(const Before, After: TDataItems;
                                 var Diff: TItemDifferences): Boolean;
var t : Integer;
    tmp : TDataItem;
    tmpDiff : TItemDifference;
begin
  for t:=0 to Before.Count-1 do
      if After.Names[Before[t].Name]=nil then
         Diff.Removed.Items.Add(Before[t]);

  for t:=0 to After.Count-1 do
      if Before.Names[After[t].Name]=nil then
         Diff.Added.Items.Add(After[t]);

  for t:=0 to Before.Count-1 do
  begin
    tmp:=After.Names[Before[t].Name];

    if tmp<>nil then
    begin
      tmpDiff:=TItemDifference.Create;

      if Same(Before[t],tmp,tmpDiff) then
         tmpDiff.Free
      else
         Diff.Modified.Items.Add(tmpDiff);
    end;
  end;

  result:=(Diff.Added.Items.Count=0) and (Diff.Removed.Items.Count=0) and
          (Diff.Renamed=nil) and (Diff.Modified.Items.Count=0);
end;

class function TDataCompare.Same(const Before, After: TDataItem;
                                 var Diff: TItemDifference): Boolean;
begin
  result:=False;

  Diff.Item:=After;
  Diff.SameKind:=Before.Kind=After.Kind;
  Diff.SameTable:=Before.AsTable=After.AsTable;

  if Diff.SameKind and Diff.SameTable then
     if Before.Items.Count=After.Items.Count then
        result:=True
     else
     begin
       //Diff.Added
       //Diff.Removed
       //Diff.Modified
     end;
end;

{ TRowDifferences }

Constructor TBaseDifferences.Create;
begin
  inherited Create;

  Added:=Items.Add('Added',TDataKind.dkInt64);
  Removed:=Items.Add('Removed',TDataKind.dkInt64);
  Modified:=Items.Add('Modified',TDataKind.dkInt64);
end;

{ TDifference }

Constructor TDifference.Create;
begin
  inherited Create;

  Items:=TItemDifferences.Create;
  Items.Name:='Items';

  Rows:=TBaseDifferences.Create;
  Rows.Name:='Rows';

  inherited Items.Add(Items);
  inherited Items.Add(Rows);
end;

end.
