{*********************************************}
{  TeeBI Software Library                     }
{  Array Parallel Sorting and Merge           }
{  Copyright (c) 2016-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Arrays.Parallel;

interface

uses
  BI.Arrays;

// Parallel hybrid sort (QuickSort + InsertionSort) using Split & Merge

// Note: Generics cannot be used here, as inner code needs "<" ">" comparer operators

type
  TParallelArray=record
  private
    class procedure DoRaise; static;
  public
    class function Sort(const Value:TInt32Array;
                        const Ascending:Boolean=True;
                        const Threads:Integer=0):TInt32Array; overload; static;

    class function Sort(const Value:TInt64Array;
                        const Ascending:Boolean=True;
                        const Threads:Integer=0):TInt64Array; overload; static;

    class function Sort(const Value:TSingleArray;
                        const Ascending:Boolean=True;
                        const Threads:Integer=0):TSingleArray; overload; static;

    class function Sort(const Value:TDoubleArray;
                        const Ascending:Boolean=True;
                        const Threads:Integer=0):TDoubleArray; overload; static;
  end;

  TSortedAscendingArray=record
  public
    class function Merge(const A,B:TInt32Array; AFrom,ATo,BFrom,BTo:TInteger):TInt32Array; overload; static;
    class function Merge(const A,B:TInt32Array):TInt32Array; overload; inline; static;
    class function Merge(const Value:Array of TInt32Array):TInt32Array; overload; static;

    class function Merge(const A,B:TInt64Array; AFrom,ATo,BFrom,BTo:TInteger):TInt64Array; overload; static;
    class function Merge(const A,B:TInt64Array):TInt64Array; overload; inline; static;
    class function Merge(const Value:Array of TInt64Array):TInt64Array; overload; static;

    class function Merge(const A,B:TSingleArray; AFrom,ATo,BFrom,BTo:TInteger):TSingleArray; overload; static;
    class function Merge(const A,B:TSingleArray):TSingleArray; overload; inline; static;
    class function Merge(const Value:Array of TSingleArray):TSingleArray; overload; static;

    class function Merge(const A,B:TDoubleArray; AFrom,ATo,BFrom,BTo:TInteger):TDoubleArray; overload; static;
    class function Merge(const A,B:TDoubleArray):TDoubleArray; overload; inline; static;
    class function Merge(const Value:Array of TDoubleArray):TDoubleArray; overload; static;
  end;

  TSortedDescendingArray=record
  public
    class function Merge(const A,B:TInt32Array; AFrom,ATo,BFrom,BTo:TInteger):TInt32Array; overload; static;
    class function Merge(const A,B:TInt32Array):TInt32Array; overload; inline; static;
    class function Merge(const Value:Array of TInt32Array):TInt32Array; overload; static;

    class function Merge(const A,B:TInt64Array; AFrom,ATo,BFrom,BTo:TInteger):TInt64Array; overload; static;
    class function Merge(const A,B:TInt64Array):TInt64Array; overload; inline; static;
    class function Merge(const Value:Array of TInt64Array):TInt64Array; overload; static;

    class function Merge(const A,B:TSingleArray; AFrom,ATo,BFrom,BTo:TInteger):TSingleArray; overload; static;
    class function Merge(const A,B:TSingleArray):TSingleArray; overload; inline; static;
    class function Merge(const Value:Array of TSingleArray):TSingleArray; overload; static;

    class function Merge(const A,B:TDoubleArray; AFrom,ATo,BFrom,BTo:TInteger):TDoubleArray; overload; static;
    class function Merge(const A,B:TDoubleArray):TDoubleArray; overload; inline; static;
    class function Merge(const Value:Array of TDoubleArray):TDoubleArray; overload; static;
  end;

implementation

uses
  System.Classes, System.Threading;

{$POINTERMATH ON}

{ TParallelArray }

class procedure TParallelArray.DoRaise;
begin
  raise EBIException.Create('Error: ParallelSort Threads must be >= 0');
end;

// Split & Merge
class function TParallelArray.Sort(const Value: TInt32Array;
  const Ascending: Boolean; const Threads: Integer): TInt32Array;

  function Merge(const Parts:Integer; const Steps:TInteger):TInt32Array;
  var t : Integer;
      jMin,
      jMax : TInteger;
  begin
    result:=Value.Copy(0,Steps);

    jMin:=Steps;

    for t:=1 to Parts-1 do
    begin
      if t=Parts-1 then
         jMax:=Value.Count
      else
      begin
        jMax:=jMin+Steps;

        if jMax>Value.Count then
           jMax:=Value.Count;
      end;

      if Ascending then
         result:=TSortedAscendingArray.Merge(result,Value,0,result.Count-1,jMin,jMax-1)
      else
         result:=TSortedDescendingArray.Merge(result,Value,0,result.Count-1,jMin,jMax-1);

      jMin:=jMax;
    end;
  end;

var tmp : Integer;
    Steps : TInteger;
begin
  if Threads=0 then
     tmp:=TThread.ProcessorCount
  else
     tmp:=Threads;

  if tmp<0 then
     DoRaise
  else
  if tmp=1 then // Single-cpu
  begin
    Value.Sort(Ascending);
    result:=Value;
  end
  else
  begin
    Steps:=Value.Count div tmp;

    // Sort in parallel
    TParallel.&For(0,tmp-1,procedure(Index:Integer)
    var tmpHigh : TInteger;
    begin
      if Index=tmp-1 then
         tmpHigh:=Value.Count-1
      else
      begin
        tmpHigh:=Pred((Index+1)*Steps);

        if tmpHigh>Value.Count-1 then
           tmpHigh:=Value.Count-1;
      end;

      Value.Sort(Index*Steps,tmpHigh,Ascending);
    end);

    // Merge all parts
    result:=Merge(tmp,Steps);
  end;
end;

class function TParallelArray.Sort(const Value: TInt64Array;
  const Ascending: Boolean; const Threads: Integer): TInt64Array;

  function Merge(const Parts:Integer; const Steps:TInteger):TInt64Array;
  var t : Integer;
      jMin,
      jMax : TInteger;
  begin
    result:=Value.Copy(0,Steps);

    jMin:=Steps;

    for t:=1 to Parts-1 do
    begin
      if t=Parts-1 then
         jMax:=Value.Count
      else
      begin
        jMax:=jMin+Steps;

        if jMax>Value.Count then
           jMax:=Value.Count;
      end;

      if Ascending then
         result:=TSortedAscendingArray.Merge(result,Value,0,result.Count-1,jMin,jMax-1)
      else
         result:=TSortedDescendingArray.Merge(result,Value,0,result.Count-1,jMin,jMax-1);

      jMin:=jMax;
    end;
  end;

var tmp : Integer;
    Steps : TInteger;
begin
  if Threads=0 then
     tmp:=TThread.ProcessorCount
  else
     tmp:=Threads;

  if tmp<0 then
     DoRaise
  else
  if tmp=1 then // Single-cpu
  begin
    Value.Sort(Ascending);
    result:=Value;
  end
  else
  begin
    Steps:=Value.Count div tmp;

    // Sort in parallel
    TParallel.&For(0,tmp-1,procedure(Index:Integer)
    var tmpHigh : TInteger;
    begin
      if Index=tmp-1 then
         tmpHigh:=Value.Count-1
      else
      begin
        tmpHigh:=Pred((Index+1)*Steps);

        if tmpHigh>Value.Count-1 then
           tmpHigh:=Value.Count-1;
      end;

      Value.Sort(Index*Steps,tmpHigh,Ascending);
    end);

    // Merge all parts
    result:=Merge(tmp,Steps);
  end;
end;

class function TParallelArray.Sort(const Value: TSingleArray;
  const Ascending: Boolean; const Threads: Integer): TSingleArray;

  function Merge(const Parts:Integer; const Steps:TInteger):TSingleArray;
  var t : Integer;
      jMin,
      jMax : TInteger;
  begin
    result:=Value.Copy(0,Steps);

    jMin:=Steps;

    for t:=1 to Parts-1 do
    begin
      if t=Parts-1 then
         jMax:=Value.Count
      else
      begin
        jMax:=jMin+Steps;

        if jMax>Value.Count then
           jMax:=Value.Count;
      end;

      if Ascending then
         result:=TSortedAscendingArray.Merge(result,Value,0,result.Count-1,jMin,jMax-1)
      else
         result:=TSortedDescendingArray.Merge(result,Value,0,result.Count-1,jMin,jMax-1);

      jMin:=jMax;
    end;
  end;

var tmp : Integer;
    Steps : TInteger;
begin
  if Threads=0 then
     tmp:=TThread.ProcessorCount
  else
     tmp:=Threads;

  if tmp<0 then
     DoRaise
  else
  if tmp=1 then // Single-cpu
  begin
    Value.Sort(Ascending);
    result:=Value;
  end
  else
  begin
    Steps:=Value.Count div tmp;

    // Sort in parallel
    TParallel.&For(0,tmp-1,procedure(Index:Integer)
    var tmpHigh : TInteger;
    begin
      if Index=tmp-1 then
         tmpHigh:=Value.Count-1
      else
      begin
        tmpHigh:=Pred((Index+1)*Steps);

        if tmpHigh>Value.Count-1 then
           tmpHigh:=Value.Count-1;
      end;

      Value.Sort(Index*Steps,tmpHigh,Ascending);
    end);

    // Merge all parts
    result:=Merge(tmp,Steps);
  end;
end;

class function TParallelArray.Sort(const Value: TDoubleArray;
  const Ascending: Boolean; const Threads: Integer): TDoubleArray;

  function Merge(const Parts:Integer; const Steps:TInteger):TDoubleArray;
  var t : Integer;
      jMin,
      jMax : TInteger;
  begin
    result:=Value.Copy(0,Steps);

    jMin:=Steps;

    for t:=1 to Parts-1 do
    begin
      if t=Parts-1 then
         jMax:=Value.Count
      else
      begin
        jMax:=jMin+Steps;

        if jMax>Value.Count then
           jMax:=Value.Count;
      end;

      if Ascending then
         result:=TSortedAscendingArray.Merge(result,Value,0,result.Count-1,jMin,jMax-1)
      else
         result:=TSortedDescendingArray.Merge(result,Value,0,result.Count-1,jMin,jMax-1);

      jMin:=jMax;
    end;
  end;

var tmp : Integer;
    Steps : TInteger;
begin
  if Threads=0 then
     tmp:=TThread.ProcessorCount
  else
     tmp:=Threads;

  if tmp<0 then
     DoRaise
  else
  if tmp=1 then // Single-cpu
  begin
    Value.Sort(Ascending);
    result:=Value;
  end
  else
  begin
    Steps:=Value.Count div tmp;

    // Sort in parallel
    TParallel.&For(0,tmp-1,procedure(Index:Integer)
    var tmpHigh : TInteger;
    begin
      if Index=tmp-1 then
         tmpHigh:=Value.Count-1
      else
      begin
        tmpHigh:=Pred((Index+1)*Steps);

        if tmpHigh>Value.Count-1 then
           tmpHigh:=Value.Count-1;
      end;

      Value.Sort(Index*Steps,tmpHigh,Ascending);
    end);

    // Merge all parts
    result:=Merge(tmp,Steps);
  end;
end;

{ TSortedAscendingArray }

class function TSortedAscendingArray.Merge(const Value: Array of TInt32Array): TInt32Array;

  function DoMerge(const L,H:Integer):TInt32Array;
  var t : Integer;
  begin
    result:=Value[L];

    for t:=L+1 to H do
        result:=Merge(result,Value[t]);
  end;

begin
  if Length(Value)>1 then
     result:=DoMerge(Low(Value),High(Value))
  else
  if Length(Value)>0 then
     result:=Value[0]
  else
     result:=nil;
end;

// From:
// http://stackoverflow.com/questions/5958169/how-to-merge-two-sorted-arrays-into-a-sorted-array
// Assumption: Self and Value are sorted ascending
class function TSortedAscendingArray.Merge(const A,B:TInt32Array;
                                           AFrom,ATo,BFrom,BTo:TInteger):TInt32Array;
var k : PInteger;
begin
  result.Resize((ATo-AFrom)+(BTo-BFrom)+2);

  k:=PInteger(result);

  while (AFrom<=ATo) and (BFrom<=BTo) do
  begin
    if A[AFrom]<B[BFrom] then
    begin
      k^:=A[AFrom];
      Inc(AFrom);
    end
    else
    begin
      k^:=B[BFrom];
      Inc(BFrom);
    end;

    Inc(k);
  end;

  while AFrom<=ATo do
  begin
    k^:=A[AFrom];
    Inc(k);
    Inc(AFrom);
  end;

  while BFrom<=BTo do
  begin
    k^:=B[BFrom];
    Inc(k);
    Inc(BFrom);
  end;
end;

class function TSortedAscendingArray.Merge(const A,B: TInt32Array): TInt32Array;
begin
  result:=Merge(A,B,0,A.Count-1,0,B.Count-1);
end;

class function TSortedAscendingArray.Merge(const A, B: TInt64Array;
                                           AFrom,ATo,BFrom,BTo:TInteger): TInt64Array;
var k : PInt64;
begin
  result.Resize((ATo-AFrom)+(BTo-BFrom)+2);

  k:=PInt64(result);

  while (AFrom<=ATo) and (BFrom<=BTo) do
  begin
    if A[AFrom]<B[BFrom] then
    begin
      k^:=A[AFrom];
      Inc(AFrom);
    end
    else
    begin
      k^:=B[BFrom];
      Inc(BFrom);
    end;

    Inc(k);
  end;

  while AFrom<=ATo do
  begin
    k^:=A[AFrom];
    Inc(k);
    Inc(AFrom);
  end;

  while BFrom<=BTo do
  begin
    k^:=B[BFrom];
    Inc(k);
    Inc(BFrom);
  end;
end;

class function TSortedAscendingArray.Merge(const A,B: TInt64Array): TInt64Array;
begin
  result:=Merge(A,B,0,A.Count-1,0,B.Count-1);
end;

class function TSortedAscendingArray.Merge(const Value: array of TInt64Array): TInt64Array;

  function DoMerge(const L,H:Integer):TInt64Array;
  var t : Integer;
  begin
    result:=Value[L];

    for t:=L+1 to H do
        result:=Merge(result,Value[t]);
  end;

begin
  if Length(Value)>1 then
     result:=DoMerge(Low(Value),High(Value))
  else
  if Length(Value)>0 then
     result:=Value[0]
  else
     result:=nil;
end;

class function TSortedAscendingArray.Merge(
  const Value: array of TSingleArray): TSingleArray;

  function DoMerge(const L,H:Integer):TSingleArray;
  var t : Integer;
  begin
    result:=Value[L];

    for t:=L+1 to H do
        result:=Merge(result,Value[t]);
  end;

begin
  if Length(Value)>1 then
     result:=DoMerge(Low(Value),High(Value))
  else
  if Length(Value)>0 then
     result:=Value[0]
  else
     result:=nil;
end;

class function TSortedAscendingArray.Merge(const A, B: TSingleArray;
                                           AFrom,ATo,BFrom,BTo:TInteger): TSingleArray;
var k : TInteger;
begin
  result.Resize((ATo-AFrom)+(BTo-BFrom)+2);

  k:=0;

  while (AFrom<=ATo) and (BFrom<=BTo) do
  begin
    if A[AFrom]<B[BFrom] then
    begin
      result[k]:=A[AFrom];
      Inc(AFrom);
    end
    else
    begin
      result[k]:=B[BFrom];
      Inc(BFrom);
    end;

    Inc(k);
  end;

  while AFrom<=ATo do
  begin
    result[k]:=A[AFrom];
    Inc(k);
    Inc(AFrom);
  end;

  while BFrom<=BTo do
  begin
    result[k]:=B[BFrom];
    Inc(k);
    Inc(BFrom);
  end;
end;

class function TSortedAscendingArray.Merge(const A,B: TSingleArray): TSingleArray;
begin
  result:=Merge(A,B,0,A.Count-1,0,B.Count-1);
end;

class function TSortedAscendingArray.Merge(const A, B: TDoubleArray;
                                           AFrom,ATo,BFrom,BTo:TInteger): TDoubleArray;
var k : TInteger;
begin
  result.Resize((ATo-AFrom)+(BTo-BFrom)+2);

  k:=0;

  while (AFrom<=ATo) and (BFrom<=BTo) do
  begin
    if A[AFrom]<B[BFrom] then
    begin
      result[k]:=A[AFrom];
      Inc(AFrom);
    end
    else
    begin
      result[k]:=B[BFrom];
      Inc(BFrom);
    end;

    Inc(k);
  end;

  while AFrom<=ATo do
  begin
    result[k]:=A[AFrom];
    Inc(k);
    Inc(AFrom);
  end;

  while BFrom<=BTo do
  begin
    result[k]:=B[BFrom];
    Inc(k);
    Inc(BFrom);
  end;
end;

class function TSortedAscendingArray.Merge(const A,
  B: TDoubleArray): TDoubleArray;
begin
  result:=Merge(A,B,0,A.Count-1,0,B.Count-1);
end;

class function TSortedAscendingArray.Merge(
  const Value: array of TDoubleArray): TDoubleArray;

  function DoMerge(const L,H:Integer):TDoubleArray;
  var t : Integer;
  begin
    result:=Value[L];

    for t:=L+1 to H do
        result:=Merge(result,Value[t]);
  end;

begin
  if Length(Value)>1 then
     result:=DoMerge(Low(Value),High(Value))
  else
  if Length(Value)>0 then
     result:=Value[0]
  else
     result:=nil;
end;

{ TSortedDescendingArray }

class function TSortedDescendingArray.Merge(const A, B: TInt32Array;
                                            AFrom,ATo,BFrom,BTo:TInteger): TInt32Array;
var k : PInteger;
begin
  result.Resize((ATo-AFrom)+(BTo-BFrom)+2);

  k:=PInteger(result);

  while (AFrom<=ATo) and (BFrom<=BTo) do
  begin
    if A[AFrom]>B[BFrom] then
    begin
      k^:=A[AFrom];
      Inc(AFrom);
    end
    else
    begin
      k^:=B[BFrom];
      Inc(BFrom);
    end;

    Inc(k);
  end;

  while AFrom<=ATo do
  begin
    k^:=A[AFrom];
    Inc(k);
    Inc(AFrom);
  end;

  while BFrom<=BTo do
  begin
    k^:=B[BFrom];
    Inc(k);
    Inc(BFrom);
  end;
end;

class function TSortedDescendingArray.Merge(const A,B: TInt32Array): TInt32Array;
begin
  result:=Merge(A,B,0,A.Count-1,0,B.Count-1);
end;

class function TSortedDescendingArray.Merge(const Value: array of TInt32Array): TInt32Array;

  function DoMerge(const L,H:Integer):TInt32Array;
  var t : Integer;
  begin
    result:=Value[L];

    for t:=L+1 to H do
        result:=Merge(result,Value[t]);
  end;

begin
  if Length(Value)>1 then
     result:=DoMerge(Low(Value),High(Value))
  else
  if Length(Value)>0 then
     result:=Value[0]
  else
     result:=nil;
end;

class function TSortedDescendingArray.Merge(const A, B: TInt64Array;
                                            AFrom,ATo,BFrom,BTo:TInteger): TInt64Array;
var k : PInt64;
begin
  result.Resize((ATo-AFrom)+(BTo-BFrom)+2);

  k:=PInt64(result);

  while (AFrom<=ATo) and (BFrom<=BTo) do
  begin
    if A[AFrom]>B[BFrom] then
    begin
      k^:=A[AFrom];
      Inc(AFrom);
    end
    else
    begin
      k^:=B[BFrom];
      Inc(BFrom);
    end;

    Inc(k);
  end;

  while AFrom<=ATo do
  begin
    k^:=A[AFrom];
    Inc(k);
    Inc(AFrom);
  end;

  while BFrom<=BTo do
  begin
    k^:=B[BFrom];
    Inc(k);
    Inc(BFrom);
  end;
end;

class function TSortedDescendingArray.Merge(const A,B: TInt64Array): TInt64Array;
begin
  result:=Merge(A,B,0,A.Count-1,0,B.Count-1);
end;

class function TSortedDescendingArray.Merge(const Value: array of TInt64Array): TInt64Array;

  function DoMerge(const L,H:Integer):TInt64Array;
  var t : Integer;
  begin
    result:=Value[L];

    for t:=L+1 to H do
        result:=Merge(result,Value[t]);
  end;

begin
  if Length(Value)>1 then
     result:=DoMerge(Low(Value),High(Value))
  else
  if Length(Value)>0 then
     result:=Value[0]
  else
     result:=nil;
end;

class function TSortedDescendingArray.Merge(const A, B: TSingleArray;
                                            AFrom,ATo,BFrom,BTo:TInteger): TSingleArray;
var k : TInteger;
begin
  result.Resize((ATo-AFrom)+(BTo-BFrom)+2);

  k:=0;

  while (AFrom<=ATo) and (BFrom<=BTo) do
  begin
    if A[AFrom]>B[BFrom] then
    begin
      result[k]:=A[AFrom];
      Inc(AFrom);
    end
    else
    begin
      result[k]:=B[BFrom];
      Inc(BFrom);
    end;

    Inc(k);
  end;

  while AFrom<=ATo do
  begin
    result[k]:=A[AFrom];
    Inc(k);
    Inc(AFrom);
  end;

  while BFrom<=BTo do
  begin
    result[k]:=B[BFrom];
    Inc(k);
    Inc(BFrom);
  end;
end;

class function TSortedDescendingArray.Merge(const A,
  B: TSingleArray): TSingleArray;
begin
  result:=Merge(A,B,0,A.Count-1,0,B.Count-1);
end;

class function TSortedDescendingArray.Merge(
  const Value: array of TSingleArray): TSingleArray;

  function DoMerge(const L,H:Integer):TSingleArray;
  var t : Integer;
  begin
    result:=Value[L];

    for t:=L+1 to H do
        result:=Merge(result,Value[t]);
  end;

begin
  if Length(Value)>1 then
     result:=DoMerge(Low(Value),High(Value))
  else
  if Length(Value)>0 then
     result:=Value[0]
  else
     result:=nil;
end;

class function TSortedDescendingArray.Merge(const A, B: TDoubleArray;
                                            AFrom,ATo,BFrom,BTo: TInteger): TDoubleArray;
var k : TInteger;
begin
  result.Resize((ATo-AFrom)+(BTo-BFrom)+2);

  k:=0;

  while (AFrom<=ATo) and (BFrom<=BTo) do
  begin
    if A[AFrom]>B[BFrom] then
    begin
      result[k]:=A[AFrom];
      Inc(AFrom);
    end
    else
    begin
      result[k]:=B[BFrom];
      Inc(BFrom);
    end;

    Inc(k);
  end;

  while AFrom<=ATo do
  begin
    result[k]:=A[AFrom];
    Inc(k);
    Inc(AFrom);
  end;

  while BFrom<=BTo do
  begin
    result[k]:=B[BFrom];
    Inc(k);
    Inc(BFrom);
  end;
end;

class function TSortedDescendingArray.Merge(const A,
  B: TDoubleArray): TDoubleArray;
begin
  result:=Merge(A,B,0,A.Count-1,0,B.Count-1);
end;

class function TSortedDescendingArray.Merge(
  const Value: array of TDoubleArray): TDoubleArray;

  function DoMerge(const L,H:Integer):TDoubleArray;
  var t : Integer;
  begin
    result:=Value[L];

    for t:=L+1 to H do
        result:=Merge(result,Value[t]);
  end;

begin
  if Length(Value)>1 then
     result:=DoMerge(Low(Value),High(Value))
  else
  if Length(Value)>0 then
     result:=Value[0]
  else
     result:=nil;
end;

end.
