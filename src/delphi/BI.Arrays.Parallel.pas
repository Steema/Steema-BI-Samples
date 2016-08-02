{*********************************************}
{  TeeBI Software Library                     }
{  Array Parallel Sorting and Merge           }
{  Copyright (c) 2016 by Steema Software      }
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
    class function Merge(const A,B:TInt32Array; const BFrom,BTo:TInteger):TInt32Array; overload; static;
    class function Merge(const A,B:TInt32Array):TInt32Array; overload; inline; static;
    class function Merge(const Value:Array of TInt32Array):TInt32Array; overload; static;

    class function Merge(const A,B:TInt64Array; const BFrom,BTo:TInteger):TInt64Array; overload; static;
    class function Merge(const A,B:TInt64Array):TInt64Array; overload; inline; static;
    class function Merge(const Value:Array of TInt64Array):TInt64Array; overload; static;

    class function Merge(const A,B:TSingleArray; const BFrom,BTo:TInteger):TSingleArray; overload; static;
    class function Merge(const A,B:TSingleArray):TSingleArray; overload; inline; static;
    class function Merge(const Value:Array of TSingleArray):TSingleArray; overload; static;

    class function Merge(const A,B:TDoubleArray; const BFrom,BTo:TInteger):TDoubleArray; overload; static;
    class function Merge(const A,B:TDoubleArray):TDoubleArray; overload; inline; static;
    class function Merge(const Value:Array of TDoubleArray):TDoubleArray; overload; static;
  end;

  TSortedDescendingArray=record
  public
    class function Merge(const A,B:TInt32Array; const BFrom,BTo:TInteger):TInt32Array; overload; static;
    class function Merge(const A,B:TInt32Array):TInt32Array; overload; inline; static;
    class function Merge(const Value:Array of TInt32Array):TInt32Array; overload; static;

    class function Merge(const A,B:TInt64Array; const BFrom,BTo:TInteger):TInt64Array; overload; static;
    class function Merge(const A,B:TInt64Array):TInt64Array; overload; inline; static;
    class function Merge(const Value:Array of TInt64Array):TInt64Array; overload; static;

    class function Merge(const A,B:TSingleArray; const BFrom,BTo:TInteger):TSingleArray; overload; static;
    class function Merge(const A,B:TSingleArray):TSingleArray; overload; inline; static;
    class function Merge(const Value:Array of TSingleArray):TSingleArray; overload; static;

    class function Merge(const A,B:TDoubleArray; const BFrom,BTo:TInteger):TDoubleArray; overload; static;
    class function Merge(const A,B:TDoubleArray):TDoubleArray; overload; inline; static;
    class function Merge(const Value:Array of TDoubleArray):TDoubleArray; overload; static;
  end;

implementation
