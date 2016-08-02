unit BI.Expression.DateTime;

// Optimized date-time functions for speed

// See repository:
// https://github.com/davidberneda/FastDateTime

interface

// Enable / Disable using optimized date-time functions
{$DEFINE FASTDATE}

{$IFDEF FASTDATE}

 {$IFDEF CPUX64}

  // 64bit

  {$IFDEF FPC}
   {$UNDEF FASTDATE} // FPC 64bit RTL is faster !
  {$ELSE}
   {$DEFINE FASTDAYOF}
   {$DEFINE FASTMONTHOF}
   {$DEFINE FASTDAYOFYEAR}
   {$DEFINE FASTYEAROF}
  {$ENDIF}
 {$ELSE}

  // 32bit

  {$IFNDEF FPC}
   {$DEFINE FASTDAYOF}
   {$DEFINE FASTMONTHOF}
  {$ENDIF}

  {$DEFINE FASTDAYOFYEAR}
  {$DEFINE FASTYEAROF}
{$ENDIF}

{$ENDIF}

type
  TBIDateTime=record
  private
    {$IFDEF FASTDATE}
    const
      D1 = 365;
      D4 = D1 * 4 + 1;
      D100 = D4 * 25 - 1;
      D400 = D100 * 4 + 1;

    class function CalcDayOfYear(T: Integer; out Y:Word): Word; static;
    class function CalcLeap(T: Integer; out D:Word): Boolean; inline; static;
    class function DateTimeToDateStamp(const DateTime: TDateTime): Integer; static;
    class function DayMonth(const T:Integer; out M:Byte): Word; overload; static;
    {$ENDIF}
  public
    class function DayOf(const DateTime: TDateTime): Word; {$IFNDEF FASTDAYOF}inline;{$ENDIF} static;
    class function DayOfTheYear(const DateTime: TDateTime):Word; {$IFNDEF FASTDAYOFYEAR}inline;{$ENDIF} static;
    class function MonthOf(const DateTime: TDateTime): Byte; {$IFNDEF FASTMONTHOF}inline;{$ENDIF} static;
    class function YearOf(const DateTime: TDateTime): Word; {$IFNDEF FASTYEAROF}inline;{$ENDIF} static;
  end;

implementation
