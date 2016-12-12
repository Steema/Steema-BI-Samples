unit BI.Data.Tools;

interface

// Divide a TDataItem into 2 parts using different methods.

uses
  BI.Arrays, BI.Data;

type
  {
    TDataSplit returns two Indices arrays (A and B) with the row numbers for
    both parts of the split
  }

  TSplitBy=(Percent,Count);

  TSplitMode=(Random,Start);

  TSplitOptions=record
  public
    By : TSplitBy;
    Count : Integer;
    Mode : TSplitMode;
    Percent : Single;

    procedure Initialize;
  end;

  // Fills A and B arrays either using random values or sequential
  TDataSplit=record
    A,
    B : TNativeIntArray;

    procedure From(const ATotal:TInteger; const AOptions:TSplitOptions);
    procedure Random(const CountA, CountB:TInteger);
    procedure Sequence(const CountA, CountB:TInteger);
  end;

  // Converts Data into float values from 0 to 1
  TDataNormalize=record
  private
    class procedure NormalizeInt32(const AData:TDataItem); static;
    class procedure NormalizeInt64(const AData:TDataItem); static;
    class procedure NormalizeSingle(const AData:TDataItem); static;
    class procedure NormalizeDouble(const AData:TDataItem); static;
    class procedure NormalizeExtended(const AData:TDataItem); static;
    class procedure NormalizeDateTime(const AData:TDataItem); static;
    class procedure NormalizeBoolean(const AData:TDataItem); static;
    class procedure NormalizeText(const AData:TDataItem); static;

    class procedure Recalculate(const AData: TDataItem;
                                const AKind: TDataKind); static;
  public
    class procedure Normalize(const AData:TDataItem); overload; static;
    class procedure Normalize(const AData:TDataArray); overload; static;
  end;

implementation
