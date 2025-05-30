{*********************************************}
{  TeeBI Software Library                     }
{  CSV (TSV) data import and export           }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.CSV;

interface

uses
  System.Classes, System.Types,
  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.Diagnostics,
  {$ENDIF}
  System.SysUtils, BI.DataItem, BI.DataSource, BI.Arrays, BI.Arrays.Strings,
  BI.Persist;

type
  EBICSV=class(EBIException);

  TTextHeaders=(Auto,Yes,No);

  TCSVHeader=record
  private
    Detected : Boolean;
    HasHeader : TTextHeaders;
  public
    Count : Integer;
    Headers : TTextHeaders;
  end;

  TBICSV=class(TBITextSource)
  private
    DeltaCapacity : Integer;

    tmpCount,
    tmpCurrent : TInteger;

    PendingGuess : Boolean;

    Data : TDataItem;
    Start : TInteger;

    FItems : TDataItems;

    t1 : TStopwatch;

    class function BISplitString(const AString:String; const ADelimiter,AQuote:Char):TStringArray;
    function Dequoted(const S:String):String;
    procedure GuessDataKinds(const AData:TDataItem; const Values:TStringArray);
    function GuessDelimiter(const AText:String):TStringArray;
    function GuessHeaders(const AString:String):Boolean;

    function FinishRowByRow(const ACount:TInteger):TDataArray;
    procedure InitRowByRow;
    procedure LoadSettings(const Definition:TDataDefinition);
    procedure RowByRow(const AString:String; var ARow:TInteger);
  protected
    function InternalImportFile(const FileName:String):TDataArray; override;
  public
  const
    AutomaticDelimiter=#0;

  var
    Delimiter : Char;
    Quote : Char;

    Header : TCSVHeader;

    Constructor Create(const Definition:TDataDefinition=nil; const MultiThread:Boolean=False); override;

    class function FileFilter:TFileFilters; override;

    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; override;
    function ImportFile(const AFileName:String):TDataArray; override;
    function ImportText(const AText:String):TDataItem;

    class function Supports(const Extension:String):Boolean; override;
  end;

  TBICSVExport=class(TBITextExport)
  private
    {$IFDEF FPC}
    IItems : TStrings;
    procedure FpcAddRows(const AIndex:TInteger);
    {$ENDIF}

    function Headers(const Prefix:String; const AItems:TDataArray):String;
    function Quoted(const S:String):String; inline;
    function Row(const AIndex:TInteger; const AItems:TDataArray):String;
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    Delimiter : Char;
    Header : Boolean;
    Quote : Char;

    Constructor Create; override;

    class function Supports(const Extension:String):Boolean; override;

    class function FileFilter:TFileFilters; override;
  end;

implementation

{.$DEFINE BISTREAMREADER} // <-- Faster than TStreamReader

uses
  {$IFNDEF FPC}
  System.StrUtils,
  {$ENDIF}

  {$IFDEF BISTREAMREADER}
  BI.Streams.Reader,
  {$ENDIF}

  BI.Languages.English;

{ TBICSV }

Constructor TBICSV.Create(const Definition:TDataDefinition; const MultiThread: Boolean);
begin
  inherited;

  Quote:='"';
  Delimiter:=TBICSV.AutomaticDelimiter;
  MissingValue:='';

  Header.Headers:=TTextHeaders.Auto;
  Header.Count:=1;

  if Definition<>nil then
     LoadSettings(Definition);
end;

procedure TBICSV.LoadSettings(const Definition:TDataDefinition);
var tmp : String;
begin
  tmp:=Definition['Quote'];

  if tmp<>'' then
     Quote:=tmp[1];

  tmp:=Definition['Delimiter'];

  if tmp<>'' then
     Delimiter:=tmp[1];

  tmp:=Definition['HeaderCount'];

  if tmp<>'' then
     Header.Count:=StrToIntDef(tmp,Header.Count);

  tmp:=Definition['Headers'];

  if SameText(tmp,'Yes') then
     Header.Headers:=TTextHeaders.Yes
  else
  if SameText(tmp,'No') then
     Header.Headers:=TTextHeaders.No;
end;

function TBICSV.Import(const Folder:String; Recursive: Boolean): TDataArray;
var tmp : String;
begin
  tmp:=Trim(IncludePattern);

  if tmp='' then
     tmp:='*.csv';

  result:=Import(Folder,tmp,ExcludePattern,Recursive);
end;

function TBICSV.Dequoted(const S:String):String;
begin
  result:={$IFDEF FPC}AnsiDequotedStr(Trim(S),Quote){$ELSE}S.Trim.DeQuotedString(Quote){$ENDIF};
end;

{$DEFINE BIBATCH}

function TBICSV.InternalImportFile(const FileName: String): TDataArray;

  // Expand Data size if necessary
  procedure EnsureCount(const ACount:TInteger);
  begin
    if Data.Count<ACount then
    begin
      Data.Resize(ACount+DeltaCapacity);
      Inc(DeltaCapacity,128);
      tmpCount:=Data.Count;
    end;
  end;

var tmpR : {$IFDEF BISTREAMREADER}TBIStreamReader{$ELSE}TStreamReader{$ENDIF};

    tmpLength,
    tmpPercent,
    OldPercent : Integer;

    tmpPos,
    tmpSize : Int64;
    tmpCancel : Boolean;

    tmpRow : TInteger;

    {$IFDEF BIBATCH}
    t : Integer;
    tmpStrings : Array[0..1023] of String;
    tmpS : String;
    tmpBatchCount : Integer;
    {$ELSE}
    tmpS : String;
    {$ENDIF}
begin
  tmpCancel:=False;
  DoProgress(0,tmpCancel);

  if not tmpCancel then
  begin
    tmpSize:=GetFileSize(FileName);
    OldPercent:=0;
    tmpPos:=0;

    tmpR:= {$IFDEF BISTREAMREADER}TBIStreamReader{$ELSE}TStreamReader{$ENDIF}.{$IFDEF FPC}CreateFile{$ELSE}Create{$ENDIF}(FileName
           {$IFDEF FPC}){$ELSE}, TEncoding.Default, True){$ENDIF};
    try
      InitRowByRow;

      tmpRow:=0;

      while not tmpR.EndOfStream do
      begin
        {$IFDEF BIBATCH}
        // Speed Test: Fill a batch of strings and process them all

        tmpBatchCount:=0;
        while not tmpR.EndOfStream do
        begin
          tmpS:=Trim(tmpR.ReadLine);

          if tmpS<>'' then
          begin
            tmpStrings[tmpBatchCount]:=tmpS;
            Inc(tmpBatchCount);

            if tmpBatchCount>1024-1 then
               break;
          end;
        end;

        if (tmpCount=0) and (tmpBatchCount>0) then
        begin
          tmpLength:=Length(tmpStrings[tmpBatchCount-1]);

          if tmpLength>0 then
             EnsureCount(tmpSize div tmpLength)
          else
             EnsureCount(tmpRow+tmpBatchCount);
        end
        else
           EnsureCount(tmpRow+tmpBatchCount);

        for t:=0 to tmpBatchCount-1 do
        begin
          RowByRow(tmpStrings[t],tmpRow);
          Inc(tmpRow);
        end;

        {$ELSE}
        tmpS:=Trim(tmpR.ReadLine);

        if tmpS<>'' then
        begin
          EnsureCount(tmpRow+1);
          RowByRow(tmpS,tmpRow);
          Inc(tmpRow);
        end;
        {$ENDIF}

        if Assigned(OnProgress) then
        begin
          {$IFDEF BIBATCH}
          for t:=0 to tmpBatchCount-1 do
              Inc(tmpPos,Length(tmpStrings[t]));
          {$ELSE}
          Inc(tmpPos,Length(tmpS)); // <-- do not * SizeOf(Char)
          {$ENDIF}

          tmpPercent:=Round(100*tmpPos/tmpSize);

          if tmpPercent<>OldPercent then
          begin
            OldPercent:=tmpPercent;
            OnProgress(Self,OldPercent,tmpCancel);

            if tmpCancel then
               break;
          end;
        end;
      end;

      if tmpCancel then
      begin
        Data.Free;
        Data:=nil;
        result:=nil;
      end
      else
        result:=FinishRowByRow(tmpRow);
    finally
      tmpR.Free;
    end;
  end;

  DoProgress(100,tmpCancel);
end;

// Try to recognize the Delimiter character that separates fields in AText
function TBICSV.GuessDelimiter(const AText:String):TStringArray;

  procedure TryDelimiter(const ADelimiter:Char);
  var L,
      tmp : Integer;
      tmpResult : TStringArray;
  begin
    tmp:=Length(result);

    tmpResult:=BISplitString(AText,ADelimiter,Quote);

    L:=Length(tmpResult);

    if L>tmp then
    begin
      if L>1 then
         Delimiter:=ADelimiter;

      result:=tmpResult;
    end;
  end;

begin
  result:=nil;

  if Delimiter=TBICSV.AutomaticDelimiter then
  begin
    TryDelimiter(',');
    TryDelimiter(#9);
    TryDelimiter(';');

    if Delimiter=TBICSV.AutomaticDelimiter then
       TryDelimiter(' ');
  end
  else
    TryDelimiter(Delimiter);
end;

{$IFNDEF FPC}
function IsQuoted(const S:String):Boolean;
begin
  result:=(S.StartsWith('"') and S.EndsWith('"')) or
          (S.StartsWith(''') and S.EndsWith('''));
end;
{$ENDIF}

function IsText(const S:String):Boolean;
var DummyDate : TDateTime;
begin
  result:=(not TStringToFloat.TryConvert(S)) and
          (not TryStrToDateTime(S,DummyDate));
end;

function AreTitles(const Fields:TStringArray):Boolean;
var S : String;
begin
  for S in Fields do
      if (not IsQuoted(S)) and (not IsText(S)) then
          Exit(False);

  result:=True;
end;

type
  TDataItemsAccess=class(TDataItems);
  TDataAccess=class(TDataItem);

function TBICSV.GuessHeaders(const AString:String):Boolean;
var tmpHeader : String;
    Fields : TStringArray;

    t,
    n : Integer;

    tmpData : TDataItem;
begin
  result:=False;

  if Header.Headers=TTextHeaders.Auto then
     tmpHeader:=Trim(AString)
  else
  if (tmpCurrent=Header.Count-1) and (Header.Count>0) then
     tmpHeader:=Trim(AString)
  else
     tmpHeader:='';

  if tmpHeader<>'' then
  begin
    // Bottleneck;
    //Fields:=Header.Split([Delimiter],Quote,Quote,TStringSplitOptions.None);

    Header.HasHeader:=Header.Headers;

    Fields:=GuessDelimiter(tmpHeader);

    n:=Length(Fields);

    if Header.HasHeader<>TTextHeaders.Auto then
       if n=0 then
          if not CallOnError(BIMsg_CSV_FirstRowNoFields) then
             raise EBICSV.Create(BIMsg_CSV_FirstRowNoFields);

    if Header.HasHeader=TTextHeaders.Auto then
    begin
      if (n=0) or (not AreTitles(Fields)) then
      begin
        Header.HasHeader:=TTextHeaders.No;
        Header.Count:=0;

        for t:=0 to High(Fields) do
            Fields[t]:='Item_'+IntToStr(t);
      end
      else
      begin
        Header.HasHeader:=TTextHeaders.Yes;
        Header.Count:=tmpCurrent; //tmpHeaderIndex+1;
      end;
    end;

    for t:=0 to High(Fields) do
    begin
      tmpData:=TDataItem.Create;

      // Expensive, but necessary.
      // Fix duplicate field names (due to lower/upper case, etc)
      tmpData.Name:=FItems.UniqueName(Dequoted(Fields[t]));

      TDataItemsAccess(FItems).AddDirect(tmpData); // <-- speed opt
    end;

    if tmpCount>0 then
       if Header.HasHeader=TTextHeaders.Yes then
          TDataAccess(Data).FCount:=tmpCount-Header.Count
       else
          TDataAccess(Data).FCount:=tmpCount-1;

    result:=True;
  end;
end;

procedure TBICSV.GuessDataKinds(const AData:TDataItem; const Values:TStringArray);
var t : Integer;
begin
  for t:=0 to High(Values) do
      TDataAccess(AData.Items[t]).FKind:=GuessKind(Values[t]);
end;

class function TBICSV.BISplitString(const AString:String; const ADelimiter,AQuote:Char):TStringArray;
begin
  result:=TStringArray.Split(AString,ADelimiter,AQuote);
end;

procedure TBICSV.RowByRow(const AString:String; var ARow:TInteger);

  procedure DoGuessKinds;
  var Values : TStringArray;
      t : Integer;
  begin
    // Bottleneck:
    Values:=BISplitString(AString,Delimiter,Quote);

    // Add potential missing header columns
    for t:=FItems.Count to High(Values) do
        FItems.Add('Unknown '+IntToStr(t),dkUnknown);

    GuessDataKinds(Data,Values);

    if tmpCount>Start then
       Data.Resize(tmpCount-Start);
  end;

  procedure DoImportLine;
  var
    Pos : Integer;
    Value : String;

    procedure AddItem;

      procedure AddNewData; // <-- local proc to avoid always call to StrArrayClr
      begin
        FItems.Add('Unknown '+IntToStr(Pos),dkUnknown);
      end;

    var tmp : TDataItem;
    begin
      Inc(Pos);

      if FItems.Count<=Pos then
         AddNewData;

      tmp:=FItems[Pos];

      // For special cases, try to set data Kind now:
      if tmp.Kind=TDataKind.dkUnknown then
      begin
        TDataAccess(tmp).FKind:=GuessKind(Value);
        tmp.Resize(Data.Count);
      end;

      SetColumn(tmp,ARow,Value);
    end;

    // Experiment using PChar (not portable). No big gain.
    {.$DEFINE PARSEPCHAR}

    {$IFDEF PARSEPCHAR}
    procedure DoParseLinePChar;
    var InQuote : Boolean;
        c  : PChar;
        ValueStart : PChar;
    begin
      InQuote:=False;

      c:=PChar(AString);
      ValueStart:=c;

      while c^>#0 do
      begin
        if c^=Quote then
        begin
          if InQuote then
             InQuote:=False
          else
          begin
            InQuote:=True;
            ValueStart:=c+1;
          end;
        end
        else
        if (not InQuote) and (c^=Delimiter) then
        begin
          SetString(Value,ValueStart,c-ValueStart);
          AddItem;
          ValueStart:=c+1;
        end;

        Inc(c);
      end;

      if c>ValueStart then
      begin
        SetString(Value,ValueStart,c-ValueStart);
        AddItem;
      end;
    end;

    {$ELSE}
    procedure DoParseLine;
    var L,
        i : Integer;
        InQuote : Boolean;
        c : Char;

        ValueStart,
        ValueSize : Integer;
    begin
      i:=1;
      InQuote:=False;

      ValueSize:=0;
      ValueStart:=i;

      L:=Length(AString);

      while i<=L do
      begin
        c:=AString[i];

        if c=Quote then
        begin
          if InQuote then
             InQuote:=False
          else
          begin
            InQuote:=True;

            ValueSize:=0;
            ValueStart:=i+1;
          end;
        end
        else
        if (not InQuote) and (c=Delimiter) then
        begin
          Value:=Copy(AString,ValueStart,ValueSize); // <-- bottleneck
          AddItem;

          if Delimiter=' ' then
             while i+1<=L do
               if AString[i+1]=Delimiter then
                  Inc(i)
               else
                  break;

          ValueSize:=0;
          ValueStart:=i+1;
        end
        else
          Inc(ValueSize);

        Inc(i);
      end;

      if ValueSize>0 then
      begin
        Value:=Copy(AString,ValueStart,ValueSize);
        AddItem;
      end;
    end;
    {$ENDIF}

  begin
    if PendingGuess then
    begin
      if Delimiter=TBICSV.AutomaticDelimiter then
         GuessDelimiter(AString);

      DoGuessKinds;
      PendingGuess:=False;
    end;

    // Split String again here inline (without calling BISplitString) for
    // VITAL performance:
    // Assert(AString<>'')

    if Length(AString)>0 then
    begin
      Pos:=-1;

      {$IFDEF PARSEPCHAR}
      DoParseLinePChar;
      {$ELSE}
      DoParseLine;
      {$ENDIF}
    end;
  end;

begin
  if Header.Detected then
     DoImportLine
  else
  begin
    if Header.Count=0 then
    begin
      Header.HasHeader:=TTextHeaders.No;
      Header.Detected:=True;
    end
    else
    begin
      if Header.Count=tmpCurrent+1 then
         Header.Detected:=GuessHeaders(AString);
    end;

    if Header.Detected then
    begin
      Start:=tmpCurrent;
      PendingGuess:=True;

      if Header.HasHeader=TTextHeaders.No then
      try
        ARow:=0;
        DoImportLine;

      except
        on Exception do
        begin
          Data.Free;
          raise;
        end;

      end
      else
        ARow:=-1;
    end;
  end;
end;

procedure TBICSV.InitRowByRow;
begin
  Data:=TDataItem.Create(True);

  // Important speed opt, cache FItems
  FItems:=Data.Items;

  t1:=TStopwatch.StartNew;

  DecimalDetected:=False;
  Header.Detected:=False;

  PendingGuess:=True;
  tmpCurrent:=0;
  DeltaCapacity:=128;
end;

function TBICSV.FinishRowByRow(const ACount:TInteger):TDataArray;
begin
  Data.Resize(ACount);
  Data.History.Times.LoadingData:=t1.ElapsedMilliseconds;

  SetLength(result,1);
  result[0]:=Data;
end;

function TBICSV.Import(const Strings:TStrings):TDataArray;
var tmpCancel : Boolean;
    tmpS : String;
    tmpRow : TInteger;
begin
  tmpCancel:=False;
  DoProgress(0,tmpCancel);

  if Assigned(Strings) and (Strings.Count>0) then
  begin
    InitRowByRow;

    tmpCount:=Strings.Count;

    tmpRow:=0;

    Data.Resize(tmpCount);

    while tmpCurrent<tmpCount do
    begin
      tmpS:=Trim(Strings[tmpCurrent]);

      if tmpS<>'' then
      begin
        RowByRow(tmpS,tmpRow);
        Inc(tmpRow);
      end;

      if Assigned(OnProgress) then
         DoProgress(100*tmpCurrent/tmpCount,tmpCancel);

      if tmpCancel then
      begin
        Data.Free;
        result:=nil;
        Exit;
      end;

      Inc(tmpCurrent);
    end;

    result:=FinishRowByRow(tmpRow);
  end
  else
    result:=nil;

  DoProgress(100,tmpCancel);
end;

function TBICSV.ImportFile(const AFileName: String): TDataArray;
begin
  result:=DoImportFile(AFileName);
end;

function TBICSV.ImportText(const AText: String): TDataItem;
var tmp : TStrings;
begin
  tmp:=TStringList.Create;
  try
    tmp.Text:=AText;
    result:=TBISource.FromData(Import(tmp));
  finally
    tmp.Free;
  end;
end;

class function TBICSV.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('CSV files','*.csv');
  result.Add('TSV files','*.tsv');
  result.Add('Text files','*.txt');
end;

class function TBICSV.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.CSV') or
          SameText(Extension,'.TXT') or
          SameText(Extension,'.TSV');
end;

{ TBICSVExport }

Constructor TBICSVExport.Create;
begin
  inherited;

  Header:=True;
  Quote:='"';
  Delimiter:=',';
end;

function TBICSVExport.Quoted(const S:String):String;
begin
  if Quote=#0 then
     result:=S
  else
     result:=Quote+S+Quote;
end;

function TBICSVExport.Row(const AIndex:TInteger; const AItems:TDataArray):String;
var H, t : Integer;
    tmp : String;
    Col : TDataItem;
begin
  result:='';

  H:=High(AItems);

  for t:=0 to H do
  begin
    Col:=AItems[t];

    if Col.AsTable then
    begin
      if Col.Master=nil then
         tmp:=Row(AIndex,Col.Items.AsArray)
      else
         tmp:='';
    end
    else
    if Col.Kind=TDataKind.dkUnknown then
       if Col.Items.Count>AIndex then
          tmp:=Quoted(Col.Items[AIndex].Name)
       else
          tmp:=''
    else
    begin
      tmp:=DataToString(Col,AIndex);

      if Col.Kind=dkText then
         tmp:=Quoted(tmp);
    end;

    result:=result+tmp;

    if t<H then
       result:=result+Delimiter;
  end;
end;

class function TBICSVExport.Supports(const Extension: String): Boolean;
begin
  result:=TBICSV.Supports(Extension);
end;

function TBICSVExport.Headers(const Prefix:String; const AItems:TDataArray):String;

  function Prefixed(const S:String):String;
  begin
    if Prefix='' then
       result:=S
    else
       result:=Prefix+'.'+S;
  end;

var H, t : Integer;
  tmp : String;
  Col : TDataItem;
begin
  result:='';

  H:=High(AItems);

  for t:=0 to H do
  begin
    Col:=AItems[t];

    if Col.AsTable and (Col.Master=nil) then
       tmp:=Headers(Prefixed(Col.Name),Col.Items.AsArray)
    else
       tmp:=Quoted(Prefixed(Col.Name));

    result:=result+tmp;

    if t<H then
       result:=result+Delimiter;
  end;
end;

{$IFDEF FPC}
procedure TBICSVExport.FpcAddRows(const AIndex:TInteger);
begin
  IItems.Add(Row(AIndex,Items));
end;
{$ENDIF}

procedure TBICSVExport.DoEmit(const AItems: TStrings);
var OldDecimal : Char;
begin
  OldDecimal:=FormatSettings.DecimalSeparator;
  try
    if FormatSettings.DecimalSeparator=Delimiter then
       FormatSettings.DecimalSeparator:='.';

    if Header then
       AItems.Add(Headers('',Items));

    if not SchemaOnly then
    begin
      {$IFDEF FPC}
      IItems:=AItems;
      {$ENDIF}

      Cursor.Loop(
         {$IFDEF FPC}
         FpcAddRows
         {$ELSE}
         procedure(const AIndex:TInteger)
         begin
           AItems.Add(Row(AIndex,Items));
         end
         {$ENDIF}
         );
    end;

  finally
    FormatSettings.DecimalSeparator:=OldDecimal;
  end;
end;

class function TBICSVExport.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('CSV (Comma separated values)','*.csv');
  result.Add('TSV (Tab separated values)','*.tsv');
end;

initialization
  TBIFileImporters.RegisterClass(TBICSV);
  TBIExporters.RegisterClass(TBICSVExport);
finalization
  TBIExporters.UnRegisterClass(TBICSVExport);
  TBIFileImporters.UnRegisterClass(TBICSV);
end.
