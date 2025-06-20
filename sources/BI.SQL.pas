{*********************************************}
{  TeeBI Software Library                     }
{  SQL Language support                       }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.SQL;

interface

uses
  BI.Arrays, BI.DataItem, BI.DataSource, BI.Summary, BI.Expression,
  BI.Persist, BI.Expressions, BI.Arrays.Strings;

type
  ESQLParser=class(EBIException);

  TGetDataProc=
    {$IFDEF FPC}
    procedure(const AName:String; out AData:TDataItem) of object;
    {$ELSE}
    reference to procedure(const AName:String; out AData:TDataItem);
    {$ENDIF}

  // Converts an SQL string to its equivalent query (TDataSelect) or
  // summary (TSummary) object instance
  TSQLParser=class
  private
    FData : TDataItem;
    FError : TBIErrorProc;
    FOnGetData : TGetDataProc;
    FText : String;

    ILength,
    IPos : Integer;

    function CallDoError(const Sender:TObject; const Error:String):Boolean;
    class function DataOf(const AParent:TDataItem; const AData:String;
                          out AExp:TExpression):TDataItem; static;
    procedure DoError(const AError:String);
    function EndOfText:Boolean;
    function ErrorProc(const APos:Integer; const AMessage:String):Boolean;
    function GetExpression: String;
    function GetExpressions:TTextArray;
    class function IgnoreError(const Sender:TObject; const Error:String):Boolean;
    function NextIdent:String;
    function Optional(const AText:String):Boolean;
    function ParseWhere(const AData:TDataItem; const AWhere:String):TExpression;
    procedure SkipDelimiters;

    {$IFDEF FPC}
    function SkipError(const APos:Integer; const AMessage:String):Boolean;
    {$ELSE}
    class function SkipError(const APos:Integer; const AMessage:String):Boolean; static;
    {$ENDIF}
  protected
    class function DataFromString(const AData:TDataItem; const S:String):TDataItem; static;
  public
    Constructor Create(const AData:TDataItem; const AText:String);

    function Calculate(const ErrorProc:TBIErrorProc=nil):TDataItem;
    class function DataFrom(const AProvider:TDataProvider): TDataItem; static;

    class function FindAggregate(var S:String; out Agg:TAggregate):Boolean; static;
    class function FindGroupByPart(var S:String; out APart:TDateTimePart):Boolean; static;
    class function GetFunction(var S:String; out AFunc:String):Boolean; static;

    function Parse(const ErrorProc:TBIErrorProc=nil):TDataProvider;

    class procedure ParseSort(const AData:TDataItem; var ASort:TSortItems; const AOrder:TTextArray;
                              const SQL:Boolean=False;
                              const Error:TBIErrorProc=nil); static;

    class function StringToData(const AData:TDataItem; const S:String; const ErrorProc:TBIErrorProc=nil):TDataItem; static;

    property OnGetData:TGetDataProc read FOnGetData write FOnGetData;
  end;

  // Converts a query (TDataSelect) or summary (TSummary) object instance into
  // its equivalent SQL string
  TBISQL=record
  private
    class function FilterOf(const AFilter:TExpression):String; static;

    // Return the "equivalent" SQL script:
    class function From(const ASelect:TDataSelect):String; overload; static;

    // Return the "equivalent" SQL script:
    class function From(const ASummary:TSummary):String; overload; static;

    class function From(const AMain:TDataItem; const AData:TDataArray):String; overload; static;

    class function FromItems(const AMain:TDataItem; const AData:TDataArray):TDataArray; static;

    class function FromWhereFilter(const AMain:TDataItem;
                                   const AFilter:TExpression;
                                   const AData:TDataArray): String; static;

    class function Where(const AMain:TDataItem; const AData:TDataArray):String; static;
  public
    // Return the "equivalent" SQL script:
    class function From(const AProvider:TDataProvider):String; overload; static;

    // Parse SQL syntax, execute and return result
    class function From(const AData:TDataItem; const SQL:String;
                        const GetData:TGetDataProc=nil;
                        const ErrorProc:TBIErrorProc=nil):TDataItem; overload; static;

    // Parse SQL syntax and return provider
    class function ProviderFrom(const AData:TDataItem; const SQL:String;
                        const GetData:TGetDataProc=nil;
                        const ErrorProc:TBIErrorProc=nil):TDataProvider; overload; static;

    // Return SQL from AData Provider:
    class function From(const AData:TDataItem):String; overload; static;
  end;

implementation

uses
  {$IFDEF FPC}
  BI.FPC,
  {$ENDIF}
  {System.}SysUtils, BI.Languages.English, BI.Query;

function IsQuoted(const S:String):Boolean;
begin
  result:=(Copy(S,1,1)='"') and (Copy(S,Length(S),1)='"');
end;

function EscapedName(const AName:String):String; overload;
begin
  if {(not IsQuoted(AName)) and} (Pos(' ',AName)>0) then
     result:='{'+AName+'}'
  else
     result:=AName;
end;

function EscapedName(const AData:TDataItem):String; overload;
begin
  result:=AData.Name;

  if not (AData is TExpressionColumn) then
     result:=EscapedName(result);
end;

function ParentAndName(const AItem:TDataItem):String;
begin
  if AItem=nil then
     result:='?' // raise ?
  else
  if (AItem.Parent=nil) or (AItem.Parent.Name='') then
     result:=EscapedName(AItem)
  else
     result:=EscapedName(AItem.Parent)+'.'+EscapedName(AItem);
end;

class function TBISQL.Where(const AMain:TDataItem; const AData:TDataArray):String;

  function FindDetail(const AItem:TDataItem):TDataItem;
  var tmp : TDataItem;
  begin
    // Pending: Use TDataHops to find the master-detail link between AMain and AItem

    if AMain<>nil then
    for tmp in AMain.Items.AsArray do
        if (tmp.Master<>nil) and (tmp.Master.Parent=AItem) then
           Exit(tmp);

    result:=nil;
  end;

  function LinkToData(const AItem:TDataItem):String;
  var tmp : TDataItem;
  begin
    tmp:=FindDetail(AItem);

    if tmp=nil then
       result:='' // raise !
    else
       result:=ParentAndName(tmp.Master)+'='+ParentAndName(tmp);
  end;

var t : Integer;
    tmpLink : String;
begin
  result:='';

  for t:=0 to AData.Count-1 do
  begin
    tmpLink:=LinkToData(AData[t]);

    if tmpLink<>'' then
    begin
      if result<>'' then
         result:=result+' and ';

      if AData.Count=1 then
         result:=result+tmpLink
      else
         result:=result+'('+tmpLink+')';
    end;
  end;
end;

class function TBISQL.FromItems(const AMain:TDataItem; const AData:TDataArray):TDataArray;

  function IsValidFrom(const AData:TDataItem):Boolean;
  begin
    result:=(AData<>nil) and (AData<>AMain) and (not AData.IsChildOf(AMain));
  end;

var t : Integer;
    tmp : TDataItem;
begin
  result:=nil;

  result.Add(AMain);

  for t:=0 to AData.Count-1 do
  begin
    tmp:=AData[t];

    if tmp.Kind<>TDataKind.dkUnknown then
       tmp:=tmp.Parent;

    if IsValidFrom(tmp) then
       if not result.Exists(tmp) then
          result.Add(tmp);
  end;
end;

class function TBISQL.From(const AMain:TDataItem; const AData:TDataArray):String;
var t,
    tmpCount : Integer;
begin
  tmpCount:=AData.Count;

  if tmpCount=0 then
     result:=' from ?'
  else
  begin
    result:=' from ';

    for t:=0 to tmpCount-1 do
    begin
      if t>0 then
         result:=result+', ';

      result:=result+From(AData[t]);
    end;
  end;
end;

function NameOfItem(const AMain,AItem:TDataItem; var AData:TDataArray):String;
begin
  if (AItem.Parent<>nil) and (AItem.Parent=AMain) then
     result:=EscapedName(AItem)
  else
  if AItem.Parent=nil then
     result:=EscapedName(AItem)
  else
  begin
    if not AData.Exists(AItem.Parent) then
       AData.Add(AItem.Parent);

    result:=ParentAndName(AItem);
  end;
end;

class function TBISQL.FilterOf(const AFilter:TExpression):String;
begin
  if AFilter=nil then
     result:=''
  else
     result:=AFilter.ToString;
end;

{ TBISQL }

function AsName(const AName:String):String;
begin
  if AName='' then
     result:=''
  else
     result:=' as '+EscapedName(AName);
end;

class function TBISQL.FromWhereFilter(const AMain:TDataItem;
                                      const AFilter:TExpression;
                                      const AData:TDataArray): String;
var tmpFilter,
    tmpWhere : String;
begin
  result:=From(AMain,AData);

  tmpWhere:=Where(AMain,AData);

  if tmpWhere<>'' then
     result:=result+' where '+tmpWhere;

  tmpFilter:=FilterOf(AFilter);

  if tmpFilter<>'' then
     if tmpWhere='' then
        result:=result+' where '+tmpFilter
     else
        result:=result+' and ('+tmpFilter+')';
end;

class function TBISQL.From(const ASelect: TDataSelect): String;
var
  tmpData : TDataArray;

  function OrderBy(const ASort:TSortItems):String;
  var t,
      tmpCount : Integer;
      tmp : String;
  begin
    result:='';

    tmpCount:=0;

    for t:=0 to High(ASort.Items) do
    if ASort.Items[t].Active then
    begin
      if tmpCount=0 then
         result:='order by '
      else
         result:=result+', ';

      if ASort.Items[t].Data is TExpressionColumn then
         tmp:=TExpressionColumn(ASort.Items[t].Data).Expression.ToString
      else
         tmp:=NameOfItem(ASelect.Data,ASort.Items[t].Data,tmpData);

      result:=result+tmp;

      if ASort.Items[t].Descending then
         result:=result+' desc';

      Inc(tmpCount);
    end;
  end;

  function ActiveSelectName(const AIndex:Integer):String;
  var tmp : Integer;
      tmpItem : TDataCursorItem;
  begin
    tmp:=0;

    for tmpItem in ASelect.Items do
        if tmpItem.Active then
        begin
          if tmp=AIndex then
             Exit(tmpItem.Name);

          Inc(tmp);
        end;

    result:='';
  end;

  function SelectedItems:String;

    function ItemToString(const AIndex:Integer; const AItem:TDataItem):String;
    begin
      if ASelect.Data=AItem then
         result:=EscapedName(AItem)+'.*'
      else
         result:=NameOfItem(ASelect.Data,AItem,tmpData)+AsName(ActiveSelectName(AIndex));
    end;

  var t,
      L : Integer;
  begin
    L:=Length(tmpData);

    if (L=1) and tmpData[0].AsTable then
       result:=' *'
    else
    begin
      result:='';

      for t:=0 to L-1 do
      begin
        result:=result+' '+ItemToString(t,tmpData[t]);

        if t<L-1 then
           result:=result+',';
      end;
    end;
  end;

var tmp : TDataItem;
    tmpFrom : TDataArray;
begin
  if (ASelect=nil) or (Length(ASelect.DataItems)=0) then
     result:=''
  else
  begin
    result:='select';

    if ASelect.Distinct then
       result:=result+' distinct';

    tmp:=ASelect.MainData;

    tmpData:=ASelect.DataItems;

    result:=result+SelectedItems;

    tmpFrom:=FromItems(tmp,tmpData);

    result:=result+FromWhereFilter(tmp,ASelect.Filter,tmpFrom);

    if ASelect.SortBy.Items<>nil then
       result:=result+' '+OrderBy(ASelect.SortBy);

    if ASelect.Max>0 then
       result:=result+' limit '+IntToStr(ASelect.Max);

    if ASelect.Start>0 then
       result:=result+' offset '+IntToStr(ASelect.Start);
  end;
end;

class function TBISQL.From(const ASummary: TSummary): String;
var
  tmpData : TDataArray;

  function MeasureOf(const AMeasure:TMeasure):String;
  var tmp : TDataItem;
  begin
    result:=AMeasure.Aggregate.ToString+'(';

    tmp:=AMeasure.RealData;

    if tmp=nil then
    begin
      if AMeasure.Expression<>nil then
         result:=result+AMeasure.Expression.ToString
    end
    else
    if tmp.AsTable then
       result:=result+'*'
    else
       result:=result+NameOfItem(ASummary.MainData,tmp,tmpData);

    result:=result+')'+AsName(AMeasure.Name);
  end;

  function GroupBy(const By:TGroupBys; const AddAsName:Boolean):String;
  var t : Integer;
      tmp : String;
      tmpReal : TDataItem;
  begin
    result:='';

    for t:=0 to By.Count-1 do
    begin
      if t>0 then
         result:=result+',';

      tmpReal:=By[t].RealData;

      if tmpReal=nil then
      begin
        if By[t].Expression<>nil then
           tmp:='('+By[t].Expression.ToString+')';
      end
      else
         tmp:=NameOfItem(ASummary.MainData,tmpReal,tmpData);

      if By[t].DatePart=TDateTimePart.None then
         result:=result+' '+tmp
      else
         result:=result+' '+By[t].DatePart.ToString+'('+tmp+')';

      if AddAsName then
         result:=result+AsName(By[t].Name);
    end;
  end;

var t : Integer;
begin
  if (ASummary=nil) or ((ASummary.Measures.Count=0) and (ASummary.By.Count=0)) then
     result:=''
  else
  begin
    ASummary.Prepare;

    tmpData.Add(ASummary.MainData);

    result:='select';

    if ASummary.By.Count>0 then
       result:=result+GroupBy(ASummary.By,True);

    if ASummary.Measures.Count>0 then
    begin
      if ASummary.By.Count>0 then
         result:=result+',';

      for t:=0 to ASummary.Measures.Count-1 do
      begin
        if t>0 then
           result:=result+',';

        result:=result+' '+MeasureOf(ASummary.Measures[t]);
      end;
    end;

    result:=result+FromWhereFilter(ASummary.MainData,ASummary.Filter,tmpData);

    // Add pending hops:
    {
    for t:=0 to High(ASummary.Hops.Data) do
        if not tmpData.Exists(ASummary.Hops.Data[t]) then
           tmpData.Add(ASummary.Hops.Data[t]);
    }


    if ASummary.By.Count>0 then
       result:=result+' group by'+GroupBy(ASummary.By,False);
  end;
end;

class function TBISQL.ProviderFrom(const AData:TDataItem; const SQL: String;
                           const GetData:TGetDataProc=nil;
                           const ErrorProc:TBIErrorProc=nil): TDataProvider;
var tmp : TSQLParser;
begin
  tmp:=TSQLParser.Create(AData,SQL);
  tmp.OnGetData:=GetData;
  try
    result:=tmp.Parse(ErrorProc);
  finally
    tmp.Free;
  end;
end;

class function TBISQL.From(const AData:TDataItem; const SQL: String;
                           const GetData:TGetDataProc=nil;
                           const ErrorProc:TBIErrorProc=nil): TDataItem;
begin
  result:=TSQLParser.DataFrom(ProviderFrom(AData,SQL,GetData,ErrorProc));
end;

{
  tmp:=TSQLParser.Create(AData,SQL);
  try
    tmp.OnGetData:=GetData;
    result:=tmp.Calculate(ErrorProc);
  finally
    tmp.Free;
  end;
}

class function TBISQL.From(const AData: TDataItem): String;
begin
  if AData.Provider=nil then
     result:=''
  else
  begin
    result:=From(AData.Provider);

    if result<>'' then
       result:='( '+result+' )';
  end;

  if result='' then
     result:=EscapedName(AData);
end;

type
  TBIQueryAccess=class(TBIQuery);

class function TBISQL.From(const AProvider: TDataProvider): String;
var tmp : TDataProvider;
begin
  if AProvider is TBIQuery then
     tmp:=TBIQueryAccess(AProvider).CreateProvider
  else
     tmp:=AProvider;

  try
    if tmp is TDataSelect then
       result:=From(tmp as TDataSelect)
    else
    if tmp is TSummary then
       result:=From(tmp as TSummary)
    else
       result:='';
  finally
    if tmp<>AProvider then
       tmp.Free;
  end;
end;

{ TSQLParser }

Constructor TSQLParser.Create(const AData:TDataItem; const AText: String);
begin
  inherited Create;

  FData:=AData;
  FText:=AText;

  ILength:=Length(FText);

  if ILength>0 then
     IPos:=1;
end;

procedure TSQLParser.DoError(const AError: String);
var tmp : String;
begin
  tmp:='SQL Parse Error: '+AError+' at position: '+IntToStr(IPos);

  if (not Assigned(FError)) or (not FError(Self,tmp)) then
     raise ESQLParser.Create(tmp);
end;

function TSQLParser.CallDoError(const Sender:TObject; const Error:String):Boolean;
begin
  DoError(Error);
  result:=True;
end;

function TSQLParser.EndOfText:Boolean;
begin
  result:=(IPos>ILength) or (ILength=0);
end;

function IsClause(const AText:String):Boolean;
begin
  result:=SameText(AText,'select') or
          SameText(AText,'from') or
          SameText(AText,'group') or
          SameText(AText,'order') or
          SameText(AText,'sort') or  // <-- alias to "order"
          SameText(AText,'where') or
          SameText(AText,'having') or
          SameText(AText,'limit') or
          SameText(AText,'offset');
end;

function TSQLParser.GetExpressions:TTextArray;
var tmp : String;
    L,Old : Integer;
    tmpExp : String;
begin
  result:=nil;

  tmpExp:='';

  while not EndOfText do
  begin
    Old:=IPos;

    tmp:=Trim(NextIdent);

    if (tmp='') or IsClause(tmp) then
    begin
      IPos:=Old;

      // Pending: Parse recurvise "select"

      break;
    end
    else
    begin
      L:=Length(tmp);

      if tmp[L]=',' then
      begin
        result.Append(Trim(tmpExp+Copy(tmp,1,L-1)));
        tmpExp:='';
      end
      else
        tmpExp:=tmpExp+' '+tmp;
    end;
  end;

  tmpExp:=Trim(tmpExp);

  if tmpExp<>'' then
     result.Append(tmpExp);
end;

function TSQLParser.GetExpression: String;
var tmp : String;
    Old : Integer;
begin
  result:='';

  while not EndOfText do
  begin
    Old:=IPos;

    tmp:=Trim(NextIdent);

    if (tmp='') or IsClause(tmp) then
    begin
      IPos:=Old;
      break;
    end
    else
      result:=result+' '+tmp;
  end;
end;

procedure TSQLParser.SkipDelimiters;
begin
  while not EndOfText do
  begin
    case FText[IPos] of
      ' ',
      #13,#9,
      #10 : Inc(IPos);
    else
      break;
    end;
  end;
end;

function TSQLParser.NextIdent:String;
var Ch : Char;
    InBreak,
    InQuote : Boolean;
    Parens : Integer;
begin
  InQuote:=False;
  InBreak:=False;

  result:='';

  Parens:=0;

  while not EndOfText do
  begin
    Ch:=FText[IPos];

    case Ch of
      '(' : begin
              result:=result+Ch;
              Inc(Parens);
            end;
      ')' : begin
              result:=result+Ch;
              Dec(Parens);
            end;

      ' ',
      #13,#9,
      #10 : if InQuote or InBreak or (Parens>0) then
               result:=result+Ch
            else
               break;

      '"' : begin
              result:=result+Ch;

              if InQuote then
              begin
                if Parens=0 then
                begin
                  Inc(IPos);
                  break;
                end
                else
                  InQuote:=False;
              end
              else
                InQuote:=True;
            end;

      '{' : begin
              result:=result+Ch;

              if (not InQuote) and (not InBreak) then
                 InBreak:=True;
            end;

      '}' : begin
              result:=result+Ch;

              if (not InQuote) and InBreak then
                 InBreak:=False;
            end;
    else
      result:=result+Ch;
    end;

    Inc(IPos);
  end;

  SkipDelimiters;
end;

function TSQLParser.Optional(const AText: String): Boolean;
var Old : Integer;
begin
  Old:=IPos;
  result:=SameText(NextIdent,AText);

  if not result then
     IPos:=Old;
end;

class function TSQLParser.GetFunction(var S:String; out AFunc:String):Boolean;
var i : Integer;
begin
  result:=False;

  i:=Pos('(',S); //S.IndexOf('(');

  if i>0 then
  begin
    AFunc:=Trim(Copy(S,1,i-1)); //S.Substring(0,i);

    S:=Copy(S,i+1,Length(S)); //S.Substring(i+1);
    i:=Pos(')',S); //S.IndexOf(')');

    if i>0 then
    begin
      S:=Copy(S,1,i-1); //S.Substring(0,i);
      result:=AFunc<>'';
    end;
  end;
end;

function ReplaceDots(const S:String):String;
begin
  result:=StringReplace(S,'.','|',[rfReplaceAll]);
end;

{$IFDEF FPC}
function TSQLParser.SkipError(const APos:Integer; const AMessage:String):Boolean;
{$ELSE}
class function TSQLParser.SkipError(const APos:Integer; const AMessage:String):Boolean;
{$ENDIF}
begin
  result:=True;
end;

class function TSQLParser.IgnoreError(const Sender:TObject; const Error:String):Boolean;
begin
  result:=True; // <-- do not raise error
end;

class function TSQLParser.DataFromString(const AData:TDataItem; const S:String):TDataItem;
begin
  result:=TSQLParser.StringToData(AData,S,IgnoreError);
end;

class function TSQLParser.StringToData(const AData:TDataItem; const S:String; const ErrorProc:TBIErrorProc):TDataItem;
var tmp : String;
begin
  if SameText(AData.Name,S) then
     result:=AData
  else
  if Pos('|',S)>0 then
     result:=TStore.OriginToData(AData,TStore.DefaultName,S,ErrorProc)
  else
  begin
    result:=AData.Items.Find(S);

    if result=nil then
    begin
      tmp:=TStore.OriginOf(AData,TStore.DefaultName);
      result:=TStore.OriginToData(nil,TStore.DefaultName,tmp+'|'+ReplaceDots(S),ErrorProc);
    end;
  end;
end;

function SplitIdents(const S:String):TArrayOfStrings{TStringArray};
var tmp : TSQLParser;
    tmpS : String;
begin
  result:=nil;

  tmp:=TSQLParser.Create(nil,S);
  try
    while not tmp.EndOfText do
    begin
      tmpS:=tmp.NextIdent;

      if tmpS<>'' then
         result.Add(tmpS);
    end;
  finally
    tmp.Free;
  end;
end;

class procedure TSQLParser.ParseSort(const AData:TDataItem; var ASort: TSortItems; const AOrder: TTextArray;
                                     const SQL:Boolean;
                                     const Error:TBIErrorProc);

  function GetSortParts(const S:String):TStringArray;
  begin
    if SQL then
       result:=SplitIdents(S)
    else
       result:=TArrayOfStrings{TStringArray}.Split(S,':');
  end;

  function ParseDirection(const S:String; out IsAscending:Boolean):Boolean;
  begin
    result:=True;

    if S='' then
       IsAscending:=True
    else
    if SameText(S,'ascending') or SameText(S,'asc') then
       IsAscending:=True
    else
    if SameText(S,'descending') or SameText(S,'desc') then
       IsAscending:=False
    else
       result:=False;
  end;

  function GuessIgnoreCase(const ASort:TStringArray):Boolean;
  begin
    // Custom syntax "case"
    if Length(ASort)>2 then
    begin
      if SameText(ASort[2],'case') then
         result:=True
      else
         result:=False
    end
    else
      result:=False;
  end;

  procedure WrongSort(const S:String);
  begin
    if Assigned(Error) then
       Error(nil,S)
    else
       raise EBIException.CreateFmt(BIMsg_Summary_ErrorSortParameter,[S]);
  end;

var tmp : String;
    tmpData : TDataItem;
    tmpSort : TStringArray;
    tmpAscending : Boolean;
    tmpExp : TExpression;
begin
  ASort.Clear;

  if AOrder<>nil then
    for tmp in AOrder do
    begin
      tmpSort:=GetSortParts(tmp);

      if Length(tmpSort)>0 then
      begin
        // Pending:
        // Allow parsing string containing an expression, not just a data item
        tmpData:=DataOf(AData,tmpSort[0],tmpExp); //DataFromString(AData,tmpSort[0]);

        if tmpData=nil then
           WrongSort(tmp)
        else
        if Length(tmpSort)>1 then
        begin
          if ParseDirection(tmpSort[1],tmpAscending) then
             ASort.Add(tmpData,tmpAscending,GuessIgnoreCase(tmpSort))
          else
             WrongSort(tmp);
        end
        else
          ASort.Add(tmpData);
      end
      else
        WrongSort(tmp);
    end;
end;

class function TSQLParser.FindAggregate(var S:String; out Agg:TAggregate):Boolean;
var tmp : String;
begin
  if GetFunction(S,tmp) then
     result:=TAggregate.FromString(tmp,Agg)
  else
     result:=False;
end;

class function TSQLParser.FindGroupByPart(var S:String; out APart:TDateTimePart):Boolean;
var tmp : String;
begin
  if GetFunction(S,tmp) then
  begin
    result:=TDateTimePart.FromString(tmp,APart);

    if not result then
       S:=tmp+'('+S+')';
  end
  else
     result:=False;
end;

class function TSQLParser.DataFrom(const AProvider:TDataProvider): TDataItem;
begin
  if AProvider=nil then
     result:=nil
  else
     result:=TDataItem.Create(AProvider);
end;

function TSQLParser.Calculate(const ErrorProc:TBIErrorProc=nil): TDataItem;
begin
  result:=DataFrom(Parse(ErrorProc));
end;

function UnBracket(const S:String):String;
begin
  if (Copy(S,1,1)='{') and (Copy(S,Length(S),1)='}') then
     result:=Copy(S,2,Length(S)-2)
  else
     result:=S;
end;

class function TSQLParser.DataOf(const AParent:TDataItem; const AData:String; out AExp:TExpression):TDataItem;

  function TryFindSubData(AParent:TDataItem; AName:String):TDataItem;
  var i : Integer;
      tmpExp : TExpression;
      tmp : String;
  begin
    result:=nil;

    i:=Pos('.',AName);

    if i>0 then
    begin
      AParent.Load;

      repeat
        i:=Pos('.',AName);

        if i>0 then
        begin
          tmp:=Copy(AName,1,i-1);

          AParent:=DataOf(AParent,tmp,tmpExp);

          if AParent=nil then
             Exit
          else
             Delete(AName,1,i);
        end;

      until i=0;

      if AName<>'' then
         AParent:=DataOf(AParent,AName,tmpExp);

      result:=AParent;
    end;
  end;

var tmpData : TDataItem;
    tmpS : String;

    {$IFDEF FPC}
    tmp : TSQLParser;
    {$ENDIF}
begin
  if AData='*' then
     result:=AParent
  else
  begin
    tmpS:=UnBracket(AData);

    if AParent=nil then
       result:=nil
    else
       result:=TryFindSubData(AParent,tmpS);

    if result=nil then
       if AParent<>nil then
          result:=TSQLParser.DataFromString(AParent{FData},tmpS);
  end;

  if result=nil then
  begin
    tmpData:=AParent;

    {$IFDEF FPC}
    tmp:=TSQLParser.Create(nil,'');
    try
      AExp:=TDataExpression.FromString(tmpData,AData,tmp.SkipError);
    finally
      tmp.Free;
    end;
    {$ELSE}
    AExp:=TDataExpression.FromString(tmpData,AData,SkipError);
    {$ENDIF}

    if AExp is TDataItemExpression then
    begin
      result:=TDataItemExpression(AExp).Data;
      AExp.Free;
      AExp:=nil;
    end;
  end;
end;

function TSQLParser.ErrorProc(const APos:Integer; const AMessage:String):Boolean;
begin
  result:=CallDoError(Self,AMessage);
end;

function TSQLParser.ParseWhere(const AData:TDataItem; const AWhere:String):TExpression;
begin
  result:=TDataFilter.FromString(AData,AWhere,ErrorProc);
end;

function UnParen(const S:String):String;
begin
  result:=Trim(S);

  while (Copy(result,1,1)='(') and (Copy(result,Length(result),1)=')') do
    result:=Trim(Copy(result,2,Length(result)-2));
end;

function TSQLParser.Parse(const ErrorProc:TBIErrorProc=nil): TDataProvider;
var
  tmpSum : TSummary;
  Expressions : TTextArray;
  tmpFrom : TDataArray;

  function FirstFrom:TDataItem;
  begin
    if Length(tmpFrom)>0 then
       result:=tmpFrom[0]
    else
    if FData=nil then
    begin
      DoError('Missing from clause');
      result:=nil; // skip warning
    end
    else
      result:=FData;
  end;

  function CreateGroup(const S:String):TGroupBy;
  var tmp : TDataItem;
      tmpExp : TExpression;
  begin
    tmp:=DataOf(FirstFrom,S,tmpExp);

    if tmp=nil then
    begin
      if tmpExp=nil then
      begin
        DoError('Cannot find data: '+S);
        result:=nil;
      end
      else
         result:=tmpSum.AddGroupBy(tmpExp);
    end
    else
      result:=tmpSum.AddGroupBy(tmp);
  end;

  procedure AddGroupBy(var AGroup:String);
  var tmpPart : TDateTimePart;
      tmp : TGroupBy;
  begin
    if TSQLParser.FindGroupByPart(AGroup,tmpPart) then
    begin
      tmp:=CreateGroup(AGroup);

      if tmp<>nil then
         tmp.DatePart:=tmpPart;
    end
    else
       CreateGroup(AGroup);
  end;

var
  OrderBy,
  Groups,
  FromData : TTextArray;

  Offset,
  Limit : TInteger;

  Having,
  Where : String;

  procedure ParseText;
  begin
    if not EndOfText then
    repeat
      if Optional('from') then
         FromData:=GetExpressions
      else
      if Optional('group') then
      begin
        if SameText(NextIdent,'by') then
           Groups:=GetExpressions
        else
           DoError('"By" missing');
      end
      else
      if Optional('order') or Optional('sort') then
      begin
        if SameText(NextIdent,'by') then
           OrderBy:=GetExpressions
        else
           DoError('"By" missing');
      end
      else
      if Optional('where') then
         Where:=GetExpression
      else
      if Optional('having') then
         Having:=GetExpression
      else
      if Optional('limit') then
      begin
        Limit:=StrToInt(GetExpression);

        if Limit<=0 then
           DoError('Limit must be a positive number greater than zero');
      end
      else
      if Optional('offset') then
      begin
        Offset:=StrToInt(GetExpression);

        if Offset<=0 then
           DoError('Offset must be a positive number greater than zero');
      end
      else
      begin
        DoError('Unexpected');
        break;
      end;

    until EndOfText;
  end;

  procedure TrySummary;
  var t : Integer;
      tmp : String;
      tmpAgg: TAggregate;
      tmpData : TDataItem;
      tmpExp : TExpression;
      tmpFirst : TDataItem;
  begin
    tmpFirst:=FirstFrom;

    for t:=Low(Expressions) to High(Expressions) do
    begin
      tmp:=Expressions[t];

      if TSQLParser.FindAggregate(tmp,tmpAgg) then
      begin
        if tmpSum=nil then
           tmpSum:=TSummary.Create(nil);

        tmpData:=DataOf(tmpFirst,tmp,tmpExp);

        if tmpData=nil then
           if tmpExp=nil then
           begin
             DoError('Missing data. '+tmp);
             break;
           end
           else
              tmpSum.AddMeasure(tmpExp,tmpAgg)
        else
           tmpSum.AddMeasure(tmpData,tmpAgg);
      end
      else
      if tmpSum=nil then
         tmpSum:=TSummary.Create(nil);
    end;

    if tmpSum<>nil then
    begin
      if tmpSum.Measures.Count=0 then
      begin
        // No Measures, it means its not a summary, but a select
        tmpSum.Free;
        tmpSum:=nil;
      end
      else
      begin
        // Pending: Sort in TSummary
        // TBISQL.ParseSort(FData,tmpSum.SortBy,OrderBy,True);

        if Where<>'' then
        begin
          if tmpFrom<>nil then
             tmpData:=tmpFrom[0]
          else
             tmpData:=FData;

          tmpSum.Filter:=ParseWhere(tmpData,Where);
        end;

        // Pending:
        //if Having<>'' then
        //   tmpSum.Having:=ParseWhere(tmpData,Having);
      end;
    end;
  end;

  function SearchInData(const AName:String):TDataItem;
  begin
    if SameText(FData.Name,AName) then
       result:=FData
    else
       result:=FData.Items.Find(AName);
  end;

  function ParseFrom:Boolean;
  var t : Integer;
      tmp : TDataItem;
  begin
    tmpFrom:=nil;

    for t:=Low(FromData) to High(FromData) do
    begin
      if Assigned(FOnGetData) then
         FOnGetData(FromData[t],tmp)
      else
         tmp:=nil;

      if (tmp=nil) and (FData<>nil) then
         tmp:=SearchInData(UnBracket(FromData[t]));

      if tmp=nil then
      begin
        tmp:=TBISQL.From(FData,UnParen(FromData[t]),FOnGetData,FError);

        if tmp=nil then
           Exit(False)
        else
           tmpFrom.Add(tmp);
      end
      else
        tmpFrom.Add(tmp);
    end;

    result:=True;
  end;

  function CreateSelect(const ASelect:TDataSelect; const Distinct:Boolean):Boolean;
  var t : Integer;
      tmpData : TDataItem;
      tmpExp : TExpression;
      tmpFirst : TDataItem;
  begin
    ASelect.Distinct:=Distinct;

    tmpFirst:=FirstFrom;

    for t:=Low(Expressions) to High(Expressions) do
    begin
      tmpData:=DataOf(tmpFirst,Expressions[t],tmpExp);

      if tmpData=nil then
      begin
        if tmpExp=nil then
        begin
          DoError('Data '+Expressions[t]+' not found');
          Exit(False);
        end
        else
           ASelect.Add(tmpExp)
      end
      else
         ASelect.Add(tmpData);
    end;

    if Length(tmpFrom)>0 then
       tmpData:=tmpFrom[0]
    else
       tmpData:=FData;

    TSQLParser.ParseSort(tmpData,ASelect.SortBy,OrderBy,True,CallDoError);

    if Where<>'' then
       ASelect.Filter:=ParseWhere(tmpData,Where);

    if Limit>0 then
       ASelect.Max:=Limit;

    if Offset>0 then
       ASelect.Start:=Offset;

    result:=True;
  end;

  procedure ParseGroups;
  var t : Integer;
  begin
    for t:=Low(Groups) to High(Groups) do
        AddGroupBy(Groups[t]);
  end;

  function TryCreateSelect(const ADistinct:Boolean):TDataSelect;
  begin
    result:=TDataSelect.Create(nil);
    try
      if not CreateSelect(result,ADistinct) then
      begin
        result.Free;
        result:=nil;
      end;
    except
      on Exception do
      begin
        result.Free;  // <-- Avoid memory leak
        raise;
      end;
    end;
  end;

var Distinct : Boolean;
begin
  result:=nil;

  FError:=ErrorProc;

  Offset:=0;
  Limit:=0;

  tmpSum:=nil;
  tmpFrom:=nil;

  Optional('select');

  Distinct:=Optional('distinct');

  Expressions:=GetExpressions;

  if Expressions<>nil then
  begin
    ParseText;

    if ParseFrom then
    begin
      try
        TrySummary;
      except
        on Exception do
        begin
          // Avoid memory leak
          tmpSum.Free;
          tmpSum:=nil;

          raise;
        end;
      end;

      if tmpSum=nil then
         result:=TryCreateSelect(Distinct)
      else
      try
        result:=tmpSum;
        ParseGroups;
      except
        on Exception do
        begin
          // Avoid memory leak
          result.Free;

          raise;
        end;
      end;
    end;
  end
  else
    DoError('Missing expression(s)');
end;

end.
