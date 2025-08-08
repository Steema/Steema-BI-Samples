{*********************************************}
{  TeeBI Software Library                     }
{  Common VCL and FMX UI helper methods       }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.UI;

interface

uses
  {System.}Classes,
  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.UITypes,
  {$ENDIF}
  BI.DataItem, BI.UI.Colors;

type
  TShowMessageProc=procedure(const AText:String);

  TCommonUI=record
  public
    const
      CRLF=sLineBreak; //#13#10;

    class var
      ShowMessage:TShowMessageProc;

    class procedure AddItems(const AData:TDataItem; const AItems:TStrings); static;
    class procedure AddInfo(const AData:TDataItem; const AItems:TStrings); static;
    class procedure AddKinds(const AItems:TStrings); static;

    class function BytesToString(const Bytes: Int64): String; static;  // 3.4GB
    class function IsURL(const AFileName:String):Boolean; static;
    class function MSecToString(const MSec:Int64):String; static;  // 2h 1min 30sec
    class function ThousandsToString(const Value: Int64): String; static; // 1,234,567
    class function ToBooleanString(const Bool:Boolean):String; static;
    class function UniqueName(const AComponent:TComponent):String; static;
  end;

  TCryptoClass=class of TCrypto;

  TCrypto=class
  public
    class var Engine : TCryptoClass; // default is "nil" (no encryption)

    class function Decrypt(const Text:String):String; overload; virtual;
    class function Decrypt(const Stream:TStream):TStream; overload; virtual;

    class function Encrypt(const Text:String):String; overload; virtual;
    class function Encrypt(const Stream:TStream):TStream; overload; virtual;
  end;

  // Returns the list of distinct items in AData, optionally ordered by their
  // frequency (count appearance on the AData array)
  TDataMapAsData=class
  public
  type
    TDataMapOrder=(None,Item,Count);

    class procedure CopyFrom(const ASource,ADest:TDataItem); static;

    class function FromData(const AData:TDataItem;
                            const AddCounts:Boolean=True;
                            const Order:TDataMapOrder=TDataMapOrder.Count;
                            const Ascending:Boolean=False):TDataItem; static;
  end;

  // Colorizer used by TBIGrid (both VCL and FMX)

  TColorizeMode=(Full,Left); // Full = full cell background

  TColorizeTextColor=(Automatic,Fixed); // Automatic = best Font color

  TDataColorizer=record
  private
    IValid : Boolean;

    IRange,
    IMin : Double;
  public
    Colors : TColorFunction;
    Data : TDataItem;
    Mode : TColorizeMode;
    TextColor : TColorizeTextColor;

    function AlphaColorOf(const Value: Double): TAlphaColor;
    function ColorOf(const Value:Double):TColor;
    function Normalized(const Value:Double):Double;
    function ValidRange:Boolean;
  end;

  TDataColorizers=Array of TDataColorizer;

  TDataColorizersHelper=record helper for TDataColorizers
  public
    procedure Add(const AItem:TDataItem);
    procedure Clear;
    function TryColorize(const AItem:TDataItem; const AIndex:Integer;
                         out APercent:Double; out AColorIndex:Integer):Boolean;
  end;

  TAlternateColor=class(TPersistent)
  private
    FColor : TColor;
    FEnabled : Boolean;

    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
  protected
    FOnChange : TNotifyEvent;
  public
  const
    DefaultColor=$F2F2F2;

    Constructor Create;
    procedure Assign(Source:TPersistent); override;
  published
    property Color:TColor read FColor write SetColor default DefaultColor;
    property Enabled:Boolean read FEnabled write SetEnabled default False;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  BI.Arrays, BI.Languages.English,
  {$IFNDEF FPC}
  System.IOUtils,
  {$ENDIF}
  {System.}SysUtils;

function Max(const A,B:Integer):Integer; overload;
begin
  if A>B then
     result:=A
  else
     result:=B;
end;

{ TBICommonUI }

class procedure TCommonUI.AddItems(const AData: TDataItem; const AItems: TStrings);
var Col : TDataItem;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    if AData<>nil then
       for Col in AData.Items.AsArray do
           AItems.AddObject(Col.Name,Col);
  finally
    AItems.EndUpdate;
  end;
end;

// Adds all available data kind names in order
class procedure TCommonUI.AddKinds(const AItems: TStrings);
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    AItems.Add(TDataKind.dkInt32.ToString);
    AItems.Add(TDataKind.dkInt64.ToString);
    AItems.Add(TDataKind.dkSingle.ToString);
    AItems.Add(TDataKind.dkDouble.ToString);
    AItems.Add(TDataKind.dkExtended.ToString);
    AItems.Add(TDataKind.dkText.ToString);
    AItems.Add(TDataKind.dkDateTime.ToString);
    AItems.Add(TDataKind.dkBoolean.ToString);
    AItems.Add(TDataKind.dkUnknown.ToString);
  finally
    AItems.EndUpdate;
  end;
end;

// http://delphi.about.com/od/delphitips2008/qt/format-bytes.htm
class procedure TCommonUI.AddInfo(const AData: TDataItem;
  const AItems: TStrings);
begin
  AItems.BeginUpdate;
  try
    AItems.Add('Name: '+AData.Name);

    if AData.Kind<>TDataKind.dkUnknown then
       AItems.Add('Data: '+IntToStr(AData.Count));

    AItems.Add('Items: '+IntToStr(AData.Items.Count));
    AItems.Add('Rows: '+IntToStr(AData.TotalRows));
    AItems.Add('Columns: '+IntToStr(AData.TotalColumns));

    AItems.Add('');
  finally
    AItems.EndUpdate;
  end;
end;

class function TCommonUI.ThousandsToString(const Value: Int64): String;
begin
  result:=Format('%.0n',[Value+0.0]);
end;

class function TCommonUI.BytesToString(const Bytes: Int64): String;
const
  B  = 1; //byte
  KB = 1024 * B; //kilobyte
  MB = 1024 * KB; //megabyte
  GB = 1024 * MB; //gigabyte
  TB = Int64(1024) * GB; //terabyte
  PB = 1024 * TB; //petabyte
begin
  if bytes > PB then
     result:= FormatFloat('0.## PB', Bytes / PB)
  else
  if bytes > TB then
     result:= FormatFloat('0.## TB', Bytes / TB)
  else
  if bytes > GB then
     result:= FormatFloat('0.## GB', Bytes / GB)
  else
  if bytes > MB then
     result:= FormatFloat('0.## MB', Bytes / MB)
  else
  if bytes > KB then
     result:= FormatFloat('0.## KB', Bytes / KB)
  else
     result:= FormatFloat('0.## bytes', Bytes) ;
end;

class function TCommonUI.IsURL(const AFileName: String): Boolean;
begin
  result:=SameText(Copy(AFileName,1,7),'HTTP://') or
          SameText(Copy(AFileName,1,8),'HTTPS://');
end;

class function TCommonUI.MSecToString(const MSec: Int64): String;
begin
  if MSec>1000 then
     result:=FormatFloat('0.## sec', MSec/1000)
  else
     result:=IntToStr(Msec)+' msec';
end;

class function TCommonUI.ToBooleanString(const Bool:Boolean):String;
begin
  if Bool then
     result:='TRUE'
  else
     result:='FALSE';
end;

class function TCommonUI.UniqueName(const AComponent:TComponent):String;
var tmp : Integer;
    tmpS : String;
begin
  tmpS:=AComponent.ClassName;

  if Copy(tmpS,1,1)='T' then
     Delete(tmpS,1,1);

  if AComponent.Owner=nil then
     result:=tmpS
  else
  begin
    tmp:=1;

    repeat
      result:=tmpS+IntToStr(tmp);

      if AComponent.Owner.FindComponent(result)=nil then
         break
      else
         Inc(tmp);

    until False;
  end;
end;

{ TCrypto }

class function TCrypto.Decrypt(const Text: String): String;
begin
  if Engine=nil then
     result:=Text
  else
     result:=Engine.Decrypt(Text);
end;

class function TCrypto.Decrypt(const Stream: TStream): TStream;
begin
  if Engine=nil then
     result:=Stream
  else
     result:=Engine.Decrypt(Stream);
end;

class function TCrypto.Encrypt(const Text: String): String;
begin
  if Engine=nil then
     result:=Text
  else
     result:=Engine.Encrypt(Text);
end;

class function TCrypto.Encrypt(const Stream: TStream): TStream;
begin
  if Engine=nil then
     result:=Stream
  else
     result:=Engine.Encrypt(Stream);
end;

{ TDataMapAsData }

class procedure TDataMapAsData.CopyFrom(const ASource,ADest:TDataItem);
begin
  // Fill the first "Value" column, using the map:
  case ASource.Kind of
    dkInt32: ADest.Int32Data:=TInt32Map(ASource.DataMap).Items.Copy;
    dkInt64: ADest.Int64Data:=TInt64Map(ASource.DataMap).Items.Copy;
   dkSingle: ADest.SingleData:=TSingleMap(ASource.DataMap).Items.Copy;
   dkDouble: ADest.DoubleData:=TDoubleMap(ASource.DataMap).Items.Copy;
 dkExtended: ADest.ExtendedData:=TExtendedMap(ASource.DataMap).Items.Copy;
     dkText: ADest.TextData:=TTextMap(ASource.DataMap).Items.Copy;
 dkDateTime: ADest.DateTimeData:=TDateTimeMap(ASource.DataMap).Items.Copy;

  dkBoolean: begin
               ADest.BooleanData[0]:=False;
               ADest.BooleanData[1]:=True;
             end;
  end;
end;

// Returns the list of distinct items in AData, ordered by their
// frequency (count appearance on the AData array)
class function TDataMapAsData.FromData(const AData:TDataItem;
                            const AddCounts:Boolean=True;
                            const Order:TDataMapOrder=TDataMapOrder.Count;
                            const Ascending:Boolean=False):TDataItem;
var Value : TDataItem;
    Count : TInteger;
begin
  // Make sure data is loaded, and datamap calculated:
  AData.Load;
  AData.Stats;

  if AData.DataMap=nil then
     result:=nil
  else
  begin
    result:=TDataItem.Create(True);

    // Add two columns, one for the values, another for the count:
    Value:=result.Items.Add(AData.Name,AData.Kind);

    if AddCounts then
       result.Items.Add('Count',TDataKind.dkInt64);

    // Resize the output result:
    Count:=AData.DataMap.Count;
    result.Resize(Count);

    // Fill the first "Value" column data, using the AData map item values:
    CopyFrom(AData,Value);

    // Fill the second "Count" column:
    if AddCounts then
       result.Items[1].Int64Data:=AData.DataMap.Map.Copy;

    // Sort the output by Item or "Count", in ascending or descending order:
    if Order=TDataMapOrder.Item then
       result.SortBy(result.Items[0],Ascending)
    else
    if AddCounts and (Order=TDataMapOrder.Count) then
       result.SortBy(result.Items[1],Ascending);
  end;
end;

{ TDataColorizer }

function TDataColorizer.AlphaColorOf(const Value: Double): TAlphaColor;
begin
  result:=Colors.FromValue(Normalized(Value));
end;

function TDataColorizer.ColorOf(const Value: Double): TColor;
begin
  result:=TColorFunction.ToRGB(Colors.FromValue(Normalized(Value)));
end;

function TDataColorizer.Normalized(const Value: Double): Double;
begin
  if ValidRange then
     result:=(Value-IMin)/IRange
  else
     result:=0;
end;

function TDataColorizer.ValidRange: Boolean;

  function ValueAt(const AData:TDataItem; const AIndex:TLoopInteger):Double;
  begin
    case AData.Kind of
        dkInt32: result:=AData.Int32Data[AIndex];
        dkInt64: result:=AData.Int64Data[AIndex];
       dkSingle: result:=AData.SingleData[AIndex];
       dkDouble: result:=AData.DoubleData[AIndex];
     dkExtended: result:=AData.ExtendedData[AIndex];
     dkDateTime: result:=AData.DateTimeData[AIndex];
    else
      result:=0;
    end;
  end;

  function FirstNotMissing(const AData:TDataItem):TInteger;
  begin
    result:=0;

    while (result<AData.Count) and AData.Missing[result] do
       Inc(result);
  end;

  procedure LoopFrom(const AData:TDataItem; const First:TInteger; out AMin,AMax:Double);
  var t : TLoopInteger;
      tmp : Double;
  begin
    AMin:=ValueAt(AData,First);
    AMax:=AMin;

    t:=First;

    while t<AData.Count do
    begin
      if not AData.Missing[t] then
      begin
        tmp:=ValueAt(AData,t);

        if tmp<AMin then
           AMin:=tmp
        else
        if tmp>AMax then
           AMax:=tmp;
      end;

      Inc(t);
    end;
  end;

var
  ValidMin : Boolean;
  IMax : Double;

  procedure Process(const AData:TDataItem);
  var t : Integer;
      tmpMin,
      tmpMax : Double;
      tmpFirst : TInteger;
  begin
    if AData.AsTable then
       for t:=0 to AData.Items.Count-1 do
           Process(AData.Items[t])
    else
    if AData.Kind<>TDataKind.dkUnknown then
    begin
      if AData.Count>0 then
      begin
        // if MissingAsZero then
        //    IMin:=TDoubleStats(AData.Stats).Min...

        tmpFirst:=FirstNotMissing(AData);

        if tmpFirst<AData.Count then
        begin
          LoopFrom(AData,tmpFirst,tmpMin,tmpMax);

          if not ValidMin then
          begin
            IMin:=tmpMin;
            IMax:=tmpMax;
            ValidMin:=True;
          end
          else
          begin
            if tmpMin<IMin then
               IMin:=tmpMin;

            if tmpMax>IMax then
               IMax:=tmpMax;
          end;
        end;
      end;
    end;
  end;

begin
  if not IValid then
  begin
    IRange:=0;
    IMin:=0;
    IMax:=0;

    ValidMin:=False;

    Process(Data);

    IRange:=IMax-IMin;

    IValid:=True;
  end;

  result:=IRange<>0;
end;

{ TGridAlternate }

Constructor TAlternateColor.Create;
begin
  inherited;
  FColor:=DefaultColor;
end;

procedure TAlternateColor.Assign(Source: TPersistent);
begin
  if Source is TAlternateColor then
  begin
    FColor:=TAlternateColor(Source).FColor;
    FEnabled:=TAlternateColor(Source).FEnabled;

    if Assigned(FOnChange) then
       FOnChange(Self);
  end
  else
    inherited;
end;

procedure TAlternateColor.SetColor(const Value: TColor);
begin
  if FColor<>Value then
  begin
    FColor:=Value;

    if Assigned(FOnChange) then
       FOnChange(Self);
  end;
end;

procedure TAlternateColor.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;

    if Assigned(FOnChange) then
       FOnChange(Self);
  end;
end;

{ TDataColorizersHelper }

procedure TDataColorizersHelper.Add(const AItem: TDataItem);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);
  Self[L].Data:=AItem;
end;

procedure TDataColorizersHelper.Clear;
begin
  Self:=nil;
end;

function TDataColorizersHelper.TryColorize(const AItem:TDataItem; const AIndex:Integer;
                         out APercent:Double; out AColorIndex:Integer):Boolean;

  function GetNumber(const AData:TDataItem; const ARecNo:Integer):Double;
  begin
    case AData.Kind of
        dkInt32: result:=AData.Int32Data[ARecNo];
        dkInt64: result:=AData.Int64Data[ARecNo];
       dkSingle: result:=AData.SingleData[ARecNo];
       dkDouble: result:=AData.DoubleData[ARecNo];
     dkExtended: result:=AData.ExtendedData[ARecNo];
     dkDateTime: result:=AData.DateTimeData[ARecNo];
    else
      result:=0;
    end;
  end;

var t : Integer;
begin
  for t:=0 to High(Self) do
      if (AItem=Self[t].Data) or AItem.IsChildOf(Self[t].Data) then
      begin
        if AItem.Missing[AIndex] then
           Exit(False)
        else
        if Self[t].ValidRange then
           case AItem.Kind of
             dkInt32,
             dkInt64,
             dkSingle,
             dkDouble,
             dkExtended,
             dkDateTime: begin
                           APercent:=GetNumber(AItem,AIndex);
                           AColorIndex:=t;

                           Exit(True);
                         end;
           end;
      end;

  result:=False;
end;

initialization
  TCommonUI.ShowMessage:=nil;
end.
