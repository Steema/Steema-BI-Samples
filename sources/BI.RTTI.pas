{*********************************************}
{  TeeBI Software Library                     }
{  RTTI Provider for ORM                      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.RTTI;

interface

{
 This unit contains a class to implement "ORM" (Object Relational Mapping)
 from custom records and classes to TDataItem objects, than can then be used
 everywhere as any other TDataItem.

 Demo project at folder: ..\Demos\Delphi\VCL\ORM_RTTI

 Pending list of features:

 - Test with recursive classes (classes inside classes)
 - Support for Array or TList properties to implement master-detail TDataItem
 - Investigate adding "Primary Key" support for fast Find and non-duplicate checks.
}

uses
  System.Classes, System.TypInfo, System.Rtti, System.Generics.Collections,
  BI.Arrays, BI.DataItem, BI.Persist, BI.Expression;

type
  TVisibility=set of TMemberVisibility;

  TRttiMembers=(Both, Fields, Properties);

  // Base RTTI Provider
  TRTTIProvider=class(TBaseDataImporter)
  private
  class var
    Context : TRttiContext;

  var
    FMembers : TRttiMembers;
    FTypeInfo : PTypeInfo;
    FVisibility : TVisibility;

    Member : TRttiMember;

    procedure GetItem(const AField:TRttiField; const AData:Pointer; const AItem:TDataItem; const APos:TInteger); overload;
    procedure GetItem(const AProp:TRttiProperty; const AData:Pointer; const AItem:TDataItem; const APos:TInteger); overload;
    procedure GetAll(const AData:TDataItem; const APos:TInteger; const AValue:TValue);

    function IsVisible(const AMember:TRttiMember):Boolean; inline;
    class function KindOf(const AType:TRttiType):TDataKind; static;
  protected
    procedure DoAdd(const AData:TDataItem; const APos:TInteger; const AValue:TValue);
    procedure GetItems(const AData:TDataItem); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    function Same(const AData:TDataItem; const APos:TInteger; const AValue:TValue):Boolean;
  public
    Constructor CreateType(const AOwner:TComponent;
                       const AType:PTypeInfo;
                       const AVisibility:TVisibility=[mvPublic,mvPublished];
                       const AMembers:TRttiMembers=TRttiMembers.Both); overload;

  published
    property Members:TRttiMembers read FMembers write FMembers;
    property Visibility:TVisibility read FVisibility write FVisibility;
  end;

  // Generic Provider
  TTypeProvider<T>=class(TRTTIProvider)
  private
    function Get(const AIndex: TInteger):T;
    procedure GetError(const AIndex:TInteger);
    procedure Put(const AIndex:TInteger; const AValue:T); inline;
    procedure TryResize(const AData:TDataItem; const ACount:TInteger);
  public
    Primary : TDataItem;

    Constructor Create(AOwner:TComponent); override;
    Constructor CreateArray(const AOwner:TComponent; const AValue:Array of T); overload;

    procedure Add(const AValue:T); overload; inline;
    procedure Add(const AValue:Array of T); overload;
    procedure Add(const AValue:TList<T>); overload;
    procedure Add(const AValue:TCollection; const AMember:String); overload;
    function Add(const AValue:TValue):TInteger; overload;

    procedure Clear; inline;
    function Count:TInteger; inline;
    procedure Delete(const AIndex:TInteger); inline;
    function Find(const AValue:T):TInteger;

    procedure Remove(const AValue:T);
    procedure Update(const AIndex:TInteger; const AValue:T);

    property Items[const Index:TInteger]:T read Get write Put; default;
  end;

  TObjectExpression=class(TExpression)
  private
    FField: String;
    FInstance: TObject;

    ICached : Boolean;
    IValue : TData;

    function Calculate: TValue;
    class function GetValue(const AInstance:TObject; const AField:String): TValue; static;
    procedure SetField(const Value: String);
    procedure SetInstance(const Value: TObject);
  public
    class function From(const AObject:TObject; const AField:String):TObjectExpression;

    procedure Assign(const Source:TExpression); override;
    class function Parse(const AContext:TObject; const S:String):TObjectExpression;
    procedure Refresh;
    function ToString:String; override;
    function Value:TData; override;

    property Instance:TObject read FInstance write SetInstance;
    property FieldName:String read FField write SetField;
  end;

implementation

uses
  System.SysUtils;

{ TRTTIProvider }

Constructor TRTTIProvider.CreateType(const AOwner:TComponent;
                                 const AType: PTypeInfo;
                                 const AVisibility:TVisibility;
                                 const AMembers:TRttiMembers);
begin
  inherited Create(AOwner);

  FMembers:=AMembers;
  FTypeInfo:=AType;
  FVisibility:=AVisibility;
end;

// Returns the appropiate TDataItem Kind from AType rtti type
class function TRTTIProvider.KindOf(const AType:TRttiType):TDataKind;

  function KindOfFloat(const AInfo:PTypeInfo):TDataKind;
  begin
    case GetTypeData(AInfo)^.FloatType of
        ftDouble: begin
                     if (AInfo=System.TypeInfo(TDate)) or
                        (AInfo=System.TypeInfo(TTime)) or
                        (AInfo=System.TypeInfo(TDateTime)) then
                        result:=TDataKind.dkDateTime
                     else
                        result:=TDataKind.dkDouble;
                  end;

      ftExtended: result:=TDataKind.dkExtended;
          ftComp: result:=TDataKind.dkInt64;
    else
        result:=TDataKind.dkSingle;
    end;
  end;

begin
  case AType.TypeKind of

    tkInteger : result:=TDataKind.dkInt32;

    tkInt64   : result:=TDataKind.dkInt64;

    tkFloat   : result:=KindOfFloat(AType.Handle);

    tkLString,
    tkString,
    tkUString,

    {$IF CompilerVersion>28}
    tkAnsiChar,
    {$ENDIF}

    tkWChar,
    tkWString : result:=TDataKind.dkText;

  else
    if (AType.Name='Boolean') or ((AType.BaseType<>nil) and (AType.BaseType.Handle=TypeInfo(Boolean))) then
       result:=TDataKind.dkBoolean
    else
       result:=TDataKind.dkUnknown;
  end;
end;

// Returns True when the AMember visibility matches our desired FVisibility
function TRTTIProvider.IsVisible(const AMember:TRttiMember):Boolean;
begin
  result:=AMember.Visibility in FVisibility;
end;

type
  TDataAccess=class(TDataItem);

// Obtains all selected fields and/or properties and adds them to AData
procedure TRTTIProvider.GetItems(const AData: TDataItem);

  procedure AddType(const AMember:TRttiMember; const AType:TRttiType);
  var tmpRTTI : TRTTIProvider;
      tmp : TDataItem;
  begin
    tmpRTTI:=TRTTIProvider.CreateType(Owner,AType.Handle);
    tmpRTTI.Member:=AMember;

    tmp:=TDataItem.Create(tmpRTTI);

    TDataAccess(tmp).TagObject:=AMember;

    // Force calling tmpRTTI provider GetItems
    tmp.Items;

    AData.Items.Add(tmp);
  end;

  procedure InternalAddType(const AMember:TRttiMember; AType:TRttiType);
  begin
    if AType.IsRecord then
       AddType(AMember,AType)
    else
    if AType is TRttiDynamicArrayType then
       InternalAddType(AMember,TRttiDynamicArrayType(AType).ElementType)
    else
    if AType is TRttiArrayType then
       InternalAddType(AMember,TRttiArrayType(AType).ElementType)
    else
       AData.Items.Add(AMember.Name,KindOf(AType),AMember);
  end;

var tmp : TRttiField;
    FType : TRttiType;
    tmpProp : TRttiProperty;
begin
  AData.AsTable:=True;

  FType:=Context.GetType(FTypeInfo);

  if Member=nil then
     AData.Name:=FType.Name
  else
     AData.Name:=Member.Name;

  if (FMembers=TRttiMembers.Both) or (FMembers=TRttiMembers.Fields) then
  for tmp in FType.GetDeclaredFields do
      if IsVisible(tmp) then
         InternalAddType(tmp,tmp.FieldType);

  if (FMembers=TRttiMembers.Both) or (FMembers=TRttiMembers.Properties) then
  for tmpProp in FType.GetDeclaredProperties do
      if IsVisible(tmpProp) and tmpProp.IsReadable then
         InternalAddType(tmpProp,tmpProp.PropertyType);

  // After adding new Items, resize them to match Parent size
  if AData.Parent<>nil then
     AData.Resize(AData.Parent.Count);
end;

procedure TRTTIProvider.Load(const AData: TDataItem; const Children: Boolean);
begin // Dummy
  AData.Items;
end;

function PointerOf(const AValue:TValue):Pointer;
begin
  if AValue.IsObject then
     result:=AValue.AsObject
  else
     result:=AValue.GetReferenceToRawData;
end;

// Returns True when AData is identical to internal FData at APos position
function TRTTIProvider.Same(const AData:TDataItem; const APos:TInteger; const AValue:TValue):Boolean;

  function CompareItem(const IsRecord:Boolean; const AItem:TDataItem; const Value:TValue):Boolean;
  var tmp : TValue;
  begin
    if IsRecord then
    begin
      AItem.Items;
      result:=TRTTIProvider(AItem.Provider).Same(AItem,APos,Value);
    end
    else
    if Value.IsArray then
    begin
      if Value.GetArrayLength>0 then
         tmp:=Value.GetArrayElement(0)
      else
         tmp:=TValue.Empty;

      result:=TRTTIProvider(AItem.Provider).Same(AItem,APos,tmp);
    end
    else
    if Value.IsEmpty then
       result:=AItem.Missing[APos]
    else
    case AItem.Kind of
      dkInt32: result:=AItem.Int32Data[APos]=Value.AsInteger;
      dkInt64: result:=AItem.Int64Data[APos]=Value.AsInt64;
     dkSingle: result:=AItem.SingleData[APos]=Value.AsExtended;
     dkDouble: result:=AItem.DoubleData[APos]=Value.AsExtended;
   dkExtended: result:=AItem.ExtendedData[APos]=Value.AsExtended;
       dkText: result:=AItem.TextData[APos]=Value.AsString;
   dkDateTime: result:=AItem.DateTimeData[APos]=Value.AsExtended;
    dkBoolean: result:=AItem.BooleanData[APos]=Value.AsBoolean;
    else
      result:=True; // Pending: Sub-DataItems
    end;
  end;

var tmp : TRttiField;
    c : Integer;
    FType : TRttiType;
    P : Pointer;
    tmpProp : TRttiProperty;
begin
  result:=True;

  c:=0;

  AData.Items;

  FType:=Context.GetType(FTypeInfo);

  P:=PointerOf(AValue);

  if (FMembers=TRttiMembers.Both) or (FMembers=TRttiMembers.Fields) then
  for tmp in FType.GetDeclaredFields do
      if IsVisible(tmp) then
      begin
        result:=CompareItem(tmp.FieldType.IsRecord,AData.Items.Item[c],tmp.GetValue(P));

        if not result then
           Exit;

        Inc(c);
      end;

  if (FMembers=TRttiMembers.Both) or (FMembers=TRttiMembers.Properties) then
  for tmpProp in FType.GetDeclaredProperties do
      if IsVisible(tmpProp) and tmpProp.IsReadable then
      begin
        result:=CompareItem(tmpProp.PropertyType.IsRecord,AData.Items.Item[c],tmpProp.GetValue(P));

        if not result then
           Exit;

        Inc(c);
      end;
end;

{.$DEFINE SPEED} // <-- when enabled, use internal TRttiMembers already preserved

// Recursive sets of fields and properties of AData into internal FData at APos position
procedure TRTTIProvider.DoAdd(const AData:TDataItem; const APos: TInteger; const AValue: TValue);

  procedure SetItem(const AType:TRttiType; const AItem:TDataItem; const Value:TValue);
  var tmp : TValue;
  begin
    if AType.IsRecord then
    begin
      AItem.Items;
      TRTTIProvider(AItem.Provider).DoAdd(AItem,APos,Value);
    end
    else
    if (AType is TRttiDynamicArrayType) or (AType is TRttiArrayType) then
    begin
      AItem.Items;

      if Value.GetArrayLength>0 {APos} then  // <-- just first element (not yet support for all array items)
         tmp:=Value.GetArrayElement(0 {APos})
      else
         tmp:=TValue.Empty;

      TRTTIProvider(AItem.Provider).DoAdd(AItem,APos,tmp);
    end
    else
    if Value.IsEmpty then
       AItem.Missing[APos]:=True
    else
    case AItem.Kind of
      dkInt32: AItem.Int32Data[APos]:=Value.AsInteger;
      dkInt64: AItem.Int64Data[APos]:=Value.AsInt64;
     dkSingle: AItem.SingleData[APos]:=Value.AsExtended;
     dkDouble: AItem.DoubleData[APos]:=Value.AsExtended;
   dkExtended: AItem.ExtendedData[APos]:=Value.AsExtended;
       dkText: AItem.TextData[APos]:=Value.AsString;
   dkDateTime: AItem.DateTimeData[APos]:=Value.AsExtended;
    dkBoolean: AItem.BooleanData[APos]:=Value.AsBoolean;
    end;
  end;

var tmp : TRttiField;
    c : Integer;
    FType : TRttiType;
    P : Pointer;
    tmpProp : TRttiProperty;

    {$IFDEF SPEED}
    tmpObj : TDataProvider;
    tmpMember : TRttiMember;
    tmpItem : TDataItem;
    tmpValue : TValue;
    {$ENDIF}
begin
  AData.Items;

  FType:=Context.GetType(FTypeInfo);

  P:=PointerOf(AValue);

  {$IFDEF SPEED}
  for c:=0 to AData.Items.Count-1 do
  begin
    tmpItem:=AData.Items.Item[c];

    tmpObj:=tmpItem.Provider;

    if tmpObj is TRTTIProvider then
    begin
      tmpItem.Items; // <-- useless?

      tmpMember:=TRTTIProvider(tmpObj).Member;

      if tmpMember is TRttiField then
         tmpValue:=TRttiField(tmpMember).GetValue(P)
      else
         tmpValue:=TRttiProperty(tmpMember).GetValue(P);

      TRTTIProvider(tmpObj).DoAdd(AData,APos,tmpValue);
    end
    else
    begin
      tmpMember:=TRttiMember(TDataAccess(tmpItem).TagObject);

      if tmpMember is TRttiField then
         SetItem(TRttiField(tmpMember).FieldType,tmpItem,TRttiField(tmpMember).GetValue(P))
      else
         SetItem(TRttiProperty(tmpMember).PropertyType,tmpItem,TRttiProperty(tmpMember).GetValue(P))
    end;
  end;
  {$ELSE}

  c:=0;

  if (FMembers=TRttiMembers.Both) or (FMembers=TRttiMembers.Fields) then
  for tmp in FType.GetDeclaredFields do
      if IsVisible(tmp) then
      begin
        SetItem(tmp.FieldType,AData.Items.Item[c],tmp.GetValue(P));
        Inc(c);
      end;

  if (FMembers=TRttiMembers.Both) or (FMembers=TRttiMembers.Properties) then
  for tmpProp in FType.GetDeclaredProperties do
      if IsVisible(tmpProp) and tmpProp.IsReadable then
      begin
        SetItem(tmpProp.PropertyType,AData.Items.Item[c],tmpProp.GetValue(P));
        Inc(c);
      end;
  {$ENDIF}
end;

// Sets the AField value from AItem data at APos position
procedure TRTTIProvider.GetItem(const AField:TRttiField; const AData:Pointer; const AItem:TDataItem; const APos:TInteger);
begin
  case AItem.Kind of
    dkInt32: AField.SetValue(AData,AItem.Int32Data[APos]);
    dkInt64: AField.SetValue(AData,AItem.Int64Data[APos]);
   dkSingle: AField.SetValue(AData,AItem.SingleData[APos]);
   dkDouble: AField.SetValue(AData,AItem.DoubleData[APos]);
 dkExtended: AField.SetValue(AData,AItem.ExtendedData[APos]);
     dkText: AField.SetValue(AData,AItem.TextData[APos]);
 dkDateTime: AField.SetValue(AData,AItem.DateTimeData[APos]);
  dkBoolean: AField.SetValue(AData,AItem.BooleanData[APos]);
  end;
end;

// Sets the AProp value from AItem data at APos position
procedure TRTTIProvider.GetItem(const AProp:TRttiProperty; const AData:Pointer; const AItem:TDataItem; const APos:TInteger);
begin
  case AItem.Kind of
    dkInt32: AProp.SetValue(AData,AItem.Int32Data[APos]);
    dkInt64: AProp.SetValue(AData,AItem.Int64Data[APos]);
   dkSingle: AProp.SetValue(AData,AItem.SingleData[APos]);
   dkDouble: AProp.SetValue(AData,AItem.DoubleData[APos]);
 dkExtended: AProp.SetValue(AData,AItem.ExtendedData[APos]);
     dkText: AProp.SetValue(AData,AItem.TextData[APos]);
 dkDateTime: AProp.SetValue(AData,AItem.DateTimeData[APos]);
  dkBoolean: AProp.SetValue(AData,AItem.BooleanData[APos]);
  end;
end;

// Recursive set of all AData fields and properties with the data at APos position
procedure TRTTIProvider.GetAll(const AData:TDataItem; const APos:TInteger; const AValue:TValue);

  procedure CheckArrayLength(var AValue:TValue; const AMember:TRttiMember);
  var tmpArr : Pointer;
      tmpLen : NativeInt;
      tmpInfo : PTypeInfo;
  begin
    if AValue.GetArrayLength=0 then
    begin
      tmpArr:=AValue.GetReferenceToRawData;
      tmpLen:=1;

      if AMember is TRttiField then
         tmpInfo:=TRttiField(AMember).FieldType.Handle
      else
         tmpInfo:=TRttiProperty(AMember).PropertyType.Handle;

      DynArraySetLength(PPointer(tmpArr)^, tmpInfo, 1, @tmpLen);
    end;
  end;

var tmpItem : TDataItem;
    tmpObj : TDataProvider;
    c : Integer;
    tmpMember : TRttiMember;
    P : Pointer;
    tmpValue : TValue;
begin
  if AValue.IsArray and (AValue.GetArrayLength>0) then // raise if Length=0 ??
     P:=AValue.GetReferenceToRawArrayElement(0)
  else
     P:=PointerOf(AValue);

  for c:=0 to AData.Items.Count-1 do
  begin
    tmpItem:=AData.Items[c];

    tmpObj:=tmpItem.Provider;

    if tmpObj is TRTTIProvider then
    begin
      tmpItem.Items;

      tmpMember:=TRTTIProvider(tmpObj).Member;

      if tmpMember is TRttiField then
         tmpValue:=TRttiField(tmpMember).GetValue(P)
      else
         tmpValue:=TRttiProperty(tmpMember).GetValue(P);

      if tmpValue.IsArray then
         CheckArrayLength(tmpValue,tmpMember);

      TRTTIProvider(tmpObj).GetAll(tmpItem,APos,tmpValue);

      if tmpMember is TRttiField then
         TRttiField(tmpMember).SetValue(P,tmpValue)
      else
         TRttiProperty(tmpMember).SetValue(P,tmpValue);
    end
    else
    begin
      tmpMember:=TRttiMember(TDataAccess(tmpItem).TagObject);

      if tmpMember is TRttiField then
         GetItem(TRttiField(tmpMember),P,tmpItem,APos)
      else
         GetItem(TRttiProperty(tmpMember),P,tmpItem,APos);
    end;
  end;
end;

{ TTypeProvider<T> }

// Creates an empty data
Constructor TTypeProvider<T>.Create(AOwner:TComponent);
begin
  CreateType(AOwner,TypeInfo(T));
end;

// Creates and appends AValue array
Constructor TTypeProvider<T>.CreateArray(const AOwner:TComponent; const AValue: array of T);
begin
  Create(AOwner);
  Add(AValue);
end;

// Returns the position of AValue row in the internal FData, or -1 if not found
function TTypeProvider<T>.Find(const AValue: T): TInteger;
var N : TLoopInteger;
begin
  for N:=0 to FData.Count-1 do
      if Same(FData,N,TValue.From<T>(AValue)) then
         Exit(N);

  result:=-1;
end;

// Internal: Resizes internal FData only when its necessary
procedure TTypeProvider<T>.TryResize(const AData:TDataItem; const ACount:TInteger);
begin
  AData.Items;

  if AData.Count<ACount then
     AData.Resize(ACount);
end;

// Appends a single AValue element of type T
procedure TTypeProvider<T>.Add(const AValue: T);
begin
  Add(TValue.From<T>(AValue));
end;

// Appends a single AValue element
function TTypeProvider<T>.Add(const AValue: TValue):TInteger;
begin
  result:=FData.Count;
  TryResize(FData,result+1);
  DoAdd(FData,result,AValue);
end;

// Appends all elements of AValue array
procedure TTypeProvider<T>.Add(const AValue: array of T);
var N,
    c,
    L : Integer;
begin
  L:=Length(AValue);

  if L>0 then
  begin
    Data.Items;

    // Allocate items in advance (much much faster)
    N:=FData.Count;
    TryResize(FData,N+L);

    // Add all items
    for c:=Low(AValue) to High(AValue) do
        DoAdd(FData,N+c-Low(AValue),TValue.From<T>(AValue[c]));
  end;
end;

// Appends all elements of AValue list
procedure TTypeProvider<T>.Add(const AValue: TList<T>);
var N, c : Integer;
begin
  if AValue.Count>0 then
  begin
    N:=Data.Count;

    // Allocate items in advance (much much faster)
    TryResize(FData,N+AValue.Count);

    for c:=0 to AValue.Count-1 do
        DoAdd(FData,N+c,TValue.From<T>(AValue.List[c]));
  end;
end;

// Removes AValue row, if it exists
procedure TTypeProvider<T>.Remove(const AValue: T);
var N : TInteger;
begin
  N:=Find(AValue);

  if N<>-1 then
     Delete(N);
end;

// This is a methods instead of a local proc, to skip compiler UStrArrayClr at Get
procedure TTypeProvider<T>.GetError(const AIndex:TInteger);
begin
  raise EBIException.Create('Error: TTypeProvider Get range error: '+IntToStr(AIndex));
end;

// Returns the data at AIndex position
function TTypeProvider<T>.Get(const AIndex: TInteger):T;
var tmpValue : TValue;
    tmp : T;
begin
  tmp:=Default(T);

  if AIndex<Count then
  begin
    // Use tmpValue as a middle-man to get and set result
    tmpValue:=TValue.From<T>(tmp);
    GetAll(FData,AIndex,tmpValue);
    result:=tmpValue.AsType<T>; // <-- cast again to T result
  end
  else
  begin
    result:=tmp; // <-- just to avoid undefined result hint
    GetError(AIndex);
  end;
end;

// Replaces data in AIndex position with AValue
procedure TTypeProvider<T>.Update(const AIndex: TInteger; const AValue: T);
begin
  Put(AIndex,AValue);
end;

// Replaces data in AIndex position with AValue (alias to Update)
procedure TTypeProvider<T>.Put(const AIndex: TInteger; const AValue: T);
begin
  DoAdd(FData,AIndex,TValue.From<T>(AValue));
end;

// Adds all elements of AValue collection
procedure TTypeProvider<T>.Add(const AValue: TCollection; const AMember:String);
var N, c : Integer;
    tmp : TPersistent;
    tmpType : TRttiType;
    tmpField : TRttiField;
begin
  if AValue.Count>0 then
  begin
    N:=FData.Count;

    // Allocate items in advance (much much faster)
    TryResize(FData,N+AValue.Count);

    // Add all items
    for c:=0 to AValue.Count-1 do
    begin
      tmp:=AValue.Items[c];
      tmpType:=Context.GetType(tmp.ClassInfo);
      tmpField:=tmpType.GetField(AMember);

      DoAdd(FData,N+c,tmpField.GetValue(tmp));
    end;
  end;
end;

// Remove data arrays recursively, without destroying Items
procedure TTypeProvider<T>.Clear;
begin
  FData.ClearData(True);
end;

function TTypeProvider<T>.Count: TInteger;
begin
  result:=FData.Count;
end;

// Removes the internal FData element at AIndex position
procedure TTypeProvider<T>.Delete(const AIndex:TInteger);
begin
  FData.Delete(AIndex);
end;

{ TObjectExpression }

procedure TObjectExpression.Assign(const Source: TExpression);
begin
  if Source is TObjectExpression then
  begin
    FInstance:=TObjectExpression(Source).FInstance;
    FField:=TObjectExpression(Source).FField;

    // Flag as invalid
    ICached:=False;
  end;

  inherited;
end;

// Helper constructor
class function TObjectExpression.From(const AObject: TObject;
  const AField: String): TObjectExpression;
begin
  result:=TObjectExpression.Create;
  result.FInstance:=AObject;
  result.FField:=AField;
end;

class function TObjectExpression.Parse(const AContext:TObject; const S: String): TObjectExpression;

  // Return sub-object of AParent (either field or property)
  function FindChild(const AParent:TObject; const AField:String):TObject;
  var tmp : TValue;
  begin
    if AParent=nil then
       result:=FindChild(AContext,AField)
    else
    begin
      tmp:=TObjectExpression.GetValue(AParent,AField);

      if tmp.IsObject then
         result:=tmp.AsObject
      else
         result:=nil;
    end;
  end;

var tmp : TObject;
    tmpS : String;
    i : Integer;
begin
  tmpS:=Trim(S);
  tmp:=nil;

  // Keep evaluating instance until last "." dot
  repeat
    i:=Pos('.',tmpS);

    if i>0 then
    begin
      tmp:=FindChild(tmp,Copy(tmpS,1,i-1));
      Delete(tmpS,1,i);
    end;

  until i=0;

  if (tmp=nil) or (tmpS='') then
     result:=nil
  else
  if TObjectExpression.GetValue(tmp,tmpS).IsEmpty then
     result:=nil
  else
     result:=TObjectExpression.From(tmp,tmpS)
end;

// Invalidate cached IValue, it will be recalculated at Value function
procedure TObjectExpression.Refresh;
begin
  ICached:=False;
end;

procedure TObjectExpression.SetField(const Value: String);
begin
  if FField<>Value then
  begin
    FField:=Value;
    ICached:=False;
  end;
end;

procedure TObjectExpression.SetInstance(const Value: TObject);
begin
  if FInstance<>Value then
  begin
    FInstance:=Value;
    FField:='';
    ICached:=False;
  end;
end;

// ie: "Edit1.Text"
function TObjectExpression.ToString: String;
begin
  if FInstance=nil then
     result:=''
  else
  begin
    if FInstance is TComponent then
       result:=TComponent(FInstance).Name
    else
       result:='';

    if result='' then
       result:=FInstance.ClassName;

    if FField<>'' then
       result:=result+'.'+FField;
  end;
end;

function TObjectExpression.Calculate: TValue;
var tmp : TRttiType;
    FRttiField : TRttiField;
    FRttiProperty : TRttiProperty;
begin
  result:=nil;

  if (FInstance<>nil) and (FField<>'') then
  begin
    tmp:=TRTTIProvider.Context.GetType(FInstance.ClassInfo);

    // Try as a Field
    FRttiField:=tmp.GetField(FField);

    if FRttiField=nil then
    begin
      // Try as a Property
      FRttiProperty:=tmp.GetProperty(FField);

      if FRttiProperty<>nil then
         result:=FRttiProperty.GetValue(FInstance);
    end
    else
      result:=FRttiField.GetValue(FInstance);
  end;
end;

class function TObjectExpression.GetValue(const AInstance:TObject; const AField:String): TValue;
var tmp : TObjectExpression;
begin
  tmp:=TObjectExpression.From(AInstance,AField);
  try
    result:=tmp.Calculate;
  finally
    tmp.Free;
  end;
end;

// Uses last cached IValue.
// Call Refresh to invalidate the cached value, and force recalculation
function TObjectExpression.Value: TData;
var tmp : TValue;
begin
  if not ICached then
  begin
    // Recalculate IValue cache

    if FInstance=nil then
       IValue:=TExpression.Null
    else
    if FField='' then
       IValue:=NativeInt(FInstance)
    else
    begin
      tmp:=Calculate;

      if tmp.IsEmpty then
         IValue:=TExpression.Null
      else
         IValue:=tmp.AsVariant;
    end;

    ICached:=True;
  end;

  result:=IValue;
end;

initialization
finalization
  TRTTIProvider.Context.Free;
end.
