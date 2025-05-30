{*********************************************}
{  TeeBI Software Library                     }
{  Custom TBIDataSet component                }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Dataset;

{$IFDEF FPC}
{$ELSE}

{$IF CompilerVersion>=20}
{$DEFINE D12}

{$IF CompilerVersion>=23}
{$DEFINE D16}

{$IF CompilerVersion>=24}
{$DEFINE D17}

{$IF CompilerVersion>=25}
{$DEFINE D18}

{$IF CompilerVersion>=27}
{$DEFINE D20}

{$IF CompilerVersion>=29}
{$DEFINE D22}

{$IF CompilerVersion>=30}
{$DEFINE D23}

{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$ENDIF}

// Virtual TDataSet for TData

interface

uses {System.}Classes, {System.}Types,

     {$IFDEF D18}
     System.Generics.Collections,

     {$IFNDEF IOS}
     {$IFNDEF ANDROID}
     {$IFNDEF LINUX}
     System.AnsiStrings,
     {$ENDIF}
     {$ENDIF}
     {$ENDIF}
     {$ENDIF}
     {Data.}DB, BI.Arrays, BI.DataItem, BI.DataSource, BI.Summary, BI.Expression,
     BI.Expression.Filter;

const
  MaxDataSize=512;

type
  {$IFDEF NEXTGEN}
  TRecordBuffer=NativeInt;
  {$ENDIF}

  TTeeRecordBuffer={$IFDEF D17}TRecordBuffer{$ELSE}{$IFDEF D12}PByte{$ELSE}PChar{$ENDIF}{$ENDIF};
  {$NODEFINE TTeeRecordBuffer}

  TTeeAddRecordBuffer={$IFDEF D17}TTeeRecordBuffer{$ELSE}Pointer{$ENDIF};
  {$NODEFINE TTeeAddRecordBuffer}

  TTeeRecordBufferP={$IFDEF D17}TRecordBuffer{$ELSE}{$IFDEF D12}PByte{$ELSE}Pointer{$ENDIF}{$ENDIF};
  {$NODEFINE TTeeRecordBufferP}

  TTeeBookmark={$IFDEF D17}TBookmark{$ELSE}Pointer{$ENDIF};
  {$NODEFINE TTeeBookmark}

  TTeeValueBuffer={$IFDEF D17}TValueBuffer{$ELSE}Pointer{$ENDIF};
  {$NODEFINE TTeeValueBuffer}

  TItemData=packed Array[0..MaxDataSize-1] of Byte;

  TItem=packed record
    Data : TItemData;
    Missing : Boolean;
  end;

  TItems=packed Array[0..10000] of TItem; // <-- dummy type, just for PItems
  PItems=^TItems;

  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    Bookmark     : Integer;
    BookmarkFlag : TBookmarkFlag;
  end;

  TBICustomDataSet=class abstract(TDataSet)
  private
    FCurRec       : Integer;
    FLastBookmark : Integer;
    FReadOnly     : Boolean;

    IActive       : Boolean;

    function BufferRecord(const Buffer:TTeeRecordBuffer):PRecInfo;
    procedure DoInternalGotoBookmark(const BookMark:TInteger);
    function FieldsAreAutomatic:Boolean;
    Function RecBufSize: Integer;
  protected
    {$IFDEF NEXTGEN}
    function AllocRecBuf: TRecBuf; override;
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
    {$ELSE}
    function AllocRecordBuffer: TTeeRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TTeeRecordBuffer); override;
    {$ENDIF}

    function BookMarkToIndex(const ABookMark:TInteger):TInteger; virtual; abstract;
    Procedure DoFillBuffer(const Buffer: TTeeRecordBuffer); virtual; abstract;
    procedure GetBookmarkData(Buffer: TTeeRecordBuffer; Data: TTeeBookmark); override;
    function GetBookmarkFlag(Buffer: TTeeRecordBuffer): TBookmarkFlag; override;
    function GetIndexPosition:TInteger; virtual; abstract;
    function GetRecNo: Integer; override;
    function GetRecord(Buffer: TTeeRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;

    procedure InternalAddRecord(Buffer: TTeeAddRecordBuffer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark:TTeeBookmark); override;
    procedure InternalHandleException; override;
    procedure InternalInitRecord(Buffer: TTeeRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure InternalSetToRecord(Buffer: TTeeRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    Function RecInfoOfs:Integer; virtual; abstract;
    procedure SetBookmarkData(Buffer: TTeeRecordBuffer; Data: TTeeBookmark); override;
    procedure SetBookmarkFlag(Buffer: TTeeRecordBuffer; Value: TBookmarkFlag); override;

    {$IFNDEF LCL}
    procedure SetFieldData(Field: TField; Buffer: TTeeValueBuffer); override;
    {$ENDIF}

    procedure SetRecNo(Value: Integer); override;
    function ValidActiveBuffer:Boolean;

    property ReadOnly:Boolean read FReadOnly write FReadOnly;
  public
    {$IFDEF LCL}
    procedure SetFieldData(Field: TField; Buffer: TTeeValueBuffer); override;
    {$ENDIF}
  published
    property Active;
    property AutoCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property AfterRefresh;
    property BeforeRefresh;

    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
  end;

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(TeeAllComponentPlatformIDs)]
  {$ENDIF}
  {$ENDIF}
  TBIDataset = class(TBICustomDataSet)
  private
    FFilter : TBIFilter;

    //[Weak}
    ICursor : TDataCursor;

    Items : TDataArray;
    FMaster: TBIDataSet;

    ILink : TMasterDataLink;

    FRowNumbers : Boolean;
    IRowItem : TDataItem; // internal, used only for row numbers "Tag"

    TotalColumns : Integer;

    IAllRows : Boolean;

    function CreateLink(const ADataSet:TBIDataSet):TMasterDataLink;
    procedure DoHideDuplicates(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure DoPostToColumn(const Rec:TInteger; const Col:TDataItem; const Buffer:Pointer);
    procedure FieldOnGetText(Sender: TField; var Text: string; DisplayText: Boolean);
    function GetData:TDataItem; inline;
    function GetProvider: TComponent;
    procedure InitColumns;
    procedure PostToColumn(const Col:TDataItem; const Buffer:Pointer);
    procedure ReadOrigin(Reader: TReader);
    procedure SetData(const Value: TDataItem);
    procedure SetFieldProperties(const AField:TField; const AData:TDataItem);
    procedure SetMaster(const Value: TBIDataSet);
    procedure SetProvider(const Value: TComponent);
    procedure SetRowNumbers(const Value: Boolean);
    procedure TryCreateLink;
    procedure WriteOrigin(Writer: TWriter);
    procedure SetFilter(const Value: TBIFilter);
  protected
    Index : TCursorIndex;

    function BookMarkToIndex(const ABookMark:TInteger):TInteger; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAfterOpen; override;
    Procedure DoFillBuffer(const Buffer: TTeeRecordBuffer); override;
    function GetIndexPosition:TInteger; override;
    function GetRecordCount: Integer; override;
    function GetRecordSize: Word; override;
    function GetRowItem: TDataItem;
    procedure InternalAddRecord(Buffer: TTeeAddRecordBuffer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    function IsCursorOpen: Boolean; override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;
    Function RecInfoOfs: Integer; override;

    {$IFNDEF FPC}
    procedure SetDataSetField(const Value: TDataSetField); override;
    {$ENDIF}

    {$IFNDEF LCL}
    procedure SetFieldData(Field: TField; Buffer: TTeeValueBuffer); override;
    {$ENDIF}

    {$IFDEF D20}
    procedure SetFieldProps(Field: TField; FieldDef: TFieldDef); override;
    {$ENDIF}

    property Link:TMasterDataLink read ILink;
  public
    type
      TBIMasterDataLink=class(TMasterDataLink)
      private
        Masters,
        Details : TDataArray;
        ICursor : TDataCursor;
        MasterCol : TExpressions;

        Old : TInteger;

        procedure ResetIndex;
      protected
        procedure ActiveChanged; override;
        procedure RecordChanged(Field: TField); override;
      public
        Destructor Destroy; override;
      end;

      TBIMasterDataLinkClass=class of TBIMasterDataLink;

    var
    PercentFormat : String;
    FloatFormat : String;
    MasterDataLinkClass : TBIMasterDataLinkClass;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Clear;

    function GetFieldData(Field: TField; {$IFDEF D18}var {$ENDIF}Buffer: TTeeValueBuffer): Boolean; override;
    procedure InvertSortBy(const AField:TField);

    {$IFDEF LCL}
    procedure SetFieldData(Field: TField; Buffer: TTeeValueBuffer); override;
    {$ENDIF}

    function DataOf(const AField:TField):TDataItem;

    procedure PrepareIndex(const AIndex:TCursorIndex; const AllRows:Boolean=True);

    procedure SetItems(const AData:TDataArray);
    procedure SetFieldOnGetText(const AField:TField; const Hide:Boolean);

    property Cursor:TDataCursor read ICursor;
  published
    property Active;

    property Data:TDataItem read GetData write SetData;
    property Filter:TBIFilter read FFilter write SetFilter;
    property Master:TBIDataSet read FMaster write SetMaster;
    property RowNumbers:Boolean read FRowNumbers write SetRowNumbers;

    //property AutoCalcFields;
    //property Constraints;
    //property DataSetField;
    //property FieldDefs;

    {$IFNDEF FPC}
    property ObjectView default True;
    {$ENDIF}

    property Provider:TComponent read GetProvider write SetProvider;

    property ReadOnly default True;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation

uses
  SysUtils, BI.DB.Dataset, BI.Persist, BI.Languages.English,
  BI.Store.Component;

{ TBICustomDataSet }

Function TBICustomDataSet.RecBufSize: Integer;
begin
  result:=RecInfoOfs + SizeOf(TRecInfo);
end;

function TBICustomDataSet.FieldsAreAutomatic:Boolean;
begin
  {$IFDEF D23}
  result:=(FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles);
  {$ELSE}
  result:=True; // (Fields.CreatedModes=[]) or (cfAutomatic in  Fields.CreatedModes);
  {$ENDIF}
end;

procedure TBICustomDataSet.InternalClose;
begin
  if FieldsAreAutomatic then
     DestroyFields;

  FLastBookmark:=0;
  FCurRec:=-1;

  IActive:=False;
end;

procedure TBICustomDataSet.InternalRefresh;
begin
  inherited;

  ClearBuffers;

  FLastBookmark:=0;
  FCurRec:=-1;
end;

procedure TBICustomDataSet.InternalHandleException;
begin
  {$IFDEF FPC}
  if Assigned(ApplicationHandleException) then
     ApplicationHandleException(Self);
  {$ELSE}
  if Assigned(System.Classes.ApplicationHandleException) then
     System.Classes.ApplicationHandleException(Self);
  {$ENDIF}
end;

procedure TBICustomDataSet.InternalDelete;
begin
  inherited;

  if FCurRec>=RecordCount then
     Dec(FCurRec);
end;

procedure TBICustomDataSet.DoInternalGotoBookmark(const BookMark:TInteger);
begin
  if BookMark=-1 then
     DatabaseError('Bookmark not found')
  else
     FCurRec:=BookMarkToIndex(BookMark);
end;

procedure TBICustomDataSet.InternalGotoBookmark(Bookmark:TTeeBookmark);
begin
  DoInternalGotoBookMark(PInteger(BookMark)^);
end;

function TBICustomDataSet.BufferRecord(const Buffer:TTeeRecordBuffer):PRecInfo;
begin
  result:=PRecInfo({$IFDEF D17}PByte{$ENDIF}(Buffer) + RecInfoOfs);
end;

procedure TBICustomDataSet.InternalSetToRecord(Buffer: TTeeRecordBuffer);
begin
  DoInternalGotoBookmark(BufferRecord(Buffer).Bookmark);
end;

procedure TBICustomDataSet.InternalAddRecord(Buffer: TTeeAddRecordBuffer; Append: Boolean);
begin
  Inc(FLastBookmark);
  if Append then InternalLast;
end;

function TBICustomDataSet.GetBookmarkFlag(Buffer: TTeeRecordBuffer): TBookmarkFlag;
begin
  Result := BufferRecord(Buffer).BookmarkFlag;
end;

procedure TBICustomDataSet.SetBookmarkFlag(Buffer: TTeeRecordBuffer; Value: TBookmarkFlag);
begin
  BufferRecord(Buffer).BookmarkFlag := Value;
end;

procedure TBICustomDataSet.GetBookmarkData(Buffer: TTeeRecordBuffer; Data: TTeeBookmark);
begin
  PInteger(Data)^ := BufferRecord(Buffer).Bookmark;
end;

procedure TBICustomDataSet.SetBookmarkData(Buffer: TTeeRecordBuffer; Data: TTeeBookmark);
begin
  BufferRecord(Buffer).Bookmark := PInteger(Data)^;
end;

{$IFDEF NEXTGEN}
function TBICustomDataSet.AllocRecBuf: TRecBuf;
begin
  Result := TRecBuf(AllocMem(RecBufSize));
end;

procedure TBICustomDataSet.FreeRecBuf(var Buffer: TRecBuf);
begin
  FreeMem(Pointer(Buffer));
  Buffer:=0;
end;

{$ELSE}
function TBICustomDataSet.AllocRecordBuffer: TTeeRecordBuffer;
begin
  GetMem(Result, RecBufSize);
end;

procedure TBICustomDataSet.FreeRecordBuffer(var Buffer: TTeeRecordBuffer);
begin
  FreeMem(Buffer, RecBufSize);
  Buffer:=nil;
end;
{$ENDIF}

procedure TBICustomDataSet.InternalInitRecord(Buffer: TTeeRecordBuffer);
begin
  FillChar({$IFDEF D17}PByte{$ENDIF}(Buffer)^, RecBufSize, 0);
end;

procedure TBICustomDataSet.InternalFirst;
begin
  FCurRec := -1;
end;

function TBICustomDataSet.GetRecNo: Integer;
begin
  UpdateCursorPos;

  if (FCurRec = -1) and (RecordCount > 0) then
     Result := 1
  else
     Result := FCurRec + 1;
end;

procedure TBICustomDataSet.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= RecordCount) then
  begin
    FCurRec := Value - 1;
    Resync([]);
  end;
end;

procedure TBICustomDataSet.InternalOpen;

  procedure InitBookMarks;
  begin
    FLastBookmark:=RecordCount;
    BookmarkSize := SizeOf(Integer);
  end;

begin
  InitBookMarks;

  FCurRec:=-1;

  InternalInitFieldDefs;

  // Very Important:
  // Call FieldDefs.Update BEFORE CreateFields
  if not FieldDefs.Updated then
     FieldDefs.Update;

  if FieldsAreAutomatic then
     CreateFields;

  IActive:=True; // <-- Very important ! Set IActive:=True BEFORE calling BindFields

  BindFields(True);
end;

procedure TBICustomDataSet.InternalLast;
begin
  FCurRec:=RecordCount;
end;

function TBICustomDataSet.IsCursorOpen: Boolean;
begin
  Result:=IActive;
end;

procedure TBICustomDataSet.SetFieldData(Field: TField; Buffer: TTeeValueBuffer);
begin
  DataEvent(deFieldChange, Integer(Field));
end;

function TBICustomDataSet.GetRecord(Buffer: TTeeRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var tmpR : PRecInfo;
begin
  result:=grError;

  if IsCursorOpen then
  begin
    if RecordCount < 1 then
       Result := grEOF
    else
    begin
      Result := grOK;

      case GetMode of
        gmNext: if FCurRec >= RecordCount - 1  then
                begin
                  FCurRec:=RecordCount-1;
                  result:=grEOF;
                end
                else
                  Inc(FCurRec);

       gmPrior: if FCurRec <= 0 then result:=grBOF
                                else Dec(FCurRec);

     gmCurrent: if (FCurRec < 0) or (FCurRec >= RecordCount) then
                   result := grError;
      end;

      if result=grOK then
      begin
        DoFillBuffer(Buffer);

        tmpR:=BufferRecord(Buffer);
        tmpR.BookmarkFlag := bfCurrent;
        tmpR.BookMark:=GetIndexPosition;
      end
      else
      if (Result = grError) and DoCheck then
         DatabaseError('No Records');
    end;
  end
  else
  if DoCheck then
     DatabaseError('No Records');
end;

function TBICustomDataSet.ValidActiveBuffer:Boolean;
begin
  result:=ActiveBuffer<>{$IFDEF D18}0{$ELSE}nil{$ENDIF};
end;

procedure TBICustomDataSet.InternalPost;
begin
  if State <> dsEdit then
  begin
    Inc(FLastBookmark);
    //FBookMarks.Add({$IFNDEF D18}Pointer{$ENDIF}(FLastBookMark));
  end;
end;

{ TBIDataset }

Constructor TBIDataset.Create(AOwner: TComponent);
begin
  inherited;

  MasterDataLinkClass:=TBIMasterDataLink;

  FReadOnly:=True;

  FFilter:=TBIFilter.Create;

  ICursor:=TDataCursor.Create(Self);

  {$IFNDEF FPC}
  ObjectView:=True;
  {$ENDIF}

  PercentFormat:='0.00%';
  FloatFormat:='0.###';

  IAllRows:=True;
end;

procedure TBIDataSet.Clear;
begin
  ICursor.Clear;
  FFilter.Clear;
end;

Destructor TBIDataset.Destroy;
begin
  Clear;

  ICursor.Free;
  ICursor:=nil;

  ILink.Free;

  // Internal dummy
  IRowItem.Free;

  FFilter.Free;

  inherited;
end;

procedure TBIDataset.Notification( AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation=opRemove then
  begin
    // Cannot be used with "Data" property because it is not a TComponent-derived instance
    {
    if (Data<>nil) and (AComponent=Data) then
       Data:=nil;
    }

    if AComponent=Master then
    begin
      Master.RemoveFreeNotification(Self);
      Master:=nil;
    end;
  end;
end;

type
  TDataCursorAccess=class(TDataCursor);

procedure TBIDataset.InitColumns;
begin
  if (ICursor=nil) or (Data=nil) then
     Items:=nil
  else
  begin
    // Pending: Dynamic loading of data rows "on-demand".
    // For large data (that is still not loaded into memory, like when
    // data is at a BIWeb or disk provider), an optional optimization should
    // be done to load rows in "batches" (groups), instead of loading all data.

    // This opt can only be done when the ICursor is not sorted and has no filter,
    // as all data is needed in advance to perform sorting or filtering.

    ICursor.LoadData;

    if ICursor.Items=nil then
       if Data.AsTable then
          Items:=Data.Items.AsArray
       else
       begin
         {$IFDEF FPC}
         Items.Resize(1);
         Items[0]:=Data;
         {$ELSE}

         {$IF CompilerVersion>27}
         Items:=[Data];
         {$ELSE}
         Items.Resize(1);
         Items[0]:=Data;
         {$ENDIF}

         {$ENDIF}
       end
    else
       Items:=ICursor.DataItems;

    ICursor.PrepareIndex(Index,IAllRows);
  end;
end;

procedure TBIDataset.PrepareIndex(const AIndex:TCursorIndex; const AllRows:Boolean=True);
begin
  Index:=AIndex;
  IAllRows:=AllRows;
  ICursor.PrepareIndex(Index,IAllRows);
  Refresh;
end;

function TBIDataset.GetRecordSize: Word;
begin
  result:=SizeOf(TItem);
end;

procedure TBIDataset.InternalDelete;
begin
  if ICursor<>nil then
     ICursor.Delete(FCurRec);

  inherited;
end;

function TBIDataset.GetRecordCount: Integer;
begin
  result:=ICursor.Count;
end;

function TBIDataset.IsCursorOpen: Boolean;
begin
  Result:=(inherited IsCursorOpen) and (ICursor<>nil) and (Data<>nil);
end;

procedure TBIDataset.DoAfterOpen;
begin
  inherited;

  if Master<>nil then
     if ILink=nil then
        TryCreateLink
     else
     if ILink is TBIMasterDataLink then
        TBIMasterDataLink(ILink).ActiveChanged;
end;

procedure TBIDataset.SetFieldProperties(const AField:TField; const AData:TDataItem);
begin
  AField.Tag:=NativeInt(AData);

  if AField is TNumericField then
  begin
    if AData.NumericValues=TNumericData.Percentages then
       TNumericField(AField).DisplayFormat:=PercentFormat
    else
    case AData.Kind of
      dkSingle,
      dkDouble,
      dkExtended: TNumericField(AField).DisplayFormat:=FloatFormat;
    end;
  end;

  SetFieldOnGetText(AField,False);
end;

procedure TBIDataset.SetFilter(const Value: TBIFilter);
begin
  FFilter.Assign(Value);
end;

procedure TBIDataset.InternalOpen;

{$IFDEF FPC}
{$DEFINE SETTAGS}
{$ELSE}
{$IFNDEF D20}
{$DEFINE SETTAGS}
{$ENDIF}
{$ENDIF}

  {$IFDEF SETTAGS}
  procedure AssignTags;

    procedure DoSetField(const AField:TField);
    {$IFNDEF FPC}
    var t : Integer;
    {$ENDIF}
    begin
      SetFieldProperties(AField,DataOf(AField));

      {$IFNDEF FPC}
      if AField is TObjectField then
         for t:=0 to TObjectField(AField).FieldCount-1 do
             DoSetField(TObjectField(AField).Fields[t]);
      {$ENDIF}
    end;

  var t : Integer;
  begin
    for t:=0 to Fields.Count-1 do
        DoSetField(Fields[t]);
  end;
  {$ENDIF}

begin
  if Data=nil then
     raise EBIException.Create(BIMsg_Dataset_NoData);

  InitColumns;

  inherited;

  {$IFDEF SETTAGS}
  AssignTags;
  {$ENDIF}
end;

procedure TBIDataset.InternalInitFieldDefs;

  procedure ValidateFields;
  var t : Integer;
  begin
    if Assigned(Fields) then
    begin
      for t:=Fields.Count-1 downto 0 do
      begin
        if FieldDefs.IndexOf(Fields[t].{$IFDEF LCL}FieldName{$ELSE}FullName{$ENDIF})=-1 then
           Fields.Remove(Fields[t]);
      end;
    end;
  end;

  function CalcTotalColumns(const Items:TDataArray):Integer;
  var t : Integer;
  begin
    result:=0;

    for t:=Low(Items) to High(Items) do
    begin
      if Items[t].AsTable then
         Inc(result,1+CalcTotalColumns(Items[t].Items.AsArray))
      else
         Inc(result);
    end;
  end;

var tmp : TFieldDef;
begin
  FieldDefs.Clear;

  if Data<>nil then
  begin
    if FRowNumbers then
    begin
      FieldDefs.BeginUpdate;

      tmp:=FieldDefs.AddFieldDef;
      tmp.Attributes:=tmp.Attributes+[TFieldAttribute.faReadonly];
      tmp.Name:='#';
      tmp.DisplayName:='#';
      tmp.DataType:=ftLargeint;

      FieldDefs.EndUpdate;
    end;

    TBIDataSetSource.Add(FieldDefs,Items);
  end;

  DisableControls;
  Fields.Clear;
  EnableControls;

  ValidateFields;

  TotalColumns:=CalcTotalColumns(Items);

  if FRowNumbers then
     Inc(TotalColumns);
end;

function BufferToString(const P:PByte):String; inline;
begin
  result:=String(PChar(P));
end;

procedure StringToBuffer(const P:PByte; const S:String);
var L : Integer;
    tmpBytes : {$IFDEF FPC}Array of Byte{$ELSE}TArray<Byte>{$ENDIF};
    tmpC: Integer;
begin
  tmpC:=SizeOf(Char);

  if S='' then
     FillChar(P^,tmpC,0)
  else
  begin
    tmpBytes:=TEncoding.Unicode.GetBytes(S);
    L:=Length(tmpBytes) {$IFDEF FPC}+1{$ENDIF};

    if L>MaxDataSize-tmpC then
       L:=MaxDataSize-tmpC;

    SetLength(tmpBytes,L+tmpC);

    {$IFDEF FPC}
    tmpBytes[L]:=0;
    {$ENDIF}

    Move(tmpBytes[0],P^,L+tmpC);
  end;
end;

procedure TBIDataset.SetItems(const AData: TDataArray);
begin
  ICursor.SetItems(AData);

  if AData=nil then
     ICursor.Data:=nil
  else
     ICursor.Data:=AData[0].Parent;
end;

procedure TBIDataset.SetData(const Value: TDataItem);
var Old : Boolean;
begin
  if Data<>Value then
  begin
    FreeAndNil(ILink);

    if csReading in ComponentState then
       Old:=Active
    else
    begin
      Old:=False;
      Close; // <-- do not Close when reading from stream (it'll never open on Loaded)
    end;

    Clear;
    //ICursor.Clear;

    ICursor.Data:=Value;

    if Old and (Data<>nil) then
       if not (csDestroying in ComponentState) then
          Open;

    TryCreateLink;
  end;
end;

type
  TDataAccess=class(TDataItem);

function TBIDataset.CreateLink(const ADataSet:TBIDataSet):TMasterDataLink;

  function FindDetails(out AMasters:TDataArray):TDataArray;

    // Pending: change to inverted THops
    function DetailColumns(const ADetail:TDataItems; const AMaster:TDataItem):TDataArray;
    var tmp,
        Item : TDataItem;
    begin
      result:=nil;

      for Item in ADetail.AsArray do
      begin
        if TDataAccess(Item).HasMaster then
        begin
          tmp:=Item.Master;

          if tmp.Parent=AMaster then
          begin
            result.Add(Item);
            AMasters.Add(tmp);
          end;
        end;
      end;
    end;

  begin
    result:=nil;
    AMasters:=nil;

    if Data is TDetailData then
       result:=DetailColumns(TDetailData(Data).Detail.Items,ADataset.Data)
    else
    if (Data.Master<>nil) and (Data.Master=Data.Parent) then
       result.Add(Data)  // <-- self-detail
    else
    if (Data.Master<>nil) and (Data.Master=Data) then
       result.Add(Data.Parent) // <-- self-detail
    else
    if Data.AsTable then
       result:=DetailColumns(Data.Items,ADataset.Data);
  end;

  procedure DoError(const AMessage:String);
  begin
    raise EBIException.Create(AMessage);
  end;

var tmpDS : TDataSource;
    tmpLink : TBIMasterDataLink;
    tmpMasters,
    tmpDetails : TDataArray;
begin
  result:=nil;

  if Data=nil then
     DoError(BIMsg_Dataset_NoData)
  else
  begin
    tmpDetails:=FindDetails(tmpMasters);

    if tmpDetails=nil then
       DoError(BIMsg_Dataset_NoDetail)
    else
    begin
      tmpLink:=MasterDataLinkClass.Create(Self);
      tmpLink.Details:=tmpDetails;
      tmpLink.Masters:=tmpMasters;
      tmpLink.ICursor:=ICursor;
      tmpLink.Old:=-1;

      tmpDS:=TDataSource.Create(Owner);
      tmpDS.DataSet:=ADataSet;

      tmpLink.DataSource:=tmpDS;

      result:=tmpLink;
    end;
  end;
end;

{$IFNDEF FPC}
procedure TBIDataset.SetDataSetField(const Value: TDataSetField);
var tmp : TBIDataSet;
    Col : TDataItem;
begin
  if (Value<>nil) and (Value.DataSet is TBIDataSet) then
  begin
    tmp:=TBIDataSet(Value.DataSet);

    Col:=TDataItem(Value.Tag);

    if Col.AsTable then
    begin
      if Col is TDetailData then
         ICursor.Data:=TDetailData(Col).Detail
      else
         ICursor.Data:=Col;

      CreateLink(tmp);
    end
    else
    if Col is TDetailData then
    begin
      ICursor.Data:=TDetailData(Col).Detail;
      CreateLink(tmp);
    end;

    {
    else
      ICursor.Data:=Col.Items[RecNo] // <-- not possible, only called once for each RecNo
    };

  end;

  inherited;
end;
{$ENDIF}

procedure CopyBuffer(const AField:TField; const ASource,ADest:PByte);
begin
  case TDataItem(AField.Tag).Kind of
     dkInt32: PInteger(ADest)^:=PInteger(ASource)^;
     dkInt64: PInt64(ADest)^:=PInt64(ASource)^;
    dkSingle: {$IFDEF FPC}PDouble{$ELSE}PSingle{$ENDIF}(ADest)^:={$IFDEF FPC}PDouble{$ELSE}PSingle{$ENDIF}(ASource)^;
    dkDouble: PDouble(ADest)^:=PDouble(ASource)^;
  dkExtended: {$IFDEF FPC}PDouble{$ELSE}PExtended{$ENDIF}(ADest)^:={$IFDEF FPC}PDouble{$ELSE}PExtended{$ENDIF}(ASource)^;
      dkText: Move(ASource^, ADest^, AField.DataSize);
  dkDateTime: PDateTime(ADest)^:=PDateTime(ASource)^;
   dkBoolean: PWordBool(ADest)^:=PWordBool(ASource)^;

   dkUnknown: if not TDataItem(AField.Tag).AsTable then
                 Move(ASource^, ADest^, AField.DataSize+1);
  end;
end;

procedure TBIDataset.SetFieldData(Field: TField; Buffer: TTeeValueBuffer);
var P,
    PDest : PByte;
begin
  if (Data<>nil) and ValidActiveBuffer then
  begin
    {$IFDEF FPC}
    P:=Buffer;
    {$ELSE}
    P:=@Buffer[0];
    {$ENDIF}

    PDest:=@(PItems(ActiveBuffer)^[Field.FieldNo-1].Data);

    PItems(ActiveBuffer)^[Field.FieldNo-1].Missing:=P=nil;

    if P<>nil then
       CopyBuffer(Field,P,PDest);
  end;

  inherited;
end;

procedure TBIDataset.TryCreateLink;
begin
  if (FMaster<>nil) and (Data<>nil) and Active then
     ILink:=CreateLink(FMaster);
end;

procedure TBIDataset.WriteOrigin(Writer: TWriter);
begin
  Writer.WriteString(TStore.OriginOf(Data,TStore.DefaultName));
end;

procedure TBIDataset.Loaded;
begin
  inherited;
  TryCreateLink;
end;

procedure TBIDataset.SetMaster(const Value: TBIDataSet);
begin
  if FMaster<>Value then
  begin
    if Assigned(FMaster) then
       FMaster.RemoveFreeNotification(Self);

    ILink.Free;

    FMaster:=Value;

    if FMaster=nil then
       ILink:=nil
    else
    begin
      FMaster.FreeNotification(Self);

      if not (csLoading in ComponentState) then
         TryCreateLink;
    end;
  end;
end;

procedure TBIDataset.SetProvider(const Value: TComponent);
begin
  if Provider<>Value then
     if Value=nil then
        Data:=nil
     else
        Data:=TComponentImporter.From(Self,Value);
end;

procedure TBIDataset.SetRowNumbers(const Value: Boolean);
var OldData : TDataItem;
    Old : Boolean;
begin
  if FRowNumbers<>Value then
  begin
    FRowNumbers:=Value;

    Old:=Active;
    OldData:=Data;
    Data:=nil;
    Data:=OldData;

    if Old then
       Open;
  end;
end;

procedure TBIDataset.ReadOrigin(Reader: TReader);
begin
  Data:=TStore.OriginToData(nil,'',Reader.ReadString);
end;

Function TBIDataset.RecInfoOfs:Integer;
begin
  result:=(GetRecordSize*TotalColumns)+SizeOf(TRecInfo);
end;

function TBIDataset.GetRowItem: TDataItem;
begin
  if IRowItem=nil then
  begin
    IRowItem:=TDataItem.Create(TDataKind.dkInt64);
    TDataAccess(IRowItem).FUnique:=True;
  end;

  result:=IRowItem;
end;

function TBIDataset.DataOf(const AField: TField): TDataItem;

  {$IFNDEF FPC}
  function FindInParent:TDataItem;
  var tmpField : TField;
      tmpFields : Array of TField;
      L : Integer;
  begin
    tmpFields:=nil;
    tmpField:=AField;

    while tmpField.ParentField<>nil do
    begin
      L:=Length(tmpFields);
      SetLength(tmpFields,L+1);
      tmpFields[L]:=tmpField;

      tmpField:=tmpField.ParentField;
    end;

    result:=Items.Find(tmpField.FieldName);

    if result<>nil then // ??
    for L:=High(tmpFields) downto 0 do
        if result.AsTable then
           result:=result.Items.Find(tmpFields[L].FieldName)
        else
           raise EBIException.CreateFmt(BIMsg_Dataset_NotAsTable,[AField.FieldName]);
  end;
  {$ENDIF}

begin
  if AField=nil then
     result:=nil
  else
  if AField.DisplayName='#' then
     result:=GetRowItem
  else
    {$IFDEF FPC}
    result:=Items.Find(AField.FieldName)
    {$ELSE}

    if AField.ParentField=nil then
       result:=Items.Find(AField.FieldName)
    else
       result:=FindInParent;
    {$ENDIF}
end;

{$IFDEF D20}
procedure TBIDataSet.SetFieldProps(Field: TField; FieldDef: TFieldDef);
var tmpDef : TFieldDef;
    tmpDefs : Array of TFieldDef;

    tmpOld,
    tmpCol : TDataItem;

    L : Integer;
    tmp : String;
begin
  inherited;

  if FieldDef.DisplayName='#' then
     SetFieldProperties(Field,GetRowItem)
  else
  if FieldDef.ParentDef=nil then
  begin
    tmpCol:=Items.Find(Field.FieldName);

    // ???
    if tmpCol<>nil then
       SetFieldProperties(Field,tmpCol);
  end
  else
  begin
    // We cannot used Field here. Let use FieldDef, because
    // Field.ParentField is still "nil", while FielDef.ParentDef is ok

    tmpDefs:=nil;
    tmpDef:=FieldDef;

    while tmpDef.ParentDef<>nil do
    begin
      L:=Length(tmpDefs);
      SetLength(tmpDefs,L+1);
      tmpDefs[L]:=tmpDef;

      tmpDef:=tmpDef.ParentDef;
    end;

    tmpCol:=Items.Find(tmpDef.Name);

    if tmpCol=nil then
       raise EBIException.CreateFmt(BIMsg_Dataset_DataNotFound,[tmpDef.Name]);

    for L:=High(tmpDefs) downto 0 do
        if tmpCol.AsTable then
        begin
          tmp:=tmpDefs[L].Name;

          tmpOld:=tmpCol;
          tmpCol:=tmpOld.Items.Find(tmp);

          if tmpCol=nil then
             if tmp.StartsWith('_') then
                tmpCol:=tmpOld.Items.Find(tmp.Substring(1));

          if tmpCol=nil then
             raise EBIException.CreateFmt(BIMsg_Dataset_DataNotFound,[tmpDefs[L].Name]);

        end
        else
           raise EBIException.CreateFmt(BIMsg_Dataset_NotAsTable,[tmpDefs[L].Name]);

    SetFieldProperties(Field,tmpCol);
  end;
end;
{$ENDIF}

procedure TBIDataSet.FieldOnGetText(Sender: TField; var Text: string; DisplayText: Boolean);
var tmpPos : TInteger;
begin
  tmpPos:=ICursor.Position(RecNo-1);

  if tmpPos>=0 then
     Text:=TDataItem(Sender.Tag).DataToString(tmpPos);
end;

procedure TBIDataSet.DoHideDuplicates(Sender: TField; var Text: string; DisplayText: Boolean);
var tmpCol,
    Col : TDataItem;
    tmp : Integer;
    tmpClear : Boolean;
    tmpA,
    tmpB : TInteger;
begin
  Col:=TDataItem(Sender.Tag);

  tmp:=RecNo-1;

  if tmp>0 then
  begin
    tmpClear:=True;
    tmpCol:=Col;

    tmpA:=ICursor.Position(tmp);
    tmpB:=ICursor.Position(tmp-1);

    if tmpCol.Count>tmpA then
    repeat
      if not tmpCol.SameData(tmpA,tmpB) then
      begin
        tmpClear:=False;
        break;
      end
      else
        tmpCol:=tmpCol.ParentGroup;

    until tmpCol=nil;
  end
  else
    tmpClear:=False;

  if tmpClear then
     Text:=''
  else
  if TDataAccess(Col).IHasDate then
     FieldOnGetText(Sender,Text,DisplayText)
  else
     Text:=Sender.AsString
end;

procedure TBIDataSet.SetFieldOnGetText(const AField:TField; const Hide:Boolean);
begin
  if Hide then
     AField.OnGetText:=DoHideDuplicates
  else
  if (AField.Tag<>0) and TDataAccess(TDataItem(AField.Tag)).IHasDate then
     AField.OnGetText:=FieldOnGetText
  else
     AField.OnGetText:=nil;
end;

function TBIDataset.BookMarkToIndex(const ABookMark: TInteger): TInteger;
begin
  if ICursor.Index=nil then
     result:=ABookMark
  else
     result:=ICursor.Index.IndexOf(ABookMark);

  // if result=-1 then raise 'Bookmark not found';
end;

Procedure TBIDataset.DoFillBuffer(const Buffer: TTeeRecordBuffer);
var
  tmp : TInteger;

  procedure FillRowNumber(var Buffer:TItem);
  begin
    Buffer.Missing:=False;
    PInt64(@Buffer.Data[0])^:=tmp+1; // Row # Starts at 1
  end;

  procedure FillColumn(var Buffer:TItem; const Col:TDataItem);
  var P : Pointer;
      tmpMissing : Boolean;
  begin
    if Col.Kind=TDataKind.dkUnknown then
       tmpMissing:=tmp>=Col.Items.Count
    else
       tmpMissing:=tmp>=Col.Count;

    if tmpMissing or Col.Missing[tmp] then
       Buffer.Missing:=True
    else
    begin
      Buffer.Missing:=False;

      P:=@Buffer.Data[0];

      case Col.Kind of
         dkInt32: PInteger(P)^ :=Col.Int32Data[tmp];
         dkInt64: PInt64(P)^   :=Col.Int64Data[tmp];
        dkSingle: {$IFDEF FPC}PDouble{$ELSE}PSingle{$ENDIF}(P)^  :=Col.SingleData[tmp];
        dkDouble: PDouble(P)^  :=Col.DoubleData[tmp];
      dkExtended: {$IFDEF FPC}PDouble{$ELSE}PExtended{$ENDIF}(P)^:=Col.ExtendedData[tmp];
          dkText: StringToBuffer(P,Col.TextData[tmp]);
      dkDateTime: PDateTime(P)^:=TimeStampToMSecs(DateTimeToTimeStamp(Col.DateTimeData[tmp]));
       dkBoolean: PWordBool(P)^:=Col.BooleanData[tmp];

       dkUnknown: if (not Col.AsTable) and (Col.Items.Count>tmp) then
                     StringToBuffer(P,Col.Items[tmp].Name);
      end;
    end;
  end;

var
  ColPos : Integer;

  procedure DoFill(const Items:TDataArray);
  var t : Integer;
  begin
    for t:=0 to High(Items) do
    begin
      if Items[t].AsTable and (Items[t].Master=nil) then
      begin
        Inc(ColPos);
        DoFill(Items[t].Items.AsArray);
      end
      else
      begin
        FillColumn(TItem(PItems(Buffer)[ColPos]),Items[t]);
        Inc(ColPos);
      end;
    end;
  end;

begin
  if Data<>nil then
  begin
    tmp:=GetIndexPosition; //ICursor.Position(FCurRec);

    ColPos:=0;

    if FRowNumbers then
    begin
      FillRowNumber(TItem(PItems(Buffer)[ColPos]));
      Inc(ColPos);
    end;

    // Pending: Dynamic loading of data rows "on-demand".
    // For large data (that is still not loaded into memory, like when
    // data is at a BIWeb or disk provider), an optional optimization should
    // be done to load rows in "batches" (groups), instead of loading all data.

    DoFill(Items);
  end;
end;

function TBIDataset.GetData: TDataItem;
begin
  result:=ICursor.Data;
end;

function TBIDataset.GetFieldData(Field: TField; {$IFDEF D18}var {$ENDIF}Buffer: TTeeValueBuffer): Boolean;
var PBuffer,
    PSource : PByte;
    Col : TDataItem;
begin
  result:=ValidActiveBuffer and (Data<>nil) and (FCurRec<>-1);

  if result then
  begin
    if PItems(ActiveBuffer)^[Field.FieldNo-1].Missing then
       result:=False
    else
    begin
      Col:=TDataItem(Field.Tag);

      if (Col<>nil) and Assigned(Buffer) then // <-- safety check needed?
      begin
        {$IFDEF FPC}
        PBuffer:=Buffer;
        {$ELSE}
        PBuffer:=@Buffer[0];
        {$ENDIF}

        PSource:=@(PItems(ActiveBuffer)^[Field.FieldNo-1].Data);

        CopyBuffer(Field,PSource,PBuffer);
      end;
    end;
  end;
end;

procedure TBIDataSet.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor<>nil then
    begin
      result:=True;

      if Filer.Ancestor is TBIDataSet then
         result:=TBIDataSet(Filer.Ancestor).Data<>Data;
    end
    else
      result:=Data<>nil;
  end;

begin
  inherited;
  Filer.DefineProperty('Origin', ReadOrigin, WriteOrigin, (Provider=nil) and DoWrite);
end;

function TBIDataset.GetIndexPosition: TInteger;
begin
  result:=ICursor.Position(FCurRec);
end;

function TBIDataset.GetProvider: TComponent;
begin
  result:=TComponentImporter.ProviderOf(Data);
end;

procedure TBIDataset.DoPostToColumn(const Rec:TInteger; const Col:TDataItem; const Buffer:Pointer);
begin
  case Col.Kind of
     dkInt32: Col.Int32Data[Rec]:=PInteger(Buffer)^;
     dkInt64: Col.Int64Data[Rec]:=PInteger(Buffer)^;
    dkSingle: Col.SingleData[Rec]:={$IFDEF FPC}PDouble{$ELSE}PSingle{$ENDIF}(Buffer)^;
    dkDouble: Col.DoubleData[Rec]:=PDouble(Buffer)^;
  dkExtended: Col.ExtendedData[Rec]:={$IFDEF FPC}PDouble{$ELSE}PExtended{$ENDIF}(Buffer)^;
      dkText: Col.TextData[Rec]:=BufferToString(Buffer);
  dkDateTime: Col.DateTimeData[Rec]:=TimeStampToDateTime(MSecsToTimeStamp({$IFDEF FPC}PComp{$ELSE}PDateTime{$ENDIF}(Buffer)^));
   dkBoolean: Col.BooleanData[Rec]:=PWordBool(Buffer)^;

   dkUnknown: if (not Col.AsTable) and (not (Col is TDetailData)) then
                 Col.Items[Rec].Name:=BufferToString(Buffer);
  end;
end;

procedure TBIDataset.PostToColumn(const Col:TDataItem; const Buffer:Pointer);
begin
  if Data<>nil then
  begin
    if FCurRec>=RecordCount then
       Data.Resize(FCurRec+1);

    if Col.Count<=FCurRec then
       Col.Resize(Col.Parent.Count);

    DoPostToColumn(FCurRec,Col,Buffer);
  end;
end;

procedure TBIDataset.InternalPost;
var t : Integer;
    Col : TDataItem;
    P : Pointer;
    tmpPos : TInteger;
begin
  if Data<>nil then
  begin
    if State=dsEdit then
       tmpPos:=RecNo-1
    else
    begin
      tmpPos:=RecordCount;
      Data.Resize(tmpPos+1);
    end;

    for t:=0 to High(Items) do
    begin
      Col:=Items[t];
      Col.Missing[tmpPos]:=PItems(ActiveBuffer)^[t].Missing;

      P:=@PItems(ActiveBuffer)^[t];

      if State=dsEdit then
         PostToColumn(Col,P)
      else
         DoPostToColumn(RecordCount-1,Col,P);
     end;
  end;

  inherited;
end;

procedure TBIDataset.InvertSortBy(const AField: TField);
var tmp : TDataItem;
    B : TBookmark;
begin
  if AField=nil then
     Exit;

  tmp:=DataOf(AField);

  if tmp<>nil then
  begin
    B:=GetBookmark;
    try
      if not ICursor.SortBy.InvertOrder(tmp) then
      begin
        // AField does not exist, cannot be inverted, so add a new one:
        ICursor.SortBy.Clear;
        ICursor.SortBy.Add(tmp,True);
      end;

      ICursor.PrepareIndex(Index);
      Refresh;

      GotoBookmark(B);
    finally
      FreeBookmark(B);
    end;
  end;
end;

procedure TBIDataset.InternalAddRecord(Buffer: TTeeAddRecordBuffer; Append: Boolean);
//var t : Integer;
begin
  inherited;

  //if Data<>nil then
  //   for t:=0 to High(Items) do
  //       DoAddSeriesPoint(PSeriesPoints(Buffer)[t],FColumns[t])
  //      ;

  //FBookMarks.Add({$IFNDEF D18}Pointer{$ENDIF}(FLastBookMark));
end;

procedure TBIDataset.InternalClose;
begin
  inherited;

  Index:=nil;
  IAllRows:=True;
end;

{ TBIDataset.TBIMasterDataLink }

Destructor TBIDataset.TBIMasterDataLink.Destroy;
begin
  DataSource.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
  inherited;
end;

procedure TBIDataset.TBIMasterDataLink.ResetIndex;

  // Return the current row position in DataSet
  function CurrentRecNo:TInteger;
  begin
    if DataSet is TBIDataset then
       result:=TBIDataSet(DataSet).GetIndexPosition
    else
       result:=DataSet.RecNo-1;
  end;

  // Check ADataSet is not in destroying state
  function IsActive(const ADataSet:TDataSet):Boolean;
  begin
    result:=(ADataSet<>nil) and
            (not (csDestroying in ADataSet.ComponentState));
  end;

  // Refresh the ICursor index with the new detail rows
  procedure RefreshDetailDataset(const AIndex:TCursorIndex=nil);
  begin
    if IsActive(DetailDataSet) and
       (DataSource<>nil) and IsActive(DataSource.DataSet) then
    begin
      ICursor.PrepareIndex(AIndex);
      TDataCursorAccess(ICursor).ValidIndex:=ICursor.Index<>nil;

      if DetailDataSet.Active then
      begin
        // stack overflow:
        //DetailDataSet.Close;
        //DetailDataSet.Open;

        DetailDataSet.Refresh; // <-- buffer AV sometimes
      end;
    end;
  end;

  function AllMissing(const AItems:TDataArray; const AIndex:TInteger):Boolean;
  var t : Integer;
  begin
    for t:=Low(AItems) to High(AItems) do
        if not AItems[t].Missing[AIndex] then
           Exit(False);

    result:=True;
  end;

  procedure SelfDetail(const AIndex:TInteger);
  begin
    if DetailDataSet.Active then
       RefreshDetailDataSet(TDataAccess(Details[0]).IMaster.GetIndex(Details[0],AIndex));
  end;

  // Create ICursor.Index with all detail rows of current Master at CurrentRecNo position
  procedure FilterDetail;
  var tmp : Integer;
  begin
    tmp:=CurrentRecNo;

    if tmp<>Old then
    begin
      if (tmp=-1) or AllMissing(Masters,tmp) then
      begin
        ICursor.Index:=nil;
        TDataCursorAccess(ICursor).ValidIndex:=False;
      end
      else
      begin
        MasterCol:=ICursor.DetailIndex(Masters,Details,tmp);

        if MasterCol=nil then
           SelfDetail(tmp)
        else
           RefreshDetailDataset;
      end;

      Old:=tmp;
    end;
  end;

begin
  if (DataSet=nil) or (not DataSet.Active) then
  begin
    ICursor.Index:=nil;
    ICursor.Filter:=nil;

    RefreshDetailDataset;
  end
  else
  if Masters=nil then
     SelfDetail(CurrentRecNo)
  else
     FilterDetail;

  // Pending: Automatic refresh of consumers
  (*
  if ICursor.Data<>nil then
     TDataAccess(ICursor.Data).FConsumers.Broadcast(TBIEvent.ChangedValues {ChangedFilter});
  *)
end;

procedure TBIDataset.TBIMasterDataLink.ActiveChanged;
begin
  inherited;
  ResetIndex;
end;

procedure TBIDataset.TBIMasterDataLink.RecordChanged(Field: TField);
begin
  inherited;
  ResetIndex;
end;

end.



