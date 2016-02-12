{*********************************************}
{  TeeBI Software Library                     }
{  Custom TBIDataSet component                }
{  Copyright (c) 2015-2016 by Steema Software }
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

{$R-}  // <-- TItems

uses System.Classes, System.Types,

     {$IFDEF D18}
     System.Generics.Collections,

     {$IFNDEF IOS}
     {$IFNDEF ANDROID}
     System.AnsiStrings,
     {$ENDIF}
     {$ENDIF}
     {$ENDIF}
     Data.DB, BI.Arrays, BI.Data, BI.DataSource, BI.Summary;

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

  PItems=^TItems;
  TItems=packed Array[0..0] of TItem;

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

  {$IFDEF D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IFDEF D18}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IFDEF D19}or pidAndroid{$ENDIF}
              {$IFDEF D22}or pidiOSDevice64{$ENDIF}
               )]
  {$ENDIF}
  TBIDataset = class(TBICustomDataSet)
  private
    //[Weak}
    ICursor : TDataCursor;

    FSelect : TDataSelect;
    FSummary : TSummary;

    Items : TDataArray;
    FMaster: TBIDataSet;

    ILink : TMasterDataLink;

    TotalColumns : Integer;

    function CreateLink(const ADataSet:TBIDataSet):TMasterDataLink;
    procedure DoHideDuplicates(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure DoPostToColumn(const Rec:TInteger; const Col:TDataItem; const Buffer:Pointer);
    procedure FieldOnGetText(Sender: TField; var Text: string; DisplayText: Boolean);
    function GetData:TDataItem; inline;
    procedure InitColumns;
    procedure PostToColumn(const Col:TDataItem; const Buffer:Pointer);
    procedure ReadOrigin(Reader: TReader);
    procedure SetData(const Value: TDataItem);
    procedure SetFieldProperties(const AField:TField; const AData:TDataItem);
    procedure SetMaster(const Value: TBIDataSet);
    procedure SetSelect(const Value: TDataSelect);
    procedure SetSummary(const Value: TSummary);
    procedure TryCreateLink;
    procedure WriteOrigin(Writer: TWriter);
  protected
    Index : TInt64Array;

    function BookMarkToIndex(const ABookMark:TInteger):TInteger; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAfterOpen; override;
    Procedure DoFillBuffer(const Buffer: TTeeRecordBuffer); override;
    function GetIndexPosition:TInteger; override;
    function GetRecordCount: Integer; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: TTeeAddRecordBuffer; Append: Boolean); override;
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
  public
    PercentFormat : String;
    FloatFormat : String;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Clear;

    function GetFieldData(Field: TField; {$IFDEF D18}var {$ENDIF}Buffer: TTeeValueBuffer): Boolean; override;
    procedure InvertSortBy(const AField:TField);

    {$IFDEF LCL}
    procedure SetFieldData(Field: TField; Buffer: TTeeValueBuffer); override;
    {$ENDIF}

    function DataOf(const AField:TField):TDataItem;

    procedure OpenSelect;
    procedure OpenSummary;

    procedure PrepareIndex(const AIndex:TCursorIndex);

    procedure SetItems(const ADatas:TDataArray);
    procedure SetFieldOnGetText(const AField:TField; const Hide:Boolean);

    property Cursor:TDataCursor read ICursor;
    property Select:TDataSelect read FSelect write SetSelect;
    property Summary:TSummary read FSummary write SetSummary;
  published
    property Data:TDataItem read GetData write SetData;
    property Master:TBIDataSet read FMaster write SetMaster;

    property Active;

    //property AutoCalcFields;
    //property Constraints;
    //property DataSetField;

    //property Filter;
    //property Filtered;
    //property FilterOptions;
    //property FieldDefs;

    {$IFNDEF FPC}
    property ObjectView default True;
    {$ENDIF}
    
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
