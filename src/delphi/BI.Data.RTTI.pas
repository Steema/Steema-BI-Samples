{*********************************************}
{  TeeBI Software Library                     }
{  RTTI Provider for ORM                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.RTTI;

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
  BI.Arrays, BI.Data, BI.Persist, BI.Expression;

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
