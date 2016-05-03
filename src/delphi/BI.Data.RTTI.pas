{*********************************************}
{  TeeBI Software Library                     }
{  RTTI Provider for ORM                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.RTTI;

interface

// This unit contains a class to implement "ORM" (Object Relational Mapping)
// from custom records and classes to TDataItem objects, than can then be used
// as with any other TDataItem.

// Demo project: ..\Demos\Import\ORM_RTTI.dproj

// Pending list of features:
{
  - Support for TCollection (use TCollectionItem members)
  - Test with recursive classes (classes inside classes)
  - Support for Array or TList properties to implement master-detail TDataItem
  - Investigate adding "Primary Key" support for fast Find and non-duplicates.
}

uses
  System.Classes, System.TypInfo, System.Rtti, System.Generics.Collections,
  BI.Arrays, BI.Data;

type
  TVisibility=set of TMemberVisibility;

  TRttiMembers=(Both, Fields, Properties);

  TRTTIProvider=class(TDataProvider)
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
    procedure GetAll(const AData:TValue; const APos:TInteger);

    function IsVisible(const AMember:TRttiMember):Boolean; inline;
    function KindOf(const AType:TRttiType):TDataKind;
  protected
    procedure DoAdd(const APos:TInteger; const AData:TValue);
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    function Same(const APos:TInteger; const AData:TValue):Boolean;
  public
    Constructor CreateType(const AType:PTypeInfo;
                       const AVisibility:TVisibility=[mvPublic,mvPublished];
                       const AMembers:TRttiMembers=TRttiMembers.Both); overload;

    procedure GetItems(const AData:TDataItem); override;

    procedure Clear;
    function Count:TInteger; inline;
    procedure Delete(const AIndex:TInteger); inline;
  published
    property Members:TRttiMembers read FMembers write FMembers;
    property Visibility:TVisibility read FVisibility write FVisibility;
  end;

  TTypeProvider<T>=class(TRTTIProvider)
  private
    procedure GetError(const AIndex:TInteger);
    procedure Put(const AIndex:TInteger; const AData:T); inline;
    procedure TryResize(const ACount:TInteger);
  public
    Primary : TDataItem;

    Constructor CreateType; overload;
    Constructor CreateArray(const AData:Array of T); overload;

    procedure Add(const AData:T); overload; inline;
    procedure Add(const AData:Array of T); overload;
    procedure Add(const AData:TList<T>); overload;
    procedure Add(const AData:TCollection); overload;
    function Add(const AData:TValue):TInteger; overload;

    function Find(const AData:T):TInteger;
    function Get(const AIndex: TInteger):T;

    procedure Remove(const AData:T);
    procedure Update(const AIndex:TInteger; const AData:T);

    property Items[const Index:TInteger]:T read Get write Put; default;
  end;

implementation
