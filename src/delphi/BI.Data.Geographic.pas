{*********************************************}
{  TeeBI Software Library                     }
{  Geographic Database Support                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Geographic;

interface

{
  This unit contains a global "TGeo" object instance to connect to the default
  "Geo" TeeBI database, located at "BISamples" store.

  It is used by TBIChart control (BI.VCL.Chart.Geo.pas unit) to determine
  which map, if any, should be used to graphically display values
}

uses
  BI.Arrays, BI.Data;

type
  TEntity=record
  private
    procedure Init(const AData:TDataItem; const AID:String=''; const AName:String='');
  public
    Pad: Integer;

    Data,
    ID,
    Name : TDataItem;

    function CodeOfName(const AName:String):String;

    function CodeToString(const ACode:Integer): String; overload;
    function CodeToString(const ACode:Int64): String; overload;
    function CodeToString(const ACode:String): String; overload;
    function CodeToString(const AData:TDataItem; const AIndex:TInteger):String; overload;

    function NameOfCode(const ACode:Integer):String;

    function SortFindLookup(const ACode:Integer;
                            const ACodeItem,ANameItem:TDataItem):String;
  end;

  TMasterDetail=record
  private
    class procedure Init; static;

    procedure Load(const AMaster,ADetail:TDataItem;
                   const ADetailToMaster:String;
                   const AMasterID:String='';
                   const ADetailID:String='';
                   const PadSize:Integer=0); overload;

    procedure Load(const AMaster,ADetail:TEntity;
                   const ADetailToMaster:String;
                   const AMasterID:String='';
                   const ADetailID:String='';
                   const PadSize:Integer=0); overload;
  public
    Master,
    Detail : TEntity;
    DetailToMaster : TDataItem;

    function Find(const AData:TDataItem; out ByCode:Boolean):Boolean; overload;

    class function FindDetail(const AData:TDataItem; out AEntity:TEntity):Boolean; static;
    class function FindMaster(const AData:TDataItem; out AMulti:TMasterDetail):Boolean; static;

    procedure Load; overload;
  end;

  TCountry=record
  private
    procedure Load;

    //class function NameOfIndex(const AIndex:TInteger): String; static;
  public
    Countries,
    ISONum,
    ISOA2,
    ISOA3,
    Capital,
    Name : TDataItem;
  end;

  TEntities=record
  private
    procedure Init(const AData:TDataItem);
  public
    type
      TAustralia=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Lands,
        Counties : TEntity;
      end;

      TBrazil=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Regions,
        States : TEntity;
      end;

      TChina=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Provinces,
        Prefectures : TEntity;
      end;

      TFrance=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Regions,
        Departements: TEntity;
      end;

      TGermany=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        States,
        Districts : TEntity;
      end;

      TItaly=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        MacroRegions,
        Regions,
        Provinces : TEntity;
      end;

      TJapan=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Regions,
        Prefectures : TEntity;
      end;

      TRussia=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Districts,
        Subjects: TEntity;
      end;

      TSpain=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Regions,
        Provinces : TEntity;
      end;

      TUK=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        NUTS,
        Counties: TEntity;
      end;

      TUSA=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        States,
        Counties : TEntity;
      end;

  var
    Country : TEntity;

    Data,

    Australia : TAustralia;
    Brazil : TBrazil;
    China : TChina;
    France : TFrance;
    Germany : TGermany;
    Italy : TItaly;
    Japan : TJapan;
    Russia : TRussia;
    Spain : TSpain;
    UK : TUK;
    USA : TUSA;
  end;

  TGeo=record
  private
    class var
      _Geo : TDataItem;

    class procedure AddSynonyms; static;
  public
    class var
      Continents : TEntity;
      Country : TCountry;
      Entities : TEntities;

    class function AllFoundIn(const AText:TDataItem; const AEntity:TEntity; const UseSynonyms:Boolean):Boolean; static;
    class procedure Check; static;
    class function EntityOf(const AData:TDataItem; const ACode:Int64):String; overload; static;
    class function EntityOf(const AData:TDataItem; const ACode:String):String; overload; static;
    class function FindSynonym(const S:String):String; static;
    class function IsChild(const AData:TDataItem):Boolean; static;
    class function LinkedTo(const AData:TDataItem):TDataItem; static;
    class function IsMultiEntity(const AData:TDataItem; out AMulti:TMasterDetail):Boolean; static;
    class function TextMapOf(const AData:TDataItem):TTextMap; static;
  end;

implementation
