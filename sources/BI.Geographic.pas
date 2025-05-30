{*********************************************}
{  TeeBI Software Library                     }
{  Geographic Database Support                }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Geographic;

interface

{
  This unit contains a global "TGeo" object instance to connect to the default
  "Geo" TeeBI database, located at "BISamples" store.

  It is used by TBIChart control (VCLBI.Chart.Geo.pas unit) to determine
  which map, if any, should be used to graphically display values
}

uses
  BI.Arrays, BI.DataItem;

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

      TCanada=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Provinces : TEntity;
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

      TIndia=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Zones,
        States : TEntity;
      end;

      TIndonesia=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Zones,
        Provinces : TEntity;
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

      TMexico=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        States : TEntity;
      end;

      TNetherlands=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Provinces,
        Municipalities : TEntity;
      end;

      TPortugal=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Regions,
        Districts : TEntity;
      end;

      TRussia=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Districts,
        Subjects: TEntity;
      end;

      TSouthAfrica=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Provinces: TEntity;
      end;

      TSouthKorea=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Types,
        Provinces: TEntity;
      end;

      TSpain=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Regions,
        Provinces : TEntity;
      end;

      TSwitzerland=record
      private
        procedure Init(const AData:TDataItem);
      public
        Data : TDataItem;

        Cantons : TEntity;
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
    Canada : TCanada;
    China : TChina;
    France : TFrance;
    Germany : TGermany;
    India : TIndia;
    Indonesia : TIndonesia;
    Italy : TItaly;
    Japan : TJapan;
    Mexico : TMexico;
    Netherlands : TNetherlands;
    Portugal : TPortugal;
    Russia : TRussia;
    SouthAfrica : TSouthAfrica;
    SouthKorea : TSouthKorea;
    Spain : TSpain;
    Switzerland : TSwitzerland;
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

uses
  BI.Persist, BI.DataSource,
  System.SysUtils;

// Temporary, until merged with Geo database
var
  Synonyms,
  SynonymNames : TTextArray;

  ILinks : Array of TMasterDetail;

{ TEntity }

function TEntity.SortFindLookup(const ACode:Integer; const ACodeItem,ANameItem:TDataItem):String;
var tmp : Integer;
    tmpExists : Boolean;
begin
  tmp:=ID.Int32Data.SortedFind(ACode,tmpExists);

  if tmpExists then
     result:=Name.TextData[tmp]
  else
     result:='';
end;

function TEntity.NameOfCode(const ACode:Integer):String;
begin
  result:=SortFindLookup(ACode,ID,Name);
end;

// Note: Duplicated at VCLBI.Chart.Geo, try to merge
function PadZeroLeft(const AValue,ALength:Integer):String;
begin
  result:=IntToStr(AValue);

  // Pad left
  while Length(result)<ALength do
        result:='0'+result;
end;

function TEntity.CodeToString(const ACode:Integer): String;
begin
  if Pad=0 then
     result:=IntToStr(ACode)
  else
     result:=PadZeroLeft(ACode,Pad);
end;

function TEntity.CodeToString(const ACode: Int64): String;
begin
  if Pad=0 then
     result:=IntToStr(ACode)
  else
     result:=PadZeroLeft(ACode,Pad);
end;

function TEntity.CodeToString(const ACode: String): String;
begin
  result:=ACode;
end;

function TEntity.CodeToString(const AData:TDataItem; const AIndex:TInteger):String;
begin
  if AData.Kind=dkInt32 then
     result:=CodeToString(AData.Int32Data[AIndex])
  else
  if AData.Kind=dkInt64 then
     result:=CodeToString(AData.Int64Data[AIndex])
  else
  if AData.Kind=dkText then
     result:=CodeToString(AData.TextData[AIndex])
  else
     result:='';
end;

procedure TEntity.Init(const AData:TDataItem; const AID,AName:String);
begin
  Data:=AData;
  Data.Load;

  if AID='' then
     ID:=Data['ID']
  else
     ID:=Data[AID];

  if AName='' then
     Name:=Data['Name']
  else
     Name:=Data[AName];
end;

function TEntity.CodeOfName(const AName: String): String;
var tmp : Integer;
begin
  tmp:=Name.TextData.IndexOf(AName);

  if tmp=-1 then
     result:=''
  else
     result:=CodeToString(ID.Int32Data[tmp]);
end;

{ TMasterDetail }

procedure TMasterDetail.Load(const AMaster,ADetail:TDataItem;
                             const ADetailToMaster:String;
                             const AMasterID:String='';
                             const ADetailID:String='';
                             const PadSize:Integer=0);
begin
  Master.Init(AMaster,AMasterID,'Name');
  Detail.Init(ADetail,ADetailID,'Name');

  DetailToMaster:=ADetail[ADetailToMaster];

  Master.Pad:=PadSize;
  Detail.Pad:=PadSize;

  Load;
end;

procedure TMasterDetail.Load(const AMaster,ADetail:TEntity;
                             const ADetailToMaster:String;
                             const AMasterID:String='';
                             const ADetailID:String='';
                             const PadSize:Integer=0);
begin
  Load(AMaster.Data,ADetail.Data,ADetailToMaster,AMasterID,ADetailID,PadSize);
end;

function TMasterDetail.Find(const AData: TDataItem;
  out ByCode: Boolean): Boolean;
begin
  if AData.Parent=Master.Data then
  begin
    ByCode:=AData=Master.ID;
    result:=True;
  end
  else
  if AData.Parent=Detail.Data then
  begin
    ByCode:=AData=Detail.ID;
    result:=True;
  end
  else
    result:=False;
end;

class function TMasterDetail.FindMaster(const AData:TDataItem; out AMulti:TMasterDetail):Boolean;
var t: Integer;
begin
  for t:=0 to High(ILinks) do
      if AData=ILinks[t].Master.Data then
      begin
        AMulti:=ILinks[t];
        Exit(True);
      end;

  result:=False;
end;

class function TMasterDetail.FindDetail(const AData:TDataItem; out AEntity:TEntity):Boolean;
var t: Integer;
begin
  for t:=0 to High(ILinks) do
      if AData=ILinks[t].Detail.Data then
      begin
        AEntity:=ILinks[t].Detail;
        Exit(True);
      end;

  result:=False;
end;

class procedure TMasterDetail.Init;
var tmp : TEntities;
begin
  SetLength(ILinks,13);

  tmp:=TGeo.Entities;

  ILinks[0].Load(TGeo.Continents.Data,TGeo.Country.Countries,'Continent','','ISO-3166-A2');
  ILinks[1].Load(tmp.Spain.Regions,tmp.Spain.Provinces,'Region','','',2);
  ILinks[2].Load(tmp.France.Regions,tmp.France.Departements,'Region');
  ILinks[3].Load(tmp.Japan.Regions,tmp.Japan.Prefectures,'Region');
  ILinks[4].Load(tmp.Australia.Lands,tmp.Australia.Counties,'State','FIPS');
  ILinks[5].Load(tmp.UK.NUTS,tmp.UK.Counties,'NUTS');
  ILinks[6].Load(tmp.Germany.States,tmp.Germany.Districts,'State');
  ILinks[7].Load(tmp.Brazil.Regions,tmp.Brazil.States,'Region');
  ILinks[8].Load(tmp.China.Provinces,tmp.China.Prefectures,'Province','ISO');
  ILinks[9].Load(tmp.Italy.MacroRegions,tmp.Italy.Regions,'MacroRegion');
  ILinks[10].Load(tmp.Italy.Regions,tmp.Italy.Provinces,'Region');
  ILinks[11].Load(tmp.Russia.Districts,tmp.Russia.Subjects,'District');
  ILinks[12].Load(tmp.USA.States,tmp.USA.Counties,'State','','FIPS',5);
end;

procedure TMasterDetail.Load;
begin
  Master.Data.Load;
  Detail.Data.Load;
end;

{ TCountry }

procedure TCountry.Load;
begin
  Countries:=TGeo._Geo['Countries'];
  Countries.Load;

  ISONum:=Countries['ISO-3166-Num'];
  ISOA2:=Countries['ISO-3166-A2'];
  ISOA3:=Countries['ISO-3166-A3'];
  Capital:=Countries['Capital'];
  Name:=Countries['Name'];
end;

{
class function TCountry.NameOfIndex(const AIndex:TInteger): String;
begin
  if AIndex=-1 then
     result:=''
  else
     result:=TGeo.Country.Name.TextData[AIndex];
end;
}

class function TGeo.IsChild(const AData:TDataItem):Boolean;
begin
  result:=(AData<>nil) and AData.IsChildOf(TGeo._Geo);
end;

// Try to find all Text strings in AMap
class function TGeo.AllFoundIn(const AText:TDataItem; const AEntity:TEntity; const UseSynonyms:Boolean):Boolean;

  function FindInMap(const AMap:TTextMap):Boolean;
  var t : Integer;
      tmp : Boolean;
  begin
    result:=False;

    for t:=0 to AText.Count-1 do
        if not AText.Missing[t] then
        begin
          AMap.Find(AText.TextData[t],tmp);

          if not tmp then
          begin
            if UseSynonyms then
            begin
              if Synonyms.Count=0 then
                 TGeo.AddSynonyms;

              Synonyms.SortedFind(AText.TextData[t],tmp,True);
            end;

            if not tmp then
               Exit(False);
          end;

          if tmp then
             result:=True;
        end;
  end;

begin
  result:=FindInMap(TextMapOf(AEntity.Name));

  if not result then
     if AEntity.ID.Kind=TDataKind.dkText then
        result:=FindInMap(TextMapOf(AEntity.ID));
end;

type
  TDataItemAccess=class(TDataItem);

class function TGeo.IsMultiEntity(const AData:TDataItem; out AMulti:TMasterDetail):Boolean;
var tmp : TDataItem;
begin
  tmp:=LinkedTo(AData);

  result:=tmp<>nil;

  if result then
     result:=TMasterDetail.FindMaster(tmp.Parent,AMulti);
end;

// Return the TextMap for AData, ensure it is created
class function TGeo.TextMapOf(const AData:TDataItem):TTextMap;
begin
  AData.Stats;

  if AData.DataMap.Map=nil then
     AData.ReCalculate;

  result:=TTextMap(AData.DataMap);
end;

class function TGeo.LinkedTo(const AData: TDataItem): TDataItem;
begin
  result:=AData;

  while result<>nil do
  begin
    if TGeo.IsChild(result) then
       Exit
    else
    if TDataItemAccess(result).HasMaster then
       result:=result.Master
    else
       break;
  end;

  result:=nil;
end;

class procedure TGeo.Check;
begin
  if _Geo=nil then
  begin
    // Pending: Optional embedded Geo database as *.res resource in Geo.pas unit
    _Geo:=TStore.Load('Geo');

    // Cache variables
    Country.Load;

    Continents.Init(_Geo['Continents'],'ID','Name');
    Entities.Init(_Geo['Entities']);

    TMasterDetail.Init;
  end;
end;

class function TGeo.EntityOf(const AData: TDataItem; const ACode: String): String;
begin
  result:=ACode;
end;

function FindLinkDetail(const AData:TDataItem; out AEntity:TMasterDetail):Boolean;
var t: Integer;
begin
  for t:=0 to High(ILinks) do
      if AData=ILinks[t].Detail.Data then
      begin
        AEntity:=ILinks[t];
        Exit(True);
      end;

  result:=False;
end;

class function TGeo.EntityOf(const AData: TDataItem; const ACode:Int64): String;
var tmp : TMasterDetail;
begin
  if FindLinkDetail(AData.Parent,tmp) then
     tmp.Detail.CodeToString(ACode)
  else
     result:=IntToStr(ACode);
end;

class function TGeo.FindSynonym(const S:String):String;
var tmpFound : Boolean;
    tmp : Integer;
begin
  if Synonyms.Count=0 then
     TGeo.AddSynonyms;

  tmp:=Synonyms.SortedFind(S,tmpFound,True);

  if tmpFound then
     result:=SynonymNames[tmp]
  else
     result:='';

  // Pending:
  // An additional step will be using SOUNDEX algorithm to find a "candidate"
  // match that resembles any existing entity name or synonym.

  // The goal is to always obtain a correct name, minimizing false positives
end;

// Note:
// "Synonyms" might better end-up inside a Geo database table instead of
// being hardcoded here
class procedure TGeo.AddSynonyms;

  procedure Add(const ASynonym,AReal:String);
  begin
    Synonyms.Append(ASynonym);
    SynonymNames.Append(AReal);
  end;

begin
  Synonyms:=nil;
  SynonymNames:=nil;

  // Warning:
  // Content here should be already in SORTED ASCENDING order

  // Pending to clean Geo names:
  Add('Bouvet Island','Bouvet IslandDependent territory of Norway');
  Add('British Virgin Islands','Virgin Islands (UK)');
  Add('Brunei Darussalam','Brunei');
  Add('Congo','(Congo-Brazzaville)');
  Add('CÃ´te d''Ivoire','Côte d''Ivoire (Ivory Coast)');
  Add('Democratic Republic of the Congo','Democratic Republic of the Congo (Congo-Kinshasa)');
  Add('Falkland Islands (Malvinas)','Falkland Islands');
  Add('Heard Island and McDonald Islands','Heard Island and McDonald IslandsExternal territory of Australia');
  Add('Holy See (Vatican City State)','Vatican City');
  Add('Iran, Islamic Republic of','Iran');
  Add('Korea','South Korea');
  Add('Korea, Republic of','South Korea');
  Add('Korea; Democratic People''s Republic of','North Korea');
  Add('Lao People''s Democratic Republic','Laos');
  Add('Macao','Macau');
  Add('Macedonia, the Former Yugoslav Republic of','Macedonia');
  Add('Micronesia; Federated States of','Micronesia');
  Add('Moldova, Republic of','Moldova');
  Add('Myanmar','Myanmar (Burma)');
  Add('Palestine, State of','Palestine');
  Add('Reunion','Réunion');
  Add('Russian Federation','Russia');
  Add('Saint Helena','Sanit Helena, Ascension and Tristan de Cunha');
  Add('Svalbard and Jan Mayen','Svalbard');
  Add('Syrian Arab Republic','Syria');
  Add('Taiwan, Province of China','Taiwan');
  Add('UK', 'United Kingdom');
  Add('United Kingdom','UK');
  Add('United Republic of Tanzania','Tanzania');
  Add('United States','United States of America');
  Add('US Virgin Islands','Virgin Islands (USA)');
  Add('USA','United States of America');
  Add('Viet Nam','Vietnam');
end;

{ TEntities }

procedure TEntities.Init(const AData: TDataItem);
begin
  Country.Init(TGeo.Country.Countries,'ISO-3166-A2');

  Data.Data:=AData;

  Australia.Init(AData['Australia']);
  Brazil.Init(AData['Brazil']);
  Canada.Init(AData['Canada']);
  China.Init(AData['China']);
  France.Init(AData['France']);
  Germany.Init(AData['Germany']);
  India.Init(AData['India']);
  Indonesia.Init(AData['Indonesia']);
  Italy.Init(AData['Italy']);
  Japan.Init(AData['Japan']);
  Mexico.Init(AData['Mexico']);
  Netherlands.Init(AData['Netherlands']);
  Portugal.Init(AData['Portugal']);
  Russia.Init(AData['Russia']);
  SouthAfrica.Init(AData['South Africa']);
  SouthKorea.Init(AData['South Korea']);
  Spain.Init(AData['Spain']);
  Switzerland.Init(AData['Switzerland']);
  UK.Init(AData['UK']);
  USA.Init(AData['USA']);
end;

{ TEntities.TUSA }

procedure TEntities.TUSA.Init(const AData: TDataItem);
begin
  Data:=AData;
  States.Init(Data['States']);
  Counties.Init(Data['Counties'],'FIPS');
end;

{ TEntities.TAustralia }

procedure TEntities.TAustralia.Init(const AData: TDataItem);
begin
  Data:=AData;
  Lands.Init(Data['Lands'],'FIPS');
  Counties.Init(Data['Counties']);
end;

{ TEntities.TBrazil }

procedure TEntities.TBrazil.Init(const AData: TDataItem);
begin
  Data:=AData;
  Regions.Init(Data['Regions']);
  States.Init(Data['States']);
end;

{ TEntities.TChina }

procedure TEntities.TChina.Init(const AData: TDataItem);
begin
  Data:=AData;
  Provinces.Init(Data['Provinces'],'ISO');
  Prefectures.Init(Data['Prefectures']);
end;

{ TEntities.TFrance }

procedure TEntities.TFrance.Init(const AData: TDataItem);
begin
  Data:=AData;
  Regions.Init(Data['Regions']);
  Departements.Init(Data['Departements']);
end;

{ TEntities.TGermany }

procedure TEntities.TGermany.Init(const AData: TDataItem);
begin
  Data:=AData;
  States.Init(Data['States']);
  Districts.Init(Data['Districts']);
end;

{ TEntities.TItaly }

procedure TEntities.TItaly.Init(const AData: TDataItem);
begin
  Data:=AData;
  MacroRegions.Init(Data['MacroRegions']);
  Regions.Init(Data['Regions']);
  Provinces.Init(Data['Provinces']);
end;

{ TEntities.TJapan }

procedure TEntities.TJapan.Init(const AData: TDataItem);
begin
  Data:=AData;
  Regions.Init(Data['Regions']);
  Prefectures.Init(Data['Prefectures']);
end;

{ TEntities.TRussia }

procedure TEntities.TRussia.Init(const AData: TDataItem);
begin
  Data:=AData;
  Districts.Init(Data['Districts']);
  Subjects.Init(Data['Subjects']);
end;

{ TEntities.TSpain }

procedure TEntities.TSpain.Init(const AData: TDataItem);
begin
  Data:=AData;
  Regions.Init(Data['Regions']);
  Provinces.Init(Data['Provinces']);
end;

{ TEntities.TUK }

procedure TEntities.TUK.Init(const AData: TDataItem);
begin
  Data:=AData;
  NUTS.Init(Data['NUTS']);
  Counties.Init(Data['Counties']);
end;

{ TEntities.TCanada }

procedure TEntities.TCanada.Init(const AData: TDataItem);
begin
  Data:=AData;
  Provinces.Init(Data['Provinces']);
end;

{ TEntities.TIndia }

procedure TEntities.TIndia.Init(const AData: TDataItem);
begin
  Data:=AData;
  Zones.Init(Data['Zones']);
  States.Init(Data['States']);
end;

{ TEntities.TMexico }

procedure TEntities.TMexico.Init(const AData: TDataItem);
begin
  Data:=AData;
  States.Init(Data['States']);
end;

{ TEntities.TSouthKorea }

procedure TEntities.TSouthKorea.Init(const AData: TDataItem);
begin
  Data:=AData;
  Types.Init(Data['Types']);
  Provinces.Init(Data['Provinces']);
end;

{ TEntities.TSouthAfrica }

procedure TEntities.TSouthAfrica.Init(const AData: TDataItem);
begin
  Data:=AData;
  Provinces.Init(Data['Provinces']);
end;

{ TEntities.TIndonesia }

procedure TEntities.TIndonesia.Init(const AData: TDataItem);
begin
  Data:=AData;
  Zones.Init(Data['Zones']);
  Provinces.Init(Data['Provinces']);
end;

{ TEntities.TPortugal }

procedure TEntities.TPortugal.Init(const AData: TDataItem);
begin
  Data:=AData;
  Regions.Init(Data['Regions']);
  Districts.Init(Data['Districts']);
end;

{ TEntities.TNetherlands }

procedure TEntities.TNetherlands.Init(const AData: TDataItem);
begin
  Data:=AData;
  Provinces.Init(Data['Provinces']);
  Municipalities.Init(Data['Municipalities']);
end;

{ TEntities.TSwitzerland }

procedure TEntities.TSwitzerland.Init(const AData: TDataItem);
begin
  Data:=AData;
  Cantons.Init(Data['Cantons']);
end;

end.
