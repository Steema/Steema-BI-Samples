{*********************************************}
{  TeeBI Software Library                     }
{  Importing data from TComponent             }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Store.Component;

interface

{
  This unit declares the TComponentImporter class.

  TComponentImporter is used to import data from any TComponent into a
  TDataItem.

  For example, we can import contents from a TStrings:

  BIGrid1.Data := TComponentImporter.From(Self, Memo1.Lines)

  Supported component classes (and derived classes):

  - Any TComponent that has a TStrings or TStringList property
  - TXMLDocument
  - TDataSet
  - TField
  - TDataSource
  - TCustomConnection (FireDAC, any other database library, etc)
  - TBaseDataImporter
  - Any TComponent that has a TDataSource property

  - Any TComponent class supported by a "plugin":

    Available plugin classes:

      - TControlImporter (see VCLBI.Component and FMXBI.Component units)

  This class is also internally used by editor dialogs to allow selecting any
  control or component to import its data
}

uses
  {System.}Classes,
  {$IFNDEF FPC}
  System.Generics.Collections,
  {$ENDIF}
  {Data.}DB, BI.DataItem, BI.Persist;

type
  TComponentImporterClass=class of TComponentImporter;

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(TeeAllComponentPlatformIDs)]
  {$ENDIF}
  {$ENDIF}
  TComponentImporter=class(TBaseDataImporter)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FSource : TComponent;

    IDataLink : TDataLink;
    IDataSource : TDataSource;
    ILoading : Boolean;

    class function IgnoreError(const Sender:TObject; const Error:String):Boolean;
    procedure SetSource(const Value: TComponent);
    class function TryFromStrings(const ASource:TComponent):TDataItem;
  protected
    function DoImport(const AComponent: TComponent):TDataItem; virtual;
    procedure GetItems(const AData:TDataItem); override;
    class function GuessFrom(const AStrings:TStrings):TDataItem; overload; static;
    class function GuessFrom(const AString:String):TDataItem; overload; static;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    class function StringsOf(const ASource:TComponent):TStrings; virtual;
  public
    class var
      Plugins : TList{$IFNDEF FPC}<TComponentImporterClass>{$ENDIF};

    Destructor Destroy; override;

    procedure Refresh;

    class function DataOf(const AComponent:TComponent):TDataItem; virtual;
    class function DataOfProvider(const AData:TDataItem):TDataItem; static;
    class function From(const AOwner,AComponent:TComponent): TDataItem; overload; static;
    class function From(const AOwner,AComponent:TComponent; const AOrigin:String): TDataItem; overload; static;
    class function IsSupported(const AComponent:TComponent):Boolean; static;
    class function Origin(const AComponent:TComponent; const AData:TDataItem):String; static;
    class function ProviderOf(const AData:TDataItem):TComponent; static;
    class function Supports(const AComponent:TComponent):Boolean; virtual;
    class function TryLoadOrigin(const AOwner,AProvider:TComponent; var AOrigin:String):TDataItem; static;
  published
    property Source:TComponent read FSource write SetSource;
  end;

implementation

uses
  {$IFDEF FPC}
  XMLRead, DOM,
  {$ELSE}
  System.Rtti, Xml.XmlDoc,
  {$ENDIF}
  BI.DataSource, BI.DB.Dataset, BI.DB;

{ TComponentImporter }

type
  TDataAccess=class(TDataItem);

// Global cache of imported Components
var
  ImportCache : TList{$IFNDEF FPC}<TComponentImporter>{$ENDIF}=nil;

Destructor TComponentImporter.Destroy;
begin
  IDataLink.Free;
  IDataSource.Free;

  if ImportCache<>nil then
     ImportCache.Remove(Self);

  inherited;
end;

class function TComponentImporter.ProviderOf(const AData:TDataItem):TComponent;
var tmp : TDataProvider;
begin
  if AData=nil then
     result:=nil
  else
  begin
    tmp:=TDataAccess(AData).GetProvider;

    if tmp is TComponentImporter then
       result:=TComponentImporter(tmp).Source
    else
    if tmp is TDataDelayProvider then
       result:=nil
    else
       result:=tmp;
  end;
end;

procedure TComponentImporter.Refresh;
begin
  Changed;
end;

class function TComponentImporter.DataOfProvider(const AData:TDataItem):TDataItem;
begin
  result:=AData;

  if result<>nil then
  repeat
    if result.Provider<>nil then
       Exit
    else
       result:=result.Parent;

  until result=nil;
end;

class function TComponentImporter.GuessFrom(const AString: String): TDataItem;
var tmp : TStrings;
begin
  tmp:=TStringList.Create;
  try
    tmp.Text:=AString;
    result:=GuessFrom(tmp);
  finally
    tmp.Free;
  end;
end;

class function TComponentImporter.GuessFrom(const AStrings:TStrings):TDataItem;
var tmp : TBIFileSourceClass;
begin
  tmp:=TBIFileSource.GuessFromContent(AStrings);

  if tmp=nil then
     result:=nil
  else
     result:=tmp.FromStrings(AStrings);
end;

class function TComponentImporter.TryFromStrings(const ASource:TComponent):TDataItem;
var tmp : TStrings;
begin
  tmp:=Self.StringsOf(ASource);

  if tmp=nil then
     result:=nil
  else
  begin
    result:=GuessFrom(tmp);

    if result<>nil then
       result.Name:=ASource.Name;
  end;
end;

class function TComponentImporter.IgnoreError(const Sender:TObject; const Error:String):Boolean;
begin
  result:=True;
end;

class function TComponentImporter.TryLoadOrigin(const AOwner,
  AProvider: TComponent; var AOrigin: String): TDataItem;
var tmp : TDataItem;
begin
  if AProvider=nil then
     tmp:=nil
  else
     tmp:=TComponentImporter.From(AOwner,AProvider);

  if AOrigin='' then
     result:=tmp
  else
  begin
    if tmp<>nil then
       tmp.Load;

    result:=TStore.OriginToData(tmp,'',AOrigin,IgnoreError);

    if result<>nil then
       AOrigin:='';
  end;
end;

class function TComponentImporter.From(const AOwner,AComponent:TComponent; const AOrigin:String): TDataItem;
var tmp : TDataItem;
begin
  if AComponent=nil then
     result:=nil
  else
  begin
    tmp:=TComponentImporter.From(AOwner as TComponent,AComponent);

    if AOrigin='' then
       result:=tmp
    else
       result:=TStore.OriginToData(tmp,'',AOrigin);
  end;
end;

procedure TComponentImporter.GetItems(const AData: TDataItem);
begin
  Load(AData,AData.AsTable);
end;

class function TComponentImporter.IsSupported(const AComponent: TComponent): Boolean;
var tmp : TComponentImporterClass;
begin
  result:=Supports(AComponent);

  if not result then
     for tmp in Plugins do
         if tmp.Supports(AComponent) then
            Exit(True);
end;

class function TComponentImporter.DataOf(const AComponent:TComponent):TDataItem;
begin
  if AComponent is TBaseDataImporter then // <-- before "is TDataProvider"
     result:=TBaseDataImporter(AComponent).Data
  else
  if AComponent is TDataProvider then
     result:=TDataProvider(AComponent).NewData
  else
     result:=nil;
end;

function FindCache(const AComponent:TComponent):TComponentImporter;
var t : Integer;
begin
  result:=nil;

  if ImportCache<>nil then
     for t:=0 to ImportCache.Count-1 do
         {$IFDEF FPC}
         if TComponentImporter(ImportCache[t]).FSource=AComponent then
            Exit(TComponentImporter(ImportCache[t]));
         {$ELSE}
         if ImportCache[t].FSource=AComponent then
            Exit(ImportCache[t]);
         {$ENDIF}
end;

procedure DestroyCache;
var t : Integer;
begin
  if ImportCache<>nil then
  begin
    for t:=0 to ImportCache.Count-1 do
        {$IFDEF AUTOREFCOUNT}
        ImportCache[t].DisposeOf;
        {$ELSE}
        {$IFDEF FPC}
        TComponentImporter(ImportCache[t]).Free;
        {$ELSE}
        ImportCache[t].Free;
        {$ENDIF}
        {$ENDIF}

    ImportCache.Free;
    ImportCache:=nil;
  end;
end;

procedure AddToCache(const AImporter:TComponentImporter);
begin
  if ImportCache=nil then
     ImportCache:=TList{$IFNDEF FPC}<TComponentImporter>{$ENDIF}.Create
  else
  if ImportCache.IndexOf(AImporter)<>-1 then
     Exit;

  ImportCache.Add(AImporter);
end;

class function TComponentImporter.From(const AOwner,AComponent:TComponent): TDataItem;

  function FromNewImporter(const AClass:TComponentImporterClass):TDataItem;
  var tmp : TComponentImporter;
  begin
    tmp:=FindCache(AComponent);

    if tmp=nil then
    begin
      tmp:=AClass.Create(AOwner);
      tmp.Source:=AComponent;
    end;

    result:=tmp.Data;
  end;

var tmp : TComponentImporterClass;
begin
  result:=DataOf(AComponent);

  if result=nil then
  begin
    if Supports(AComponent) then
       result:=FromNewImporter(TComponentImporter)
    else
    for tmp in Plugins do
        if tmp.Supports(AComponent) then
        begin
          result:=tmp.DataOf(AComponent);

          if result=nil then
             Exit(FromNewImporter(tmp))
          else
             break;
        end;
  end;
end;

procedure TComponentImporter.Load(const AData: TDataItem;
  const Children: Boolean);

  procedure DoLoad;
  var tmp : TDataItem;
      tmpArray : TDataArray;
  begin
    AData.Clear;

    if FSource<>nil then
    begin
      if Title='' then
         AData.Name:=Source.Name
      else
         AData.Name:=Title;

      tmp:=DoImport(FSource);

      if tmp<>nil then
      try
        tmp.Load;

        tmp.Name:=AData.Name;

        SetLength(tmpArray,1);
        tmpArray[0]:=tmp;

        TDataDefinition.Merge(AData,tmpArray);

        TDataAccess(AData).ClearDelay;
      finally
        tmp.Free;
      end;
    end;
  end;

begin
  if not ILoading then
  begin
    ILoading:=True;
    try
      DoLoad;
    finally
      ILoading:=False;
    end;
  end;
end;

function TComponentImporter.DoImport(const AComponent: TComponent):TDataItem;

  function FromDataSet(const ADataset:TDataset):TDataItem;
  begin
    if ADataset.Active then
       result:=TBIDataSetSource.From(ADataset,AComponent.Name)
    else
       result:=nil;
  end;

  function FromConnection(const AConnection:TCustomConnection):TDataItem;
  var tmp : TBIDB;
  begin
    if AConnection.DataSetCount=0 then
    begin
      tmp:=TBIDB.Create;
      try
        result:=TDataItem.Create(tmp.Import(AConnection));
        result.Name:=AConnection.Name;
      finally
        tmp.Free;
      end;
    end
    else
      result:=TBIDataSetSource.From(AConnection);
  end;

begin
  if (AComponent is TDataSource) and (TDataSource(AComponent).DataSet<>nil) then
     result:=FromDataset(TDataSource(AComponent).DataSet)
  else
  if AComponent is TDataSet then
     result:=FromDataset(TDataSet(AComponent))
  else
  if AComponent is TField then
     result:=TBIDataSetSource.From(TField(AComponent))
  else
  if AComponent is TBaseDataImporter then // <-- before "is TDataProvider"
     result:=TBaseDataImporter(AComponent).Data
  else
  if AComponent is TDataProvider then
     result:=TDataProvider(AComponent).NewData
  else
  if AComponent is TCustomConnection then
     result:=FromConnection(TCustomConnection(AComponent))
  else
     result:=TryFromStrings(AComponent);
end;

procedure TComponentImporter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (AComponent=FSource) and (Operation=TOperation.opRemove) then
     Source:=nil;
end;

class function TComponentImporter.Origin(const AComponent: TComponent;
  const AData: TDataItem): String;
var tmp : TDataItem;
begin
  if (AComponent=nil) or (AComponent is TDataDelayProvider) then
     tmp:=nil
  else
     tmp:=TComponentImporter.DataOfProvider(AData);

  if AData=tmp then
     result:=''
  else
     result:=TStore.OriginOf(AData,TStore.DefaultName,tmp);
end;

type
  TBIDataLink=class(TDataLink)
  private
    Importer : TComponentImporter;
  protected
    procedure DataEvent(Event: TDataEvent; Info: {$IFDEF FPC}Ptrint{$ELSE}NativeInt{$ENDIF}); override;
  end;

procedure TComponentImporter.SetSource(const Value: TComponent);

  function CreateDataSource(const ADataSet:TDataSet):TDataSource;
  begin
    IDataSource.Free;
    IDataSource:=TDataSource.Create(Self);
    IDataSource.DataSet:=ADataSet;

    result:=IDataSource;
  end;

  procedure AddDataLink(const ADataSource:TDataSource);
  begin
    IDataLink:=TBIDataLink.Create;
    TBIDataLink(IDataLink).Importer:=Self;
    IDataLink.DataSource:=ADataSource;
  end;

begin
  if FSource<>Value then
  begin
    if FSource<>nil then
    begin
      FSource.RemoveFreeNotification(Self);

      IDataLink.Free;
      IDataSource.Free;
    end;

    FSource:=Value;

    if FSource<>nil then
    begin
      AddToCache(Self);

      FSource.FreeNotification(Self);

      if FSource is TDataSource then
         AddDataLink(TDataSource(FSource))
      else
      if FSource is TDataSet then
         AddDataLink(CreateDataSource(TDataSet(FSource)));
    end;

    Changed;
  end;
end;

class function TComponentImporter.StringsOf(const ASource: TComponent): TStrings;
begin
  {$IFNDEF FPC}
  if ASource is TXMLDocument then
     result:=TXMLDocument(ASource).XML
  else
  {$ENDIF}
     result:=nil;
end;

class function TComponentImporter.Supports(const AComponent: TComponent): Boolean;

  function HasStrings(const AComponent:TComponent):Boolean;
  begin
    result:=StringsOf(AComponent)<>nil;
  end;

  function CanImport(const AComponent:TComponent):Boolean;
  begin
    result:=HasStrings(AComponent) or
            (AComponent is TDataset) or
            (AComponent is TField) or
            (AComponent is TDataSource) or
            (AComponent is TCustomConnection) or
            // (AComponent is TDatabase) or <-- BDE: use "BI.DB.BDE.pas"
            (AComponent is TBaseDataImporter);
  end;

  function HasDataSource(const AComponent:TComponent):Boolean;
  {$IFDEF FPC}
  begin
    result:=False; // pending RTTI with FPC
  end;
  {$ELSE}
  var tmp : TRttiType;
      tmpProps : TArray<TRttiProperty>;
      tmpProp : TRttiProperty;

      Rtti : TRttiContext;
      tmpDataSource : TRttiType;
  begin
    result:=False;

    Rtti:=TRttiContext.Create;

    tmp:=Rtti.GetType(AComponent.ClassInfo);

    if tmp<>nil then
    begin
      tmpDataSource:=Rtti.GetType(TDataSource);

      tmpProps:=tmp.GetProperties;

      for tmpProp in tmpProps do
          if tmpProp.PropertyType=tmpDataSource then
             Exit(True);
    end;
  end;
  {$ENDIF}

begin
  result:=CanImport(AComponent) or
          HasDataSource(AComponent);
end;

{ TBIDataLink }

procedure TBIDataLink.DataEvent(Event: TDataEvent; Info: {$IFDEF FPC}Ptrint{$ELSE}NativeInt{$ENDIF});
begin
  inherited;

  if not (csDestroying in Importer.ComponentState) then
     Importer.Changed;
end;

initialization
  TComponentImporter.Plugins:=TList{$IFNDEF FPC}<TComponentImporterClass>{$ENDIF}.Create;
finalization
  TComponentImporter.Plugins.Free;
  DestroyCache;
end.

