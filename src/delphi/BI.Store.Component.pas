{*********************************************}
{  TeeBI Software Library                     }
{  Importing data from TComponent             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Store.Component;

interface

uses
  System.Classes, System.Types, System.Generics.Collections, Data.DB,
  BI.Data, BI.Persist;

type
  TComponentImporterClass=class of TComponentImporter;

  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TComponentImporter=class(TBaseDataImporter)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FSource : TComponent;

    IDataLink : TDataLink;
    ILoading : Boolean;

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
      Plugins : TList<TComponentImporterClass>;

    Constructor Create(AOwner:TComponent); override;
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
