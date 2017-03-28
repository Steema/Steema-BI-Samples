{*********************************************}
{  TeeBI Software Library                     }
{  Importing data from TComponent             }
{  Copyright (c) 2015-2017 by Steema Software }
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

      - TControlImporter (see BI.VCL.Component and BI.FMX.Component units)

  This class is also internally used by editor dialogs to allow selecting any
  control or component to import its data
}

uses
  {System.}Classes,
  {$IFNDEF FPC}
  System.Generics.Collections,
  {$ENDIF}
  {Data.}DB, BI.Data, BI.Persist;

type
  TComponentImporterClass=class of TComponentImporter;

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
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
