unit BI.Store.Component;

interface

uses
  System.Classes, System.Types, System.Generics.Collections,
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
  TComponentImporter=class(TDataProvider)
  private
    FData : TDataItem;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FSource : TComponent;

    function GetData: TDataItem;
    procedure Notify(const AEvent:TBIEvent);
    procedure SetSource(const Value: TComponent);
    function TryFromStrings(const ASource:TComponent):TDataItem;
  protected
    function DoImport(const AComponent: TComponent):TDataItem; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    function StringsOf(const ASource:TComponent):TStrings; virtual;
    class function Supports(const AComponent:TComponent):Boolean; virtual;
  public
    class var
      Plugins : TList<TComponentImporterClass>;

    Destructor Destroy; override;

    class function From(const AOwner,AComponent:TComponent): TDataItem; static;

    property Data:TDataItem read GetData;
  published
    property Source:TComponent read FSource write SetSource;
  end;

implementation
