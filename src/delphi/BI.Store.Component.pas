unit BI.Store.Component;

interface

uses
  System.Types, BI.Data, BI.Persist;

type
  TBIDesignImporter=class(TDataImporter)
  private
  public
    Constructor Create(const AStore:String; const ADefinition:TDataDefinition); override;
    Constructor CreatePath(const AStore:String; const ADefinition:TDataDefinition);

    Destructor Destroy; override;

    function AllData:TStringDynArray; override;
    function GetDefinition(const AName:String):TDataDefinition; override;
    function Import:TDataArray; override;
    class function IsRemote:Boolean; override;
    function Load(const AName:String):TDataItem; override;

    class function Supports(const Kind:TDataDefinitionKind):Boolean; override;
  end;

implementation
