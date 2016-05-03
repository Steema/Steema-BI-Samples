unit BI.VCL.Component;
{.$DEFINE FMX}

interface

uses
  System.Classes, BI.Data, BI.Store.Component;

type
  TControlImporter=class(TComponentImporter)
  protected
    function DoImport(const AComponent: TComponent):TDataItem; override;
    function StringsOf(const ASource:TComponent):TStrings; override;
    class function Supports(const AComponent:TComponent):Boolean; override;
  public
    class function DataOf(const AComponent:TComponent):TDataItem; static;
    class function HasData(const AComponent:TComponent):Boolean; static;
    class function From(const AOwner,ASource:TComponent):TDataItem; static;
  end;

implementation
