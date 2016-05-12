unit BI.FMX.Component;
{$DEFINE FMX}

interface

uses
  System.Classes, BI.Data, BI.Store.Component;

type
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TControlImporter=class(TComponentImporter)
  protected
    function DoImport(const AComponent: TComponent):TDataItem; override;
  public
    class function DataOf(const AComponent:TComponent):TDataItem; override;
    class function HasDataProperty(const AComponent:TComponent):Boolean; static;
    class function StringsOf(const ASource:TComponent):TStrings; override;
    class function Supports(const AComponent:TComponent):Boolean; override;
  end;

implementation
