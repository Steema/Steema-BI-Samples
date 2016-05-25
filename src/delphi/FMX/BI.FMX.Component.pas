{*********************************************}
{  TeeBI Software Library                     }
{  Importing data from VCL Controls           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Component;
{$DEFINE FMX}

interface

{
  This unit contains a TControlImporter component for VCL and FMX.

  Its purpose is to obtain data from any supported TControl.

  Like for example, using Memo.Lines text to import (in JSON, CSV, XML format),
  or using the Data property of a BI control (Grid, Chart, Tree, etc).

  Usage example:

  tmp:=TControlImporter.Create(Self);
  tmp.Source:=Memo1;

  BIGrid1.Data:=tmp.Data;
}

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
    class function HasTextProperty(const AComponent:TComponent):Boolean; static;
    class function StringsOf(const ASource:TComponent):TStrings; override;
    class function Supports(const AComponent:TComponent):Boolean; override;
    class function TextOf(const AComponent:TComponent):String; overload; static;
  end;

implementation
