{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid control for FireMonkey             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Grid;

interface

uses
  System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Layouts,
  Data.DB, BI.DataSet, BI.Data, BI.DataSource, FMX.Menus, BI.UI;

type
  TBIGridPluginClass=class of TBIGridPlugin;

  // Pending: Try to merge VCL and FMX TBIGridPlugin classes into a single one.

  // Abstract class to define Grid control classes as "plugins" of TBIGrid.
  // See BI.FMX.Grid.Grid unit for an example, using the standard FMX TGrid.
  TBIGridPlugin=class abstract
  protected
    IAlternate : TAlternateColor;

    procedure ChangedAlternate(Sender:TObject); virtual; abstract;
    function GetDataSource: TDataSource; virtual; abstract;
    function GetTotals:Boolean; virtual; abstract;
    procedure SetTotals(const Value:Boolean); virtual; abstract;
    procedure SetDataSource(const Value: TDataSource); virtual; abstract;
  public
    class var
      Engine : TBIGridPluginClass;

    Constructor Create(const AOwner:TComponent); virtual; abstract;

    procedure BindTo(const ADataSet:TDataSet); virtual; abstract;
    procedure Colorize(const AItems:TDataColorizers); virtual; abstract;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); virtual; abstract;
    function GetObject:TObject; virtual; abstract;

    property DataSource:TDataSource read GetDataSource write SetDataSource;
    property Totals:Boolean read GetTotals write SetTotals;
  end;

  // Generic Grid control that "links" a TDataItem with a Grid.
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TBIGrid = class(TLayout)
  private
    FAlternate : TAlternateColor;

    IDataSet : TBIDataSet;
    IPlugin : TBIGridPlugin;

    procedure CreateNewDataSet;
    function GetBIData: TDataItem;
    procedure ReadOrigin(Reader: TReader);
    procedure SetAlternate(const Value: TAlternateColor);
    procedure SetBIData(const Value: TDataItem);
    procedure SetPlugin(const Value: TBIGridPlugin);
    procedure WriteOrigin(Writer: TWriter);
    procedure SetDataSet(const Value: TBIDataSet);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure BindTo(const AData: TDataItem); overload;
    procedure BindTo(const ADatas: TDataArray); overload;

    procedure Colorize(const AItems:TDataColorizers);

    procedure Duplicates(const AData:TDataItem; const Hide:Boolean);

    class function Embedd(const AOwner:TComponent; const AParent:TFmxObject):TBIGrid; static;

    property DataSet:TBIDataSet read IDataSet write SetDataSet;
    property Plugin:TBIGridPlugin read IPlugin write SetPlugin;
  published
    property Alternate:TAlternateColor read FAlternate write SetAlternate;
    property Data:TDataItem read GetBIData write SetBIData;
  end;

  // Helper methods for Firemonkey:
  TFMXCommon=record
  public
    class procedure AddForm(const AForm: TCommonCustomForm; const AParent: TFmxObject); static;
    class function Ask(const ATitle,ACaption:String; var AValue:String):Boolean; static;
    class procedure LoadPosition(const AForm:TCommonCustomForm; const Key:String); static;
    class procedure Popup(const APopup:TPopupMenu; const AControl:TControl); static;
    class procedure SavePosition(const AForm:TCommonCustomForm; const Key:String); static;
    class function YesNo(const AMessage:String):Boolean; static;
  end;

implementation
