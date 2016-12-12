{*********************************************}
{  TeeBI Software Library                     }
{  TTeeBISource class                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Chart.Source;
{$DEFINE FMX}

interface

uses
  System.Classes,
  {$IFDEF FMX}
  FMXTee.Engine,
  {$ELSE}
  VCLTee.TeEngine,
  {$ENDIF}
  BI.Data, BI.Data.CollectionItem;

type
  TTeeBISource=class(TTeeSeriesSource)
  private
    FItem : TDataCollectionItem;

    function GetData: TDataItem;
    function GetProvider: TComponent;
    procedure SetData(const Value: TDataItem);
    procedure SetProvider(const Value: TComponent);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    class Function Description:String; override;
    class Function Editor:TComponentClass; override;

    Procedure Load; override;
  published
    property Active;
    property Data:TDataItem read GetData write SetData;
    property Provider:TComponent read GetProvider write SetProvider;
    property Series;
  end;

implementation
