{*********************************************}
{  TeeBI Software Library                     }
{  Memory Cache Status dialog                 }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Status;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, BI.DataItem, FMX.ListBox, FMXBI.Grid, FMX.Objects;

type
  TStoreStatus = class(TForm)
    Layout1: TLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label1: TLabel;
    LMemory: TLabel;
    Layout2: TLayout;
    BUnloadAll: TButton;
    Image1: TImage;
    procedure BUnloadAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    Grid : TBIGrid;
    Data : TDataItem;

    procedure RefreshInfo;
    procedure RefreshMemory;
  public
    { Public declarations }
    class procedure View(const AOwner:TComponent);
  end;

implementation
