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

{$R *.fmx}

uses
  BI.UI, BI.Persist, BI.Info;

{ TStoreStatus }

procedure TStoreStatus.BUnloadAllClick(Sender: TObject);
begin
  TStore.UnLoadAll;
  RefreshMemory;
end;

procedure TStoreStatus.FormCreate(Sender: TObject);
begin
  Grid:=TBIGrid.Create(Self);
  Grid.Align:=TUICommon.AlignClient;
  Grid.Parent:=TabItem1;

  RefreshInfo;
end;

procedure TStoreStatus.FormDestroy(Sender: TObject);
begin
  Data.Free;
end;

procedure TStoreStatus.RefreshInfo;
begin
  Data.Free;
  Data:=TDataInfo.Create(TStores.GlobalCache);
  Data.Name:=TStore.DefaultName;

  Grid.BindTo(Data);
  RefreshMemory;
end;

procedure TStoreStatus.Image1Click(Sender: TObject);
begin
  RefreshInfo;
end;

procedure TStoreStatus.RefreshMemory;
var Avail,
    Used : Int64;
begin
  Avail:=TMemory.Available;
  Used:=TMemory.Allocated;

  if Avail=0 then
     LMemory.Text:=''
  else
     LMemory.Text:=FormatFloat('0.##%',Used*100/Avail)+' of '+
                   TCommonUI.BytesToString(Avail)+
                   '  ('+TCommonUI.BytesToString(Used)+')';

  BUnloadAll.Enabled:=TStores.GlobalCache.Count>0;
end;

class procedure TStoreStatus.View(const AOwner: TComponent);
begin
  with TStoreStatus.Create(AOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
