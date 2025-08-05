{*********************************************}
{  TeeBI Software Library                     }
{  BIChart Editor dialog                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Chart;

interface

{
  This dialog is used to edit BIChart controls.

  It offers the UI to change all properties that BIChart has in addition of the
  normal TChart properties, as well as allowing changing the Data to chart.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  VclTee.TeeConst, VclTee.TeeEditCha, VCLBI.Chart, VCLBI.DataControl,
  VCLBI.Grid, VCLBI.GridForm, Vcl.Buttons, Vcl.CheckLst, BI.DataItem,

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  {$IFDEF TEEPRO}
  VCLTee.TeeEdit, VCLTee.TeeWorldSeries, VCLTee.TeeMapSeries,
  {$ENDIF}

  VCLBI.Editor.ListItems, VCLTee.TeeProcs, VCLTee.EditChar;

// XE6 dcc32 BUG, workaround not available
{$IF CompilerVersion>27}
{$I BI.Chart.Options.inc} // <-- see .inc contents for reason/explanation
{$ENDIF}

{$IFDEF TEEPRO}
{$IFDEF CHARTLAYERS}
  {$DEFINE PROLAYERS}
{$ENDIF}
{$ENDIF}

type
  TBIChartEditor = class(TForm)
    PageControl1: TPageControl;
    PanelButtons: TPanel;
    Panel9: TPanel;
    BOK: TButton;
    TabOptions: TTabSheet;
    TabChart: TTabSheet;
    TabData: TTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    PageMode: TPageControl;
    Tab2D: TTabSheet;
    Tab3D: TTabSheet;
    Panel3D: TPanel;
    BSwapXY: TSpeedButton;
    BSwapYZ: TSpeedButton;
    BSwapXZ: TSpeedButton;
    Shape1: TShape;
    Shape2: TShape;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CBX: TComboBox;
    CBY: TComboBox;
    CBZ: TComboBox;
    LB3D: TListBox;
    LBXYZ: TListBox;
    RG3D: TRadioGroup;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    RBAuto: TRadioButton;
    RBXY: TRadioButton;
    RB3D: TRadioButton;
    RBFinancial: TRadioButton;
    RBGeo: TRadioButton;
    RGDirection: TRadioGroup;
    TabFinancial: TTabSheet;
    TabGeo: TTabSheet;
    Label7: TLabel;
    CBOpen: TComboBox;
    CBAutoFinancial: TCheckBox;
    CBClose: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    CBHigh: TComboBox;
    Label10: TLabel;
    CBLow: TComboBox;
    LBFinancial: TListBox;
    CB3D: TCheckBox;
    PanelYTable: TPanel;
    TabView: TTabSheet;
    RGView: TRadioGroup;
    CBOpenGL: TCheckBox;
    RGLegend: TRadioGroup;
    RGMarks: TRadioGroup;
    Label12: TLabel;
    CBVolume: TComboBox;
    Panel3: TPanel;
    GroupBox2: TLabel;
    LB2D: TListBox;
    Label11: TLabel;
    CBStacked: TComboBox;
    Panel4: TPanel;
    PanelItems: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CBX2D: TComboBox;
    CBText: TComboBox;
    CBColors: TComboBox;
    PanelY: TPanel;
    CBHoriz2D: TComboBox;
    CBFlags: TCheckBox;
    CBStates: TCheckBox;
    GroupBox3: TGroupBox;
    CBAutoMap: TCheckBox;
    CBMap: TComboBox;
    CBCities: TCheckBox;
    CBMap3D: TCheckBox;
    GroupBox4: TGroupBox;
    CBAutoTitle: TCheckBox;
    BEditTitle: TButton;
    procedure PageControl1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBAutoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RBXYClick(Sender: TObject);
    procedure RB3DClick(Sender: TObject);
    procedure RBFinancialClick(Sender: TObject);
    procedure RBGeoClick(Sender: TObject);
    procedure RG3DClick(Sender: TObject);
    procedure LB2DClick(Sender: TObject);
    procedure BSwapXYClick(Sender: TObject);
    procedure BSwapYZClick(Sender: TObject);
    procedure BSwapXZClick(Sender: TObject);
    procedure CBTextChange(Sender: TObject);
    procedure LB3DClick(Sender: TObject);
    procedure CBX2DChange(Sender: TObject);
    procedure RGDirectionClick(Sender: TObject);
    procedure LBXYZClick(Sender: TObject);
    procedure CBXChange(Sender: TObject);
    procedure CBYChange(Sender: TObject);
    procedure CBZChange(Sender: TObject);
    procedure RGViewClick(Sender: TObject);
    procedure CBOpenGLClick(Sender: TObject);
    procedure RGLegendClick(Sender: TObject);
    procedure RGMarksClick(Sender: TObject);
    procedure CBOpenChange(Sender: TObject);
    procedure CBCloseChange(Sender: TObject);
    procedure CBHighChange(Sender: TObject);
    procedure CBLowChange(Sender: TObject);
    procedure CBVolumeChange(Sender: TObject);
    procedure CBStackedChange(Sender: TObject);
    procedure CBHoriz2DChange(Sender: TObject);
    procedure CBFlagsClick(Sender: TObject);
    procedure CBStatesClick(Sender: TObject);
    procedure CBAutoMapClick(Sender: TObject);
    procedure CBCitiesClick(Sender: TObject);
    procedure CBMap3DClick(Sender: TObject);
    procedure CBMapChange(Sender: TObject);
    procedure LBFinancialClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBAutoTitleClick(Sender: TObject);
    procedure BEditTitleClick(Sender: TObject);
  private
    { Private declarations }

    FChart : TBIChart;

    IChanging : Boolean;

    IListY,
    IListYTable : TFormListItems;

    IGrid : TBIGridForm;

    {$IFDEF TEEPRO}
    ChartEditor : TChartEditorPanel;
    {$ELSE}
    ChartEditor : TChartEditForm;
    {$ENDIF}

    procedure Changed3DItem;
    procedure ChangeY(const AIndex:Integer; const ACombo:TComboBox);
    procedure CreateChartEditor;
    function CurrentText:TDataItem;
    function DataOf(const ACombo:TComboBox):TDataItem;
    procedure DoExchange(const A,B:TComboBox);
    procedure Recreate;
    procedure RecreateY(Sender:TObject);
    procedure ResetEditor;
    procedure SetModeOptions;
    procedure SetPostSettings;
    procedure SetChart(const Value: TBIChart);
    procedure SetChartEditorChart;

    {$IFDEF TEEPRO}
    function WorldMap:TWorldSeries;
    {$ENDIF}
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RefreshData(const AItems:TBIChartItems); overload;
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const AChart:TBIChart); static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AChart:TBIChart):TBIChartEditor; static;

    procedure RefreshData; overload;

    property Chart:TBIChart read FChart write SetChart;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF TEEPRO}
  VCLTee.TeeComma,
  VCLTee.TeeThemeEditor,
  VCLTee.TeeToolsGalleryDemos, VCLTee.TeeAnimationsGalleryDemos,
  VCLTee.TeeSurfa, VCLTee.OHLChart,
  VCLTee.CandleCh, VCLTee.TeePolar, VCLTee.TeeTreeMapSeries,
  VCLTee.TeeTernary, VCLTee.TeePoin3, VCLTee.TeeTriSurface,

  VCLTee.TeeGLCanvas,
  {$ENDIF}

  VclTee.TeePenDlg, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeGDIPlus,
  VCLBI.DataSelect,

  VCLBI.Chart.Plugin,

  VCLBI.Editor.Query; // <-- needed to "hook" a BIChart inside the Query Preview tab

type
  TBIChartAccess=class(TBIChart);
  TBITChartAccess=class(TBITChart);

const
  RegistryChartEditor='ChartEditor';

procedure TBIChartEditor.Button1Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=Chart.Data;

  if TDataSelector.Choose(Self,IGrid.Grid,tmp,Self) then
  begin
    IGrid.Grid.Data:=tmp;
    Chart.Data:=tmp;

    IChanging:=True;
    try
      ResetEditor;
      RefreshData(Chart.Options.Items);
    finally
      IChanging:=False;
    end;
  end;
end;

procedure TBIChartEditor.ChangeY(const AIndex:Integer; const ACombo:TComboBox);
begin
  if Chart.Options.Items.Y.Count<=AIndex then
     SetLength(Chart.Options.Items.Y,AIndex+1);

  Chart.Options.Items.Y[AIndex]:=DataOf(ACombo);
  Recreate;
end;

procedure TBIChartEditor.CBAutoMapClick(Sender: TObject);
begin
  CBMap.Enabled:=not CBAutoMap.Checked;
end;

procedure TBIChartEditor.CBAutoTitleClick(Sender: TObject);
begin
  if CBAutoTitle.Checked then
     Chart.Options.Title:=TBIChartTitle.Automatic
  else
     Chart.Options.Title:=TBIChartTitle.Custom;

  BEditTitle.Enabled:=Chart.Options.Title=TBIChartTitle.Custom;
end;

procedure TBIChartEditor.CBCloseChange(Sender: TObject);
begin
  ChangeY(1,CBClose);
end;

function FindSeries(const AList:TChartSeriesList; const AClass:TChartSeriesClass):TChartSeries;
var t : Integer;
begin
  for t:=0 to AList.Count-1 do
      if AList[t].InheritsFrom(AClass) then
         Exit(AList[t]);

  result:=nil;
end;

{$IFDEF TEEPRO}
function TBIChartEditor.WorldMap:TWorldSeries;
begin
  result:=FindSeries(Chart.Chart.SeriesList,TWorldSeries) as TWorldSeries;
end;
{$ENDIF}

procedure TBIChartEditor.CBFlagsClick(Sender: TObject);
{$IFDEF TEEPRO}
var tmp : TWorldSeries;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
  tmp:=WorldMap;

  if tmp<>nil then
     tmp.Flags.Visible:=CBFlags.Checked;
  {$ENDIF}
end;

procedure TBIChartEditor.CBHighChange(Sender: TObject);
begin
  ChangeY(2,CBHigh);
end;

procedure TBIChartEditor.CBHoriz2DChange(Sender: TObject);
begin
  Chart.Options.SeriesDirection:=TBISeriesDirection(CBHoriz2D.ItemIndex);
  LB2DClick(Self);
end;

procedure TBIChartEditor.CBLowChange(Sender: TObject);
begin
  ChangeY(3,CBLow);
end;

procedure TBIChartEditor.CBMap3DClick(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  if CBMap3D.Checked then
     WorldMap.RenderMode:=TMapRenderMode.rm3D
  else
     WorldMap.RenderMode:=TMapRenderMode.rmDefault;
  {$ENDIF}
end;

procedure TBIChartEditor.CBOpenChange(Sender: TObject);
begin
  ChangeY(0,CBOpen);
end;

procedure TBIChartEditor.CBOpenGLClick(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  if CBOpenGL.Checked then
     Chart.Chart.Canvas:=TGLCanvas.Create
  else
     Chart.Chart.Canvas:=TGDIPlusCanvas.Create;
  {$ENDIF}
end;

procedure TBIChartEditor.CBStackedChange(Sender: TObject);
begin
  Chart.Options.Stacked:=TBIChartStacked(CBStacked.ItemIndex);
end;

function TBIChartEditor.DataOf(const ACombo:TComboBox):TDataItem;
var tmp : Integer;
begin
  tmp:=ACombo.ItemIndex;

  if tmp=-1 then
     result:=nil
  else
     result:=TDataItem(ACombo.Items.Objects[tmp]);
end;

function TBIChartEditor.CurrentText:TDataItem;
begin
  result:=DataOf(CBText);
end;

procedure TBIChartEditor.CBTextChange(Sender: TObject);
begin
  if CBText.ItemIndex<>-1 then
  begin
    Chart.Options.Items.Text:=CurrentText;
    Recreate;
  end;
end;

procedure TBIChartEditor.CBStatesClick(Sender: TObject);
{$IFDEF PROLAYERS}
var tmp : TWorldSeries;
{$ENDIF}
begin
  {$IFDEF PROLAYERS}
  tmp:=WorldMap;

  if tmp<>nil then
     tmp.Layers.States:=CBStates.Checked;
  {$ENDIF}
end;

procedure TBIChartEditor.CBVolumeChange(Sender: TObject);
begin
  ChangeY(4,CBVolume);
end;

procedure TBIChartEditor.CBX2DChange(Sender: TObject);
var tmp : Integer;
    tmpData : TDataItem;
begin
  tmp:=CBX2D.ItemIndex;

  if tmp<>-1 then
  begin
    tmpData:=TDataItem(CBX2D.Items.Objects[tmp]);

    if tmpData=nil then
       if Chart.Options.Items.X<>nil then
       begin
         IListY.TryAdd(Chart.Options.Items.X);
         IListYTable.TryAdd(Chart.Options.Items.X);
       end;

    Chart.Options.Items.X:=tmpData;
    Recreate;
  end;
end;

procedure TBIChartEditor.Changed3DItem;
begin
  CB3D.Checked:=False;
  TBIChartAccess(Chart).DirectRefresh;
end;

procedure TBIChartEditor.CBXChange(Sender: TObject);
var tmp : Integer;
begin
  tmp:=CBX.ItemIndex;

  if tmp<>-1 then
  begin
    Chart.Options.Items.X:=TDataItem(CBX.Items.Objects[tmp]);
    Changed3DItem;
  end;
end;

procedure TBIChartEditor.CBYChange(Sender: TObject);
var tmp : Integer;
begin
  tmp:=CBY.ItemIndex;

  if tmp<>-1 then
  begin
    if Chart.Options.Items.Y=nil then
       SetLength(Chart.Options.Items.Y,1);

    Chart.Options.Items.Y[0]:=TDataItem(CBY.Items.Objects[tmp]);
    Changed3DItem;
  end;
end;

procedure TBIChartEditor.CBZChange(Sender: TObject);
var tmp : Integer;
begin
  tmp:=CBZ.ItemIndex;

  if tmp<>-1 then
  begin
    Chart.Options.Items.Z:=TDataItem(CBZ.Items.Objects[tmp]);
    Changed3DItem;
  end;
end;

class procedure TBIChartEditor.Edit(const AOwner: TComponent;
  const AChart: TBIChart);
begin
  with TBIChartEditor.Create(AOwner) do
  try
    Chart:=AChart;
    ShowModal;
  finally
    Free;
  end;
end;

class function TBIChartEditor.Embedd(const AOwner: TComponent;
  const AParent: TWinControl; const AChart: TBIChart): TBIChartEditor;
begin
  result:=TBIChartEditor.Create(AOwner);
  result.Chart:=AChart;
  result.PanelButtons.Visible:=False;

  TUICommon.AddForm(result,AParent);
end;

procedure TBIChartEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,RegistryChartEditor);
end;

procedure TBIChartEditor.FormCreate(Sender: TObject);
begin
  IChanging:=True;

  PageControl1.ActivePage:=TabOptions;

  {$IFNDEF TEEPRO}
  RB3D.Enabled:=False;
  RBFinancial.Enabled:=False;
  RBGeo.Enabled:=False;

  CBOpenGL.Enabled:=False;
  {$ENDIF}

  LB2D.Items.Objects[1]:=TObject(TAreaSeries);
  LB2D.Items.Objects[2]:=TObject(TBarSeries);
  LB2D.Items.Objects[3]:=TObject(TLineSeries);
  LB2D.Items.Objects[4]:=TObject(TLineSeries);
  LB2D.Items.Objects[5]:=TObject(TPieSeries);
  LB2D.Items.Objects[6]:=TObject(TPointSeries);

  {$IFDEF TEEPRO}
  LB2D.Items.Objects[7]:=TObject(TPolarSeries);
  LB2D.Items.Objects[8]:=TObject(TRadarSeries);

  // 3D Grid
  LB3D.Items.Objects[1]:=TObject(TContourSeries);
  LB3D.Items.Objects[2]:=TObject(TColorGridSeries);
  LB3D.Items.Objects[3]:=TObject(TSurfaceSeries);
  LB3D.Items.Objects[4]:=TObject(TTowerSeries);
  LB3D.Items.Objects[5]:=TObject(TTriSurfaceSeries);

  // 3D XYZ
  LBXYZ.Items.Objects[1]:=TObject(TPoint3DSeries);
  LBXYZ.Items.Objects[2]:=TObject(TTernarySeries);
  LBXYZ.Items.Objects[3]:=TObject(TTriSurfaceSeries);

  {$ELSE}
  LB2D.Items.Delete(7);
  LB2D.Items.Delete(7);
  {$ENDIF}

  LB2D.ItemIndex:=0;
  LBXYZ.ItemIndex:=0;
  LB3D.ItemIndex:=0;
  LBFinancial.ItemIndex:=0;

  IListY:=TFormListItems.Create(Self);
  IListY.OnChanged:=RecreateY;
  TUICommon.AddForm(IListY,PanelY);

  IListYTable:=TFormListItems.Embed(Self,PanelYTable);
  IListYTable.OnChanged:=RecreateY;

  {$IFNDEF PROLAYERS}
  CBStates.Visible:=False;
  CBCities.Visible:=False;
  {$ENDIF}
end;

function FindClass(const AItems:TStrings; const AClass:TChartSeriesClass):Integer;
var t : Integer;
begin
  for t:=0 to AItems.Count-1 do
      if AItems.Objects[t]=TObject(AClass) then
         Exit(t);

  result:=-1;
end;

procedure TBIChartEditor.ResetEditor;
  
  {$IFDEF PROLAYERS}
  procedure SetGeoLayers;
  var tmp : TWorldSeries;
  begin
    tmp:=WorldMap;

    if tmp<>nil then
    begin
      CBStates.Checked:=tmp.Layers.States;
      CBCities.Checked:=tmp.Layers.Cities.Visible;
    end;
  end;
  {$ENDIF}

var tmp : TBIChartMode;
begin
  tmp:=Chart.Options.Mode;

  if tmp=TBIChartMode.Automatic then
     RBAuto.Checked:=True
  else
  {$IFDEF TEEPRO}
  if tmp=TBIChartMode.ThreeD then
     RB3D.Checked:=True
  else
  if tmp=TBIChartMode.Financial then
     RBFinancial.Checked:=True
  else
  if tmp=TBIChartMode.Geographic then
     RBGeo.Checked:=True
  else
  {$ENDIF}
     RBXY.Checked:=True;

  RG3D.ItemIndex:=Ord(Chart.Options.XYZMode);
  RGDirection.ItemIndex:=Ord(Chart.Options.Direction);

  CBHoriz2D.ItemIndex:=Ord(Chart.Options.SeriesDirection);

  if Chart.Options.Series2D=nil then
     LB2D.ItemIndex:=0
  else
     LB2D.ItemIndex:=FindClass(LB2D.Items,Chart.Options.Series2D);

  if Chart.Options.Series3D=nil then
     LB3D.ItemIndex:=0
  else
     LB3D.ItemIndex:=FindClass(LB3D.Items,Chart.Options.Series3D);

  {$IFDEF PROLAYERS}
  SetGeoLayers;
  {$ENDIF}
end;

procedure TBIChartEditor.FormShow(Sender: TObject);
begin
  ResetEditor;
  RefreshData(Chart.Options.Items);

  TUICommon.LoadPosition(Self,RegistryChartEditor);

  IChanging:=False;
end;

function Calc2DSeries(const AIndex:Integer; const Horizontal:Boolean):TChartSeriesClass;
begin
  case AIndex of
    1: if Horizontal then
          result:=THorizAreaSeries
       else
          result:=TAreaSeries;

    2: if Horizontal then
          result:=THorizBarSeries
       else
          result:=TBarSeries;

    3,4 :
       if Horizontal then
          result:=THorizLineSeries
       else
          result:=TLineSeries;

    5: result:=TPieSeries;

    6: result:=TPointSeries;

    {$IFDEF TEEPRO}
    7: result:=TPolarSeries;
    8: result:=TRadarSeries;
    {$ENDIF}
  else
    result:=nil;
  end;
end;

procedure TBIChartEditor.LB2DClick(Sender: TObject);
var tmp : TChartSeriesClass;
begin
  tmp:=Calc2DSeries(LB2D.ItemIndex,CBHoriz2D.ItemIndex=1);

  CBHoriz2D.Enabled:=LB2D.ItemIndex in [0,1,2,3,4]; // also Automatic

  Chart.Options.ChangeSeries2D(tmp,CBHoriz2D.ItemIndex=1,LB2D.ItemIndex=4);

  Recreate;
end;

function Needs3D(const AClass:TChartSeriesClass):Boolean;
begin
  {$IFDEF TEEPRO}
  result:=(AClass.InheritsFrom(TSurfaceSeries)) or
          (AClass.InheritsFrom(TTriSurfaceSeries)) or
          (AClass.InheritsFrom(TTowerSeries));
  {$ELSE}
  result:=False;
  {$ENDIF}
end;

procedure TBIChartEditor.LB3DClick(Sender: TObject);
{$IFDEF TEEPRO}
var tmp : TChartSeriesClass;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
  tmp:=TChartSeriesClass(LB3D.Items.Objects[LB3D.ItemIndex]);

  TBITChartAccess(Chart.Chart).Series3D:=tmp;

  if (tmp<>nil) and Needs3D(tmp) then
  begin
    Chart.Chart.View3DOptions.Orthogonal:=True;
    Chart.Chart.Chart3DPercent:=50;
    Chart.Chart.View3D:=True;
  end
  else
    Chart.Chart.View3D:=False;

  Recreate;
  {$ENDIF}
end;

procedure TBIChartEditor.LBFinancialClick(Sender: TObject);
{$IFDEF TEEPRO}
//var tmp : TChartSeriesClass;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
//  tmp:=TChartSeriesClass(LBFinancial.Items.Objects[LBFinancial.ItemIndex]);
//  Chart.Options.SeriesFinancial:=tmp;

  Recreate;
  {$ENDIF}
end;

procedure TBIChartEditor.LBXYZClick(Sender: TObject);
{$IFDEF TEEPRO}
var tmp : TChartSeriesClass;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
  tmp:=TChartSeriesClass(LBXYZ.Items.Objects[LBXYZ.ItemIndex]);

  TBITChartAccess(Chart.Chart).Series3D:=tmp;
  Recreate;
  {$ENDIF}
end;

procedure TBIChartEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation=TOperation.opRemove then
     if FChart=AComponent then
        Chart:=nil;
end;

procedure TBIChartEditor.SetPostSettings;
begin
  {$IFDEF TEEPRO}
  CBOpenGL.Checked:=Chart.Chart.Canvas is TGLCanvas;
  {$ENDIF}

  RGView.ItemIndex:=Ord(Chart.Options.Dimensions);
  RGLegend.ItemIndex:=Ord(Chart.Options.Legend);
  RGMarks.ItemIndex:=Ord(Chart.Options.Marks);
  CBAutoTitle.Checked:=Chart.Options.Title=TBIChartTitle.Automatic;
end;

procedure TBIChartEditor.Recreate;
begin
  if not IChanging then
  begin
    TBIChartAccess(Chart).DirectRefresh;
    SetPostSettings;
  end;
end;

procedure TBIChartEditor.RecreateY(Sender:TObject);
begin
  CB3D.Checked:=False;

  Chart.Options.Items.Y:=TFormListItems(Sender).Items;

//  if ARefresh then
     Recreate;
end;

procedure TBIChartEditor.RefreshData;
begin
  RefreshData(Chart.Options.Items);
end;

procedure TBIChartEditor.CreateChartEditor;
begin
  {$IFDEF TEEPRO}
  ChartEditor:=TChartEditorPanel.Create(Self);
  ChartEditor.Editor.Tree.Visible:=True;

  SetChartEditorChart;

  ChartEditor.Align:=TAlign.alClient;
  ChartEditor.Parent:=TabChart;
  {$ELSE}

  ChartEditor:=TChartEditForm.Create(Self);
  ChartEditor.PanBottom.Visible:=False;
  SetChartEditorChart;
  TUICommon.AddForm(ChartEditor,TabChart);

  {$ENDIF}
end;

procedure TBIChartEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabData then
  begin
    if IGrid=nil then
       IGrid:=TBIGridForm.Embedd(Self,TabData,Chart.Data)
    else
    if IGrid.Grid.Data=nil then
       IGrid.Grid.Data:=Chart.Data;
  end
  else
  if PageControl1.ActivePage=TabChart then
  begin
    if ChartEditor=nil then
       CreateChartEditor;
  end;
end;

procedure TBIChartEditor.DoExchange(const A,B:TComboBox);
var tmpA,
    tmpB: Integer;
    tmpNewA,
    tmpNewB: Integer;
begin
  tmpA:=A.ItemIndex;
  tmpB:=B.ItemIndex;

  if tmpA<>-1 then
  begin
    B.Items.AddObject(A.Items[tmpA],A.Items.Objects[tmpA]);
    tmpNewB:=B.Items.Count-1;
    A.Items.Delete(tmpA);
  end
  else
    tmpNewB:=-1;

  if tmpB<>-1 then
  begin
    A.Items.AddObject(B.Items[tmpB],B.Items.Objects[tmpB]);
    tmpNewA:=A.Items.Count-1;
    B.Items.Delete(tmpB);

    if tmpNewB<>-1 then
       Dec(tmpNewB);
  end
  else
    tmpNewA:=-1;

  A.ItemIndex:=tmpNewA;
  B.ItemIndex:=tmpNewB;
end;

procedure TBIChartEditor.BEditTitleClick(Sender: TObject);
begin
  EditChartTitle(Self,Chart.Chart.Title);
end;

procedure TBIChartEditor.BSwapXYClick(Sender: TObject);
begin
  DoExchange(CBX,CBY);
  TBIChartAccess(Chart).ExchangeXY;
  CB3D.Checked:=False;
end;

procedure TBIChartEditor.SetChartEditorChart;
begin
  if ChartEditor<>nil then
     if FChart=nil then
        ChartEditor.Chart:=nil
     else
        ChartEditor.Chart:=FChart.Chart;
end;

procedure TBIChartEditor.SetChart(const Value: TBIChart);
begin
  if FChart<>Value then
  begin
    if FChart<>nil then
       FChart.RemoveFreeNotification(Self);

    FChart:=Value;

    if FChart<>nil then
       FChart.FreeNotification(Self);

    SetChartEditorChart;
  end;
end;

procedure TBIChartEditor.SetModeOptions;

  function Is2D:Boolean;
  begin
    result:=TBIChartAccess(Chart).RealMode=TBIChartMode.XY;
  end;

  function Is3D:Boolean;
  begin
    result:=TBIChartAccess(Chart).RealMode=TBIChartMode.ThreeD;
  end;

  function IsFinancial:Boolean;
  begin
    result:=TBIChartAccess(Chart).RealMode=TBIChartMode.Financial;
  end;

  function IsGeo:Boolean;
  begin
    result:=TBIChartAccess(Chart).RealMode=TBIChartMode.Geographic;
  end;

  function RealModeXYZ:Boolean;
  begin
    result:=TBIChartAccess(Chart).RealMode3D=TBIChart3DMode.XYZ;
  end;

  function IsHorizontalSeries:Boolean;
  var tmp : TChartSeriesClass;
  begin
    tmp:=TBITChartAccess(Chart.Chart).FSeries2D;

    result:=tmp<>nil;

    if result then
       result:=tmp.InheritsFrom(THorizLineSeries) or
               tmp.InheritsFrom(THorizAreaSeries) or
               tmp.InheritsFrom(THorizBarSeries);
  end;

  procedure Set3DOptions;
  begin
    RG3D.ItemIndex:=Ord(Chart.Options.XYZMode);

    LBXYZ.Visible:=RealModeXYZ;
    LB3D.Visible:=not LBXYZ.Visible;

    PanelYTable.Visible:=TBIChartAccess(Chart).RealMode3D=TBIChart3DMode.Table;
    Panel3D.Visible:=not PanelYTable.Visible;
  end;

begin
  RB3D.Enabled:=Chart.Options.Items.CanThreeD;
  RBFinancial.Enabled:=Chart.Options.Items.CanFinancial;
  RBGeo.Enabled:=Chart.Options.Items.CanGeoGraphic;

  if Is3D then
  begin
    TUICommon.ShowUnique(Tab3D);
    Set3DOptions;
  end
  else
  if Is2D then
     TUICommon.ShowUnique(Tab2D)
  else
  if IsFinancial then
  begin
    TUICommon.ShowUnique(TabFinancial);
  end
  else
  if IsGeo then
     TUICommon.ShowUnique(TabGeo)
  else
     PageMode.Visible:=False;

  RGDirection.ItemIndex:=Ord(Chart.Options.Direction);
  CBStacked.ItemIndex:=Ord(Chart.Options.Stacked);
end;

procedure TBIChartEditor.BSwapYZClick(Sender: TObject);
begin
  DoExchange(CBY,CBZ);
  TBIChartAccess(Chart).ExchangeYZ;
  CB3D.Checked:=False;
end;

procedure TBIChartEditor.BSwapXZClick(Sender: TObject);
begin
  DoExchange(CBX,CBZ);
  TBIChartAccess(Chart).ExchangeXZ;
  CB3D.Checked:=False;
end;

procedure TBIChartEditor.RGViewClick(Sender: TObject);
begin
  Chart.Options.Dimensions:=TBIChartDimensions(RGView.ItemIndex);
end;

procedure TBIChartEditor.RB3DClick(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  if not IChanging then
  begin
    Chart.Options.Mode:=TBIChartMode.ThreeD;
    SetModeOptions;
  end;
  {$ENDIF}
end;

procedure TBIChartEditor.RBAutoClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Chart.Options.Mode:=TBIChartMode.Automatic;
    SetModeOptions;
  end;
end;

procedure TBIChartEditor.RBFinancialClick(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  if not IChanging then
  begin
    Chart.Options.Mode:=TBIChartMode.Financial;
    SetModeOptions;
  end;
  {$ENDIF}
end;

procedure TBIChartEditor.RBGeoClick(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  if not IChanging then
  begin
    Chart.Options.Mode:=TBIChartMode.Geographic;
    SetModeOptions;
  end;
  {$ENDIF}
end;

procedure TBIChartEditor.RBXYClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Chart.Options.Mode:=TBIChartMode.XY;
    SetModeOptions;
  end;
end;

procedure TBIChartEditor.RefreshData(const AItems:TBIChartItems);

  procedure AddCombo3D(const ACombo:TComboBox);

    procedure TryAdd(const AData:TDataItem);
    begin
      if AData<>nil then
         if ACombo.Items.IndexOfObject(AData)=-1 then
            ACombo.Items.AddObject(AData.Name,AData);
    end;

  var tmp : TDataItem;
  begin
    ACombo.Items.BeginUpdate;
    try
      ACombo.Clear;

      TryAdd(AItems.X);

      for tmp in AItems.Y do
          TryAdd(tmp);

      TryAdd(AItems.Z);

    finally
      ACombo.Items.EndUpdate;
    end;
  end;

  procedure TrySelect(const X,Y,Z:TDataItem);
  begin
    CBX.ItemIndex:=CBX.Items.IndexOfObject(X);
    CBY.ItemIndex:=CBX.Items.IndexOfObject(Y);
    CBZ.ItemIndex:=CBX.Items.IndexOfObject(Z);
  end;

  procedure TryAddFinancial(const AIndex:Integer; const ACombo:TComboBox);
  begin
    TFormListItems.AddCombo(ACombo,AItems.Y);

    if AItems.Y.Count>AIndex then
       ACombo.ItemIndex:=ACombo.Items.IndexOfObject(AItems.Y[AIndex])
    else
       ACombo.ItemIndex:=-1;
  end;

var tmpX : TDataItem;
begin
  SetModeOptions;

  tmpX:=AItems.X;

  TFormListItems.AddCombo(CBX2D,[tmpX]);
  TFormListItems.DoAddItems(CBX2D.Items,AItems.Y);

  IListY.AddItems(AItems.Y,False);
  IListY.CheckAll(True);

  IListYTable.AddItems(AItems.Y,False);
  IListYTable.CheckAll(True);

  if tmpX<>nil then
  begin
    IListY.LBItems.Items.AddObject(tmpX.Name,tmpX);
    IListYTable.LBItems.Items.AddObject(tmpX.Name,tmpX);
  end;

  TFormListItems.AddCombo(CBText,[AItems.Text]);
  TFormListItems.AddCombo(CBColors,[AItems.Group]);

  // 3D
  AddCombo3D(CBX);
  AddCombo3D(CBY);
  AddCombo3D(CBZ);

  if AItems.Y<>nil then
  begin
    if tmpX=nil then
    begin
      if AItems.Y.Count>1 then
         TrySelect(AItems.Y[0],
                   AItems.Y[1],
                   AItems.Z)
    end
    else
      TrySelect(tmpX,
                AItems.Y[0],
                AItems.Z);
  end;

  // Financial
  TryAddFinancial(0,CBOpen);
  TryAddFinancial(1,CBHigh);
  TryAddFinancial(2,CBLow);
  TryAddFinancial(3,CBClose);
  TryAddFinancial(4,CBVolume);

  SetPostSettings;
end;

procedure TBIChartEditor.RG3DClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Chart.Options.XYZMode:=TBIChart3DMode(RG3D.ItemIndex);
    SetModeOptions;
  end;
end;

procedure TBIChartEditor.RGDirectionClick(Sender: TObject);
begin
  if not IChanging then
     Chart.Options.Direction:=TBIChartDirection(RGDirection.ItemIndex);
end;

procedure TBIChartEditor.RGLegendClick(Sender: TObject);
begin
  Chart.Options.Legend:=TBIChartLegend(RGLegend.ItemIndex);
end;

procedure TBIChartEditor.RGMarksClick(Sender: TObject);
begin
  Chart.Options.Marks:=TBIChartMarks(RGMarks.ItemIndex);
end;

{$IFDEF TEEPRO}
type
  TTempChartEditor=class
  private
    procedure Edit(Sender:TObject);
  end;

procedure TTempChartEditor.Edit(Sender:TObject);
var tmp : TTeeCommander;
begin
  tmp:=TTeeCommander(TSpeedButton(Sender).Owner);

  TBIChartEditor.Edit(tmp,tmp.Parent.Controls[1] as TBIChart);
end;

var
  BIChartEditor : TTempChartEditor=nil;
{$ENDIF}

procedure ShowQueryEditor(const AParent:TWinControl;
                          const AProvider:TComponent;
                          const AData:TDataItem);
var BIChart : TBIChart;
    {$IFDEF TEEPRO}
    Commander : TTeeCommander;
    {$ENDIF}
begin
  if AParent.ControlCount=0 then
  begin
    {$IFDEF TEEPRO}
    Commander:=TTeeCommander.Create(AParent.Owner);
    Commander.Align:=TUICommon.AlignTop;
    Commander.Parent:=AParent;

    if BIChartEditor=nil then
       BIChartEditor:=TTempChartEditor.Create;

    Commander.ButtonEdit.OnClick:=BIChartEditor.Edit;
    {$ENDIF}

    BIChart:=TBIChart.Create(AParent.Owner);
    BIChart.Align:=TUICommon.AlignClient;
    BIChart.Parent:=AParent;

    {
    BIChart.Chart.Gradient.StartColor:=clWhite;
    BIChart.Chart.Gradient.EndColor:=$202020;
    BIChart.Chart.Gradient.Visible:=True;
    }
  end
  else
    BIChart:=AParent.Controls[{$IFDEF TEEPRO}1{$ELSE}0{$ENDIF}] as TBIChart;

  if AData=nil then
     BIChart.Provider:=AProvider
  else
     BIChart.Data:=AData;

  {$IFDEF TEEPRO}
  TTeeCommander(AParent.Controls[0]).Panel:=BIChart.Chart;
  {$ENDIF}
end;

procedure TBIChartEditor.CBCitiesClick(Sender: TObject);
{$IFDEF PROLAYERS}
var tmp : TWorldSeries;
{$ENDIF}
begin
  {$IFDEF PROLAYERS}
  tmp:=WorldMap;

  if tmp<>nil then
     tmp.Layers.Cities.Visible:=CBCities.Checked;
  {$ENDIF}
end;

procedure TBIChartEditor.CBMapChange(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  WorldMap.Map:=TWorldMap(CBMap.Items.Objects[CBMap.ItemIndex]);
  {$ENDIF}
end;

initialization
  TBIQueryEditor.OnShowEditor:=ShowQueryEditor;
finalization
  TBIQueryEditor.OnShowEditor:=nil;
  {$IFDEF TEEPRO}
  BIChartEditor.Free;
  {$ENDIF}
end.
