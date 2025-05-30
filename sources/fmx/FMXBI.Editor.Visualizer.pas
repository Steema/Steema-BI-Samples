{*********************************************}
{  TeeBI Software Library                     }
{  Data Visualizer Editor                     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Editor.Visualizer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<26} // Cannot use FireMonkeyVersion<21 (or 21.0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion<28} // Cannot use FireMonkeyVersion<21 (or 21.0)
  {$DEFINE HASFMX22}
  {$ENDIF}

  {$IFNDEF HASFMX22}
  FMX.Controls.Presentation, FMX.EditBox, FMX.SpinBox,
  {$ENDIF}

  FMX.Dialogs, FMX.Layouts,

  FMXBI.Visualizer,
  FMX.ListBox, FMX.StdCtrls, FMX.TabControl, FMX.Edit;

type
  TVisualizerEditor = class(TForm)
    TabControl1: TTabControl;
    TabGroups: TTabItem;
    TabValues: TTabItem;
    Layout1: TLayout;
    Button1: TButton;
    Layout2: TLayout;
    SBUpGroup: TSpeedButton;
    SBDownGroup: TSpeedButton;
    Layout3: TLayout;
    Layout4: TLayout;
    SBUpValue: TSpeedButton;
    SBDownValue: TSpeedButton;
    LBGroups: TListBox;
    LBValues: TListBox;
    PageSettings: TTabControl;
    Layout5: TLayout;
    TabCombo: TTabItem;
    LClass: TLabel;
    CBClass: TComboBox;
    LCurrent: TLabel;
    LCurrentHeader: TLabel;
    Label1: TLabel;
    CBGroupBy: TComboBox;
    TabMulti: TTabItem;
    TabList: TTabItem;
    Label2: TLabel;
    CBComboAlign: TComboBox;
    Label3: TLabel;
    CBMultiScroll: TComboBox;
    Label4: TLabel;
    UDColumns: TSpinBox;
    Label5: TLabel;
    CBListAlign: TComboBox;
    procedure SBUpGroupClick(Sender: TObject);
    procedure SBDownGroupClick(Sender: TObject);
    procedure SBUpValueClick(Sender: TObject);
    procedure SBDownValueClick(Sender: TObject);
    procedure LBValuesChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LBGroupsChange(Sender: TObject);
    procedure LBGroupsChangeCheck(Sender: TObject);
    procedure LBValuesChangeCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBComboAlignChange(Sender: TObject);
    procedure CBMultiScrollChange(Sender: TObject);
    procedure UDColumnsChangeTracking(Sender: TObject);
    procedure CBListAlignChange(Sender: TObject);
    procedure CBClassChange(Sender: TObject);
  private
    { Private declarations }

    Viz : TBIComposer;
    VizUI : TBIVisualizerUI;

    IChanging : Boolean;

    procedure MoveGroup(const ADelta:Integer);
    procedure MoveValue(const ADelta:Integer);
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl):TVisualizerEditor;
    procedure Refresh(const AVisualizer:TBIComposer);
    procedure ShowSettings(const ATab:TTabItem);
  end;

implementation

{$R *.fmx}

uses
  BI.DataItem, FMXBI.Grid;

procedure TVisualizerEditor.SBDownGroupClick(Sender: TObject);
begin
  MoveGroup(1);
end;

procedure TVisualizerEditor.SBDownValueClick(Sender: TObject);
begin
  MoveValue(1);
end;

procedure TVisualizerEditor.SBUpGroupClick(Sender: TObject);
begin
  MoveGroup(-1);
end;

procedure TVisualizerEditor.SBUpValueClick(Sender: TObject);
begin
  MoveValue(-1);
end;

procedure TVisualizerEditor.UDColumnsChangeTracking(Sender: TObject);
var tmp : TGroupMultiControl;
begin
  if not IChanging then
  begin
    tmp:=VizUI.GetMulti(LBGroups.ItemIndex);

    if tmp<>nil then
       tmp.Columns:=Round(UDColumns.Value);
  end;
end;

procedure TVisualizerEditor.Button1Click(Sender: TObject);
begin
  Viz.BestOrder;
  Refresh(Viz);
end;

procedure TVisualizerEditor.CBClassChange(Sender: TObject);
begin
  if VizUI.ChangeClass(LBGroups.ItemIndex,CBClass.ItemIndex) then
     LBGroupsChange(Self);
end;

procedure TVisualizerEditor.CBComboAlignChange(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBGroups.ItemIndex;

  if tmp<>-1 then
//     TGroupCombo.Align:=TGroupComboAlign(CBComboAlign.ItemIndex);
end;

procedure TVisualizerEditor.CBListAlignChange(Sender: TObject);
var tmp : TGroupList;
begin
  tmp:=VizUI.GetList(LBGroups.ItemIndex);

  if tmp<>nil then
     tmp.Align:=TGroupAlign(CBListAlign.ItemIndex);
end;

procedure TVisualizerEditor.CBMultiScrollChange(Sender: TObject);
var tmp : TGroupMultiControl;
begin
  tmp:=VizUI.GetMulti(LBGroups.ItemIndex);

  if tmp<>nil then
     tmp.Scroll:=TGroupMultiScroll(CBMultiScroll.ItemIndex);
end;

class function TVisualizerEditor.Embedd(const AOwner: TComponent;
  const AParent: TWinControl): TVisualizerEditor;
begin
  result:=TVisualizerEditor.Create(AOwner);
  TUICommon.AddForm(result,AParent);
end;

procedure TVisualizerEditor.FormCreate(Sender: TObject);
begin
  TBIVisualizerUI.AddClasses(CBClass.Items);
end;

procedure TVisualizerEditor.ShowSettings(const ATab:TTabItem);
var t : Integer;
begin
  PageSettings.Visible:=True;

  for t:=0 to PageSettings.TabCount-1 do
      PageSettings.Tabs[t].Visible:=PageSettings.Tabs[t]=ATab;

  PageSettings.ActiveTab:=ATab;
end;

procedure TVisualizerEditor.LBGroupsChange(Sender: TObject);

  {
  procedure SetCBRender(const ACombo:TComboBox);
  begin
    if TGroupChart.GlobalOptions.Render=nil then
       ACombo.ItemIndex:=0
    else
    if TGroupChart.GlobalOptions.Render=TTeeCanvas3D then
       ACombo.ItemIndex:=1
    else
       ACombo.ItemIndex:=2;
  end;
  }

var tmp : Integer;
    tmpCurrent : TGroup;
begin
  tmp:=LBGroups.ItemIndex;

  CBClass.Enabled:=(tmp<>-1);// and (tmp<LBGroups.Count-1);
  LClass.Enabled:=CBClass.Enabled;

  SBUpGroup.Enabled:=(tmp>0);
  SBDownGroup.Enabled:=(tmp<>-1) and (tmp<LBGroups.Count-1);

  LCurrent.Text:='';
  LCurrentHeader.Visible:=False;
  PageSettings.Visible:=False;

  if tmp=-1 then
  begin
    CBClass.ItemIndex:=-1;
    CBGroupBy.Clear;
  end
  else
  begin
    TBIVisualizerUI.AddGroupByItems(CBGroupBy.Items,Viz.Groups[tmp].Data,tmp);

    CBClass.ItemIndex:=TGroup.ClassIndexOf(Viz.Groups[tmp].GroupClass);

    tmpCurrent:=Viz.Groups[tmp].Current;

    if tmpCurrent<>nil then
    begin
      if CBClass.ItemIndex=0 then
      begin
        LCurrent.Text:=Copy(tmpCurrent.ClassName,7,255);
        LCurrentHeader.Visible:=True;
      end;

      IChanging:=True;

      if tmpCurrent is TGroupCombo then
      begin
        CBComboAlign.ItemIndex:=Ord(TGroupCombo(tmpCurrent).Align);
        ShowSettings(TabCombo);
      end
      else
      if tmpCurrent is TGroupMultiControl then
      begin
        CBMultiScroll.ItemIndex:=Ord((tmpCurrent as TGroupMultiControl).Scroll);
        UDColumns.Value:=(tmpCurrent as TGroupMultiControl).Columns;
        ShowSettings(TabMulti);
      end
      else
      if tmpCurrent is TGroupList then
      begin
        CBListAlign.ItemIndex:=Ord((tmpCurrent as TGroupList).Align);
        ShowSettings(TabList);
      end
      else
      begin
        // Get Editor class from tmpCurrent and call ShowSettings
        //ShowSettings(TabChart);
      end;

      IChanging:=False;
    end;
  end;
end;

procedure TVisualizerEditor.LBGroupsChangeCheck(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBGroups.ItemIndex;

  if tmp<>-1 then
  begin
    Viz.Groups[tmp].Enabled:=LBGroups.Selected.IsChecked;
    Viz.ReCalculate;
  end;
end;

procedure TVisualizerEditor.LBValuesChange(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBValues.ItemIndex;

  SBUpValue.Enabled:=(tmp>0);
  SBDownValue.Enabled:=(tmp<>-1) and (tmp<LBValues.Count-1);
end;

procedure TVisualizerEditor.LBValuesChangeCheck(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBValues.ItemIndex;

  if tmp<>-1 then
  begin
    Viz.Values[tmp].Enabled:=LBValues.Selected.IsChecked;
    Viz.ReCalculate;
  end;
end;

procedure TVisualizerEditor.MoveGroup(const ADelta: Integer);
var tmp : Integer;
begin
  tmp:=LBGroups.ItemIndex;
  Viz.Groups.Exchange(tmp,tmp+ADelta);

  Viz.ReCalculate;

  LBGroups.ItemsExchange(LBGroups.ItemByIndex(tmp),LBGroups.ItemByIndex(tmp+ADelta));

  LBGroups.ItemIndex:=tmp+ADelta;
  LBGroupsChange(Self);
end;

procedure TVisualizerEditor.MoveValue(const ADelta:Integer);
var tmp : Integer;
begin
  tmp:=LBValues.ItemIndex;
  Viz.Values.Exchange(tmp,tmp+ADelta);

  Viz.ReCalculate;

  LBValues.ItemsExchange(LBValues.ItemByIndex(tmp),LBValues.ItemByIndex(tmp+ADelta));

  LBValues.ItemIndex:=tmp+ADelta;
  LBValuesChange(Self);
end;

procedure TVisualizerEditor.Refresh(const AVisualizer: TBIComposer);
begin
  Viz:=AVisualizer;
  VizUI.Viz:=Viz;

  if Viz<>nil then
  begin
    TBIVisualizerUI.AddItems(LBGroups,Viz.Groups);
    TBIVisualizerUI.AddItems(LBValues,Viz.Values);

    if LBGroups.Count>0 then
    begin
      LBGroups.ItemIndex:=0;
      LBGroupsChange(Self);
    end;
  end;
end;

end.
