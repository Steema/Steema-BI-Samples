{*********************************************}
{  TeeBI Software Library                     }
{  Data Visualizer Editor                     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Visualizer;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}

  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.Visualizer, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.CheckLst, Vcl.ComCtrls, VCLBI.Grid,
  BI.Summary, VCLBI.DataControl;

type
  TVisualizerEditor = class(TForm)
    LBGroups: TCheckListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    BUp: TSpeedButton;
    BDown: TSpeedButton;
    PageItems: TPageControl;
    PageSettings: TPageControl;
    TabCombo: TTabSheet;
    TabMulti: TTabSheet;
    LMultiScroll: TLabel;
    CBMultiScroll: TComboBox;
    Label3: TLabel;
    EColumns: TEdit;
    UDColumns: TUpDown;
    TabList: TTabSheet;
    Button1: TButton;
    TabValues: TTabSheet;
    LBValues: TCheckListBox;
    Panel5: TPanel;
    Panel6: TPanel;
    SBUpValue: TSpeedButton;
    SBDownValue: TSpeedButton;
    Panel7: TPanel;
    Button2: TButton;
    TabPage: TTabSheet;
    CBExpandLast: TCheckBox;
    Group: TTabSheet;
    Panel4: TPanel;
    LCurrentHeader: TLabel;
    LCurrent: TLabel;
    Label9: TLabel;
    CBGroupBy: TComboBox;
    RGClass: TRadioGroup;
    CBCheckBoxes: TCheckBox;
    TabTrack: TTabSheet;
    Label14: TLabel;
    ERowHeight: TEdit;
    UDRowHeight: TUpDown;
    CBSplitters: TCheckBox;
    TabSelected: TTabSheet;
    CBSelected: TCheckListBox;
    TabControl: TTabSheet;
    Label12: TLabel;
    CBControlAlign: TComboBox;
    CBControlLabel: TCheckBox;
    TabGrid: TTabSheet;
    Panel8: TPanel;
    SpeedButton2: TSpeedButton;
    CBDuplicates: TCheckBox;
    CBColorize: TCheckBox;
    BIGrid1: TBIGrid;
    procedure LBGroupsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBMultiScrollChange(Sender: TObject);
    procedure BUpClick(Sender: TObject);
    procedure BDownClick(Sender: TObject);
    procedure LBGroupsClickCheck(Sender: TObject);
    procedure EColumnsChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LBValuesClickCheck(Sender: TObject);
    procedure SBUpValueClick(Sender: TObject);
    procedure SBDownValueClick(Sender: TObject);
    procedure LBValuesClick(Sender: TObject);
    procedure CBGroupByChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBControlAlignChange(Sender: TObject);
    procedure CBExpandLastClick(Sender: TObject);
    procedure RGClassClick(Sender: TObject);
    procedure CBCheckBoxesClick(Sender: TObject);
    procedure ERowHeightChange(Sender: TObject);
    procedure CBSplittersClick(Sender: TObject);
    procedure PageSettingsChange(Sender: TObject);
    procedure CBSelectedClickCheck(Sender: TObject);
    procedure CBControlLabelClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CBColorizeClick(Sender: TObject);
    procedure CBDuplicatesClick(Sender: TObject);
  private
    { Private declarations }
    Viz : TBIComposer;

    VizUI : TBIVisualizerUI;

    IChanging : Boolean;

    procedure ChangeAlign(const AIndex:Integer);

    procedure FillSelected(const AItem:TVisualizerItem);
    procedure MoveGroup(const ADelta:Integer);
    procedure MoveValue(const ADelta:Integer);
    procedure ShowGroupSettings(const AGroup:TGroup);
  protected
    procedure ShowSettings(const ATab:TTabSheet);
  public
    { Public declarations }

    procedure CheckDuplicates(const ASummary:TSummary);

    class procedure Edit(const AOwner:TComponent; const AVisualizer:TBIComposer); static;
    class function Embedd(const AOwner:TComponent; const AParent:TWinControl):TVisualizerEditor; static;

    procedure Refresh(const AVisualizer:TBIComposer);
  end;

implementation

{$R *.dfm}

uses
  BI.DataItem, BI.Arrays, VCLBI.DataSelect, BI.UI, VCLBI.Editor.BIGrid,
  VCLBI.Grid.DBGrid;

{ TVisualizerEditor }

class procedure TVisualizerEditor.Edit(const AOwner:TComponent; const AVisualizer:TBIComposer);
begin
  with TVisualizerEditor.Create(AOwner) do
  try
    Refresh(AVisualizer);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TVisualizerEditor.MoveGroup(const ADelta:Integer);
var tmp : Integer;
begin
  tmp:=LBGroups.ItemIndex;
  Viz.Groups.Exchange(tmp,tmp+ADelta);

  Viz.ReCalculate;

  LBGroups.Items.Exchange(tmp,tmp+ADelta);

  LBGroups.ItemIndex:=tmp+ADelta;
  LBGroupsClick(Self);
end;

procedure TVisualizerEditor.BDownClick(Sender: TObject);
begin
  MoveGroup(1);
end;

procedure TVisualizerEditor.BUpClick(Sender: TObject);
begin
  MoveGroup(-1);
end;

procedure TVisualizerEditor.Button1Click(Sender: TObject);
begin
  Viz.BestOrder;
  Refresh(Viz);
end;

procedure TVisualizerEditor.Button2Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=Viz.Data;
  TDataSelector.Choose(Self,Viz);

  if Viz.Data<>tmp then
     Refresh(Viz);
end;

procedure TVisualizerEditor.CBCheckBoxesClick(Sender: TObject);
var tmp : TGroupList;
begin
  tmp:=VizUI.GetList(LBGroups.ItemIndex);

  if tmp<>nil then
     tmp.CheckBoxes:=CBCheckBoxes.Checked;
end;

procedure TVisualizerEditor.CBExpandLastClick(Sender: TObject);
var tmp : TGroupMultiControl;
begin
  if not IChanging then
  begin
    tmp:=VizUI.GetMulti(LBGroups.ItemIndex);

    if tmp<>nil then
       tmp.ExpandLast:=CBExpandLast.Checked;
  end;
end;

procedure TVisualizerEditor.CBGroupByChange(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBGroups.ItemIndex;

  if tmp<>-1 then
  begin
    Viz.Groups[tmp].GroupBy:=TDataItem(CBGroupBy.Items.Objects[CBGroupBy.ItemIndex]);
    Viz.ReCalculate;
  end;
end;

procedure TVisualizerEditor.ChangeAlign(const AIndex:Integer);
var tmp : Integer;
begin
  tmp:=LBGroups.ItemIndex;

  if tmp<>-1 then
  begin
    if Viz.Groups[tmp].Current is TGroupControlAlign then
       (Viz.Groups[tmp].Current as TGroupControlAlign).Align:=TGroupAlign(AIndex);
  end;
end;

procedure TVisualizerEditor.CBControlLabelClick(Sender: TObject);
var tmp : TGroupControl;
begin
  tmp:=VizUI.GetControl(LBGroups.ItemIndex);

  if tmp<>nil then
     tmp.ShowLabel:=(Sender as TCheckBox).Checked;
end;

procedure TVisualizerEditor.CBDuplicatesClick(Sender: TObject);
begin
//  CheckDuplicates;
end;

procedure TVisualizerEditor.CBMultiScrollChange(Sender: TObject);
var tmp : TGroupMultiControl;
begin
  tmp:=VizUI.GetMulti(LBGroups.ItemIndex);

  if tmp<>nil then
     tmp.Scroll:=TGroupMultiScroll(CBMultiScroll.ItemIndex);
end;

procedure TVisualizerEditor.CBColorizeClick(Sender: TObject);
var tmp : TDataColorizers;
    t : Integer;
    tmpValues : TVisualizerItems;
begin
  tmpValues:=Viz.Values;

  if CBColorize.Checked and (tmpValues.Count>0) then
  begin
    SetLength(tmp,tmpValues.Count);

    for t:=0 to tmpValues.Count-1 do
        tmp[t].Data:=tmpValues[t].Data;

    BIGrid1.Colorize(tmp);
  end
  else
    BIGrid1.Colorize(nil);
end;

procedure TVisualizerEditor.CBControlAlignChange(Sender: TObject);
begin
  ChangeAlign(CBControlAlign.ItemIndex);
end;

procedure TVisualizerEditor.CBSelectedClickCheck(Sender: TObject);
var tmp : TVisualizerItem;
    tmpIndex : Integer;
begin
  tmp:=Viz.Groups[LBGroups.ItemIndex];

  tmpIndex:=CBSelected.ItemIndex;
  tmp.Selected[tmpIndex]:=CBSelected.Checked[tmpIndex];
end;

procedure TVisualizerEditor.CBSplittersClick(Sender: TObject);
var tmp : TGroupMultiControl;
begin
  if not IChanging then
  begin
    tmp:=VizUI.GetMulti(LBGroups.ItemIndex);

    if tmp<>nil then
       tmp.Splitters:=CBSplitters.Checked;
  end;
end;

procedure TVisualizerEditor.EColumnsChange(Sender: TObject);
var tmp : TGroupMultiControl;
begin
  if not IChanging then
  begin
    tmp:=VizUI.GetMulti(LBGroups.ItemIndex);

    if tmp<>nil then
       tmp.Columns:=UDColumns.Position;

    CBExpandLast.Enabled:=UDColumns.Position>1;
  end;
end;

class function TVisualizerEditor.Embedd(const AOwner: TComponent;
  const AParent: TWinControl): TVisualizerEditor;
begin
  result:=TVisualizerEditor.Create(AOwner);
  result.Align:=TAlign.alClient;
  TUICommon.AddForm(result,AParent);
end;

procedure TVisualizerEditor.ERowHeightChange(Sender: TObject);
var tmp : TGroupMultiControl;
begin
  if not IChanging then
  begin
    tmp:=VizUI.GetMulti(LBGroups.ItemIndex);

    if tmp<>nil then
       tmp.RowHeight:=UDRowHeight.Position;
  end;
end;

procedure TVisualizerEditor.FormCreate(Sender: TObject);
begin
  TBIVisualizerUI.AddClasses(RGClass.Items);

  PageSettings.ActivePage:=Group;
end;

procedure TVisualizerEditor.ShowSettings(const ATab:TTabSheet);
var t : Integer;
begin
  if not PageSettings.Visible then
     PageSettings.Visible:=True;

  for t:=2 to PageSettings.PageCount-1 do
      PageSettings.Pages[t].TabVisible:=PageSettings.Pages[t]=ATab;
end;

procedure TVisualizerEditor.ShowGroupSettings(const AGroup:TGroup);
var tmpMulti : TGroupMultiControl;
begin
  if AGroup is TGroupTrackbar then
  begin
    ShowSettings(TabTrack);
    TabTrack.TabVisible:=False; // not yet
  end
  else
  if AGroup is TGroupMultiControl then
  begin
    tmpMulti:=(AGroup as TGroupMultiControl);

    CBMultiScroll.ItemIndex:=Ord(tmpMulti.Scroll);
    UDColumns.Position:=tmpMulti.Columns;
    CBExpandLast.Checked:=tmpMulti.ExpandLast;
    CBSplitters.Checked:=tmpMulti.Splitters;
    UDRowHeight.Position:=tmpMulti.RowHeight;

    ShowSettings(TabMulti);
  end
  else
  if AGroup is TGroupList then
     ShowSettings(TabList)
  else
  if AGroup is TGroupPage then
  begin
    case (AGroup as TGroupPage).PageControl.TabPosition of
      TTabPosition.tpLeft: CBControlAlign.ItemIndex:=0;
      TTabPosition.tpTop: CBControlAlign.ItemIndex:=1;
      TTabPosition.tpRight: CBControlAlign.ItemIndex:=2;
      TTabPosition.tpBottom: CBControlAlign.ItemIndex:=3;
    end;

    ShowSettings(TabPage);
    TabPage.TabVisible:=False; // not yet
  end
  else
  begin
    // Search for group class Editor and invoke its ShowSettings
    ShowSettings(Group);
  end;

  if AGroup is TGroupControlAlign then
  begin
    CBControlAlign.ItemIndex:=Ord(TGroupControlAlign(AGroup).Align);
    TabControl.TabVisible:=True;
  end;

  if AGroup is TGroupControl then
  begin
    CBControlLabel.Checked:=(AGroup as TGroupControl).ShowLabel;
    TabControl.TabVisible:=True;
  end;
end;

procedure TVisualizerEditor.SpeedButton2Click(Sender: TObject);
begin
  TBIGridEditor.Edit(Self,BIGrid1);
end;

procedure TVisualizerEditor.FillSelected(const AItem:TVisualizerItem);
var //tmp : TDataMap;
    t : TLoopInteger;
begin
  CBSelected.Items.BeginUpdate;
  try
    CBSelected.Clear;

    for t:=0 to AItem.Count-1 do
    begin
      CBSelected.Items.Add(AItem.AsString(t));
      CBSelected.Checked[t]:=AItem.Selected[t];
    end;
  finally
    CBSelected.Items.EndUpdate;
  end;
end;

procedure TVisualizerEditor.LBGroupsClick(Sender: TObject);
var tmp : Integer;
    tmpCurrent : TGroup;
begin
  IChanging:=True;

  tmp:=LBGroups.ItemIndex;

  RGClass.Enabled:=(tmp<>-1);// and (tmp<LBGroups.Count-1);

  BUp.Enabled:=(tmp>0);
  BDown.Enabled:=(tmp<>-1) and (tmp<LBGroups.Count-1);

  LCurrent.Caption:='';
  LCurrentHeader.Visible:=False;
  PageSettings.Hide;

  if tmp=-1 then
  begin
    RGClass.ItemIndex:=-1;
    CBGroupBy.Clear;

    if Viz.Main<>nil then
    begin
      ShowGroupSettings(Viz.Main);
      Group.TabVisible:=False;
      TabSelected.TabVisible:=False;
    end;
  end
  else
  begin
    TBIVisualizerUI.AddGroupByItems(CBGroupBy.Items,Viz.Groups[tmp].Data,tmp);

    CBGroupBy.Enabled:=CBGroupBy.Items.Count>1;

    RGClass.ItemIndex:=TGroup.ClassIndexOf(Viz.Groups[tmp].GroupClass);

    Group.TabVisible:=True;
    TabSelected.TabVisible:=True;

    tmpCurrent:=Viz.Groups[tmp].Current;

    if tmpCurrent<>nil then
    begin
      if RGClass.ItemIndex=0 then
      begin
        LCurrent.Caption:=Copy(tmpCurrent.ClassName,7,255);
        LCurrentHeader.Visible:=True;
      end;

      ShowGroupSettings(tmpCurrent);

      if PageSettings.ActivePage=TabSelected then
         FillSelected(Viz.Groups[tmp]);
    end;
  end;

  IChanging:=False;
end;

procedure TVisualizerEditor.LBGroupsClickCheck(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBGroups.ItemIndex;

  if tmp<>-1 then
     Viz.Groups[tmp].Enabled:=LBGroups.Checked[tmp];
end;

procedure TVisualizerEditor.LBValuesClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBValues.ItemIndex;

  SBUpValue.Enabled:=(tmp>0);
  SBDownValue.Enabled:=(tmp<>-1) and (tmp<LBValues.Count-1);
end;

procedure TVisualizerEditor.LBValuesClickCheck(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBValues.ItemIndex;

  if tmp<>-1 then
  begin
    Viz.Values[tmp].Enabled:=LBValues.Checked[tmp];
    Viz.ReCalculate;
  end;
end;

procedure TVisualizerEditor.Refresh(const AVisualizer: TBIComposer);
begin
  Viz:=AVisualizer;
  VizUI.Viz:=Viz;

  if Viz=nil then
     BIGrid1.Data:=nil
  else
  begin
    TBIVisualizerUI.AddItems(LBGroups,Viz.Groups);
    TBIVisualizerUI.AddItems(LBValues,Viz.Values);

    if LBGroups.Count>0 then
       LBGroups.ItemIndex:=0;

    LBGroupsClick(Self);

    BIGrid1.Data:=Viz.Data;
  end;

  CBColorize.Checked:=False;
end;

procedure TVisualizerEditor.RGClassClick(Sender: TObject);
begin
  if not IChanging then
     if VizUI.ChangeClass(LBGroups.ItemIndex,RGClass.ItemIndex) then
        LBGroupsClick(Self);
end;

procedure TVisualizerEditor.MoveValue(const ADelta:Integer);
var tmp : Integer;
begin
  tmp:=LBValues.ItemIndex;
  Viz.Values.Exchange(tmp,tmp+ADelta);

  Viz.ReCalculate;

  LBValues.Items.Exchange(tmp,tmp+ADelta);

  LBValues.ItemIndex:=tmp+ADelta;
  LBValuesClick(Self);
end;

procedure TVisualizerEditor.PageSettingsChange(Sender: TObject);
begin
  if PageSettings.ActivePage=TabSelected then
     FillSelected(Viz.Groups[LBGroups.ItemIndex]);
end;

procedure TVisualizerEditor.SBUpValueClick(Sender: TObject);
begin
  MoveValue(-1);
end;

procedure TVisualizerEditor.SBDownValueClick(Sender: TObject);
begin
  MoveValue(1);
end;

procedure TVisualizerEditor.CheckDuplicates(const ASummary:TSummary);
var t : Integer;
begin
  // Hide duplicates
  for t:=0 to High(ASummary.By) do
      if ASummary.By[t].DestData<>nil then
         BIGrid1.Duplicates(ASummary.By[t].DestData, CBDuplicates.Checked);
end;

end.
