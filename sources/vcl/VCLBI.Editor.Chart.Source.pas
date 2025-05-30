{*********************************************}
{  TeeBI Software Library                     }
{  TTeeBISource VCL Editor Dialog             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Chart.Source;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.TeeSourceEdit, Vcl.StdCtrls,
  VCLTee.TeCanvas, Vcl.ExtCtrls, VCLBI.Chart.Source;

type
  TValueControl=record
  public
    Text : TLabel;
    Combo : TComboBox;
  end;

  TBISourceEditor = class(TBaseSourceEditor)
    GroupFields: TScrollBox;
    LLabels: TLabel;
    CBLabelsField: TComboFlat;
    PanelData: TPanel;
    BSelectData: TButton;
    LData: TLabel;
    procedure BSelectDataClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BApplyClick(Sender: TObject);
    procedure CBLabelsFieldChange(Sender: TObject);
  private
    { Private declarations }

    FValueControls : Array of TValueControl;

    procedure ChangedCombo(Sender: TObject);
    procedure CreateControls;
    procedure DestroyCombos;
    procedure FillCombos;
    procedure RefreshControls;
    procedure RefreshDataLabel;
    function Source:TTeeBISource;
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const ASource:TTeeBISource); static;
  end;

implementation

{$R *.dfm}

uses
  BI.DataItem, VCLBI.DataSelect, VCLTee.TeEngine;

procedure TBISourceEditor.CBLabelsFieldChange(Sender: TObject);
begin
  inherited;
  BApply.Enabled:=True;
end;

procedure TBISourceEditor.ChangedCombo(Sender: TObject);
begin
  BApply.Enabled:=True;
end;

procedure TBISourceEditor.CreateControls;

  function CreateLabel(const ATop:Integer; const ACaption:String):TLabel;
  begin
    result:=TLabel.Create(Self);
    result.Left:=LLabels.Left;
    result.Top:=ATop+4;
    result.Caption:=ACaption+':';
    result.Parent:=GroupFields;
  end;

  function CreateCombo(const ATop:Integer):TComboBox;
  begin
    result:=TComboFlat.Create(Self);
    result.Left:=CBLabelsField.Left;
    result.Top:=ATop;
    result.Width:=CBLabelsField.Width;
    result.Style:=TComboBoxStyle.csDropDownList;
    result.Parent:=GroupFields;
  end;

  procedure AddControl(const AList:TChartValueList; const ATop:Integer);

    function NewControl:TValueControl;
    begin
      result.Text:=CreateLabel(ATop,AList.Name);
      result.Combo:=CreateCombo(ATop);

      result.Text.FocusControl:=result.Combo;

      result.Combo.Tag:=ObjectToTag(AList);

      result.Combo.OnChange:=ChangedCombo;
    end;

  var L : Integer;
  begin
    L:=Length(FValueControls);
    SetLength(FValueControls,L+1);

    FValueControls[L]:=NewControl;
  end;

var t,
    tmpTop : Integer;
    tmp : TChartValueList;
begin
  DestroyCombos;

  tmpTop:=30;

  for t:=0 to TheSeries.ValuesList.Count-1 do
  begin
    tmp:=TheSeries.ValuesList[t];

    if tmp.Name<>'' then
    begin
      AddControl(tmp,tmpTop);
      Inc(tmpTop,30);
    end;
  end;
end;

procedure TBISourceEditor.DestroyCombos;
var tmp : TValueControl;
begin
  for tmp in FValueControls do
  begin
    tmp.Text.Free;
    tmp.Combo.Free;
  end;

  FValueControls:=nil;

  CBLabelsField.Clear;
end;

procedure TBISourceEditor.FillCombos;

  procedure FillCombo(const ACombo:TComboBox; const Numeric:Boolean);

    procedure AddItems(const AItems:TDataArray);
    var tmp : TDataItem;
    begin
       for tmp in AItems do
           if (tmp.Kind=TDataKind.dkUnknown) and tmp.AsTable then
               AddItems(tmp.Items.AsArray)
           else
           if (not Numeric) or (tmp.Kind.IsNumeric or (tmp.Kind=TDataKind.dkDateTime)) then
              ACombo.Items.Add(tmp.Name);
    end;

  var tmp : TChartValueList;
  begin
    ACombo.Items.BeginUpdate;
    try
      ACombo.Clear;

      ACombo.Items.Add('');

      if Source.Data<>nil then
         AddItems(Source.Data.Items.AsArray);

      tmp:=TChartValueList(ACombo.Tag);

      if tmp<>nil then
         if tmp.ValueSource<>'' then
            ACombo.ItemIndex:=ACombo.Items.IndexOf(tmp.ValueSource);

    finally
      ACombo.Items.EndUpdate;
    end;
  end;

var tmp : TValueControl;
begin
  FillCombo(CBLabelsField,False);

  for tmp in FValueControls do
      FillCombo(tmp.Combo,True);

  if TheSeries.XLabelsSource<>'' then
     CBLabelsField.ItemIndex:=CBLabelsField.Items.IndexOf(TheSeries.XLabelsSource);
end;

procedure TBISourceEditor.RefreshControls;
begin
  RefreshDataLabel;
  CreateControls;
  FillCombos;
end;

procedure TBISourceEditor.BApplyClick(Sender: TObject);
var tmp : TValueControl;
    tmpList : TChartValueList;
begin
  inherited;

  Source.Close;

  TheSeries.BeginUpdate;
  try
    for tmp in FValueControls do
    begin
      tmpList:=TChartValueList(tmp.Combo.Tag);
      tmpList.ValueSource:=tmp.Combo.Text;
    end;

    TheSeries.XLabelsSource:=CBLabelsField.Text;
  finally
    TheSeries.EndUpdate;
  end;

  CheckReplaceSource(IDataSource);

  Source.Open;

  BApply.Enabled:=False;
end;

procedure TBISourceEditor.BSelectDataClick(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=Source.Data;

  if TDataSelector.Choose(Self,Self,tmp) then
  begin
    Source.Data:=tmp;
    RefreshControls;

    BApply.Enabled:=True;
  end;
end;

class procedure TBISourceEditor.Edit(const AOwner: TComponent;
  const ASource: TTeeBISource);
begin
  with TBISourceEditor.Create(AOwner) do
  try
    Align:=TAlign.alNone;
    Tag:=ObjectToTag(ASource.Series);

    ShowModal;
  finally
    Free;
  end;
end;

procedure TBISourceEditor.FormShow(Sender: TObject);
begin
  inherited;

  CBSources.Hide;
  LLabel.Hide;

  BSelectData.Parent:=Pan;
  LData.Parent:=Pan;

  PanelData.Hide;

  CheckDataSource(TTeeBISource);

  CBLabelsField.Style:=TComboBoxStyle.csDropDownList;

  RefreshControls;

  BApply.Enabled:=Assigned(TheSeries) and (TheSeries.DataSource<>IDataSource);
end;

function TBISourceEditor.Source:TTeeBISource;
begin
  result:=IDataSource as TTeeBISource;
end;

procedure TBISourceEditor.RefreshDataLabel;
var tmp : TTeeBISource;
begin
  tmp:=Source;

  if (tmp=nil) or (tmp.Data=nil) then
     LData.Caption:=''
  else
     LData.Caption:=tmp.Data.Name;
end;

end.
