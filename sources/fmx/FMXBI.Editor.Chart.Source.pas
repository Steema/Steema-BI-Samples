{*********************************************}
{  TeeBI Software Library                     }
{  TTeeBISource FMX Editor Dialog             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Editor.Chart.Source;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  FMX.StdCtrls, FMX.ListBox, FMX.Edit, FMX.Layouts,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMXTee.Constants, FMXTee.Engine, FMXTee.Editor.Source, FMXBI.Chart.Source;

type
  TValueControl=record
  public
    Text : TLabel;
    Combo : TComboBox;
  end;

  TBISourceEditor = class(TBaseSourceEditor)
    LayoutData: TLayout;
    GroupFields: TLayout;
    LLabels: TLabel;
    CBLabelsField: TComboBox;
    BSelectData: TButton;
    LData: TLabel;
    procedure BApplyClick(Sender: TObject);
    procedure CBLabelsFieldChange(Sender: TObject);
    procedure BSelectDataClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
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

{$R *.fmx}

uses
  FMXTee.Canvas, BI.DataItem, FMXBI.DataManager;

// XE6 dcc32 BUG, workaround not available
{$IF CompilerVersion>27}
{$I BI.Chart.Options.inc} // <-- see .inc contents for reason/explanation
{$ENDIF}

{ TBITeeSourceEditor }

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

      if tmp.Combo.Selected=nil then
         tmpList.ValueSource:=''
      else
         tmpList.ValueSource:=tmp.Combo.Selected.Text;
    end;

    if CBLabelsField.Selected=nil then
       TheSeries.XLabelsSource:=''
    else
       TheSeries.XLabelsSource:=CBLabelsField.Selected.Text;
  finally
    TheSeries.EndUpdate;
  end;

  CheckReplaceSource(Source);

  Source.Open;

  BApply.Enabled:=False;
end;

procedure TBISourceEditor.BSelectDataClick(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TDataManager.ChooseData(Self,'',Source.Data);

  if tmp<>nil then
  begin
    Source.Data:=tmp;
    RefreshControls;

    BApply.Enabled:=True;
  end;
end;

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
    result.Position.X:=LLabels.Position.X;
    result.Position.Y:=ATop+4;
    result.Text:=ACaption+':';
    result.Parent:=GroupFields;
  end;

  function CreateCombo(const ATop:Integer):TComboBox;
  begin
    result:=TComboBox.Create(Self);
    result.Position.X:=CBLabelsField.Position.X;
    result.Position.Y:=ATop;
    result.Width:=CBLabelsField.Width;
    result.Parent:=GroupFields;
  end;

  procedure AddControl(const AList:TChartValueList; const ATop:Integer);

    function NewControl:TValueControl;
    begin
      result.Text:=CreateLabel(ATop,AList.Name);
      result.Combo:=CreateCombo(ATop);

      //result.Text.FocusControl:=result.Combo;

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

class procedure TBISourceEditor.Edit(const AOwner: TComponent;
  const ASource: TTeeBISource);
begin
  with TBISourceEditor.Create(AOwner) do
  try
    //Align:=TAlign.alNone;
    Tag:=ObjectToTag(ASource.Series);

    ShowModal;
  finally
    Free;
  end;
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

procedure TBISourceEditor.FormActivate(Sender: TObject);
begin
  inherited;
  FormShow(Sender);
end;

procedure TBISourceEditor.FormShow(Sender: TObject);
begin
  inherited;

  CBSources.Visible:=False;
  LLabel.Visible:=False;

  BSelectData.Parent:=Pan;
  LData.Parent:=Pan;

  LayoutData.Visible:=False;

  CheckDataSource(TTeeBISource);

  RefreshControls;

  BApply.Enabled:=Assigned(TheSeries) and (TheSeries.DataSource<>Source);
end;

procedure TBISourceEditor.RefreshControls;
begin
  RefreshDataLabel;
  CreateControls;
  FillCombos;
end;

procedure TBISourceEditor.RefreshDataLabel;
var tmp : TTeeBISource;
begin
  tmp:=Source;

  if (tmp=nil) or (tmp.Data=nil) then
     LData.Text:=''
  else
     LData.Text:=tmp.Data.Name;
end;

function TBISourceEditor.Source: TTeeBISource;
var tmp : TComponent;

    {$IFNDEF HASIDATASOURCE}
    // Access private variable
    tmpRtti : TRttiContext;
    tmpType : TRttiType;
    tmpField : TRttiField;
    {$ENDIF}
begin
  {$IFDEF HASIDATASOURCE}
  tmp:=IDataSource;
  {$ELSE}
  tmpRtti:=TRttiContext.Create;
  tmpType:=tmpRtti.GetType(ClassType);
  tmpField:=tmpType.GetField('IDataSource');
  tmp:=tmpField.GetValue(Self).AsObject as TComponent;
  {$ENDIF}

  result:=tmp as TTeeBISource;
end;

end.
