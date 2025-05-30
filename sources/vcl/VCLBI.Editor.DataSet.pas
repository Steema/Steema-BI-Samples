{*********************************************}
{  TeeBI Software Library                     }
{  TBIDataSet editor dialog                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.DataSet;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.DataSet, Vcl.StdCtrls, Vcl.ComCtrls,
  Data.DB, Vcl.Grids, Vcl.DBGrids, VCLBI.Grid,
  Vcl.ExtCtrls, VCLBI.GridForm,
  VCLBI.DataSelect, BI.DataItem, BI.Query, BI.Persist, VCLBI.DataControl,
  VCLBI.Tree;

type
  TBIDataSetEditor = class(TForm)
    Panel2: TPanel;
    CBPreview: TCheckBox;
    Panel4: TPanel;
    BOK: TButton;
    BCancel: TButton;
    PageControl1: TPageControl;
    TabOptions: TTabSheet;
    TabData: TTabSheet;
    Backup: TBIDataset;
    Panel3: TPanel;
    CBActive: TCheckBox;
    CBReadonly: TCheckBox;
    Label1: TLabel;
    BITree1: TBITree;
    Button1: TButton;
    procedure CBPreviewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageControl1Change(Sender: TObject);
    procedure CBActiveClick(Sender: TObject);
    procedure CBReadonlyClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    DataSet : TBIDataSet;

    ISelect : TDataSelector;
    IPreview : TBIGridForm;

    IChanging,
    IModified : Boolean;

    procedure ChangeData(const AData:TDataItem);
    procedure ClosedPreview(Sender: TObject; var Action: TCloseAction);
    procedure PreviewNewData;
    procedure SelectedData(Sender: TObject);
    procedure SetModified;
    procedure SetStructure;
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const ADataSet:TBIDataSet):Boolean; static;

    procedure Refresh(const ADataSet:TBIDataSet);
  end;

implementation

{$R *.dfm}

uses
  BI.DataSource, VCLBI.Editor.Query, BI.UI;

{ TBIDataSetEditor }

procedure TBIDataSetEditor.BOKClick(Sender: TObject);
begin
  if IModified then
  begin
    DataSet.Close;

    DataSet.Data:=Backup.Data;
    DataSet.ReadOnly:=CBReadonly.Checked;
    DataSet.Active:=CBActive.Checked;

    IModified:=False;
  end;
end;

procedure TBIDataSetEditor.ClosedPreview(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(IPreview,'DataSetPreview');
  CBPreview.Checked:=False;
end;

procedure TBIDataSetEditor.Button1Click(Sender: TObject);
var tmp : TBIQuery;
begin
  tmp:=TBIQuery.Create(DataSet.Owner);

  if TBIQueryEditor.Edit(Self,tmp) then
  begin
    tmp.Name:=TCommonUI.UniqueName(tmp);
    ChangeData(tmp.Data);
  end
  else
    tmp.Free;
end;

procedure TBIDataSetEditor.CBActiveClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Backup.Active:=CBActive.Checked;
    SetModified;
  end;
end;

procedure TBIDataSetEditor.CBPreviewClick(Sender: TObject);
begin
  if CBPreview.Checked then
  begin
    if IPreview=nil then
    begin
      IPreview:=TBIGridForm.Create(Self);
      IPreview.OnClose:=ClosedPreview;
    end;

    IPreview.Data:=Backup.Data;

    TUICommon.LoadPosition(IPreview,'DataSetPreview');

    IPreview.Show;
  end
  else
  if IPreview<>nil then
     IPreview.Close;
end;

procedure TBIDataSetEditor.CBReadonlyClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Backup.ReadOnly:=CBReadonly.Checked;
    SetModified;
  end;
end;

class function TBIDataSetEditor.Edit(const AOwner: TComponent;
                                     const ADataSet: TBIDataSet):Boolean;
begin
  with TBIDataSetEditor.Create(AOwner) do
  try
    Refresh(ADataSet);
    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

procedure TBIDataSetEditor.SetStructure;
begin
  BITree1.Data:=Backup.Data;
  BITree1.Expand(nil);
end;

procedure TBIDataSetEditor.ChangeData(const AData:TDataItem);
begin
  Backup.Data:=AData;

  SetStructure;
  PreviewNewData;
  SetModified;
end;

procedure TBIDataSetEditor.SelectedData(Sender:TObject);
begin
  if ISelect.Selected<>nil then
     ChangeData(ISelect.Selected);
end;

procedure TBIDataSetEditor.SetModified;
begin
  IModified:=True;
  BOK.Enabled:=True;
end;

procedure TBIDataSetEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabData then
  begin
    if ISelect=nil then
    begin
      ISelect:=TDataSelector.Embedd(Self,TabData,DataSet);
      ISelect.OnSelect:=SelectedData;
    end;
  end;
end;

procedure TBIDataSetEditor.PreviewNewData;
begin
  if IPreview<>nil then
     IPreview.Grid.Data:=Backup.Data;
end;

procedure TBIDataSetEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if IPreview<>nil then
     ClosedPreview(IPreview,Action);

  TUICommon.SavePosition(Self,'DataSet');
end;

procedure TBIDataSetEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if IModified then
     CanClose:=TUICommon.YesNo('Dataset has been modified. Discard changes?')
  else
     CanClose:=True;
end;

procedure TBIDataSetEditor.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage:=TabOptions;
  TUICommon.LoadPosition(Self,'DataSet');
end;

procedure TBIDataSetEditor.Refresh(const ADataSet: TBIDataSet);
begin
  DataSet:=ADataSet;

  IChanging:=True;
  try
    Backup.Data:=DataSet.Data;
    Backup.ReadOnly:=DataSet.ReadOnly;
    Backup.Active:=DataSet.Active;

    CBReadonly.Checked:=Backup.ReadOnly;
    CBActive.Checked:=Backup.Active;

    SetStructure;
 finally
    IChanging:=False;
  end;
end;

end.
