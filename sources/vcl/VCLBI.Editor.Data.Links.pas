unit VCLBI.Editor.Data.Links;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  VCLBI.DataControl, VCLBI.Grid, Vcl.ExtCtrls, BI.Persist,
  BI.DataItem;

type
  TDataLinksEditor = class(TForm)
    Panel2: TPanel;
    BAdd: TButton;
    BDelete: TButton;
    BIGrid1: TBIGrid;
    PanelLink: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    EMaster: TEdit;
    EDetail: TEdit;
    procedure BIGrid1DataChange(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure EDetailChange(Sender: TObject);
    procedure EMasterChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    IData : TDataDefinition;
    ILinks : TDataRelations;

    function ChangedField(const ANum:Integer; const AValue:String):Boolean;
    function LinksToData:TDataItem;
    procedure RefreshGrid;
    procedure SaveLinks;
    function TryChangeLink(const ARoot:TDataItem; const S:String):String;
  public
    { Public declarations }

    class function Embed(const AOwner:TComponent;
                         const AParent:TWinControl):TDataLinksEditor; static;

    procedure Refresh(const AData:TDataDefinition);
  end;

implementation

{$R *.dfm}

uses
  VCLBI.DataManager;

procedure TDataLinksEditor.SaveLinks;
begin
  IData.SaveLinks;
end;

function TDataLinksEditor.LinksToData:TDataItem;
var tmpMaster,
    tmpDetail : TDataItem;
    t : Integer;
begin
  result:=TDataItem.Create(True);

  tmpMaster:=result.Items.Add('Master',TDataKind.dkText);
  tmpDetail:=result.Items.Add('Detail',TDataKind.dkText);

  result.Resize(ILinks.Count);

  for t:=0 to ILinks.Count-1 do
  begin
    tmpMaster.TextData[t]:=ILinks[t].Master;
    tmpDetail.TextData[t]:=ILinks[t].Detail;
  end;
end;

procedure TDataLinksEditor.RefreshGrid;
begin
  BIGrid1.Data.Free;
  BIGrid1.Data:=LinksToData;
end;

function TDataLinksEditor.TryChangeLink(const ARoot:TDataItem; const S:String):String;
var tmp,
    tmpNew : TDataItem;
begin
  if S='' then
     tmp:=nil
  else
     tmp:=TStore.OriginToData(ARoot,'',S);

  tmpNew:=TDataManager.Choose(Self,tmp,True);

  if tmpNew=tmp then
     result:=S
  else
     result:=TStore.OriginOf(tmpNew,'')
end;

procedure TDataLinksEditor.SpeedButton1Click(Sender: TObject);
begin
  EMaster.Text:=TryChangeLink(nil,EMaster.Text);
end;

procedure TDataLinksEditor.SpeedButton2Click(Sender: TObject);
begin
  EDetail.Text:=TryChangeLink(TStore.Load(IData.Description),EDetail.Text);
end;

procedure TDataLinksEditor.Refresh(const AData:TDataDefinition);
begin
  IData:=AData;
  ILinks:=IData.Links;
  RefreshGrid;
end;

procedure TDataLinksEditor.BAddClick(Sender: TObject);
begin
  ILinks.Add('','');
  RefreshGrid;
  BIGrid1.CurrentRow:=BIGrid1.Data.Count-1;
end;

procedure TDataLinksEditor.BDeleteClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=BIGrid1.CurrentRow;

  if tmp<>-1 then
  begin
    // Pending remove: data...Detail.Master:=nil

    ILinks.Delete(tmp);
    SaveLinks;

    RefreshGrid;

    if tmp<BIGrid1.Data.Count then
       BIGrid1.CurrentRow:=tmp
    else
       BIGrid1.CurrentRow:=BIGrid1.Data.Count-1;
  end;
end;

procedure TDataLinksEditor.BIGrid1DataChange(Sender: TObject);
var tmp : Integer;
begin
  IChanging:=True;
  try
    EMaster.Text:='';
    EDetail.Text:='';

    tmp:=BIGrid1.CurrentRow;

    BDelete.Enabled:=tmp<>-1;
    PanelLink.Enabled:=tmp<>-1;

    if tmp<>-1 then
    begin
      EMaster.Text:=ILinks[tmp].Master;
      EDetail.Text:=ILinks[tmp].Detail;
    end;
  finally
    IChanging:=False;
  end;
end;

function TDataLinksEditor.ChangedField(const ANum:Integer; const AValue:String):Boolean;
begin
  result:=False;

  if not IChanging then
  if BIGrid1.CurrentRow<>-1 then
  begin
    BIGrid1.Data.Items[ANum].TextData[BIGrid1.CurrentRow]:=AValue;
    BIGrid1.Invalidate;

    result:=True;
  end;
end;

procedure TDataLinksEditor.EDetailChange(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(EDetail.Text);

  if tmp<>'' then
     if ChangedField(1,tmp) then
     begin
       ILinks[BIGrid1.CurrentRow].Detail:=tmp;
       SaveLinks;
     end;
end;

procedure TDataLinksEditor.EMasterChange(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(EMaster.Text);

  if tmp<>'' then
     if ChangedField(0,tmp) then
     begin
       ILinks[BIGrid1.CurrentRow].Master:=tmp;
       SaveLinks;
     end;
end;

class function TDataLinksEditor.Embed(const AOwner: TComponent;
  const AParent: TWinControl): TDataLinksEditor;
begin
  result:=TDataLinksEditor.Create(AOwner);
  TUICommon.AddForm(result,AParent);
end;

procedure TDataLinksEditor.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
end;

end.
