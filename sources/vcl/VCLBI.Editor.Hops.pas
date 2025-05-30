unit VCLBI.Editor.Hops;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BI.Expressions, BI.Query, BI.DataItem;

type
  THopsViewer = class(TForm)
    LBHops: TListBox;
    LMain: TLabel;
    LCount: TLabel;
    LBItems: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    LParent: TLabel;
    procedure FormShow(Sender: TObject);
    procedure LBHopsClick(Sender: TObject);
  private
    { Private declarations }

    FHops : TDataHops;

    procedure AddHopItems(const AHop:THops);
    procedure AddHops;
  public
    { Public declarations }

    class function HopsFrom(const AProvider:TDataProvider):TDataHops; overload; static;
    class function HopsFrom(const AQuery:TBIQuery):TDataHops; overload; static;

    class procedure View(const AOwner:TComponent; const AHops:TDataHops); overload; static;
    class procedure View(const AOwner:TComponent; const AQuery:TBIQuery); overload; static;
  end;

implementation

{$R *.dfm}

uses
  BI.Summary, BI.DataSource;

{ THopsViewer }

procedure THopsViewer.AddHops;
var t : Integer;
    tmp : THops;
begin
  LBHops.Items.BeginUpdate;
  try
    LBHops.Clear;

    for t:=0 to High(FHops.Hops) do
    begin
      tmp:=FHops.Hops[t];

      if tmp.RealSource=nil then
         LBHops.Items.AddObject('(nil)',tmp)
      else
         LBHops.Items.AddObject(tmp.RealSource.Name,tmp);
    end;

  finally
    LBHops.Items.EndUpdate;
  end;
end;

procedure THopsViewer.FormShow(Sender: TObject);
begin
  if FHops.Main=nil then
     LMain.Caption:='(nil)'
  else
     LMain.Caption:=FHops.Main.Name;

  AddHops;
end;

function HopsFromSelect(const ASelect:TDataSelect):TDataHops;
var tmp : TDataArray;
begin
  tmp:=ASelect.DataItems;
  result:=TDataHops.Create;
  TDataSelect.SetupHops(result,tmp);
end;

type
  TBIQueryAccess=class(TBIQuery);
  TSummaryAccess=class(TSummary);
  TSummaryItemAccess=class(TSummaryItem);

function HopsFromSummary(const ASummary:TSummary):TDataHops;
var t : Integer;
begin
  ASummary.Prepare;
  result:=TSummaryAccess(ASummary).Hops;

  for t:=0 to ASummary.By.Count-1 do
      if ASummary.By[t].Active then
         result.Add(TSummaryItemAccess(ASummary.By[t]).Source,False);

  // Avoid destroy at Summary free
  TSummaryAccess(ASummary).Hops:=nil;
end;

class function THopsViewer.HopsFrom(const AProvider:TDataProvider):TDataHops;
begin
  if AProvider is TSummary then
     result:=HopsFromSummary(TSummary(AProvider))
  else
  if AProvider is TDataSelect then
     result:=HopsFromSelect(TDataSelect(AProvider))
  else
     result:=nil;
end;

class function THopsViewer.HopsFrom(const AQuery: TBIQuery): TDataHops;
var tmp : TDataProvider;
begin
  tmp:=TBIQueryAccess(AQuery).CreateProvider;
  try
    result:=HopsFrom(tmp);
  finally
    tmp.Free;
  end;
end;

procedure THopsViewer.AddHopItems(const AHop:THops);
var t : Integer;
begin
  LBItems.Clear;

  for t:=0 to {$IFDEF FPC}High(AHop.Items){$ELSE}AHop.Items.Count-1{$ENDIF} do
      LBItems.Items.AddObject(AHop.Items[t].ToString,AHop.Items[t]);
end;

procedure THopsViewer.LBHopsClick(Sender: TObject);
var tmp : THops;
    tmpIndex : Integer;
begin
  tmpIndex:=LBHops.ItemIndex;

  if tmpIndex<>-1 then
  begin
    tmp:=FHops.Hops[tmpIndex];

    LCount.Caption:=IntToStr(Length(tmp.Items));

    AddHopItems(tmp);

    if tmp.Parent=nil then
       LParent.Caption:=''
    else
       LParent.Caption:=LBHops.Items[LBHops.Items.IndexOfObject(tmp.Parent)];
  end;
end;

class procedure THopsViewer.View(const AOwner: TComponent; const AHops: TDataHops);
begin
  with THopsViewer.Create(AOwner) do
  try
    FHops:=AHops;
    ShowModal;
  finally
    Free;
  end;
end;

class procedure THopsViewer.View(const AOwner: TComponent; const AQuery:TBIQuery);
var tmp : TDataHops;
begin
  tmp:=THopsViewer.HopsFrom(AQuery);

  if tmp<>nil then
  try
    THopsViewer.View(AOwner,tmp);
  finally
    tmp.Free;
  end;
end;

end.
