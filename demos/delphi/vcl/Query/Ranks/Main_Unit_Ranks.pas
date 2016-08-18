unit Main_Unit_Ranks;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.VCL.DataControl, BI.VCL.Grid, BI.Data, BI.VCL.GridForm;

type
  TFormDataRank = class(TForm)
    BIGrid1: TBIGrid;
    Panel1: TPanel;
    CBHideGroups: TCheckBox;
    Label1: TLabel;
    CBGroup: TComboBox;
    BAsGrid: TButton;
    CBAscending: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CBHideGroupsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBGroupChange(Sender: TObject);
    procedure BAsGridClick(Sender: TObject);
    procedure CBAscendingClick(Sender: TObject);
  private
    { Private declarations }

    procedure CreateRank(const GroupBy:TDataItem);
    procedure HideDuplicates;
    procedure PresentGrid(const AData:TDataItem);
    procedure RefreshGrid;
    function SelectedGroup:TDataItem;
    procedure SortByGroup;
    procedure TryColorizeRanks(const AGrid:TBIGrid);
  public
    { Public declarations }
  end;

var
  FormDataRank: TFormDataRank;

implementation

{$R *.dfm}

uses
  BI.Data.Rank, BI.Data.Expressions, BI.UI, BI.Data.Gridify;

// Try to find an item named "Rank" to destroy it
procedure RemoveItem(const AData:TDataItem);
var tmp : TDataItem;
begin
  tmp:=AData.Items.Find('Rank');

  if tmp<>nil then
     tmp.Free;
end;

procedure TFormDataRank.CreateRank(const GroupBy:TDataItem);
var tmp : TDataItem;
    tmpRank : TDataItem;
    tmpGroups : TDataArray;
begin
  tmp:=BIGrid1.Data;

  // First remove "Rank" item, if it exists
  RemoveItem(tmp);

  // Then create a new "Rank" item

  if GroupBy=nil then
     tmpGroups:=nil
  else
     tmpGroups.Add(GroupBy);

  tmpRank:=TDataRank.From(tmp,
                          tmpGroups,
                          tmp['Happiness'],
                          CBAscending.Checked);

  tmpRank.Name:='Rank';

  // And add the new "Rank" item to table
  tmp.Items.Add(tmpRank);

  RefreshGrid;
end;

// Returns a simple table with random data
function RandomTable:TDataItem;
const
  PersonNames:Array[0..4] of String=('Johnny','Mike','Angela','Julia','Claire');

  Max_Year=5;

var Year,
    Person,
    Happiness : TDataItem;

    tmpRow,
    t,
    p : Integer;
begin
  // Create table
  result:=TDataItem.Create(True);

  // Add 3 fields
  Year:=result.Items.Add('Year',TDataKind.dkInt32);
  Person:=result.Items.Add('Person',TDataKind.dkText);
  Happiness:=result.Items.Add('Happiness',TDataKind.dkSingle);

  // Add values
  result.Resize((Max_Year+1)*5);

  for t:=0 to Max_Year do
      for p:=0 to 4 do
      begin
        tmpRow:=(t*5)+p;

        Year.Int32Data[tmpRow]:=2000+t;
        Person.TextData[tmpRow]:=PersonNames[p];
        Happiness.SingleData[tmpRow]:=Random(1000)*0.1;
      end;
end;

// Sort data by (optional) group and Happiness
procedure TFormDataRank.SortByGroup;
var Sort : TSortItems;
    tmp : TDataItem;
begin
  Sort.Clear;

  tmp:=SelectedGroup;

  if tmp<>nil then
     Sort.Add(tmp);

  Sort.Add(BIGrid1.Data['Happiness'],False);

  Sort.SortData(BIGrid1.Data);
end;

function TFormDataRank.SelectedGroup:TDataItem;
begin
  case CBGroup.ItemIndex of
    0: result:=nil;
    1: result:=BIGrid1.Data['Year'];
  else
    result:=BIGrid1.Data['Person'];
  end;
end;

procedure TFormDataRank.PresentGrid(const AData:TDataItem);
var tmp : TBIGridForm;
begin
  tmp:=TBIGridForm.Create(Self);
  try
    tmp.Caption:=AData.Name;
    tmp.Grid.Data:=AData;
    TryColorizeRanks(tmp.Grid);

    tmp.ShowModal;
  finally
    tmp.Free;
  end;
end;

procedure TFormDataRank.BAsGridClick(Sender: TObject);
var tmp : TDataItem;
    tmpGroup,
    tmpOther : String;
begin
  if CBGroup.ItemIndex=0 then
  begin
    tmpGroup:='Year';
    tmpOther:='Person';
  end
  else
  begin
    if CBGroup.ItemIndex=1 then
       tmpOther:='Person'
    else
       tmpOther:='Year';

    tmpGroup:=CBGroup.Items[CBGroup.ItemIndex];
  end;

  tmp:=TGridify.From(BIGrid1.Data,'Rank',tmpGroup,tmpOther);
  try
    PresentGrid(tmp);
  finally
    tmp.Free;
  end;
end;

procedure TFormDataRank.CBAscendingClick(Sender: TObject);
begin
  CBGroupChange(Self);
end;

procedure TFormDataRank.CBGroupChange(Sender: TObject);
begin
  CreateRank(SelectedGroup);

  CBHideGroups.Enabled:=SelectedGroup<>nil;
end;

procedure TFormDataRank.CBHideGroupsClick(Sender: TObject);
begin
  RefreshGrid;
end;

procedure TFormDataRank.FormCreate(Sender: TObject);
begin
  BIGrid1.Data:=RandomTable;

  CBGroupChange(Self);
end;

procedure TFormDataRank.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free; // remove used memory
end;

// Paint grid cells in colors
procedure TFormDataRank.TryColorizeRanks(const AGrid:TBIGrid);
var tmp : TDataColorizers;
    tmpRank : TDataItem;
begin
  tmp.Clear;

  tmpRank:=AGrid.Data.Items.Find('Rank');

  if tmpRank<>nil then
     tmp.Add(tmpRank);

  AGrid.Colorize(tmp);
end;

procedure TFormDataRank.HideDuplicates;
var tmp : TDataItem;
begin
  tmp:=SelectedGroup;

  if tmp<>nil then
     BIGrid1.Duplicates(tmp,True);
end;

procedure TFormDataRank.RefreshGrid;
begin
  SortByGroup;

  BIGrid1.RefreshData;

  if CBHideGroups.Checked then
     HideDuplicates;

  TryColorizeRanks(BIGrid1);
end;

end.
