{*********************************************}
{  TeeBI Software Library                     }
{  Search and Filter Data Example             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Unit_DataSearch_Demo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Data.DB,
  Vcl.Grids, Vcl.DBGrids, Vcl.Buttons, Vcl.DBCtrls,

  // TeeBI units used in this example

  BI.Data, BI.Persist, BI.Data.Search,
  BI.VCL.Grid, BI.VCL.DataManager,
  BI.Dataset, BI.DataSource, BI.Arrays;

type
  TFormSearchDemo = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ESearch: TEdit;
    CBCaseSensitive: TCheckBox;
    CBPartial: TComboBox;
    BIGrid1: TBIGrid;
    CBAll: TCheckBox;
    CBItems: TComboBox;
    Label2: TLabel;
    LFound: TLabel;
    CBBackground: TCheckBox;
    RGMode: TRadioGroup;
    BIDataset1: TBIDataset;
    DataSource1: TDataSource;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    Button1: TButton;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    CurrentRecord: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ESearchChange(Sender: TObject);
    procedure CBCaseSensitiveClick(Sender: TObject);
    procedure CBPartialChange(Sender: TObject);
    procedure CBAllClick(Sender: TObject);
    procedure CBItemsChange(Sender: TObject);
    procedure RGModeClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    { Private declarations }

    // Class to perform search and filtering
    Search : TDataSearch;

    // Keep the number of search matches
    CurrentHit,
    TotalHits : TInteger;

    // Keep old Grid formatting when highlighting cells
    OldBack : TColor;
    OldStyle : TFontStyles;

    procedure SetLabelFound;
    procedure SetSearch(const AIndex:TCursorIndex);
    procedure SetupHighlight;

    // Grid cell highlighting
    procedure AfterHighlight(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure BeforeHighlight(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);

    procedure FocusCell;

    procedure IncCurrent(const Delta:TInteger);
    function IsHit(const ACol,ARow:Integer):Boolean;
  public
    { Public declarations }
  end;

var
  FormSearchDemo: TFormSearchDemo;

implementation

{$R *.dfm}

uses
  BI.UI, BI.VCL.Grid.DBGrid, System.UITypes;

procedure TFormSearchDemo.CBCaseSensitiveClick(Sender: TObject);
begin
  // Toggle case-sensitivity and do search again

  Search.CaseSensitive:=CBCaseSensitive.Checked;
  ESearchChange(Self);
end;

procedure TFormSearchDemo.CBItemsChange(Sender: TObject);
begin
  CBAll.Checked:=CBItems.ItemIndex=-1;
  CBItems.Enabled:=not CBAll.Checked;

  // Search in all sub-items ?

  if CBItems.ItemIndex=-1 then
     Search.Items:=nil
  else
     // Select a single column to search
     Search.Items:=TDataItem(CBItems.Items.Objects[CBItems.ItemIndex]);

  // Search again
  ESearchChange(Self);
end;

procedure TFormSearchDemo.CBPartialChange(Sender: TObject);
begin
  // Change in which part of texts to search

  case CBPartial.ItemIndex of
    0: Search.TextPart:=TDataSearchPart.Anywhere;
    1: Search.TextPart:=TDataSearchPart.AtStart;
    2: Search.TextPart:=TDataSearchPart.AtEnd;
  else
    Search.TextPart:=TDataSearchPart.Exact;
  end;

  // Search again
  ESearchChange(Self);
end;

procedure TFormSearchDemo.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  // Show the current grid record number and the total number of records

  CurrentRecord.Caption:=BIDataset1.RecNo.ToString+' / '+BIDataset1.RecordCount.ToString;
end;

procedure TFormSearchDemo.CBAllClick(Sender: TObject);
begin
  // Select all fields or a single field

  if CBAll.Checked then
     CBItems.ItemIndex:=-1
  else
     CBItems.ItemIndex:=0;

  CBItems.Enabled:=not CBAll.Checked;

  CBItemsChange(Self);
end;

procedure TFormSearchDemo.SBDownClick(Sender: TObject);
begin
  IncCurrent(1);
end;

procedure TFormSearchDemo.SBUpClick(Sender: TObject);
begin
  IncCurrent(-1);
end;

procedure TFormSearchDemo.SetLabelFound;
begin
  // Show how many rows have been found at LFound label

  if Search.Hits.Enabled then
  begin
    if TotalHits=0 then
       LFound.Caption:='0'
    else
       LFound.Caption:=(CurrentHit+1).ToString+'/'+TotalHits.ToString;
  end
  else
     LFound.Caption:=BIDataset1.Cursor.Index.Count.ToString;
end;

procedure TFormSearchDemo.SetSearch(const AIndex:TCursorIndex);
begin
  // Use search results to filter rows

  if RGMode.ItemIndex=2 then
     BIDataset1.PrepareIndex(nil) // <-- no filter, show all rows
  else
     BIDataset1.PrepareIndex(AIndex); // <-- show only rows with search matches

  // Display number of search hits
  CurrentHit:=0;
  TotalHits:=Search.Hits.Count;

  SetLabelFound;

  SBUp.Enabled:=False;
  SBDown.Enabled:=Search.Hits.Enabled and (TotalHits>1);

  // Focus the first cell in the grid that has a search match
  if Search.Hits.Enabled and (TotalHits>0) then
     FocusCell;
end;

procedure TFormSearchDemo.ESearchChange(Sender: TObject);
var Text : String;
begin
  // Trim search text, remove spaces at start and end of text
  Text:=Trim(ESearch.Text);

  // Empty search means showing the full dataset again
  if Text='' then
  begin
    Search.Stop;
    SetSearch(nil);  // <-- full data
  end
  else
  if CBBackground.Checked then
     Search.BackgroundFind(Text)  // <-- Search using background thread, dont wait
  else
     Search.Find(Text);  // <-- Search and wait until search is finished
end;

procedure TFormSearchDemo.FormCreate(Sender: TObject);
begin
  // Load sample data from default "BISamples" folder
  BIDataset1.Data:=TStore.Load('BISamples','MovieDB')['Movies'];
  BIDataset1.Open;

  // Add all sub-items (fields) of sample data at "CBItems" combobox
  TCommonUI.AddItems(BIDataset1.Data,CBItems.Items);

  // Initially restrict search to use the first field of the table
  CBItems.ItemIndex:=0;
  CBItemsChange(Self);

  // Set grid to initially show the full dataset
  Search.Source:=BIDataset1.Data;

  // Format to match searches against floating point item values
  Search.FloatFormat:='0.###';

  RGModeClick(Self);

  // When a search finishes, show results at BIGrid and count at label.
  // This is necessary for "background thread" searches.
  Search.OnFinished:=procedure(const AIndex:TCursorIndex)
  begin
    TThread.Synchronize(nil,procedure
    begin
      SetSearch(AIndex);
    end);
  end;

  DataSource1.DataSet:=BIDataset1;

  SetupHighlight;
end;

// Prepare grid events for cell highlighting
procedure TFormSearchDemo.SetupHighlight;
var Grid : TBIDBGrid;
begin
  Grid:=(BIGrid1.Plugin.GetObject as TBIDBGrid);

  Grid.OnBeforeDrawCell:=BeforeHighlight;
  Grid.OnAfterDrawCell:=AfterHighlight;

  Grid.Options:=Grid.Options+[dgAlwaysShowSelection];
end;

procedure TFormSearchDemo.RGModeClick(Sender: TObject);
begin
  // Change between filtering rows and/or highlighting grid cells
  Search.Hits.Enabled:=RGMode.ItemIndex<>1;

  // Search again
  ESearchChange(Self);
end;

type
  TBIDBGridAccess=class(TBIDBGrid);

// Prepare Column Font and Color depending if the cell is a "Search Hit" or not
procedure TFormSearchDemo.BeforeHighlight(Sender: TObject; ACol, ARow: Longint;
    Rect: TRect; State: TGridDrawState);
var Grid : TBIDBGrid;
    tmpCol : TColumn;
begin
  Grid:=(Sender as TBIDBGrid);

  if ARow>=Grid.TopRow then
  begin
    // Determine the grid column for the given "ACol" parameter
    tmpCol:=TBIDBGridAccess(Grid).ColumnOf(ACol);

    if tmpCol<>nil then
    begin
      // Remember cell formatting
      OldBack:=tmpCol.Color;
      OldStyle:=tmpCol.Font.Style;

      // If cell contains a match, change font style and background color
      if IsHit(ACol,ARow) then
      begin
        tmpCol.Font.Style:=[fsBold];
        tmpCol.Color:=clSilver;
      end
      else
      begin
        // No search match, restore default font style and back color
        tmpCol.Font.Style:=[];
        tmpCol.Color:=tmpCol.DefaultColor;
      end;
    end;
  end;
end;

procedure TFormSearchDemo.Button1Click(Sender: TObject);
var tmp : TDataItem;
begin
  // Show the Data Manager dialog to choose data
  tmp:=TDataManager.Choose(Self,nil,True);

  if tmp<>nil then
  begin
    // Reset dataset and controls to use the selected data
    TCommonUI.AddItems(tmp,CBItems.Items);

    BIDataset1.Data:=tmp;
    Search.Source:=tmp;

    BIDataset1.Open;

    // Clear search, show full dataset
    ESearch.Text:='';
    ESearchChange(Self);
  end;
end;

// Restore Column Font and Color
procedure TFormSearchDemo.AfterHighlight(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
var G : TBIDBGrid;
    tmpCol : TColumn;
begin
  G:=(Sender as TBIDBGrid);

  if ARow>=G.TopRow then
  begin
    // Determine the grid column for the given "ACol" parameter
    tmpCol:=TBIDBGridAccess(G).ColumnOf(ACol);

    if tmpCol<>nil then
    begin
      // Restore grid column formatting
      tmpCol.Font.Style:=OldStyle;
      tmpCol.Color:=OldBack;
    end;
  end;
end;

// Go to previous or next hit
procedure TFormSearchDemo.IncCurrent(const Delta: TInteger);
begin
  Inc(CurrentHit,Delta);

  SBUp.Enabled:=CurrentHit>0;
  SBDown.Enabled:=CurrentHit<(TotalHits-1);

  SetLabelFound;

  FocusCell;
end;

// Returns True when the grid cell matches the search
function TFormSearchDemo.IsHit(const ACol,ARow:Integer):Boolean;
var tmp : TDataItem;
    tmpRow : TInteger;
    tmpCol : TColumn;
    Grid : TBIDBGrid;
begin
  result:=False;

  if Search.Hits.Enabled and (ACol>=0) then
  begin
    // Get the real Grid control
    Grid:=TBIDBGrid(BIGrid1.Plugin.GetObject);

    // Get the grid column from the ACol parameter
    tmpCol:=TBIDBGridAccess(Grid).ColumnOf(ACol);

    if (tmpCol<>nil) and (tmpCol.Field<>nil) then
    begin
      // Returns the TDataItem that corresponds to the grid column Field
      tmp:=BIDataset1.DataOf(tmpCol.Field);

      if tmp<>nil then
      begin
        // Obtain the row number (considering filtering)
        tmpRow:=BIDataset1.Cursor.Position(BIDataset1.RecNo-1+ARow-TBIDBGridAccess(Grid).Row);

        // Ask Search.Hits for this row and data
        result:=Search.Hits.Exists(tmpRow,tmp);
      end;
    end;
  end;
end;

// Speed Bottleneck
procedure TFormSearchDemo.FocusCell;
var tmpRow : TInteger;
    tmpData : TDataItem;
    Grid : TBIDBGrid;
    tmpCol : TColumn;
begin
  // Obtain the row and data for the Nth "hit"
  Search.Hits.Find(CurrentHit,tmpRow,tmpData);

  if tmpRow<>-1 then
  begin
    // Get the real Grid control
    Grid:=TBIDBGrid(BIGrid1.Plugin.GetObject);

    // Change the dataset row
    Grid.DataSource.DataSet.RecNo:=tmpRow+1;

    tmpCol:=Grid.ColumnOf(tmpData);

    if tmpCol<>nil then
       // Select the grid column that belongs to the selected Field
       Grid.SelectedField:=Grid.ColumnOf(tmpData).Field;
  end;
end;

end.
