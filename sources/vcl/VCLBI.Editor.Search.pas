{*********************************************}
{  TeeBI Software Library                     }
{  Search Editor Panel                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Search;

interface

{
  This form displays the UI necessary to work with TDataSearch.
  TDataSearch is used to find free-text inside TDataItem values.
  TBIGrid uses this form to allow searching text inside grid cells.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Buttons, Vcl.StdCtrls,
  BI.Search, BI.DataSet, BI.Arrays, BI.DataItem;

type
  TGetDataSet=function:TBIDataSet of object;

  TSearchEditor = class(TForm)
    ESearch: TEdit;
    SBMenu: TSpeedButton;
    PopupMenu1: TPopupMenu;
    Casesensitive1: TMenuItem;
    Searchat1: TMenuItem;
    Anywhere1: TMenuItem;
    Start1: TMenuItem;
    End1: TMenuItem;
    Exact1: TMenuItem;
    LHits: TLabel;
    SBClose: TSpeedButton;
    SBDown: TSpeedButton;
    SBUp: TSpeedButton;
    procedure ESearchChange(Sender: TObject);
    procedure Casesensitive1Click(Sender: TObject);
    procedure Exact1Click(Sender: TObject);
    procedure SBMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
  private
    { Private declarations }

    FOnChanged : TNotifyEvent;
    FOnGetDataSet : TGetDataSet;

    procedure SetCurrent(const Value:TInteger);

  public
    { Public declarations }

    CurrentColor,
    HighLightColor : TColor;

    Current,
    Total : TInteger;

    Search : TDataSearch;

    procedure Clear;

    class function Embed(const AOwner:TComponent; const AParent:TWinControl;
                         const AAlign:TAlign):TSearchEditor; static;

    function HasHit(const ARow:Integer;
                    const AItem:TDataItem;
                    out AIndex:TInteger):Boolean;

    function HasHits:Boolean;
    function HitIndex(const ARow:Integer;
                      const AItem:TDataItem):TInteger;

    procedure Reposition(const ALeft:Integer);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
    property OnGetDataSet:TGetDataSet read FOnGetDataSet write FOnGetDataSet;
  end;

implementation

{$R *.dfm}

uses
  VCLBI.Menus, BI.DataSource;

// Enable or disable "case sensitive" search
procedure TSearchEditor.Casesensitive1Click(Sender: TObject);
var tmp : TMenuItem;
begin
  tmp:=Sender as TMenuItem;
  tmp.Checked:=not tmp.Checked;

  Search.CaseSensitive:=tmp.Checked;

  // Perform search again
  ESearchChange(Self);
end;

// Returns a new TSearchEditor embedded into AParent
class function TSearchEditor.Embed(const AOwner: TComponent;
  const AParent: TWinControl; const AAlign: TAlign): TSearchEditor;
begin
  result:=TSearchEditor.Create(AOwner);
  result.Align:=AAlign;
  result.Height:=20;
  result.BorderStyle:=bsNone;
  result.Parent:=AParent;
end;

// Perform a new search when edit box is changed
procedure TSearchEditor.ESearchChange(Sender: TObject);
var tmp : TBIDataset;
begin
  if Assigned(FOnGetDataSet) then
  begin
    tmp:=OnGetDataSet;

    if tmp=nil then
       LHits.Visible:=False
    else
    begin
      // Do the search
      Search.Source:=tmp.Data;
      Search.Index:=tmp.Cursor.Index;
      Search.Find(ESearch.Text);

      if ESearch.Text='' then
      begin
        // Nothing to search, hide controls
        LHits.Visible:=False;
        SBDown.Visible:=False;
        SBUp.Visible:=False;
      end
      else
      begin
        // Refresh controls with search results
        Total:=Search.Hits.Count;

        LHits.Visible:=Total>0;
        SBDown.Visible:=Total>1;
        SBUp.Visible:=SBDown.Visible;

        if Total>0 then
           SetCurrent(1)
        else
           LHits.Caption:='';
      end;

      // Call OnChanged
      if Assigned(FOnChanged) then
         FOnChanged(Self);
    end;
  end;
end;

// Enable up and down buttons, show total search hits at label
procedure TSearchEditor.SetCurrent(const Value:TInteger);
begin
  Current:=Value;

  // Enable or disable up and down buttons
  SBDown.Enabled:=Current<Total;
  SBUp.Enabled:=Current>1;

  // Refresh hits label
  LHits.Caption:=IntToStr(Current)+'/'+IntToStr(Total);
end;

// Check / Uncheck the clicked radioitem menu option
procedure TSearchEditor.Exact1Click(Sender: TObject);
var tmp : TMenuItem;
begin
  tmp:=Sender as TMenuItem;

  if not tmp.Checked then
  begin
    tmp.Checked:=True;

    case tmp.Parent.IndexOf(tmp) of
      0: Search.TextPart:=TDataSearchPart.Anywhere;
      1: Search.TextPart:=TDataSearchPart.AtStart;
      2: Search.TextPart:=TDataSearchPart.AtEnd;
    else
      Search.TextPart:=TDataSearchPart.Exact;
    end;

    // Perform search again
    ESearchChange(Self);
  end;
end;

procedure TSearchEditor.FormCreate(Sender: TObject);
begin
  Search.Hits.Enabled:=True;

  HighLightColor:=$90A0D2;
  CurrentColor:=clRed;
end;

// Returns true when the last search returned at least one hit
function TSearchEditor.HasHits:Boolean;
begin
  result:=Search.Hits.Items<>nil;
end;

// Returns the index number of the search hit at a given row and data item,
// or -1 if not found
function TSearchEditor.HitIndex(const ARow:Integer;
                                const AItem:TDataItem):TInteger;
begin
  result:=Search.Hits.IndexOf(ARow,AItem);
end;

// Returns true when the data item at a given row contains the search text
function TSearchEditor.HasHit(const ARow:Integer;
                              const AItem:TDataItem;
                              out AIndex:TInteger):Boolean;
begin
  result:=Search.Hits.Exists(ARow,AItem);

  // Return the search hit index, if found
  if result then
     AIndex:=HitIndex(ARow,AItem)
  else
     AIndex:=-1;
end;

// Move to next search hit
procedure TSearchEditor.SBDownClick(Sender: TObject);
begin
  SetCurrent(Current+1);

  if Assigned(FOnChanged) then
     FOnChanged(Self);
end;

// Show search options popup menu
procedure TSearchEditor.SBMenuClick(Sender: TObject);
begin
  TBIMenu.Popup(PopupMenu1,(Sender as TControl));
end;

// Move to previous search hit
procedure TSearchEditor.SBUpClick(Sender: TObject);
begin
  SetCurrent(Current-1);

  if Assigned(FOnChanged) then
     FOnChanged(Self);
end;

// Reset all controls Left positions
procedure TSearchEditor.Reposition(const ALeft:Integer);
var tmp : Integer;
begin
  tmp:=SBClose.Left+SBClose.Width+1;

  if ALeft>tmp then
     tmp:=ALeft;

  ESearch.Left:=tmp;
  SBMenu.Left:=ESearch.Left+ESearch.Width+4;
  LHits.Left:=SBMenu.Left+SBMenu.Width+4;
end;

procedure TSearchEditor.Clear;
begin
  ESearch.Text:='';
end;

end.
