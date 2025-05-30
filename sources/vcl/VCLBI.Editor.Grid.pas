{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid editor dialog                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Grid;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Grids, Vcl.CheckLst, Vcl.DBGrids, Data.DB, VCLBI.Grid.DBGrid;

type
  {$IFDEF FPC}
  HWND=Cardinal;
  {$ENDIF}

  TBIDBGridEditor = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CBOptions: TCheckListBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    EColWidth: TEdit;
    ERowHeight: TEdit;
    UDColWidth: TUpDown;
    UDRowHeight: TUpDown;
    Label5: TLabel;
    ELineWidth: TEdit;
    UDLineWidth: TUpDown;
    TabSheet3: TTabSheet;
    CBStyle: TComboBox;
    CBBorder: TCheckBox;
    BBackColor: TButton;
    BTitleFont: TButton;
    FontDialog1: TFontDialog;
    BCellsFont: TButton;
    FontDialog2: TFontDialog;
    CBReadOnly: TCheckBox;
    Tree: TTreeView;
    GBColumn: TGroupBox;
    BColColor: TButton;
    CBColVisible: TCheckBox;
    Button1: TButton;
    FontDialog3: TFontDialog;
    CBColAlign: TComboBox;
    CBColExpanded: TCheckBox;
    Label1: TLabel;
    TabMenu: TTabSheet;
    CBMenu: TCheckBox;
    CBSort: TCheckBox;
    procedure CBStyleChange(Sender: TObject);
    procedure CBOptionsClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBBorderClick(Sender: TObject);
    procedure BBackColorClick(Sender: TObject);
    procedure EColWidthChange(Sender: TObject);
    procedure ERowHeightChange(Sender: TObject);
    procedure ELineWidthChange(Sender: TObject);
    procedure BTitleFontClick(Sender: TObject);
    procedure FontDialog1Apply(Sender: TObject; Wnd: HWND);
    procedure BCellsFontClick(Sender: TObject);
    procedure FontDialog2Apply(Sender: TObject; Wnd: HWND);
    procedure CBReadOnlyClick(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure BColColorClick(Sender: TObject);
    procedure CBColVisibleClick(Sender: TObject);
    procedure FontDialog3Apply(Sender: TObject; Wnd: HWND);
    procedure Button1Click(Sender: TObject);
    procedure CBColAlignChange(Sender: TObject);
    procedure CBColExpandedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBSortClick(Sender: TObject);
    procedure CBMenuClick(Sender: TObject);
  private
    { Private declarations }

    Grid : TBIDBGrid;
    ISetting : Boolean;

    function Column:TColumn;

    {$IFDEF FPC}
    procedure SetColumnAlignment(const Col:TColumn);
    procedure SetColumnColor(const Col:TColumn);
    procedure SetColumnFont(const Col:TColumn);
    {$ENDIF}
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const ABIGrid:TBIDBGrid); static;
    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AGrid:TBIDBGrid):TBIDBGridEditor; static;

    procedure FillColumns;
    procedure Refresh(const AGrid:TBIDBGrid);
  end;

implementation

{$R *.dfm}

uses
  VCLBI.Grid;

procedure TBIDBGridEditor.BBackColorClick(Sender: TObject);
var tmp : TColor;
begin
  if TUICommon.EditColor(Self,Grid.Color,tmp) then
     Grid.Color:=tmp;
end;

procedure TBIDBGridEditor.BCellsFontClick(Sender: TObject);
begin
  FontDialog2.Font:=Grid.Font;

  if FontDialog2.Execute then
     Grid.Font:=FontDialog2.Font;
end;

procedure TBIDBGridEditor.BTitleFontClick(Sender: TObject);
begin
  FontDialog1.Font:=Grid.TitleFont;

  if FontDialog1.Execute then
     Grid.TitleFont:=FontDialog1.Font;
end;

type
  TColumnProc={$IFNDEF FPC}reference to{$ENDIF} procedure(const Col:TColumn) {$IFDEF FPC}of object{$ENDIF};

procedure Traverse(const Col:TColumn; const Proc:TColumnProc);
{$IFNDEF FPC}
var t : Integer;
{$ENDIF}
begin
  if Col<>nil then
  begin
    Proc(Col);

    {$IFNDEF FPC}
    if Col.Field is TObjectField then
       for t:=0 to TObjectField(Col.Field).Fields.Count-1 do
           Traverse(TBIDBGrid(Col.Grid).ColumnOf(TObjectField(Col.Field).Fields[t]),Proc);
    {$ENDIF}
  end;
end;

{$IFDEF FPC}
procedure TBIDBGridEditor.SetColumnFont(const Col:TColumn);
begin
  Col.Font:=FontDialog3.Font;
end;

procedure TBIDBGridEditor.SetColumnColor(const Col:TColumn);
begin
//  Col.Color:=tmp;
end;

procedure TBIDBGridEditor.SetColumnAlignment(const Col:TColumn);
begin
  Col.Alignment:=TAlignment(CBColAlign.ItemIndex);
end;
{$ENDIF}

procedure TBIDBGridEditor.Button1Click(Sender: TObject);
begin
  FontDialog3.Font:=Column.Font;

  if FontDialog3.Execute then
  begin
     Traverse(Column,
     {$IFDEF FPC}
     SetColumnFont
     {$ELSE}
     procedure(const Col:TColumn)
     begin
       Col.Font:=FontDialog3.Font;
     end
     {$ENDIF}
     );

     Grid.Invalidate;
  end;
end;

procedure TBIDBGridEditor.BColColorClick(Sender: TObject);
var tmp : TColor;
begin
  if TUICommon.EditColor(Self,Column.Color,tmp) then
  begin
    Traverse(Column,
     {$IFDEF FPC}
     SetColumnColor
     {$ELSE}
     procedure(const Col:TColumn)
     begin
       Col.Color:=tmp;
     end
     {$ENDIF});

    Grid.Invalidate;
  end;
end;

procedure TBIDBGridEditor.CBBorderClick(Sender: TObject);
begin
  if CBBorder.Checked then
     Grid.BorderStyle:=bsSingle
  else
     Grid.BorderStyle:=bsNone;
end;

procedure TBIDBGridEditor.CBColAlignChange(Sender: TObject);
begin
  Traverse(Column,
    {$IFDEF FPC}
    SetColumnAlignment
    {$ELSE}
    procedure(const Col:TColumn)
    begin
      Col.Alignment:=TAlignment(CBColAlign.ItemIndex);
    end
    {$ENDIF}
    );
end;

procedure TBIDBGridEditor.CBColExpandedClick(Sender: TObject);
begin
  Column.Expanded:=CBColExpanded.Checked;
end;

procedure TBIDBGridEditor.CBColVisibleClick(Sender: TObject);
begin
  Column.Visible:=CBColVisible.Checked;
end;

procedure TBIDBGridEditor.CBMenuClick(Sender: TObject);
begin
  Grid.MenuButton.Visible:=CBMenu.Checked;
end;

const
  Options:Array[0..{$IFDEF FPC}11{$ELSE}13{$ENDIF}] of TDBGridOption=(
    dgAlwaysShowEditor,
    dgAlwaysShowSelection,
    dgCancelOnExit,
    dgColLines,
    dgColumnResize,
    dgConfirmDelete,
    dgEditing,
    dgIndicator,
    dgRowLines,
    dgRowSelect,
    dgTabs,
    {$IFNDEF FPC}
    dgTitleClick,
    dgTitleHotTrack,
    {$ENDIF}
    dgTitles);

procedure TBIDBGridEditor.CBOptionsClickCheck(Sender: TObject);

  procedure SetOption(const AOption:TDBGridOption);
  begin
    if CBOptions.Checked[CBOptions.ItemIndex] then
       Grid.Options:=Grid.Options+[AOption]
    else
       Grid.Options:=Grid.Options-[AOption];
  end;

begin
  SetOption(Options[CBOptions.ItemIndex]);
end;

procedure TBIDBGridEditor.CBReadOnlyClick(Sender: TObject);
begin
  Grid.ReadOnly:=CBReadOnly.Checked;
end;

procedure TBIDBGridEditor.CBSortClick(Sender: TObject);
begin
  if not ISetting then
     Grid.ColumnSort:=CBSort.Checked;
end;

procedure TBIDBGridEditor.CBStyleChange(Sender: TObject);
begin
  if not ISetting then
  begin
    {$IFNDEF FPC}
    Grid.DrawingStyle:=TGridDrawingStyle(CBStyle.ItemIndex);
    BBackColor.Enabled:=Grid.DrawingStyle=TGridDrawingStyle.gdsClassic;
    {$ENDIF}
  end;
end;

function TBIDBGridEditor.Column: TColumn;
begin
  if Tree.Selected=nil then
     result:=nil
  else
     result:=TColumn(Tree.Selected.Data);
end;

procedure TBIDBGridEditor.EColWidthChange(Sender: TObject);
begin
  if UDColWidth.Showing then
     Grid.DefaultColWidth:=UDColWidth.Position;
end;

class procedure TBIDBGridEditor.Edit(const AOwner: TComponent; const ABIGrid:TBIDBGrid);
begin
  with TBIDBGridEditor.Create(AOwner) do
  try
    Refresh(ABIGrid);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TBIDBGridEditor.ELineWidthChange(Sender: TObject);
begin
  if not ISetting then
     Grid.GridLineWidth:=UDLineWidth.Position;
end;

class function TBIDBGridEditor.Embedd(const AOwner: TComponent;
  const AParent: TWinControl; const AGrid: TBIDBGrid): TBIDBGridEditor;
begin
  result:=TBIDBGridEditor.Create(AOwner);
  result.Refresh(AGrid);

  TUICommon.AddForm(result,AParent);
end;

procedure TBIDBGridEditor.ERowHeightChange(Sender: TObject);
begin
  if not ISetting then
     Grid.DefaultRowHeight:=UDRowHeight.Position;
end;

procedure TBIDBGridEditor.FillColumns;

  procedure AddColumn(const AParent:TTreeNode; const Col:TColumn);
  var tmp : TTreeNode;
      t : Integer;
  begin
    if Col<>nil then
    begin
      tmp:=Tree.Items.AddChildObject(AParent,Col.Title.Caption,Col);

      {$IFNDEF FPC}
      if Col.Field is TObjectField then
         for t:=0 to TObjectField(Col.Field).Fields.Count-1 do
             AddColumn(tmp,Grid.ColumnOf(TObjectField(Col.Field).Fields[t]));
      {$ENDIF}
    end;
  end;

var t : Integer;
begin
  ISetting:=True;

  Tree.Items.BeginUpdate;
  try
    Tree.Items.Clear;

    for t:=0 to Grid.Columns.Count-1 do
        {$IFNDEF FPC}
        if Grid.Columns[t].ParentColumn=nil then
        {$ENDIF}
           AddColumn(nil,Grid.Columns[t]);
  finally
    Tree.Items.EndUpdate;
  end;

  ISetting:=False;

    if Tree.Items.Count>0 then
    begin
      Tree.Items[0].Expanded:=True;
      Tree.Select(Tree.Items[0]);
    end;
end;

procedure TBIDBGridEditor.FontDialog1Apply(Sender: TObject; Wnd: HWND);
begin
  Grid.TitleFont:=FontDialog1.Font;
end;

procedure TBIDBGridEditor.FontDialog2Apply(Sender: TObject; Wnd: HWND);
begin
  Grid.Font:=FontDialog2.Font;
end;

procedure TBIDBGridEditor.FontDialog3Apply(Sender: TObject; Wnd: HWND);
begin
  Traverse(Column,
    {$IFDEF FPC}
    SetColumnFont
    {$ELSE}
    procedure(const Col:TColumn)
    begin
      Col.Font:=FontDialog3.Font;
    end
    {$ENDIF});
end;

procedure TBIDBGridEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,'Grid');
end;

procedure TBIDBGridEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabSheet1;
end;

procedure TBIDBGridEditor.FormShow(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,'Grid');

  if Tag<>0 then
     Refresh(TBIDBGrid(Tag));

  if Tree.Items.Count>0 then
     Tree.SetFocus;
end;

procedure TBIDBGridEditor.Refresh(const AGrid:TBIDBGrid);

  procedure CheckOption(const Index:Integer; const AOption:TDBGridOption);
  begin
    CBOptions.Checked[Index]:=AOption in Grid.Options;
  end;

var t : Integer;
begin
  Grid:=AGrid;

  ISetting:=True;

  {$IFNDEF FPC}
  CBStyle.ItemIndex:=Ord(Grid.DrawingStyle);
  {$ENDIF}

  for t:=0 to High(Options) do
      CheckOption(t, Options[t]);

  CBBorder.Checked:=Grid.BorderStyle=bsSingle;

  {$IFNDEF FPC}
  BBackColor.Enabled:=Grid.DrawingStyle=TGridDrawingStyle.gdsClassic;
  {$ENDIF}

  CBSort.Checked:=Grid.ColumnSort;
  CBMenu.Checked:=Grid.MenuButton.Visible;

  UDColWidth.Position:=Grid.DefaultColWidth;
  UDRowHeight.Position:=Grid.DefaultRowHeight;
  UDLineWidth.Position:=Grid.GridLineWidth;
  CBReadOnly.Checked:=Grid.ReadOnly;

  ISetting:=False;

  FillColumns;
end;

procedure TBIDBGridEditor.TreeChange(Sender: TObject; Node: TTreeNode);
var Col : TColumn;
begin
  if not ISetting then
  begin
    Col:=Column;

    GBColumn.Visible:=Col<>nil;

    if Col<>nil then
    begin
      CBColVisible.Checked:=Col.Visible;
      CBColAlign.ItemIndex:=Ord(Col.Alignment);

      {$IFNDEF FPC}
      CBColExpanded.Enabled:=Col.Expandable;
      CBColExpanded.Checked:=Col.Expandable and Col.Expanded;
      {$ENDIF}
    end;
  end;
end;

initialization
  RegisterClass(TBIDBGridEditor);
end.
