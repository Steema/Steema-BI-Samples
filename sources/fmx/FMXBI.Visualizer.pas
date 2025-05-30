{*********************************************}
{  TeeBI Software Library                     }
{  Data Visualizer Control                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Visualizer;
{$DEFINE FMX}
{$SCOPEDENUMS ON}

interface

{$IFNDEF FPC}
{$IF CompilerVersion>26}
{$DEFINE XE6}
{$ENDIF}
{$ENDIF}

uses
  System.Classes, Data.DB,
  BI.DataItem, BI.Arrays, BI.DataSource,

  {$IFDEF FMX}
  System.Types,
  Fmx.Types, Fmx.Controls, Fmx.StdCtrls, Fmx.ListBox, Fmx.TabControl,
  Fmx.Layouts, Fmx.Objects, Fmx.TreeView, FMXBI.DataControl, FMXBI.Grid
  {$ELSE}

  Vcl.Controls, VCL.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Forms,
  VCLBI.DataControl, VCLBI.Grid
  {$ENDIF}
  ;

type
  TGroup=class;

  TGroupClass=class of TGroup;

  TVisualizerItems=class;

  TVisualizerItem=class(TCollectionItem) // TDataCollectionItem !
  private
    FEnabled : Boolean;
    FItems : TVisualizerItems;

    procedure ReCalculate;
    procedure SetEnabled(const Value:Boolean);
    function GetSelected(const Index: TInteger): Boolean;
    procedure SetSelected(const Index: TInteger; const Value: Boolean);
  public
    Current : TGroup;
    Data : TDataItem;
    GroupBy : TDataItem;
    GroupClass : TGroupClass;

    Constructor Create(Collection:TCollection); override;
    Destructor Destroy; override;

    function AsString(const AIndex:TInteger):String;
    function Count:TInteger;
    function IsEnabled:Boolean;
    class function MapToString(const AData:TDataItem; const AMap:TDataMap; const APos:TInteger):String; static;

    property Selected[const Index:TInteger]:Boolean read GetSelected write SetSelected;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default True;
  end;

  TBIComposer=class;

  TVisualizerItems=class(TOwnedCollection)
  private
    IParent : TBIComposer;

    function GetItem(const Index: Integer): TVisualizerItem;
    procedure SetItem(const Index: Integer; const Value: TVisualizerItem);
  public
    type
      TEnabled=record
      private
        IOwner : TVisualizerItems;
      public
        function Count:Integer;
        function First(const Start:Integer):Integer; overload;
        function First:Integer; overload;
        function Last:Integer;
      end;

    var
      Enabled : TEnabled;

    Constructor Create(AOwner: TPersistent);

    function Add(const AData:TDataItem):TVisualizerItem;
    procedure Exchange(const A,B:Integer);

    property Items[const Index:Integer]:TVisualizerItem read GetItem write SetItem; default;
  end;

  {$IFDEF FMX}
  TWinControl=TControl;
  TPageControl=TTabControl;
  {$ENDIF}

  TGroupProc<T>={$IFNDEF FPC}reference to{$ENDIF} procedure(const AGroup:T);

  TGroupClasses=Array of TGroupClass;

  TGroupClassesHelper=record helper for TGroupClasses
  public
    procedure Add(const Value:TGroupClass);
  end;

  TGroup=class(TComponent)
  private
  type
    TPendingItem=record
    public
      Group : TGroup;
      Next : Integer;
      Rows : TCursorIndex;
      Data : TDataItem;
      Control : TComponent;
      Position : TInteger;
    end;

  var
    FGroups : Array of TGroup;

    procedure AddGroup(const AGroup:TGroup);
  protected
    CanAddValues : Boolean;

    FItems : Array of TPendingItem;
    FVisualizer : TBIComposer;

    FixedWidth : Boolean;
    Position : TInteger;

    function AddItem(const AData:TDataItem; const ANext:Integer;
                  const ARows:TCursorIndex; const APos:TInteger):TInteger;

    function AsString(const AData:TDataItem):String; overload;
    function AsString(const AIndex:TInteger):String; overload;

    procedure Init; virtual;
    procedure Finished; virtual;

    function GetDataString:String;

    {$IFNDEF FPC}
    procedure Traverse<T:Class>(const AProc:TGroupProc<T>);
    {$ENDIF}
  public
  class var
    GroupClasses:TGroupClasses;

  var
    Parent : TGroup;
    Item : TVisualizerItem;

    Control : TWinControl;
    Data : TDataItem;

    Index : TCursorIndex;

    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); virtual;
    procedure Add(const AIndex:TInteger); virtual;

    procedure AddValues(const AComponent:TComponent;
                        const Values:TVisualizerItems;
                        const AGroup:TVisualizerItem;
                        const ARows:TCursorIndex); virtual;

    class function BestControl(const AIndex,ATotal,AValues:Integer):TGroupClass; virtual;

    class function ClassIndexOf(const AClass:TGroupClass):Integer; static;
    function MapToString:String;
  end;

  {$IFDEF FMX}
  TWinControlClass=class of TWinControl;
  {$ENDIF}

  TGroupControl=class(TGroup)
  private
    FLabel : TLabel;
    FShowLabel : Boolean;

    procedure AddCaptionPanel;
    function CreateControlClass(const AClass:TWinControlClass):TWinControl;
    procedure SetShowLabel(const Value:Boolean);
  protected
    procedure CheckControl(const AIndex: Integer);
    procedure CreateLabel(const AText:String);
    procedure DoSetShowLabel(const Value:Boolean); virtual;
    procedure ShowItem(const AIndex:Integer);
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    procedure Assign(Source:TPersistent); override;

    property GroupLabel:TLabel read FLabel;
  published
    property ShowLabel:Boolean read FShowLabel write SetShowLabel default True;
  end;

  TGroupAlign=(Left,Top,Right,Bottom);

  TGroupControlAlign=class(TGroupControl)
  private
    FAlign : TGroupAlign;

    procedure AlignControl(const AControl:TControl);
    procedure ResetWidth;
    procedure SetAlign(const Value:TGroupAlign);
  protected
    function CalcDefaultWidth:Integer; virtual;
    procedure ChangeAlign(const Value: TGroupAlign); virtual;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Align:TGroupAlign read FAlign write SetAlign default TGroupAlign.Left;
  end;

  TGroupList=class(TGroupControlAlign)
  private
    FList  : TCustomListBox;

    procedure ChangeList(const Value: Boolean);
    procedure CheckedList(Sender:TObject);
    procedure ClickedList(Sender:TObject);
    function GetCheckBoxes: Boolean;
    procedure SetCheckBoxes(const Value: Boolean);
  protected
    function CalcDefaultWidth:Integer; override;
    procedure Init; override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;

    procedure Assign(Source:TPersistent); override;

    property List:TCustomListBox read FList;
  published
    property CheckBoxes:Boolean read GetCheckBoxes write SetCheckBoxes default False;
  end;

  TGroupCombo=class(TGroupControlAlign)
  private
    FCombo : TComboBox;

    IComboLabel : TLabel;

    procedure ClickedCombo(Sender:TObject);
    procedure CreateCombo;
    procedure SetComboLeft;
  protected
    function CalcDefaultWidth:Integer; override;
    procedure Init; override;
    procedure Finished; override;
    procedure DoSetShowLabel(const Value:Boolean); override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;

    property ComboBox:TComboBox read FCombo;
  published
    property Align default TGroupAlign.Top;
  end;

  TGroupPage=class(TGroupControlAlign)
  private
    FPage : TPageControl;

    procedure ChangedTab(Sender:TObject);
  protected
    procedure ChangeAlign(const Value: TGroupAlign); override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;

    procedure Assign(Source:TPersistent); override;

    property PageControl:TPageControl read FPage;
  published
    {$IFDEF FMX}
    //property TabStyle...
    {$ENDIF}
  end;

  TGroupGrid=class(TGroupControlAlign)
  private
    FData : TDataItem;
    FGrid : TBIGrid;

    ISplitter : TSplitter;

    procedure DataChange(Sender: TObject; Field: TField);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    procedure Add(const AIndex:TInteger); override;

    procedure AddValues(const AComponent: TComponent;
                        const Values:TVisualizerItems;
                        const AGroup:TVisualizerItem;
                        const ARows: TCursorIndex); override;

    property Grid:TBIGrid read FGrid;
  end;

  TGroupTrackbar=class(TGroupControlAlign)
  private
    FTrack : TTrackbar;

    procedure ClickedTrack(Sender:TObject);
    procedure ResizeTrackBar;
  protected
    procedure ChangeAlign(const Value: TGroupAlign); override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    property TrackBar:TTrackBar read FTrack;
  end;

  TGroupButtons=class(TGroupControlAlign)
  private
    FPanel : TWinControl;

    procedure ClickedButton(Sender:TObject);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;
  end;

  TGroupMultiScroll=(Automatic,Yes,No);

  // Redefine TCoordinate here (it is also at VCLTee.TeeCanvas and FMXTee.Canvas)
  // to not depend on a recent TeeChart version.
  TCoordinate={$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};

  TGroupMultiControl=class(TGroupControl)
  private
    FColumns : Integer;
    FExpandLast : Boolean;
    FRowHeight: Integer;
    FScroll : TGroupMultiScroll;
    FSplitters : Boolean;

    FScrollBox : TScrollBox;

    FScroller : Boolean;

    IExtras : Array of TControl;

    procedure AddExtra(const AControl:TControl);
    function AvailableWidth(const AControl:TWinControl; const AMax:Integer):TCoordinate;
    function CanResizeWidth(const AControl:TControl):Boolean;
    procedure ClearExtras;
    procedure ControlResized(Sender:TObject);
    procedure DoReLayout;
    procedure DoResize(const AControl:TControl; const Columns:Integer);
    procedure ReLayout;
    procedure SetColumns(const Value: Integer);
    procedure SplitHoriz(const AControl:TWinControl);
    procedure SplitVert(const AControl:TWinControl);
    procedure SubPanelControls(const APanel:TWinControl; const Columns:Integer);
    procedure SetExpandLast(const Value: Boolean);
    procedure SetScroll(const Value: TGroupMultiScroll);
    procedure SetRowHeight(const Value: Integer);
    procedure SetSplitters(const Value: Boolean);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    procedure Add(const AIndex:TInteger); override;
    procedure Assign(Source:TPersistent); override;

    property ScrollBox:TScrollBox read FScrollBox;
  published
    property Columns:Integer read FColumns write SetColumns default 2;
    property ExpandLast:Boolean read FExpandLast write SetExpandLast default False;
    property RowHeight:Integer read FRowHeight write SetRowHeight default 0;
    property Scroll:TGroupMultiScroll read FScroll write SetScroll default TGroupMultiScroll.Automatic;
    property Splitters:Boolean read FSplitters write SetSplitters default False;
  end;

  {$IFDEF FMX}
  TTreeNode=TTreeViewItem;
  {$ENDIF}

  TGroupTree=class(TGroupControlAlign)
  private
    FTree  : TTreeView;

    IParent : TTreeNode;

    procedure ChangedNode(Sender: TObject{$IFNDEF FMX}; Node: TTreeNode{$ENDIF});
  protected
    procedure Init; override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    procedure Add(const AIndex:TInteger); override;

    property TreeView:TTreeView read FTree;
  end;

  // Nested TBIComposer
  TGroupVisualizer=class(TGroup)
  private
    FVisualizer : TBIComposer;
  protected
    procedure Init; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    property Visualizer:TBIComposer read FVisualizer;
  end;

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(TeeAllComponentPlatformIDs)]
  {$ENDIF}
  {$ENDIF}
  TBIComposer=class(TBIDataControl)
  private
    FGroups,
    FValues : TVisualizerItems;

    FMain : TGroup;

    function BestGroup(const AItem:TVisualizerItem; const AIndex:Integer; const APrevious:TGroup):TGroup;

    procedure CreateCurrent;
    procedure CreateGroups;
    procedure Guess;
    function IndexOfEnabled(const AIndex:Integer):Integer;
    procedure ResetGroups;

    procedure SetGroups(const Value: TVisualizerItems);
    procedure SetValues(const Value: TVisualizerItems);
    procedure SortGroups;
  protected
    function AddGroup(const APrevious:TGroup; const AIndex:Integer;
             const AParent:TComponent;
             const ARows:TCursorIndex):TGroup;

    procedure AddItems;
    procedure Loaded; override;
    procedure SetDataDirect(const Value:TDataItem); override;
  public
    class var
      ValuesGroupClass : TGroupClass;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    // Pending: Automatic Guess:
    // procedure BindTo(const ASummary:TSummary);

    procedure BestOrder;
    procedure Clear;
    procedure ReCalculate;

    property Main:TGroup read FMain;
  published
    property Groups:TVisualizerItems read FGroups write SetGroups;
    property Values:TVisualizerItems read FValues write SetValues;
  end;

  // Helper class for both VCL and FMX TBIComposer editor dialogs:
  TBIVisualizerUI=record
  public
    Viz : TBIComposer;

    class procedure AddClasses(const AItems:TStrings); static;
    class procedure AddGroupByItems(const AItems:TStrings; const AData:TDataItem;
                                    const AIndex: Integer); static;
    class procedure AddItems(const AList: TCustomListBox; const AItems: TVisualizerItems); static;

    function ChangeClass(const AGroup,AClass:Integer):Boolean;

    function GetControl(const AIndex:Integer):TGroupControl;
    function GetList(const AIndex:Integer):TGroupList;
    function GetMulti(const AIndex:Integer):TGroupMultiControl;
  end;

implementation

uses
  {$IFNDEF FMX}
  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  {$ENDIF}
  {$ENDIF}

  System.SysUtils,

  {$IFDEF FMX}
  Fmx.Forms,

  {$IF CompilerVersion<26} // Cannot use FireMonkeyVersion<21 (or 21.0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  System.UITypes, Fmx.Platform,

  {$ELSE}
  Vcl.Graphics, Vcl.Buttons, VCL.CheckLst, VCLBI.Grid.DBGrid, VCL.DBGrids,
  {$ENDIF}

  BI.Expression, BI.Persist, BI.Summary, BI.Store.Component, BI.Expressions;

{ TVisualizerItem }

Constructor TVisualizerItem.Create(Collection: TCollection);
begin
  inherited;
  FEnabled:=True;
end;

Destructor TVisualizerItem.Destroy;
begin
  Current.Free;
  FItems.Free;
  inherited;
end;

function TVisualizerItem.GetSelected(const Index: TInteger): Boolean;
begin
  if FItems=nil then
     result:=True
  else
     result:=FItems[Index].Enabled;
end;

function TVisualizerItem.IsEnabled: Boolean;
var t : TLoopInteger;
begin
  result:=FEnabled;

  if result and (FItems<>nil) then
  begin
    result:=False;

    for t:=0 to FItems.Count-1 do
        if FItems[t].IsEnabled then
           Exit(True);
  end;
end;

type
  TDataAccess=class(TDataItem);

class function TVisualizerItem.MapToString(const AData:TDataItem; const AMap:TDataMap; const APos:TInteger):String;
begin
  case AData.Kind of
     dkInt32: if TDataAccess(AData).IHasDate then
                 result:=TDataAccess(AData).IDate.AsString(APos)
              else
                 result:=TInt32Map(AMap).AsString(APos);

     dkInt64: result:=TInt64Map(AMap).AsString(APos);
      dkText: result:=TTextMap(AMap).AsString(APos);
   dkBoolean: result:=BoolToStr(APos=0,True);
   dkUnknown: result:=AData.Items[APos].Name;
  else
    result:='';
  end;
end;

procedure TVisualizerItem.ReCalculate;
begin
  TVisualizerItems(Collection).IParent.AddItems;
end;

procedure TVisualizerItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    ReCalculate;
  end;
end;

procedure TVisualizerItem.SetSelected(const Index: TInteger; const Value: Boolean);
var t : Integer;
begin
  if Selected[Index]<>Value then
  begin
    if FItems=nil then
    begin
      FItems:=TVisualizerItems.Create(Self);
      FItems.IParent:=TVisualizerItems(Collection).IParent;

      for t:=0 to Count-1 do
          FItems.Add(Data);
    end;

    FItems[Index].Enabled:=Value;
  end;
end;

function TVisualizerItem.Count:TInteger;
var tmp : TDataMap;
begin
  if Data=nil then
     result:=0
  else
  if Data.AsTable or (Data.Kind=TDataKind.dkUnknown) then
     result:=Data.Items.Count
  else
  begin
    Data.Load;
    tmp:=Data.DataMap;

    if tmp=nil then
       result:=0
    else
       result:=tmp.Count;
  end;
end;

function TVisualizerItem.AsString(const AIndex:TInteger):String;
var tmp : TDataMap;
begin
  if Data=nil then
     result:=''
  else
  if Data.AsTable or (Data.Kind=TDataKind.dkUnknown) then
     result:=Data{$IFDEF FPC}.Items{$ENDIF}[AIndex].Name
  else
  begin
    Data.Load;
    tmp:=Data.DataMap;

    if tmp=nil then
       result:=''
    else
       result:=MapToString(Data,tmp,AIndex);
  end;
end;

{ TVisualizerItems }

Constructor TVisualizerItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TVisualizerItem);

  if AOwner is TBIComposer then
     IParent:=TBIComposer(AOwner);

  Enabled.IOwner:=Self;
end;

procedure TVisualizerItems.Exchange(const A, B: Integer);
begin
  Items[A].Index:=B;
end;

// Returns the index of the first AItems that is Enabled, starting from 0
function TVisualizerItems.TEnabled.First:Integer;
begin
  result:=First(-1);
end;

function TVisualizerItems.GetItem(const Index: Integer): TVisualizerItem;
begin
  result:=TVisualizerItem(inherited Items[Index]);
end;

procedure TVisualizerItems.SetItem(const Index: Integer;
  const Value: TVisualizerItem);
begin
  inherited Items[Index]:=Value;
end;

function TVisualizerItems.Add(const AData: TDataItem):TVisualizerItem;
begin
  result:=(inherited Add) as TVisualizerItem;
  result.Data:=AData;
end;

{ TVisualizerItems.TEnabled }

// Returns the number of AItems that are Enabled
function TVisualizerItems.TEnabled.Count:Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to IOwner.Count-1 do
      if IOwner[t].IsEnabled then
         Inc(result);
end;

// Returns the index of the first AItems that is Enabled, starting from Start+1
function TVisualizerItems.TEnabled.First(const Start:Integer):Integer;
var t : Integer;
begin
  for t:=Start+1 to IOwner.Count-1 do
      if IOwner[t].IsEnabled then
         Exit(t);

  result:=-1;
end;

// Returns the index of the last AItems that is Enabled
function TVisualizerItems.TEnabled.Last;
var t : Integer;
begin
  for t:=IOwner.Count-1 downto 0 do
      if IOwner[t].IsEnabled then
         Exit(t);

  result:=-1;
end;

procedure DoAddControl(const AGroup:TGroup; const AParent:TComponent);
begin
  if (AGroup.Control<>nil) and (AGroup.Control.Owner=AGroup) then
  begin
    AGroup.Control.Align:=TUICommon.AlignClient;

    if AParent is TWinControl then
       AGroup.Control.Parent:=TWinControl(AParent);
  end;
end;

{ TBIComposer }

Constructor TBIComposer.Create(AOwner: TComponent);
begin
  inherited;

  //DoubleBuffered:=True; <-- Breaks TeeChart OpenGL Canvas !

  FGroups:=TVisualizerItems.Create(Self);
  FValues:=TVisualizerItems.Create(Self);
end;

Destructor TBIComposer.Destroy;
begin
  FMain.Free;
  FValues.Free;
  FGroups.Free;
  inherited;
end;

procedure TBIComposer.AddItems;
var tmpCount,
    tmpFirst : Integer;
begin
  FMain.Free;
  FMain:=nil;

  tmpCount:=FGroups.Enabled.Count;

  if tmpCount>0 then
  begin
    tmpFirst:=FGroups.Enabled.First;
    FMain:=AddGroup(nil,tmpFirst,nil,nil);
    FMain.FVisualizer:=Self;
  end
  else
  if FValues.Enabled.Count>0 then
  begin
    FMain:=ValuesGroupClass.CreateData(nil,nil);
    FMain.FVisualizer:=Self;
    DoAddControl(FMain,Self);

    FMain.Init;
    try
      FMain.AddValues(nil,FValues,nil,nil);
    finally
      FMain.Finished;
    end;
  end;
end;

function UniqueRatio(const AData:TDataItem):Single;
begin
  if AData.AsTable then
     result:=1
  else
  if AData.Unique or (AData.Count=0) then
     result:=1
  else
  begin
    AData.Load;
    AData.Stats;

    if AData.DataMap=nil then
       result:=1
    else
       result:=AData.DataMap.Count/AData.Count;
  end;
end;

procedure TBIComposer.Loaded;
begin
  inherited;

  if Data<>nil then
     ReCalculate;
end;

procedure TBIComposer.CreateCurrent;
var t : Integer;
    tmpItem : TVisualizerItem;
    tmp : TGroup;
begin
  tmp:=nil;

  for t:=0 to FGroups.Count-1 do
  begin
    tmpItem:=FGroups[t];

    if tmpItem.Current=nil then
    begin
      if tmpItem.GroupClass=nil then
         tmp:=BestGroup(tmpItem,IndexOfEnabled(t),tmp)
      else
         tmp:=tmpItem.GroupClass.CreateData(tmpItem,tmp);

      tmpItem.Current:=tmp;
    end
    else
      tmp:=tmpItem.Current;
  end;
end;

procedure TBIComposer.CreateGroups;
var t : Integer;
begin
  if FGroups.Count>0 then
  begin
    FGroups[0].Current.Free;

    for t:=0 to FGroups.Count-1 do
        FGroups[t].Current:=nil;
  end;

  CreateCurrent;
end;

procedure TBIComposer.ResetGroups;
begin
  Guess;
  SortGroups;
  CreateGroups;
end;

procedure TBIComposer.SetDataDirect(const Value: TDataItem);
begin
  if not (csDestroying in ComponentState) then
  begin
    if Value=nil then
       Clear
    else
    begin
      Clear;
      Value.Load;
      ResetGroups;

      if not (csLoading in ComponentState) then
         AddItems;
    end;
  end;
end;

procedure TBIComposer.SetGroups(const Value: TVisualizerItems);
begin
  FGroups.Assign(Value);
end;

procedure TBIComposer.SetValues(const Value: TVisualizerItems);
begin
  FValues.Assign(Value);
end;

procedure TBIComposer.ReCalculate;
begin
  CreateGroups;
  AddItems;
end;

procedure TBIComposer.BestOrder;
begin
  if Data<>nil then
  begin
    ResetGroups;
    AddItems;
  end;
end;

procedure TBIComposer.SortGroups;

  // Returns the number of items for a group
  function GroupItems(const AData:TDataItem):TInteger;
  begin
    if AData.AsTable then
       result:=AData.Items.Count
    else
       result:=AData.DataMap.Count;
  end;

  // Returns True if group "New" should be considered as "better"
  // than group "Old".
  // Better means the New group should be used before the Old group.
  function IsBetter(const AOld,ANew:Integer):Boolean;
  var tmpOld,
      tmpNew : Single;
  begin
    tmpOld:=UniqueRatio(FGroups[AOld].Data);
    tmpNew:=UniqueRatio(FGroups[ANew].Data);

    if tmpNew=tmpOld then
       // For equal ratios, return the data that has less unique items
       result:=GroupItems(FGroups[ANew].Data)<GroupItems(FGroups[AOld].Data)
    else
       result:=tmpNew<tmpOld;
  end;

var t,tt : Integer;
begin
  for t:=1 to FGroups.Count-1 do
  begin
    tt:=t-1;

    while (tt>=0) and IsBetter(tt,t) do
    begin
      FGroups[tt+1].Index:=tt;
      Dec(tt);
    end;

    FGroups[tt+1].Index:=t;
  end;
end;

// Ensures AData is loaded, and returns its map
function MapOf(const AData:TDataItem):TDataMap;
begin
  if AData=nil then
     result:=nil
  else
  begin
    AData.Load;
    AData.Stats;
    result:=AData.DataMap;
  end;
end;

// Return the total count of enabled groups up to including AIndex
function TBIComposer.IndexOfEnabled(const AIndex:Integer):Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to AIndex-1 do
      if FGroups[t].IsEnabled then
         Inc(result);
end;

// Creates and returns the "best" group class for AItem
function TBIComposer.BestGroup(const AItem:TVisualizerItem; const AIndex:Integer; const APrevious:TGroup):TGroup;

  function NumberOfItems(const AData:TDataItem):TInteger;
  begin
    if AData.DataMap=nil then
       result:=AData.Items.Count
    else
       result:=AData.DataMap.Count;
  end;

var L : Integer;
    tmpBestClass : TGroupClass;
begin
  L:=FGroups.Enabled.Count;

  tmpBestClass:=ValuesGroupClass.BestControl(AIndex,L,Values.Enabled.Count);

  if tmpBestClass=nil then
     if (AIndex=L-3) and (NumberOfItems(AItem.Data)<12) then
        tmpBestClass:=TGroupMultiControl
     else
        tmpBestClass:=TGroupCombo;

  result:=tmpBestClass.CreateData(AItem,APrevious);
end;

function TBIComposer.AddGroup(const APrevious:TGroup; const AIndex:Integer;
                                const AParent:TComponent;
                                const ARows:TCursorIndex):TGroup;

  procedure Add(const APos:TInteger; const GroupData:TDataItem);

    function Matches(const AIndex:TInteger):Boolean; overload;
    begin
      result:=GroupData.EqualsMap(AIndex,APos);
    end;

    function Matches(const AData:TInt32Array; const Value:TInteger):TCursorIndex; overload;
    var t : TLoopInteger;
    begin
      result:=nil;

      for t:=0 to AData.Count-1 do
          if AData[t]=Value then
             result.Append(t);
    end;

    function Matches(const AData:TTextArray; const Value:String):TCursorIndex; overload;
    var t : TLoopInteger;
    begin
      result:=nil;

      for t:=0 to AData.Count-1 do
          if AData[t]=Value then
             result.Append(t);
    end;

  var
    GroupMap : TDataMap;

    function Matches(const AData:TDataItem; const APos:TInteger):TCursorIndex; overload;
    var t : TLoopInteger;
    begin
      case GroupData.Kind of
        dkInt32: result:=Matches(GroupData.Int32Data,TInt32Map(GroupMap)[APos]);
         dkText: result:=Matches(GroupData.TextData,TTextMap(GroupMap)[APos]);
        {
        dkInt64: ;
        dkSingle: ;
        dkDouble: ;
        dkExtended: ;
        dkDateTime: ;
        dkBoolean: ;
        dkUnknown: ;
        }
      else
      begin
        result:=nil;

        for t:=0 to GroupData.Count-1 do
            if Matches(t) then
               result.Append(t);
      end;
      end;
    end;

    function FilterRows:TCursorIndex;
    var t : TLoopInteger;
    begin
      if GroupData.Kind=TDataKind.dkUnknown then
         result:=ARows
      else
      begin
        if ARows=nil then
           result:=Matches(GroupData,APos)
        else
        begin
          // Slower
          result:=nil;

          for t:=0 to High(ARows) do
              if Matches(ARows[t]) then
                 result.Append(ARows[t]);
        end;
      end;
    end;

    {
    function ValueIsAlsoGroup(const AData:TDataItem):Boolean;
    var t : Integer;
    begin
      if AData.AsTable then
         if AData.Items.Count>0 then
            if AData.Items[0].AsTable then
               for t:=0 to FGroups.Count-2 do
                   if FGroups[t].Enabled and (FGroups[t].Data=AData.Items[0]) then
                      Exit(True);

      result:=False;
    end;

    function AnyValuesIsGroup:Boolean;
    var t : Integer;
    begin
      for t:=0 to FValues.Count-1 do
          if FValues[t].Enabled and ValueIsAlsoGroup(FValues[t].Data) then
             Exit(True);

      result:=False;
    end;
    }

    function IsSelected(const APos:TInteger):Boolean;
    var tmp : TVisualizerItem;
    begin
      if AIndex=-1 then
         result:=True
      else
      begin
        tmp:=FGroups[AIndex];

        if tmp.FItems=nil then
           result:=True
        else
           result:=tmp.FItems[APos].Enabled;
      end;
    end;

  var tmpNext : Integer;
  begin
    if IsSelected(APos) then
    begin
      case GroupData.Kind of
         dkInt32,
         dkInt64,
          dkText,
       dkBoolean,
       dkUnknown: begin
                    GroupMap:=MapOf(GroupData);
                    tmpNext:=FGroups.Enabled.First(AIndex);

                    result.Position:=result.AddItem(GroupData,tmpNext,FilterRows,APos);
                    result.Add(result.Position);
                  end;
      end;
    end;
  end;

  procedure AddChildren(const AGroup:TVisualizerItem);
  var
    Hops : TDataHops;

    procedure DoAddChildren(const AData:TDataItem);

      function FindRowInMap(const ARow:TInteger; out AIndex:TNativeInteger):Boolean;
      begin
        if AData.Kind=TDataKind.dkUnknown then
           result:=False
        else
        if AData.Missing[ARow] then
           result:=False
        else
           result:=AData.FindInMap(ARow,AIndex);
      end;

    var t : Integer;
        tmpIndex : TNativeInteger;
        tmpMap : TBooleanArray;
        AMap : TDataMap;
    begin
      if AData.Kind=TDataKind.dkUnknown then
         for t:=0 to AData.Items.Count-1 do
             Add(t,AData)
      else
      if AData.Kind=TDataKind.dkBoolean then
      begin
        Add(0,AData);
        Add(1,AData);
      end
      else
      begin
        // Search ARows in AMap, and then add all found map items
        AMap:=MapOf(AData);

        // Optimization when ARows=nil:
        if ARows=nil then
        begin
          // Add all AMap items:
          for t:=0 to AMap.Count-1 do
              Add(t,AData);
        end
        else
        // Optimization when ARows has only one item:
        if Length(ARows)=1 then
        begin
          if FindRowInMap(ARows[0],tmpIndex) then
             Add(tmpIndex,AData);
        end
        else
        begin
          // Multi-row case. Find all rows in map.
          SetLength(tmpMap,AMap.Count);

          // First initialize temp map to False
          for t:=0 to High(tmpMap) do
              tmpMap[t]:=False;

          // Search all rows:
          for t:=0 to High(ARows) do
              if FindRowInMap(ARows[t],tmpIndex) then
                 tmpMap[tmpIndex]:=True;

          // Add all True temp Map items:
          for t:=0 to High(tmpMap) do
              if tmpMap[t] then
                 Add(t,AData);
        end;
      end;
    end;

  begin
    if (AGroup.GroupBy=nil) or (AGroup.GroupBy=AGroup.Data) then
       DoAddChildren(AGroup.Data)
    else
    begin
      Hops:=TDataHops.Create;
      try
        Hops.Main:=AGroup.Data.Parent;
        Hops.Add(AGroup.GroupBy.Parent);

        Hops.Init;

        if Hops.Valid then
        begin
          AGroup.GroupBy.Load;
          DoAddChildren(AGroup.GroupBy)
        end;
      finally
        Hops.Free;
      end;
    end;
  end;

  function IsSubTable:Boolean;
  begin
    result:=(Values.Count=0) and (APrevious<>nil) and
            (APrevious.Data.Kind=TDataKind.dkUnknown) and (not APrevious.Data.AsTable);
  end;

  procedure AddResults(const AGroup:TGroup);
  var tmpNext : Integer;
      tmpGroup : TVisualizerItem;
  begin
    if AIndex=-1 then
       tmpNext:=-1
    else
       tmpNext:=FGroups.Enabled.First(AIndex);

    if tmpNext=-1 then
    begin
      if AIndex=-1 then
         tmpGroup:=nil
      else
         tmpGroup:=FGroups[AIndex];

      if AGroup.CanAddValues then
         AGroup.AddValues(AParent,FValues,tmpGroup,ARows)
      else
      if tmpGroup<>nil then
         AddChildren(tmpGroup);
    end
    else
       AddChildren(FGroups[AIndex]);
  end;

  function CreateGroup:TGroup;
  var tmpItem : TVisualizerItem;
  begin
    if AIndex=-1 then
    begin
      if IsSubTable then
         result:=TGroupVisualizer.CreateData(nil,APrevious)
      else
         result:=ValuesGroupClass.CreateData(nil,APrevious);
    end
    else
    begin
      tmpItem:=FGroups[AIndex];

      if tmpItem.GroupClass=nil then
         result:=BestGroup(tmpItem,IndexOfEnabled(AIndex),APrevious)
      else
         result:=tmpItem.GroupClass.CreateData(tmpItem,APrevious);

      if tmpItem.Current<>nil then
         result.Assign(tmpItem.Current);
    end;
  end;

var tmpParent : TComponent;
begin
  result:=CreateGroup;

  result.FVisualizer:=Self;
  result.Index:=ARows;

  if AParent=nil then
     tmpParent:=Self
  else
     tmpParent:=AParent;

  DoAddControl(result,tmpParent);

  result.Init;
  try
    AddResults(result);
  finally
    result.Finished;
  end;
end;

procedure TBIComposer.Clear;
begin
  FGroups.Clear;
  FValues.Clear;
end;

// Determines which Data items are Groups, and which are Values
procedure TBIComposer.Guess;

  procedure AddGroup(const AData:TDataItem);
  begin
    // Pending: Verify AData is not a Master of any existing group:
    // (ie: avoid duplicate groups based on the same dimension)
    FGroups.Add(AData);
  end;

  // Try to obtain at least one measure value, removing it from Groups:
  procedure TryUseAnyGroupAsMeasure;
  var t : Integer;
  begin
    for t:=0 to FGroups.Count-1 do
    case TSummaryItem.GuessType(FGroups[t].Data) of
      Measure,
      GroupOrMeasure: begin
                        FValues.Add(FGroups[t].Data);
                        FGroups.Delete(t);
                        break;
                      end;
    end;
  end;

var tmp,
    tmpItem : TDataItem;
begin
  Clear;

  if Data<>nil then
  begin
    if (not Data.AsTable) and (Data.Kind=TDataKind.dkUnknown) then
       AddGroup(Data)
    else
    begin
      for tmp in Data.Items.AsArray do
      begin
        if tmp.AsTable then
        begin
          tmpItem:=tmp;

          repeat
            if (tmpItem.Items.Count>0) and tmpItem.Items[0].AsTable then
            begin
              tmpItem:=tmpItem.Items[0];
              AddGroup(tmpItem);

              if (tmpItem.Items.Count>0) and tmpItem.Items[0].AsTable then
                 tmpItem:=tmpItem.Items[0];
            end
            else
              tmpItem:=nil;

          until tmpItem=nil;

          FValues.Add(tmp);
        end
        else
          case TSummaryItem.GuessType(tmp) of
             GroupBy,
             GroupOrMeasure: AddGroup(tmp);
          else
             FValues.Add(tmp);
          end;
      end;

      if FValues.Count=0 then
         TryUseAnyGroupAsMeasure;
    end;
  end;
end;

function NewPanel(const AOwner:TComponent; const ACaption:String=''):TWinControl;
var tmp : {$IFDEF FMX}TLayout{$ELSE}TPanel{$ENDIF};
    {$IFDEF FMX}
    tmpText : TText;
    {$ENDIF}
begin
  {$IFDEF FMX}
  tmp:=TLayout.Create(AOwner);

  if ACaption<>'' then
  begin
    tmpText:=TText.Create(tmp);
    tmpText.Text:=ACaption;
    tmpText.Align:=TUICommon.AlignClient;
  end;

  {$ELSE}
  tmp:=TPanel.Create(AOwner);
  tmp.BevelOuter:=TPanelBevel.bvNone;
  tmp.Caption:=ACaption;
  tmp.ParentColor:=False;
  //tmp.DoubleBuffered:=True;
  {$ENDIF}

  result:=tmp;
end;

// Returns the maximum pixel size of all AItems texts
function DefaultWidth(const AItems:TStrings; const ACanvas:TCanvas):Integer;
var t : Integer;
    tmpThumb,
    tmp : Integer;
begin
  result:=24;

  {$IFDEF FMX}
  tmpThumb:=24;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  tmpThumb:=GetSystemMetrics(SM_CXHTHUMB);
  {$ELSE}
  tmpThumb:=24;
  {$ENDIF}
  {$ENDIF}

  for t:=0 to AItems.Count-1 do
  begin
    tmp:={$IFDEF FMX}Round{$ENDIF}(ACanvas.TextWidth(AItems[t]))+8+tmpThumb;

    if tmp>result then
       result:=tmp;
  end;
end;

{$IFDEF FMX}
type
  TControlHelper=class helper for TControl
  public
    function ControlCount:Integer;
  end;

  TTabControlHelper=class helper for TTabControl
  public
    function ActivePageIndex:Integer;
    function PageCount:Integer;
  end;

function TControlHelper.ControlCount:Integer;
begin
  result:=ControlsCount;
end;

function TTabControlHelper.ActivePageIndex:Integer;
begin
  result:=TabIndex;
end;

function TTabControlHelper.PageCount:Integer;
begin
  result:=TabCount;
end;
{$ENDIF}

function AddSplitter(const AControl:TWinControl;
           const AAlign:{$IFDEF FMX}TAlignLayout{$ELSE}TAlign{$ENDIF};
           const ADivisor:Integer):TSplitter;

  function MaxPos(const IsRight:Boolean):Integer;
  var t : Integer;
      tmp : {$IFDEF FMX}System.Types.{$ENDIF}TRect;
  begin
    result:=0;

    for t:=0 to AControl.ControlCount-1 do
    begin
      tmp:=AControl.Controls[t].BoundsRect{$IFDEF FMX}.Round{$ENDIF};

      if IsRight then
      begin
        if tmp.Right>result then
           result:=tmp.Right;
      end
      else
        if tmp.Bottom>result then
           result:=tmp.Bottom;
    end;
  end;

var tmpPos : Integer;
begin
  if AControl.ControlCount>0 then
  begin
    if AAlign=TUICommon.AlignLeft then
       tmpPos:=MaxPos(True)
    else
       tmpPos:=MaxPos(False)+1;
  end
  else
  begin
    if AAlign=TUICommon.AlignLeft then
       tmpPos:=({$IFDEF FMX}Round{$ENDIF}(AControl.Width) div ADivisor)+1
    else
       tmpPos:=({$IFDEF FMX}Round{$ENDIF}(AControl.Height) div ADivisor)+1;
  end;

  result:=TSplitter.Create(AControl.Owner);

  {$IFNDEF FMX}
  result.ResizeStyle:=TResizeStyle.rsUpdate;
  result.Color:=RGB($CC,$C0,$D0);
  {$ENDIF}

  result.Align:=AAlign;

  result.Parent:=AControl;

  if AAlign=TUICommon.AlignLeft then
     result.{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF}:=tmpPos
  else
  if AAlign=TUICommon.AlignTop then
     result.{$IFDEF FMX}Position.Y{$ELSE}Top{$ENDIF}:=tmpPos;
end;

{$IFDEF FMX}
type
  TLabelHelper=class helper for TLabel
  private
    function GetCaption: String;
    procedure SetCaption(const Value: String);
  public
    property Caption:String read GetCaption write SetCaption;
  end;

{ TLabelHelper }

function TLabelHelper.GetCaption: String;
begin
  result:=Text;
end;

procedure TLabelHelper.SetCaption(const Value: String);
begin
  Text:=Value;
end;
{$ENDIF}

{ TGroupControlAlign }

Constructor TGroupControlAlign.CreateData(const AItem:TVisualizerItem; const AParent:TGroup);
begin
  inherited;

  FAlign:=TGroupAlign.Left;

  // Prevent resize width:
  FixedWidth:=True;
end;

procedure TGroupControlAlign.Assign(Source: TPersistent);
begin
  if Source is TGroupControlAlign then
     ChangeAlign(TGroupControlAlign(Source).FAlign);

  inherited;
end;

function TGroupControlAlign.CalcDefaultWidth: Integer;
begin
  {$IFDEF FMX}
  result:=Round(FLabel.ParentControl.Width);
  {$ELSE}
  result:=FLabel.Parent.Width;
  {$ENDIF}
end;

procedure TGroupControlAlign.ResetWidth;
var tmp : TWinControl;
    tmpLabel,
    tmpW : Integer;
begin
  {$IFDEF FMX}
  tmp:=FLabel.ParentControl;
  {$ELSE}
  tmp:=FLabel.Parent;
  {$ENDIF}

//  if (FAlign=TGroupAlign.Left) or (FAlign=TGroupAlign.Right) then
  begin
    //if Control.ControlCount=1 then
    //   tmp.Align:=TUICommon.AlignClient
    //else
    begin
      tmpW:=CalcDefaultWidth;

      if (FLabel<>nil) and (FLabel.Canvas<>nil) then
      begin
        tmpLabel:={$IFDEF FMX}Round{$ENDIF}(FLabel.Canvas.TextWidth(FLabel.Caption))+4;

        if tmpLabel>tmpW then
           tmpW:=tmpLabel;
      end;

      tmp.Width:=tmpW;
    end;
  end;
end;

procedure TGroupControlAlign.Finished;
begin
  ResetWidth;
end;

procedure TGroupControlAlign.AlignControl(const AControl:TControl);
begin
  case FAlign of
     TGroupAlign.Left: AControl.Align:=TUICommon.AlignLeft;
      TGroupAlign.Top: AControl.Align:=TUICommon.AlignTop;
    TGroupAlign.Right: AControl.Align:=TUICommon.AlignRight;
   TGroupAlign.Bottom: AControl.Align:=TUICommon.AlignBottom;
  end;
end;

procedure TGroupControlAlign.ChangeAlign(const Value: TGroupAlign);
var tmp : TWinControl;
begin
  if FAlign<>Value then
  begin
    FAlign:=Value;

    if FLabel<>nil then
    begin
      {$IFDEF FMX}
      tmp:=FLabel.ParentControl;
      {$ELSE}
      tmp:=FLabel.Parent;
      {$ENDIF}

      if tmp<>nil then
         AlignControl(tmp);
    end;

    FixedWidth:=(FAlign=TGroupAlign.Left) or (FAlign=TGroupAlign.Right);
  end;
end;

procedure TGroupControlAlign.SetAlign(const Value: TGroupAlign);
begin
  {$IFNDEF FPC}
  Traverse<TGroupControlAlign>(procedure(const AGroup:TGroupControlAlign)
  begin
    AGroup.ChangeAlign(Value);
  end);
  {$ENDIF}
end;

{ TGroupList }

Constructor TGroupList.CreateData(const AItem:TVisualizerItem; const AParent:TGroup);
begin
  inherited;

  FList:=CreateControlClass(TListBox) as TListBox;
  TListBox(FList).{$IFNDEF FMX}OnClick{$ELSE}OnChange{$ENDIF}:=ClickedList;

  AddSplitter(Control,TUICommon.AlignLeft,2);
end;

procedure TGroupList.Init;
begin
  inherited;
  FList.Items.BeginUpdate; // <-- VCL problem, "Control" has no parent?
end;

procedure TGroupList.ChangeList(const Value: Boolean);
var tmp : TCustomListBox;
begin
  if CheckBoxes<>Value then
  begin
    if Value and (FList is TListBox) then
    begin
      {$IFDEF FMX}
      tmp:=TListBox.Create(Self);
      TListBox(tmp).ShowCheckboxes:=True;
      {$ELSE}
      tmp:=TCheckListBox.Create(Self);
      {$ENDIF}

      tmp.Align:=TUICommon.AlignClient;
      tmp.Parent:=FList.Parent;

      {$IFNDEF FMX}
      if FList.Showing then
      {$ENDIF}
      begin
        tmp.Items:=FList.Items;
        tmp.ItemIndex:=FList.ItemIndex;

        if tmp.ItemIndex<>-1 then
           {$IFDEF FMX}
           tmp.ItemByIndex(tmp.ItemIndex).IsChecked:=True;
           {$ELSE}
           TCheckListBox(tmp).Checked[tmp.ItemIndex]:=True;
           {$ENDIF}
      end;

      FList.Free;
      FList:=tmp;

      {$IFDEF FMX}
      TListBox(FList).OnChangeCheck:=CheckedList;
      {$ELSE}
      TCheckListBox(FList).OnClickCheck:=CheckedList;
      {$ENDIF}
    end
    else
    if (not Value) and CheckBoxes then
    begin
      tmp:=TListBox.Create(Self);
      tmp.Align:=TUICommon.AlignClient;
      tmp.Parent:=FList.Parent;

      {$IFNDEF FMX}
      if FList.Showing then
      {$ENDIF}
      begin
        tmp.Items:=FList.Items;
        tmp.ItemIndex:=FList.ItemIndex;
      end;

      FList.Free;
      FList:=tmp;

      TListBox(FList).{$IFNDEF FMX}OnClick{$ELSE}OnChange{$ENDIF}:=ClickedList;
    end;
  end;
end;

procedure TGroupList.SetCheckBoxes(const Value: Boolean);
begin
  {$IFNDEF FPC}
  Traverse<TGroupList>(procedure(const AGroup:TGroupList)
  begin
    AGroup.ChangeList(Value);
  end);
  {$ENDIF}
end;

procedure TGroupList.Finished;
begin
  FList.Items.EndUpdate;

  if FList.Count>0 then
  begin
    FList.ItemIndex:=0;
    ClickedList(FList);
  end;

  inherited;
end;

function TGroupList.GetCheckBoxes: Boolean;
begin
  {$IFDEF FMX}
  result:=TListBox(FList).ShowCheckboxes;
  {$ELSE}
  result:=(FList is TCheckListBox);
  {$ENDIF}
end;

procedure TGroupList.Add(const AIndex:TInteger);
begin
  inherited;
  FList.Items.Add(AsString(AIndex));
end;

procedure TGroupList.ClickedList(Sender: TObject);
begin
  ShowItem(FList.ItemIndex);
end;

procedure TGroupList.Assign(Source: TPersistent);
begin
  if Source is TGroupList then
     CheckBoxes:=TGroupList(Source).CheckBoxes;

  inherited;
end;

function TGroupList.CalcDefaultWidth:Integer;
begin
  if FList.Canvas=nil then
     result:={$IFDEF FMX}Round{$ENDIF}(FList.Width)
  else
     result:=DefaultWidth(FList.Items,FList.Canvas);
end;

procedure TGroupList.CheckedList(Sender: TObject);
begin
  // ??
end;

type
  {$IFDEF FMX}
  TTabSheet=TTabItem;
  {$ENDIF}

  // Just a helper class
  TBIPageControl=class(TPageControl)
  public
    function AddTab(const ACaption:String):TTabSheet;
  end;

function TBIPageControl.AddTab(const ACaption:String):TTabSheet;
begin
  result:=TTabSheet.Create(Self);

  {$IFDEF FMX}
  result.Text:=ACaption;
  result.Parent:=Self;
  {$ELSE}
  result.Caption:=ACaption;
  result.PageControl:=Self;
  {$ENDIF}
end;

{ TGroupPage }

Constructor TGroupPage.CreateData(const AItem:TVisualizerItem; const AParent:TGroup);
begin
  inherited;
  FAlign:=TGroupAlign.Top;

  CreateLabel(Data.Name);

  FLabel.Align:=TUICommon.AlignTop;

  FPage:=TBIPageControl.Create(Self);
  //FPage.DoubleBuffered:=True;

  FPage.OnChange:=ChangedTab;

  {$IFNDEF FMX}
  FLabel.FocusControl:=FPage;
  {$ENDIF}

  FLabel.Parent:=Control;

  FPage.Align:=TUICommon.AlignClient;
  FPage.Parent:=Control;
end;

procedure TGroupPage.Assign(Source: TPersistent);
begin
  inherited;
  ChangeAlign(FAlign);
end;

procedure TGroupPage.ChangeAlign(const Value: TGroupAlign);
var tmp : TTabPosition;
begin
  if FAlign<>Value then
  begin
    FAlign:=Value;

    // FMX: Pending replace with TGroupPageAlign enum specific to TTabControl
    case Value of
      TGroupAlign.Left: tmp:=TTabPosition.{$IFDEF FMX}{$IFDEF XE6}None{$ELSE}tpNone{$ENDIF}{$ELSE}tpLeft{$ENDIF};
       TGroupAlign.Top: tmp:=TTabPosition.{$IFDEF FMX}{$IFDEF XE6}Top{$ELSE}tpTop{$ENDIF}{$ELSE}tpTop{$ENDIF};
     TGroupAlign.Right: tmp:=TTabPosition.{$IFDEF FMX}{$IFDEF XE6}Dots{$ELSE}tpDots{$ENDIF}{$ELSE}tpRight{$ENDIF};
      else
        tmp:=TTabPosition.{$IFDEF FMX}{$IFDEF XE6}Bottom{$ELSE}tpBottom{$ENDIF}{$ELSE}tpBottom{$ENDIF};
      end;

    PageControl.TabPosition:=tmp;
  end;
end;

procedure TGroupPage.ChangedTab(Sender: TObject);
var tmp : Integer;
begin
  tmp:=FPage.ActivePageIndex;

  Position:=tmp;

  if FItems[tmp].Group=nil then
  begin
    FItems[tmp].Control:=FPage.{$IFDEF FMX}Tabs[tmp]{$ELSE}Pages[tmp]{$ENDIF};
    FItems[tmp].Group:=FVisualizer.AddGroup(Self,FItems[tmp].Next,FItems[tmp].Control,FItems[tmp].Rows);
  end;
end;

{
type
  TPageAccess=class(TPageControl);

procedure TGroupPage.Init;
begin
  inherited;
  TPageAccess(FPage).Tabs.BeginUpdate;
end;
}

procedure TGroupPage.Finished;
begin
  inherited;

//  TPageAccess(FPage).Tabs.EndUpdate;

  if FPage.PageCount>0 then
  begin
    FPage.{$IFDEF FMX}TabIndex{$ELSE}ActivePageIndex{$ENDIF}:=0;
    ChangedTab(Self);
  end;
end;

procedure TGroupPage.Add(const AIndex:TInteger);
begin
  inherited;
  TBIPageControl(FPage).AddTab(AsString(AIndex));
end;

{ TGroupMultiControl }

type
  TControlAccess=class(TWinControl);

Constructor TGroupMultiControl.CreateData(const AItem:TVisualizerItem; const AParent:TGroup);
begin
  inherited;

  // Create the "real" panel to place items:
  FScrollBox:=TScrollBox.Create(Self);

  {$IFNDEF FMX}
  FScrollBox.BorderStyle:=bsNone;

  {$IFNDEF FPC}
  FScrollBox.BevelOuter:=TBevelCut.bvNone;
  {$ENDIF}

  FScrollBox.HorzScrollBar.Tracking:=True;
  FScrollBox.VertScrollBar.Tracking:=True;
  {$ENDIF}

  FScrollBox.Align:=TUICommon.AlignClient;

  {$IFNDEF FMX}
  FScrollBox.Color:=clWhite;
  {$ENDIF}

  FScroll:=TGroupMultiScroll.Automatic;
  FColumns:=2;

  FScrollBox.OnResize:=ControlResized;

  FScrollBox.Parent:=Control;
end;

procedure TGroupMultiControl.AddExtra(const AControl: TControl);
var L : Integer;
begin
  L:=Length(IExtras);
  SetLength(IExtras,L+1);
  IExtras[L]:=AControl;
end;

procedure TGroupMultiControl.Assign(Source: TPersistent);
begin
  if Source is TGroupMultiControl then
  begin
    FColumns:=TGroupMultiControl(Source).FColumns;
    FExpandLast:=TGroupMultiControl(Source).FExpandLast;
    FScroll:=TGroupMultiControl(Source).FScroll;
  end
  else
    inherited;
end;

// Pending to evaluate:
{
type
  TSplitPanel=class(TPanel)
  private
    FSplitter : TSplitter;
  public
    procedure Add(const A,B:TControl);
    property Horizontal:Boolean read FHorizontal write FHorizontal default True;
  end;
}

function TGroupMultiControl.CanResizeWidth(const AControl:TControl):Boolean;
begin
  if AControl.Owner is TGroup then
     result:=not TGroup(AControl.Owner).FixedWidth
  else
     result:=(not (AControl is TSplitter));
end;

function TGroupMultiControl.AvailableWidth(const AControl:TWinControl; const AMax:Integer):TCoordinate;
var t : Integer;
    tmpControl : TControl;
    tmpCount : Integer;
begin
  result:=AControl.Width;

  tmpCount:=1;

  for t:=0 to AControl.ControlCount-2 do
  begin
    tmpControl:=AControl.Controls[t];

    if tmpControl.Align=TUICommon.AlignLeft then
       if CanResizeWidth(tmpControl) then
          Inc(tmpCount)
       else
          result:=result-tmpControl.Width;
  end;

  if tmpCount>AMax then
     tmpCount:=AMax;

  result:={$IFDEF FMX}result/tmpCount{$ELSE}result div tmpCount{$ENDIF};
end;

// Splits all controls in AControl even horizontally
procedure TGroupMultiControl.SplitHoriz(const AControl:TWinControl);
var t : Integer;
    tmp : Integer;
    tmpW : TCoordinate;
    tmpControl : TControl;
begin
  tmp:=AControl.ControlCount;

  // Align to Left
  for t:=0 to tmp-2 do
      AControl.Controls[t].Align:=TUICommon.AlignLeft;

  // Align last one to Client
  AControl.Controls[tmp-1].Align:=TUICommon.AlignClient;

  // Resize all but last

  tmpW:=AvailableWidth(AControl,100000);

  for t:=0 to tmp-2 do
  begin
    tmpControl:=AControl.Controls[t];

    if CanResizeWidth(tmpControl) then
       tmpControl.Width:=tmpW
    else
    if tmpControl.Owner is TGroupControlAlign then
       if TGroupControlAlign(tmpControl.Owner).FixedWidth then
          TGroupControlAlign(tmpControl.Owner).ResetWidth;
  end;

  // Add vertical splitters between them
  if Splitters then
     for t:=0 to tmp-2 do
         AddExtra(AddSplitter(AControl,TUICommon.AlignLeft,tmp));
end;

// Splits all controls in AControl even vertically
procedure TGroupMultiControl.SplitVert(const AControl:TWinControl);
var t : Integer;
    tmp : Integer;
begin
  tmp:=AControl.ControlCount;

  // Align all but last to Top
  for t:=0 to tmp-2 do
      AControl.Controls[t].Align:=TUICommon.AlignTop;

  // Align last one to Client
  AControl.Controls[tmp-1].Align:=TUICommon.AlignClient;

  // Resize all but last
  for t:=0 to tmp-2 do
      AControl.Controls[t].Height:={$IFDEF FMX}Round{$ENDIF}(AControl.Height) div tmp;

  // Add horizontal splitters between them
  if Splitters then
     for t:=0 to tmp-2 do
         AddExtra(AddSplitter(AControl,TUICommon.AlignTop,tmp));
end;

// Splits controls in APanel (without changing APanel Align or size),
// and adds TSplitter in between all them
procedure TGroupMultiControl.SubPanelControls(const APanel:TWinControl; const Columns:Integer);

  function SplitMany(const ACount,AVertical:Integer):TWinControl;
  var t : Integer;
  begin
    result:=NewPanel(APanel.Owner);

    AddExtra(result);

    if AVertical=1 then
    else
       result.Align:=TUICommon.AlignTop;

    result.Width:=APanel.Width;
    result.Height:={$IFDEF FMX}Round{$ENDIF}(APanel.Height) div AVertical;
    result.Parent:=APanel;

    for t:=0 to ACount-1 do
        APanel.Controls[0].Parent:=result;

    SplitHoriz(result);
  end;

var tmp : Integer;
begin
  tmp:=APanel.ControlCount;

  if tmp>0 then
  begin
    if tmp=1 then
       APanel.Controls[0].Align:=TUICommon.AlignClient
    else
    if tmp=2 then
       SplitHoriz(APanel)
    else
    if tmp=3 then
    begin
      APanel.Controls[2].Align:=TUICommon.AlignClient;

      if Columns=1 then
         SplitVert(APanel)
      else
      if Columns<3 then
      begin
        SplitMany(2,2) {$IFNDEF FMX}.Top:=0{$ENDIF};

        if not ExpandLast then
        begin
          APanel.Controls[0].Align:=TUICommon.AlignLeft;
          APanel.Controls[0].Width:=APanel.Controls[1].Width;
        end;

        {$IFNDEF FMX}
        APanel.Controls[1].Top:=0;
        {$ENDIF}

        if Splitters then
           AddExtra(AddSplitter(APanel,TUICommon.AlignTop,2));
      end
      else
        SplitMany(3,1){$IFNDEF FMX}.Top:=0{$ENDIF};
    end
    else
    if tmp=4 then
    begin
      if Columns=1 then
         SplitVert(APanel)
      else
      if Columns=2 then
      begin
        SplitMany(2,2);
        SplitMany(2,2);

        APanel.Controls[0].Height:={$IFDEF FMX}Round{$ENDIF}(APanel.Height) div 2;
        APanel.Controls[1].Align:=TUICommon.AlignClient;

        if Splitters then
           AddExtra(AddSplitter(APanel,TUICommon.AlignTop,2));
      end
      else
      if Columns=3 then
      begin
        SplitMany(3,2){$IFNDEF FMX}.Top:=0{$ENDIF};

        {$IFNDEF FMX}
        APanel.Controls[1].Top:=0;
        {$ENDIF}

        if Splitters then
           AddExtra(AddSplitter(APanel,TUICommon.AlignTop,2));
      end
      else
        SplitMany(4,1){$IFNDEF FMX}.Top:=0{$ENDIF};
    end
    else
      APanel.Controls[tmp-1].Align:=TUICommon.AlignClient;
  end;
end;

procedure TGroupMultiControl.SetColumns(const Value: Integer);
begin
  {$IFNDEF FPC}
  Traverse<TGroupMultiControl>(procedure(const AGroup:TGroupMultiControl)
  begin
    if AGroup.FColumns<>Value then
    begin
      AGroup.FColumns:=Value;
      AGroup.ReLayout;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupMultiControl.SetExpandLast(const Value: Boolean);
begin
  {$IFNDEF FPC}
  Traverse<TGroupMultiControl>(procedure(const AGroup:TGroupMultiControl)
  begin
    if AGroup.FExpandLast<>Value then
    begin
      AGroup.FExpandLast:=Value;
      AGroup.ReLayout;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupMultiControl.SetRowHeight(const Value: Integer);
begin
  {$IFNDEF FPC}
  Traverse<TGroupMultiControl>(procedure(const AGroup:TGroupMultiControl)
  begin
    if AGroup.FRowHeight<>Value then
    begin
      AGroup.FRowHeight:=Value;
      AGroup.ReLayout;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupMultiControl.SetScroll(const Value: TGroupMultiScroll);
begin
  {$IFNDEF FPC}
  Traverse<TGroupMultiControl>(procedure(const AGroup:TGroupMultiControl)
  begin
    if AGroup.FScroll<>Value then
    begin
      AGroup.FScroll:=Value;
      AGroup.ReLayout;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupMultiControl.SetSplitters(const Value: Boolean);
begin
  {$IFNDEF FPC}
  Traverse<TGroupMultiControl>(procedure(const AGroup:TGroupMultiControl)
  begin
    if AGroup.FSplitters<>Value then
    begin
      AGroup.FSplitters:=Value;
      AGroup.ReLayout;
    end;
  end);
  {$ENDIF}
end;

(*
procedure TGroupMultiControl.SwitchToScrollBox;
var tmpControl : TControl;
    tmpPanel : TWinControl;
    t,
    tmpCount : Integer;
    tmpIsControl : Boolean;
begin
//  tmp.DisableAlign;
//  try
    while FPanel.ControlCount>0 do
    begin
      if Columns=1 then
      begin
        tmpControl:=FPanel.Controls[0];
      end
      else
      begin
        tmpCount:=Columns;

        if FPanel.ControlCount<tmpCount then
           tmpCount:=FPanel.ControlCount;

        tmpPanel:=NewPanel(Self);
        tmpPanel.Width:=tmp.Width;

        AddExtra(tmpPanel);

        for t:=0 to tmpCount-1 do
        begin
          tmpControl:=FPanel.Controls[0];

          if t=0 then
             tmpPanel.Height:={$IFDEF FMX}Round{$ENDIF}(tmpControl.Height) div tmpCount;

          if t=tmpCount-1 then
             tmpControl.Align:=TUICommon.AlignClient
          else
          begin
            tmpControl.Align:=TUICommon.AlignLeft;
            tmpControl.Width:={$IFDEF FMX}Round{$ENDIF}(FPanel.Width) div tmpCount;
          end;

          tmpControl.Parent:=tmpPanel;

          if t<tmpCount-1 then
             if Splitters then
                AddExtra(AddSplitter(tmpPanel,TUICommon.AlignLeft,tmpCount));
        end;

        tmpControl:=tmpPanel;
      end;

      tmpControl.Align:=TUICommon.AlignTop;
      tmpControl.Width:=tmp.Width;
      tmpControl.Parent:=tmp;
    end;
{
  finally
//    tmp.EnableAlign;
  end;
}
end;
*)

procedure TGroupMultiControl.ClearExtras;
var t : Integer;
    tmp : TWinControl;
begin
  for t:=0 to High(IExtras) do
  begin
    if IExtras[t] is TWinControl then
    begin
      tmp:=TWinControl(IExtras[t]);

      while tmp.ControlCount>0 do
            tmp.Controls[0].Parent:=FScrollBox;
    end;

    IExtras[t].Free;
  end;

  IExtras:=nil;
end;

procedure TGroupMultiControl.DoReLayout;

  procedure ResizeScrollBox;
  var t : Integer;
      tmp : TControl;
  begin
    if FRowHeight<>0 then
       for t:=0 to FScrollBox.ControlCount-1 do
       begin
         tmp:=FScrollBox.Controls[t];

         if not (tmp is TSplitter) then
         begin
           tmp.Align:=TUICommon.AlignTop;
           tmp.Height:=FRowHeight;
         end;
       end;
  end;

begin
  ClearExtras;

  SubPanelControls(FScrollBox,Columns);

  FScroller:=(Scroll=TGroupMultiScroll.Yes) or
             ( (Scroll=TGroupMultiScroll.Automatic) and (Length(FItems)>(Columns*2)) );

  if FScroller then
     ResizeScrollBox
  else
     ControlResized(FScrollBox);
end;

procedure TGroupMultiControl.ReLayout;
begin
  DoReLayout;
end;

procedure TGroupMultiControl.Finished;
begin
  ReLayout;
  inherited;
end;

// Returns how many controls in AControl are not TSplitter
function NoSplitterCount(const AControl:TWinControl):Integer;
var t : Integer;
begin
  result:=0;

  if AControl<>nil then
     for t:=0 to AControl.ControlCount-1 do
         if not (AControl.Controls[t] is TSplitter) then
            Inc(result);
end;

// Realigns all controls to space them evenly
procedure TGroupMultiControl.DoResize(const AControl:TControl; const Columns:Integer);

  procedure InnerResize(const AControl:TWinControl);
  var t : Integer;
  begin
    for t:=0 to AControl.ControlCount-1 do
        DoResize(AControl.Controls[t],Columns);
  end;

var tmpS : TCoordinate;
    tmpCount : Integer;
begin
  if (AControl.Owner is TGroupControl) and
     (AControl=TGroupControl(AControl.Owner).FLabel) then
  else
  if not (AControl is TSplitter) then
  begin
    tmpCount:=NoSplitterCount(AControl.{$IFDEF FMX}ParentControl{$ELSE}Parent{$ENDIF});

    if AControl.Align=TUICommon.AlignTop then
    begin
      tmpS:={$IFDEF FMX}Round{$ENDIF}(AControl.{$IFDEF FMX}ParentControl{$ELSE}Parent{$ENDIF}.Height) div tmpCount;

      if AControl.Height<>tmpS then
         AControl.Height:=tmpS;
    end
    else
    if AControl.Align=TUICommon.AlignLeft then
    begin
      if CanResizeWidth(AControl) then
      begin
        tmpS:=AvailableWidth(AControl.{$IFDEF FMX}ParentControl{$ELSE}Parent{$ENDIF},Columns);

        if tmpS>0 then
           if AControl.Width<>tmpS then
              AControl.Width:=tmpS;
      end;
    end;

    if AControl is TWinControl then
       InnerResize(TWinControl(AControl));
  end;
end;

procedure TGroupMultiControl.ControlResized(Sender:TObject);
begin
  if not FScroller then
     DoResize(TWinControl(Sender),Columns);
end;

procedure TGroupMultiControl.Add(const AIndex:TInteger);
begin
  inherited;

  FItems[AIndex].Group:=FVisualizer.AddGroup(Self,FItems[AIndex].Next,FScrollBox,FItems[AIndex].Rows);
  FItems[AIndex].Control:=FItems[AIndex].Group.Control;
end;

{ TGroup }

procedure TGroup.Add(const AIndex: TInteger);
begin
end;

procedure TGroup.AddGroup(const AGroup:TGroup);
var L : Integer;
begin
  L:=Length(FGroups);
  SetLength(FGroups,L+1);
  FGroups[L]:=AGroup;
end;

function TGroup.AddItem(const AData:TDataItem; const ANext: Integer;
                const ARows: TCursorIndex; const APos:TInteger):TInteger;
begin
  result:=Length(FItems);

  SetLength(FItems,result+1);

  FItems[result].Position:=APos;
  FItems[result].Control:=nil;
  FItems[result].Next:=ANext;
  FItems[result].Rows:=ARows;
  FItems[result].Data:=AData;
end;

procedure TGroup.AddValues(const AComponent: TComponent;
                           const Values:TVisualizerItems;
                           const AGroup:TVisualizerItem;
                           const ARows: TCursorIndex);
begin
end;

Constructor TGroup.CreateData(const AItem:TVisualizerItem; const AParent: TGroup);
begin
  inherited Create(AParent);
  Item:=AItem;
  Parent:=AParent;

  if Item<>nil then
  begin
    if Item.GroupBy=nil then
       Data:=Item.Data
    else
       Data:=Item.GroupBy;

    if Item.Current<>nil then
       Item.Current.AddGroup(Self);
  end;
end;

class function TGroup.ClassIndexOf(const AClass:TGroupClass):Integer;
var t : Integer;
begin
  if AClass<>nil then
  begin
    for t:=0 to High(TGroupControl.GroupClasses) do
        if TGroupControl.GroupClasses[t]=AClass then
           Exit(t+1);
  end;

  result:=0;
end;

class function TGroup.BestControl(const AIndex,ATotal,AValues:Integer):TGroupClass;
begin
  result:=nil;
end;

{.$DEFINE ALIGNTEST}

procedure TGroup.Init;
begin
  {$IFDEF ALIGNTEST}
  Control.DisableAlign;
  {$ENDIF}
end;

function TGroup.AsString(const AData:TDataItem):String;
begin
  result:=TVisualizerItem.MapToString(AData,MapOf(AData),FItems[Position].Position);
end;

function TGroup.AsString(const AIndex:TInteger):String;
var tmp : TDataItem;
    tmpPos : TInteger;
begin
  tmp:=FItems[AIndex].Data;
  tmpPos:=FItems[AIndex].Position;

  if tmp.Kind=TDataKind.dkUnknown then
     result:=tmp{$IFDEF FPC}.Items{$ENDIF}[tmpPos].Name
  else
     result:=TVisualizerItem.MapToString(tmp,MapOf(tmp),tmpPos);
end;

{$IFNDEF FPC}
procedure TGroup.Traverse<T>(const AProc: TGroupProc<T>);
var tmp : TGroup;
begin
  if Self is T then
     AProc(Self);

  for tmp in FGroups do
      tmp.Traverse<T>(AProc);
end;
{$ENDIF}

procedure TGroup.Finished;
begin
  {$IFDEF ALIGNTEST}
  Control.EnableAlign;
  {$ENDIF}
end;

function TGroup.GetDataString:String;
var tmp : TDataItem;
begin
  result:='';

  tmp:=Data;

  if tmp.AsTable then
  begin
    if tmp.Items.Count>Position then
       result:=tmp.Items[Position].Name;
  end
  else
  if tmp.Count>Position then
     result:=tmp.DataToString(Position);
end;

function TGroup.MapToString:String;
begin
  if FItems=nil then
     result:=TVisualizerItem.MapToString(Data,MapOf(Data),Position)
  else
     result:=AsString(Position);
end;

{ TGroupCombo }

Constructor TGroupCombo.CreateData(const AItem:TVisualizerItem; const AParent: TGroup);
begin
  inherited;
  FAlign:=TGroupAlign.Top;
  CreateCombo;
end;

procedure TGroupCombo.CreateCombo;

  (*
  function TryMergeWithParent(var ALeft:Integer):TWinControl;
  var tmpPanel : TWinControl;
      tmpParent : TGroup;
  begin
    Control:=TGroupCombo(AParent).Control;
    result:=Control.Controls[0] as TWinControl;

    tmpParent:=AParent;

    while tmpParent<>nil do
    begin
      if tmpParent is TGroupCombo then
      begin
        tmpPanel:=TGroupCombo(tmpParent).Control.Controls[0] as TWinControl;
        Inc(ALeft,{$IFDEF FMX}Round{$ENDIF}(tmpPanel.Controls[tmpPanel.ControlCount-1].BoundsRect.Right)+8);
        tmpParent:=tmpParent.Parent;
      end
      else
        break;
    end;
  end;
  *)

var tmpPos : Integer;
begin
  FCombo:=CreateControlClass(TComboBox) as TComboBox;

  AlignControl(FCombo.{$IFDEF FMX}ParentControl{$ELSE}Parent{$ENDIF});

  FCombo.Align:=TUICommon.AlignNone;
  FLabel.Align:=TUICommon.AlignNone;

  {$IFNDEF FMX}
  FCombo.Style:=TComboBoxStyle.csDropDownList;
  {$ENDIF}

  {$IFDEF FMX}
  FCombo.Position.Y:=4;
  {$ELSE}
  FCombo.Top:=4;
  {$ENDIF}

  {$IFDEF FMX}
  FCombo.OnChange:=ClickedCombo;
  {$ELSE}
  FCombo.OnClick:=ClickedCombo;
  {$ENDIF}

  // Center Vertically:
  FLabel.{$IFDEF FMX}ParentControl{$ELSE}Parent{$ENDIF}.Height:=8+FCombo.Height;

  tmpPos:=(4+{$IFDEF FMX}Round{$ENDIF}(FCombo.Height-FLabel.Height)) div 2;

  {$IFDEF FMX}
  FLabel.{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF}:=6;
  {$ENDIF}

  FLabel.{$IFDEF FMX}Position.Y{$ELSE}Top{$ENDIF}:=tmpPos;
end;

procedure TGroupCombo.Add(const AIndex:TInteger);
begin
  inherited;
  FCombo.Items.Add(AsString(AIndex));
end;

function TGroupCombo.CalcDefaultWidth: Integer;
begin
  if FCombo.Canvas=nil then
     result:={$IFDEF FMX}Round{$ENDIF}(FCombo.Width)
  else
     result:=DefaultWidth(FCombo.Items,FCombo.Canvas);
end;

procedure TGroupCombo.ClickedCombo(Sender: TObject);
begin
  ShowItem(FCombo.ItemIndex);
end;

procedure TGroupCombo.Init;
begin
  inherited;
  FCombo.Items.BeginUpdate; // <-- VCL problem, "Control" has no parent?
end;

procedure TGroupCombo.SetComboLeft;
var tmp : TCoordinate;
begin
  if FLabel.Visible then
     tmp:=FLabel.BoundsRect.Right
  else
     tmp:=0;

  FCombo.{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF}:=tmp+{$IFDEF FMX}14{$ELSE}4{$ENDIF};
end;

procedure TGroupCombo.Finished;

  procedure ComboLabel(const AText:String);
  begin
    IComboLabel:=TLabel.Create(Self);

    {$IFDEF FMX}
    IComboLabel.Text:=AText;
    {$ELSE}
    IComboLabel.Caption:=' '+AText;
    {$ENDIF}

    {$IFDEF FMX}
    IComboLabel.WordWrap:=False;
    IComboLabel.AutoSize:=True;

    IComboLabel.Position.X:=FCombo.Position.X;
    IComboLabel.Position.Y:=(FCombo.Position.Y+(0.5*FCombo.Height))-(0.5*IComboLabel.Height);
    {$ELSE}

    IComboLabel.Left:=FCombo.Left;
    IComboLabel.Top:=Round((FCombo.Top+(0.5*FCombo.Height))-(0.5*IComboLabel.Height));
    {$ENDIF}
   

    IComboLabel.Parent:=FCombo.Parent;

    //IComboLabel.Parent.Height:=FLabel.Height+4;
  end;

begin
  inherited;

  SetComboLeft;

  FCombo.Items.EndUpdate;

  if FCombo.Items.Count<2 then
  begin
    FCombo.Visible:=False;

    if IComboLabel=nil then
    begin
      if FCombo.Items.Count>0 then
         ComboLabel(FCombo.Items[0]);
    end
    else
    begin
      if FCombo.Items.Count>0 then
         IComboLabel.Caption:=FCombo.Items[0]
      else
         IComboLabel.Caption:='';
    end;

    if IComboLabel<>nil then
       IComboLabel.Visible:=True;
  end
  else
  begin
    if IComboLabel<>nil then
       IComboLabel.Visible:=False;

    FCombo.Visible:=True;
  end;

  if FCombo.Items.Count>0 then
  begin
    if FCombo.Canvas<>nil then
       FCombo.Width:=DefaultWidth(FCombo.Items,FCombo.Canvas);

    FCombo.DropDownCount:=23;

    FCombo.ItemIndex:=0;
    ClickedCombo(FCombo);
  end
  else
    FCombo.Enabled:=False;
end;

procedure TGroupCombo.DoSetShowLabel(const Value:Boolean);
begin
  inherited;
  SetComboLeft;
end;

{ TGroupTrackbar }

Constructor TGroupTrackbar.CreateData(const AItem:TVisualizerItem; const AParent:TGroup);
begin
  inherited;
  FAlign:=TGroupAlign.Top;

  FTrack:=CreateControlClass(TTrackBar) as TTrackBar;
  FTrack.Align:=TUICommon.AlignLeft;
  FTrack.{$IFDEF FMX}ParentControl{$ELSE}Parent{$ENDIF}.Align:=TUICommon.AlignTop;

  {$IFNDEF FMX}
  FTrack.LineSize:=1;
  FTrack.PageSize:=1;
  {$ENDIF}

  FTrack.Height:=24;
  FTrack.OnChange:=ClickedTrack;

  FTrack.{$IFDEF FMX}ParentControl{$ELSE}Parent{$ENDIF}.Height:=FLabel.Height+FTrack.Height+8;
end;

const
  VerticalOrientation={$IFDEF FMX}TOrientation.{$IFDEF XE6}Vertical{$ELSE}orVertical{$ENDIF}{$ELSE}TTrackBarOrientation.trVertical{$ENDIF};
  HorizontalOrientation={$IFDEF FMX}TOrientation.{$IFDEF XE6}Horizontal{$ELSE}orHorizontal{$ENDIF}{$ELSE}TTrackBarOrientation.trHorizontal{$ENDIF};

procedure TGroupTrackbar.ChangeAlign(const Value: TGroupAlign);
begin
  inherited;

  if (Value=TGroupAlign.Left) or (Value=TGroupAlign.Right) then
  begin
    FTrack.Orientation:=VerticalOrientation;
    FTrack.Align:=TUICommon.AlignTop;
  end
  else
  begin
    FTrack.Orientation:=HorizontalOrientation;
    FTrack.Align:=TUICommon.AlignLeft;
  end;

  ResizeTrackBar;
end;

procedure TGroupTrackbar.ClickedTrack(Sender: TObject);
begin
  Position:={$IFDEF FMX}Round(FTrack.Value){$ELSE}FTrack.Position{$ENDIF};

  FLabel.Caption:=AsString(FItems[Position].Data);

  ShowItem(Position);
end;

procedure TGroupTrackbar.ResizeTrackBar;
var tmp : TCoordinate;
begin
  tmp:=FTrack.Max*8;

  if tmp<150 then
     tmp:=150;

  if FTrack.Orientation=HorizontalOrientation then
     FTrack.Width:=tmp
  else
     FTrack.Height:=tmp;
end;

procedure TGroupTrackbar.Finished;
var tmp : Integer;
begin
  tmp:=Length(FItems);

  if tmp=0 then
     FTrack.Enabled:=False
  else
  begin
    FTrack.Max:=tmp-1;

    ResizeTrackBar;

    if tmp<=100 then
       FTrack.Frequency:=1;

    FTrack.{$IFDEF FMX}Value{$ELSE}Position{$ENDIF}:=0;
    ClickedTrack(Self);
  end;

  inherited;
end;

{ TGroupControl }

Constructor TGroupControl.CreateData(const AItem: TVisualizerItem; const AParent: TGroup);
begin
  inherited;

  FShowLabel:=True;

  Control:=NewPanel(Self);

  if AParent is TGroupMultiControl then
     AddCaptionPanel;
end;

procedure TGroupControl.CheckControl(const AIndex: Integer);
begin
  if FItems[AIndex].Control=nil then
  begin
    FItems[AIndex].Group:=FVisualizer.AddGroup(Self,FItems[AIndex].Next,Control,FItems[AIndex].Rows);
    FItems[AIndex].Control:=FItems[AIndex].Group.Control;
  end;
end;

procedure TGroupControl.AddCaptionPanel;
var tmpS : String;
begin
  if (Parent=nil) or (Parent.Data=nil) then
     tmpS:=''
  else
     tmpS:=Parent.Data.Name+' '+Parent.MapToString;

  CreateLabel(tmpS);

  FLabel.Font.Size:=14;
  FLabel.Align:=TUICommon.AlignTop;

  {$IFDEF FMX}
  FLabel.TextAlign:=TTextAlign.{$IFDEF XE6}Center{$ELSE}taCenter{$ENDIF};
  {$ELSE}
  FLabel.Alignment:=TAlignment.taCenter;
  {$ENDIF}

  FLabel.Parent:=Control;
end;

procedure TGroupControl.Assign(Source:TPersistent);
begin
  if Source is TGroupControl then
     ShowLabel:=TGroupControl(Source).FShowLabel
  else
     inherited;
end;

procedure TGroupControl.DoSetShowLabel(const Value:Boolean);
begin
  if FShowLabel<>Value then
  begin
    FShowLabel:=Value;

    if FLabel<>nil then
       FLabel.Visible:=FShowLabel;
  end;
end;

procedure TGroupControl.SetShowLabel(const Value:Boolean);
begin
  {$IFNDEF FPC}
  Traverse<TGroupControl>(procedure(const AGroup:TGroupControl)
  begin
    AGroup.DoSetShowLabel(Value);
  end);
  {$ENDIF}
end;

function TGroupControl.CreateControlClass(const AClass: TWinControlClass): TWinControl;
var tmp : TWinControl;
begin
  tmp:=NewPanel(Self);

  result:=AClass.Create(Self);
  result.Align:=TUICommon.AlignClient;
  result.Parent:=tmp;

  if Data=nil then
     CreateLabel('?')
  else
     CreateLabel(Data.Name);

  FLabel.Align:=TUICommon.AlignTop;

  {$IFNDEF FMX}
  FLabel.FocusControl:=result;
  {$ENDIF}

  FLabel.Parent:=tmp;

  tmp.Align:=TUICommon.AlignLeft;
  tmp.Parent:=Control;
end;

procedure TGroupControl.CreateLabel(const AText: String);
begin
  FLabel:=TLabel.Create(Self);

  FLabel.Visible:=FShowLabel;

  {$IFDEF FMX}
  FLabel.Text:=AText;
  {$ELSE}
  FLabel.Caption:=' '+AText;
  {$ENDIF}

  {$IFDEF FMX}
  FLabel.WordWrap:=False;
  FLabel.AutoSize:=True;
  {$ELSE}
  FLabel.Font.Size:=Round(FLabel.Font.Size*1.2);
  FLabel.Font.Style:=[fsBold];

  FLabel.Height:=18;
  {$ENDIF}
end;

procedure TGroupControl.ShowItem(const AIndex: Integer);
var t : Integer;
    tmp : TComponent;
begin
  Position:=AIndex;

  CheckControl(AIndex);

  for t:=0 to High(FItems) do
  begin
    tmp:=FItems[t].Control;

    if tmp<>nil then
       TWinControl(tmp).Visible:=t=AIndex;
  end;
end;

{ TGroupGrid }

Constructor TGroupGrid.CreateData(const AItem:TVisualizerItem; const AParent: TGroup);
begin
  inherited;

  CanAddValues:=True;

  FGrid:=CreateControlClass(TBIGrid) as TBIGrid;

  {$IFNDEF FMX}
  FGrid.ReadOnly:=True;
  {$ENDIF}

  ISplitter:=AddSplitter(Control,TUICommon.AlignLeft,2);
end;

Destructor TGroupGrid.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TGroupGrid.DataChange(Sender: TObject; Field: TField);
var tmp : Integer;
begin
  tmp:=FGrid.{$IFNDEF FMX}DataSource.{$ENDIF}DataSet.RecNo-1;

  if tmp>=0 then
     ShowItem(tmp);
end;

procedure TGroupGrid.Finished;
{$IFNDEF FMX}
var tmp : TObject;
{$ENDIF}
begin
  inherited;

  FGrid.Data:=FData;

  if FData<>nil then
  begin
    if FData.AsTable then
    begin
      {$IFDEF FMX}
      if FGrid.Parent is TLayout then
         TLayout(FGrid.Parent).Align:=TUICommon.AlignClient;
      {$ENDIF}
    end
    else
    begin
      {$IFNDEF FMX}
      tmp:=FGrid.Plugin.GetObject;

      if tmp is TBIDBGrid then
         TBIDBGrid(tmp).Options:=TBIDBGrid(tmp).Options-[TDBGridOption.dgTitles];
      {$ENDIF}

      {$IFNDEF FMX}
      FGrid.DataSource.OnDataChange:=DataChange;
      {$ENDIF}

      if FData.Count>0 then
         DataChange(Self,nil);
    end;
  end;
end;

procedure TGroupGrid.Add(const AIndex:TInteger);
begin
  inherited;

  if FData=nil then
     FData:=TDataItem.Create(TDataKind.dkText);

  FData.TextData.Append(AsString(AIndex));
  Inc(TDataAccess(FData).FCount);
end;

procedure TGroupGrid.AddValues(const AComponent: TComponent;
                               const Values:TVisualizerItems;
                               const AGroup:TVisualizerItem;
                               const ARows: TCursorIndex);

  procedure AddItem(const AItem:TVisualizerItem);
  var t : TLoopInteger;
      tmp : TDataItem;
  begin
    AItem.Data.Load;

    tmp:=TDataItem.Create(AItem.Data.Kind);
    tmp.Name:=AItem.Data.Name;

    if ARows=nil then
       TDataAccess(tmp).CloneData(AItem.Data,0,AItem.Data.Count)
    else
    begin
      tmp.Resize(ARows.Count);

      for t:=0 to ARows.Count-1 do
          tmp.CopyFrom(t,AItem.Data,ARows[t]);
    end;

    FData.Items.Add(tmp);
  end;

var t : Integer;
begin
  FData.Free;
  FData:=TDataItem.Create(True);

  if (Values.Enabled.Count>0) or (AGroup<>nil) then
  begin
    if AGroup<>nil then
       AddItem(AGroup);

    {$IFDEF FMX}
    (Grid.Parent as TControl).Align:=TUICommon.AlignClient;
    (Grid.Parent as TControl).Visible:=True;
    {$ELSE}
    Grid.Parent.Align:=TUICommon.AlignClient;
    Grid.Parent.Visible:=True;
    {$ENDIF}

    ISplitter.Visible:=Parent<>nil;

    for t:=0 to Values.Count-1 do
        if Values[t].Enabled then
           AddItem(Values[t]);

    TDataAccess(FData).FCount:=FData.Items[0].Count;

    FLabel.Visible:=False;
  end
  else
  begin
    (Grid.Parent {$IFDEF FMX}as TControl{$ENDIF}).Visible:=False;
    ISplitter.Visible:=False;
  end;
end;

{ TGroupButtons }

Constructor TGroupButtons.CreateData(const AItem:TVisualizerItem; const AParent:TGroup);
begin
  inherited;
  CreateLabel(Data.Name);

  FLabel.Align:=TUICommon.AlignTop;
  FLabel.Parent:=Control;

  FPanel:=NewPanel(Self);
  FPanel.Align:=TUICommon.AlignTop;
  FPanel.Parent:=Control;
end;

procedure TGroupButtons.Add(const AIndex:TInteger);
var tmp : TSpeedButton;
    tmpS : String;
begin
  inherited;

  tmp:=TSpeedButton.Create(Self);

  tmpS:=AsString(AIndex);

  {$IFDEF FMX}
  tmp.Text:=tmpS;
  {$ELSE}
  tmp.Caption:=tmpS;
  tmp.GroupIndex:=1;
  {$ENDIF}

  tmp.Width:=FLabel.Canvas.TextWidth(tmpS)+8;

  {$IFNDEF FMX}
  tmp.AllowAllUp:=True;
  {$ENDIF}

  tmp.Tag:=FPanel.ControlCount;

  tmp.OnClick:=ClickedButton;

  if FPanel.ControlCount>0 then
     tmp.{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF}:=FPanel.Controls[FPanel.ControlCount-1].BoundsRect.Right+1;

  tmp.Align:=TUICommon.AlignLeft;

  FPanel.Height:=tmp.Height;

  tmp.Parent:=FPanel;
end;

procedure TGroupButtons.ClickedButton(Sender: TObject);
begin
  {$IFNDEF FMX}
  TSpeedButton(Sender).Down:=True;
  {$ENDIF}

  ShowItem(TSpeedButton(Sender).Tag);
end;

procedure TGroupButtons.Finished;
var tmp : TControl;
begin
  inherited;

  if FPanel.ControlCount>0 then
  begin
    tmp:=FPanel.Controls[0];
    //tmp.Align:=TUICommon.AlignClient;

    ClickedButton(tmp);
  end;
end;

{ TBIVisualizerUI }

class procedure TBIVisualizerUI.AddGroupByItems(const AItems:TStrings;
         const AData:TDataItem; const AIndex: Integer);
var tmp,
    tmpParent,
    tmpItem : TDataItem;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    AItems.Add('');

    if AIndex<>-1 then
    begin
      tmp:=AData;

      if tmp<>nil then
      begin
        if TDataAccess(tmp).HasMaster then
        begin
          tmpParent:=tmp.Master.Parent;

          if (tmpParent<>nil) and tmpParent.AsTable then
             for tmpItem in tmpParent.Items.AsArray do
                 if tmpItem<>tmp.Master then
                    AItems.AddObject(tmpItem.Name,tmpItem);
        end;
      end;
    end;
  finally
    AItems.EndUpdate;
  end;
end;

class procedure TBIVisualizerUI.AddItems(const AList: TCustomListBox;
  const AItems: TVisualizerItems);
var t : Integer;
begin
  AList.Items.BeginUpdate;
  try
    AList.Clear;

    for t:=0 to AItems.Count-1 do
    begin
      AList.Items.Add(AItems[t].Data.Name);

      {$IFDEF FMX}
      AList.ItemByIndex(t).IsChecked:=AItems[t].Enabled;
      {$ELSE}
      if AList is TCheckListBox then
         TCheckListBox(AList).Checked[t]:=AItems[t].Enabled;
      {$ENDIF}
    end;

  finally
    AList.Items.EndUpdate;
  end;
end;

class procedure TBIVisualizerUI.AddClasses(const AItems:TStrings);
var tmp : TGroupClass;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    AItems.Add('Automatic');

    for tmp in TGroupControl.GroupClasses do
        AItems.Add(Copy(tmp.ClassName,7,255));
  finally
    AItems.EndUpdate;
  end;
end;

function TBIVisualizerUI.GetControl(const AIndex:Integer):TGroupControl;
begin
  if (AIndex<>-1) and (Viz.Groups[AIndex].Current is TGroupControl) then
     result:=TGroupControl(Viz.Groups[AIndex].Current)
  else
     result:=nil;
end;

function TBIVisualizerUI.GetMulti(const AIndex:Integer):TGroupMultiControl;
begin
  if (AIndex<>-1) and (Viz.Groups[AIndex].Current is TGroupMultiControl) then
     result:=TGroupMultiControl(Viz.Groups[AIndex].Current)
  else
     result:=nil;
end;

function TBIVisualizerUI.GetList(const AIndex:Integer):TGroupList;
begin
  if (AIndex<>-1) and (Viz.Groups[AIndex].Current is TGroupList) then
     result:=TGroupList(Viz.Groups[AIndex].Current)
  else
     result:=nil;
end;

function TBIVisualizerUI.ChangeClass(const AGroup,AClass:Integer):Boolean;
var tmp : Integer;
    tmpNew : TGroupClass;
begin
  result:=False;

  tmp:=AGroup;

  if tmp<>-1 then
  begin
    if AClass=0 then
       tmpNew:=nil
    else
       tmpNew:=TGroupControl.GroupClasses[AClass-1];

    if Viz.Groups[tmp].GroupClass<>tmpNew then
    begin
      Viz.Groups[tmp].GroupClass:=tmpNew;
      Viz.CreateGroups;

      Viz.AddItems;
      result:=True;
    end;
  end;
end;

{ TGroupTree }

procedure TGroupTree.Add(const AIndex:TInteger);
{$IFDEF FMX}
var tmp : TTreeViewItem;
{$ENDIF}
begin
  inherited;

  {$IFDEF FMX}
  tmp:=TTreeViewItem.Create(Self);
  tmp.Text:=AsString(AIndex);
  tmp.Data:=Self;

  if IParent=nil then
     tmp.Parent:=FTree
  else
     IParent.AddObject(tmp);

  {$ELSE}
  FTree.Items.AddChildObject(IParent,AsString(AIndex),Pointer(Self));
  {$ENDIF}
end;

procedure TGroupTree.ChangedNode(Sender: TObject{$IFNDEF FMX}; Node: TTreeNode{$ENDIF});
var tmp : TGroupTree;
    {$IFDEF FMX}
    Node : TTreeViewItem;
    {$ENDIF}
begin
  {$IFDEF FMX}
  Node:=FTree.Selected;
  {$ENDIF}

  if (Node<>nil) and (Node.Count=0) then
  begin
    tmp:=TGroupTree(Node.Data{$IFDEF FMX}.AsObject{$ENDIF});
    tmp.ShowItem(Node.Index);
  end;
end;

Constructor TGroupTree.CreateData(const AItem:TVisualizerItem; const AParent: TGroup);

  function GetRootNode(const AIndex:Integer):TTreeNode;
  {$IFDEF FMX}
  begin
    result:=FTree.ItemByIndex(AIndex);
  end;
  {$ELSE}
  var t,tmp : Integer;
  begin
    result:=nil;

    tmp:=-1;

    for t:=0 to FTree.Items.Count-1 do
        if FTree.Items[t].Level=0 then
        begin
          Inc(tmp);

          if tmp=AIndex then
             Exit(FTree.Items[t]);
        end;
  end;
  {$ENDIF}

var tmp : TTreeNode;
begin
  inherited;

  if AParent is TGroupTree then
  begin
    FTree:=TGroupTree(AParent).TreeView;

    tmp:=TGroupTree(AParent).IParent;

    if tmp=nil then
       IParent:=GetRootNode(AParent.Position)
    else
       IParent:=tmp.{$IFDEF FMX}Items{$ELSE}{$IFDEF FPC}Items{$ELSE}Item{$ENDIF}{$ENDIF}[AParent.Position];

    Control:=TGroupTree(AParent).Control;
    FLabel:=TGroupTree(AParent).FLabel;
  end
  else
  begin
    IParent:=nil;

    FTree:=CreateControlClass(TTreeView) as TTreeView;
    FTree.OnChange:=ChangedNode;

    {$IFNDEF FMX}
    FTree.HideSelection:=False;
    FTree.ReadOnly:=True;
    {$ENDIF}

    AddSplitter(Control,TUICommon.AlignLeft,2);
  end;
end;

procedure TGroupTree.Finished;
begin
  inherited;

  FTree.{$IFNDEF FMX}Items.{$ENDIF}EndUpdate;

  if FTree.{$IFNDEF FMX}Items.{$ENDIF}Count>0 then
     FTree.Items[0].{$IFDEF FMX}IsSelected{$ELSE}Selected{$ENDIF}:=True;
end;

procedure TGroupTree.Init;
begin
  inherited;
  FTree.{$IFNDEF FMX}Items.{$ENDIF}BeginUpdate;
end;

{ TGroupVisualizer }

Constructor TGroupVisualizer.CreateData(const AItem: TVisualizerItem;
  const AParent: TGroup);
begin
  inherited;
  FVisualizer:=TBIComposer.Create(Self);
  Control:=FVisualizer;
end;

Destructor TGroupVisualizer.Destroy;
begin
  FVisualizer.Free;
  inherited;
end;

procedure TGroupVisualizer.Init;
var tmp : TDataItem;
begin
  inherited;
  tmp:=Parent.FItems[Parent.Position].Data;

  if tmp.Kind=TDataKind.dkUnknown then
     if not tmp.AsTable then
        tmp:=tmp{$IFDEF FPC}.Items{$ENDIF}[Parent.Position];

  FVisualizer.Data:=tmp;
end;

{ TGroupClassesHelper }

procedure TGroupClassesHelper.Add(const Value:TGroupClass);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);
  Self[L]:=Value;
end;

initialization
  // Register all Group classes available:
  SetLength(TGroup.GroupClasses,8);

  TGroup.GroupClasses[0]:=TGroupList;
  TGroup.GroupClasses[1]:=TGroupCombo;
  TGroup.GroupClasses[2]:=TGroupPage;
  TGroup.GroupClasses[3]:=TGroupTrackbar;
  TGroup.GroupClasses[4]:=TGroupMultiControl;
  TGroup.GroupClasses[5]:=TGroupGrid;
  TGroup.GroupClasses[6]:=TGroupButtons;
  TGroup.GroupClasses[7]:=TGroupTree;

  TBIComposer.ValuesGroupClass:=TGroupGrid;

finalization
  TGroup.GroupClasses:=nil;
end.
