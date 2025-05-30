{*********************************************}
{  TeeBI Software Library                     }
{  Importing data from VCL Controls           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Component;
{$DEFINE FMX}

interface

{
  This unit contains a TControlImporter component for VCL and FMX.

  Its purpose is to obtain data from any supported TControl.

  Like for example, using Memo.Lines text to import (in JSON, CSV, XML format),
  or using the Data property of a BI control (Grid, Chart, Tree, etc).

  Usage example:

   var tmp : TControlImporter;
   tmp:=TControlImporter.Create(Self);
   tmp.Source:=Memo1;

   BIGrid1.Data:=tmp.Data;

  Simpler usage:

   BIGrid1.Data:=TControlImporter.From(Self,Memo1);


  Supported TControl classes (and derived classes):

  VCL and Firemonkey:

  - TCustomEdit
  - TCustomGrid
  - TCustomTreeView
  - TCustomListView
  - TBIDataControl ( TBIGrid, TBIChart, TBITree, etc )
  - TDataProvider ( many TeeBI classes: TBIQuery etc etc )

  Firemonkey only:

  - TCustomComboEdit
  - TPopupBox
  - TPopupColumn
  - TCustomListBox
  - TCustomComboBox
  - TCustomMemo

  When a control is not supported, TControlImporter calls its parent class
  to try to obtain data from it.

  See BI.Store.Component unit :  TComponentImporter class

}

uses
  System.Classes, BI.DataItem, BI.Store.Component;

type
  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(TeeAllComponentPlatformIDs)]
  {$ENDIF}
  {$ENDIF}
  TControlImporter=class(TComponentImporter)
  protected
    function DoImport(const AComponent: TComponent):TDataItem; override;
  public
    class function DataOf(const AComponent:TComponent):TDataItem; override;
    class function HasDataProperty(const AComponent:TComponent):Boolean; static;
    class function HasTextProperty(const AComponent:TComponent):Boolean; static;
    class function StringsOf(const ASource:TComponent):TStrings; override;
    class function Supports(const AComponent:TComponent):Boolean; override;
    class function TextOf(const AComponent:TComponent):String; overload; static;
  end;

implementation

uses
  System.SysUtils,
  BI.DataSource, BI.Persist,
  {$IFDEF FMX}
  FMX.Controls, FMX.StdCtrls, FMXBI.DataControl,
  FMX.Memo, FMX.ListBox, FMX.ComboEdit, FMX.ExtCtrls, FMX.Grid,
  {$ELSE}
  Vcl.Controls, Vcl.StdCtrls, VCLBI.DataControl, Vcl.Grids, Vcl.ComCtrls,
  {$ENDIF}

  {$IFNDEF FPC}
  BI.Excel,
  {$ENDIF}

  BI.JSON, BI.XMLData, BI.CSV;

class function TControlImporter.DataOf(const AComponent: TComponent): TDataItem;
begin
  if AComponent is TBIDataControl then
     result:=TBIDataControl(AComponent).Data
  else
     result:=inherited;
end;

{$IFNDEF FMX}

type
  TGridAccess=class(TCustomGrid);

function DataFrom(const AGrid:TCustomGrid):TDataItem; overload;
var t,
    tt : Integer;
    tmp : TDataItem;
begin
  result:=TDataItem.Create(True);

  for t:=0 to TGridAccess(AGrid).ColCount-1 do
      result.Items.Add(IntToStr(t),TDataKind.dkText);

  result.Resize(TGridAccess(AGrid).RowCount);

  for t:=0 to TGridAccess(AGrid).ColCount-1 do
  begin
    tmp:=result.Items[t];

    for tt:=0 to TGridAccess(AGrid).RowCount-1 do
        tmp.TextData[tt]:=TGridAccess(AGrid).GetEditText(t,tt);
  end;
end;

type
  TTreeAccess=class(TCustomTreeView);

// Convert TCustomTreeView nodes into a TDataItem
function DataFrom(const ATree:TCustomTreeView):TDataItem; overload;

  procedure AddNodes(const AParent:TDataItem; const ANode:TTreeNode);
  var tmp : TDataItem;
      t : Integer;
  begin
    tmp:=AParent.Items.Add(ANode.Text,TDataKind.dkUnknown);

    for t:=0 to ANode.Count-1 do
        AddNodes(tmp,ANode[t]);
  end;

var tmp : TTreeNode;
begin
  result:=TDataItem.Create;

  tmp:=TTreeAccess(ATree).Items.GetFirstNode;

  while tmp<>nil do
  begin
    if tmp.Parent=nil then
       AddNodes(result,tmp);

    tmp:=tmp.GetNext;
  end;
end;

type
  TListAccess=class(TCustomListView);

// Convert TCustomListView items into a TDataItem
function DataFrom(const AList:TCustomListView):TDataItem; overload;

  procedure AddColumns(const AData:TDataItem);
  var t,
      tmp : Integer;
  begin
    tmp:=TListAccess(AList).Columns.Count;

    for t:=0 to tmp-1 do
        AData.Items.Add(TListAccess(AList).Columns[t].Caption,TDataKind.dkText);
  end;

  procedure AddItems(const AData:TDataItem);
  var t,
      tt : Integer;
      tmpCount : Integer;
      tmpItem : TListItem;
  begin
    AData.Resize(AList.Items.Count);

    tmpCount:=TListAccess(AList).Columns.Count;

    if tmpCount>0 then
       for t:=0 to AList.Items.Count-1 do
       begin
         tmpItem:=AList.Items[t];

         AData.Items[0].TextData[t]:=tmpItem.Caption;

         for tt:=1 to tmpCount-1 do
             AData.Items[tt].TextData[t]:=tmpItem.SubItems[tt-1];
       end;
  end;

begin
  result:=TDataItem.Create(True);
  AddColumns(result);
  AddItems(result);
end;
{$ENDIF}

function TControlImporter.DoImport(const AComponent: TComponent):TDataItem;
begin
  if AComponent is TBIDataControl then
     result:=TDataClone.Clone(TBIDataControl(AComponent).Data)
  else
  {$IFNDEF FMX}
  if AComponent is TCustomGrid then
     result:=DataFrom(TCustomGrid(AComponent))
  else
  if AComponent is TCustomTreeView then
     result:=DataFrom(TCustomTreeView(AComponent))
  else
  if AComponent is TCustomListView then
     result:=DataFrom(TCustomListView(AComponent))
  else
  {$ENDIF}
  if HasTextProperty(AComponent) then
     result:=GuessFrom(TextOf(AComponent))
  else
     result:=inherited;
end;

class function TControlImporter.HasDataProperty(const AComponent: TComponent): Boolean;
begin
  result:=(AComponent is TBIDataControl) or (AComponent is TDataProvider);
end;

class function TControlImporter.HasTextProperty(const AComponent: TComponent): Boolean;
begin
  result:=(AComponent is TCustomEdit);
end;

{$IFDEF FMX}
{$DEFINE CUSTOMBOX}
{$ELSE}
{$IFDEF FPC}
{$ENDIF}
{$DEFINE CUSTOMBOX}
{$ENDIF}

class function TControlImporter.StringsOf(const ASource:TComponent):TStrings;
begin
  if ASource is TCustomMemo then
     result:=TCustomMemo(ASource).Lines
  else
  if ASource is TCustomListBox then
     result:=TCustomListBox(ASource).Items
  else
  if ASource is {$IFDEF CUSTOMBOX}TCustomComboBox{$ELSE}TCustomCombo{$ENDIF} then
     result:={$IFDEF CUSTOMBOX}TCustomComboBox{$ELSE}TCustomCombo{$ENDIF}(ASource).Items
  else
    // TCustomRadioGroup
    // TCustomRibbonComboBox
    // TCustomRichEdit
    // TDdeClientItem
    // TCustomOutline

     result:=inherited;
end;

class function TControlImporter.Supports(const AComponent: TComponent): Boolean;
begin
  result:=HasDataProperty(AComponent) or
          (StringsOf(AComponent)<>nil) or
          HasTextProperty(AComponent) or

          {$IFDEF FMX}
          (AComponent is TCustomComboEdit) or
          (AComponent is TPopupBox) or
          (AComponent is TPopupColumn) or
          (AComponent is TCustomListBox) or
          (AComponent is TCustomComboBox) or
          (AComponent is TCustomMemo)
          {$ELSE}
          (AComponent is TCustomGrid) or
          (AComponent is TCustomTreeView) or
          (AComponent is TCustomListView)
          {$ENDIF}
          ;
end;

class function TControlImporter.TextOf(const AComponent: TComponent): String;
begin
  if AComponent is TCustomEdit then
     result:=TCustomEdit(AComponent).Text
  else
     result:='';
end;

initialization
  TComponentImporter.Plugins.Add(TControlImporter);
finalization
  TComponentImporter.Plugins.Remove(TControlImporter);
end.
