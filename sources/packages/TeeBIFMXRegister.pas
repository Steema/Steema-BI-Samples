{*********************************************}
{  TeeBI Software Library                     }
{  Firemonkey Components Design-time          }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit TeeBIFMXRegister;

interface

procedure Register;

implementation

{$IFNDEF DONOTUSETEECHART}
{$DEFINE USETEECHART}
{$ENDIF}

uses
  System.Classes, System.SysUtils,
  DesignIntf, DesignEditors, PropertyCategories,
  BI.DataItem,

  FMXBI.Grid, FMXBI.Tree,

  {$IFDEF USETEECHART}
  FMXBI.Chart, FMXBI.Visualizer.Chart, FMXBI.Chart.Source,
  {$ENDIF}

  FMXBI.Visualizer,
  // DEPRECATED: FMXBI.Dashboard,
  FMXBI.DataManager,
  BI.DataSet, FMXBI.Editor.Grid, TeeBIFMXAbout,
  BI.Languages.English;

type
  TBIComponentEditor=class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index:Integer); override;
    function GetVerbCount:Integer; override;
    function GetVerb(Index:Integer):String; override;
  end;

  TBIGridCompEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;

{ TBIComponentEditor }

procedure TBIComponentEditor.Edit;
begin // default, do nothing
end;

procedure TBIComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index=GetVerbCount-1 then
     TAboutBI.Show(nil)
  else
  if Index=GetVerbCount-2 then
     Edit
  else
     inherited;
end;

function TBIComponentEditor.GetVerb(Index: Integer): String;
begin
  if Index=GetVerbCount-1 then
     result:=BIMsg_About
  else
  if Index=GetVerbCount-2 then
     result:=BIMsg_Edit
  else
     result:=inherited;
end;

function TBIComponentEditor.GetVerbCount: Integer;
begin
  result:=inherited+2;
end;

{ TBIGridCompEditor }

procedure TBIGridCompEditor.Edit;
begin
  if TBIGridEditor.Edit(nil,TBIGrid(Component)) then
     Designer.Modified;
end;

procedure Register;
begin
  RegisterComponents('TeeBI',[TBIGrid,
                              {$IFDEF USETEECHART}
                              TBIChart,
                              {$ENDIF}
                              TBITree,
                              TBIComposer
                              // DEPRECATED: TBIVisual
                              ]);

  RegisterComponentEditor(TBIGrid,TBIGridCompEditor);

  {$IFDEF USETEECHART}
  RegisterComponentEditor(TBIChart,TBIComponentEditor);
  {$ENDIF}

  RegisterComponentEditor(TBITree,TBIComponentEditor);
  RegisterComponentEditor(TBIComposer,TBIComponentEditor);
  // DEPRECATED: RegisterComponentEditor(TBIVisual,TBIComponentEditor);
end;

end.
