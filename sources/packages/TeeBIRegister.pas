{*********************************************}
{  TeeBI Software Library                     }
{  Design-time Registration                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit TeeBIRegister;

interface

procedure Register;

implementation

{.$DEFINE NOFIREDAC} // <-- do not modify, automatic recognition at TeeBIRecompile

{$IFDEF FPC}
{$DEFINE NOFIREDAC}
{$DEFINE DONOTUSETEECHART}
{$ENDIF}

{$IFNDEF DONOTUSETEECHART}
{$DEFINE USETEECHART}
{$ENDIF}

uses
  Windows, Classes, SysUtils, Dialogs,

  {$IFDEF FPC}
  LCLIntf, PropEdits, ComponentEditors, 
  {$ELSE}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ENDIF}

  TypInfo, Forms, ActnList, Menus,
  BI.DataItem, BI.DB.DataSet, BI.Expression.Filter,

  // Editors
  VCLBI.DataManager, VCLBI.Grid, VCLBI.Menus,
  VCLBI.Grid.DBGrid, VCLBI.Editor.DataSet, VCLBI.Editor.BIGrid,
  VCLBI.Tree, VCLBI.Editor.Query, VCLBI.DataSelect,

  // Deprecated: VCLBI.Editor.Workflow,

  VCLBI.Editor.DataComponent,
  VCLBI.Editor.Data, VCLBI.Editor.DynamicFilter,
  VCLBI.Editor.Sort,

  {$IFDEF USETEECHART}
  VCLBI.Chart,
  VCLBI.Visualizer.Chart,
  VCLBI.Editor.Visualizer.Chart,
  VCLBI.Editor.Chart,
  // DEPRECATED VCLBI.ChartFunctions,
  // DEPRECATED VCLBI.Dashboard.Chart,
  VCLBI.Chart.Source,
  {$ENDIF}

  VCLBI.Visualizer, VCLBI.Editor.Visualizer,

  // DEPRECATED BI.Dashboard, BI.Dashboard.Loader, BI.Dashboard.Layouts,
  // DEPRECATED VCLBI.Dashboard,

  // DEPRECATED VCLBI.Editor.Template,

  {$IFNDEF FPC}
  TeeBIDebugVisualizer, ToolsAPI,
  // DEPRECATED TeeBIRegisterCreator,
  TeeBIRegisterMenu,
  TeeBIRegisterAbout, 
  {$ENDIF}

  TeeBIAbout,

  // Importers:

  BI.DB, BI.CSV, BI.JSON,

  {$IFNDEF NOFIREDAC}
  {$IF CompilerVersion>26}
  BI.DB.Fire,
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF FPC}
  BI.Excel, BI.Rtti,
  {$ENDIF}

  BI.XMLData, BI.HTML,

  //BI.DB.BDE, <-- obsolete
  //BI.DB.ClientDataSet, <-- Midas ?

  // Providers
  BI.Store.Component, VCLBI.Component, BI.Summary, BI.Summary.Totals,
  BI.DataSource, BI.Web, BI.Persist,

  // Components
  BI.DataSet, BI.Query,

  // Constants
  BI.Languages.English;

type
  TBIClassProperty=class(TClassProperty)
  protected
    {$IFDEF LCL}
    function Designer:TBIClassProperty;
    {$ENDIF}
    function GetObject:Integer;
  public
    function GetAttributes : TPropertyAttributes; override;
    function GetValue: String; override;
  end;

  TBIClassSubProperties=class(TBIClassProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
  end;

  TBIDataProperty=class(TBIClassProperty)
  public
    procedure Edit; override;
  end;

  TBIFilterProperty=class(TBIClassProperty)
  public
    procedure Edit; override;
  end;

  TBISortByProperty=class(TBIClassProperty)
  public
    procedure Edit; override;
  end;

  TBIComponentEditor=class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index:Integer); override;
    function GetVerbCount:Integer; override;
    function GetVerb(Index:Integer):String; override;
  end;

  TBIDataSetCompEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;

  TBIQueryCompEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;

  TBIGridCompEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;

  TBITreeCompEditor=class(TBIComponentEditor);

  {$IFDEF USETEECHART}
  TBIChartCompEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;
  {$ENDIF}

  TBIComposerEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;

  {
  TBIWorkflowCompEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;
  }

  TBIDataDefinitionEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;

  TBIDataDefinitionProperty=class(TBIClassProperty)
  public
    procedure Edit; override;
  end;

  TBIControlImporterEditor=class(TBIComponentEditor)
  public
    procedure Edit; override;
  end;

  // DEPRECATED:
  {
  TDashboardProperty=class(TClassProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    function GetValue:String; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TBIVisualEditor=class(TBIComponentEditor)
  public
    procedure ExecuteVerb(Index:Integer); override;
    function GetVerbCount:Integer; override;
    function GetVerb(Index:Integer):String; override;

    procedure Edit; override;
    procedure Load;
  end;
  }

{ TBIClassProperty }

function TBIClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{$IFDEF LCL}
function TBIClassProperty.Designer:TBIClassProperty;
begin
  result:=Self;
end;
{$ENDIF}

function TBIClassProperty.GetObject: Integer;
begin
  result:=GetOrdValue;
end;

function TBIClassProperty.GetValue: String;
var tmp : TObject;
begin
  if GetObject=0 then
     FmtStr(Result, '(%s)', [GetPropType^.Name]) // Do not localize
  else
  begin
    tmp:=TObject(GetObject);

    if tmp is TDataItem then
       result:=TDataItem(tmp).Name
    else
       result:='';

    if result='' then
       result:='(unnamed)';
  end;
end;

{ TBIDataProperty }

procedure TBIDataProperty.Edit;

  function TryCollectionItem(const AValue:TPersistent):TComponent;
  var tmp : TCollectionItem;
  begin
    result:=nil;

    if AValue is TCollectionItem then
    begin
      tmp:=TCollectionItem(AValue);

      if (tmp.Collection<>nil) and (tmp.Collection.Owner is TComponent) then
         result:=TComponent(tmp.Collection.Owner)
    end;
  end;

var tmp : TDataItem;
    tmpComp : TPersistent;
    tmpEdited : TComponent;
begin
  tmpComp:=GetComponent(0);

  if tmpComp is TComponent then
     tmpEdited:=TComponent(tmpComp)
  else
     tmpEdited:=TryCollectionItem(tmpComp);

  tmp:=TDataItem({$IFNDEF FPC}System.TypInfo.{$ENDIF}GetOrdProp(tmpComp,'Data'));

  if TDataSelector.Choose(nil,tmpEdited,tmp) then
  begin
    {$IFNDEF FPC}System.TypInfo.{$ENDIF}SetOrdProp(tmpComp,'Data',NativeInt(tmp));
    Designer.Modified;
  end;
end;

{ TBIFilterProperty }

procedure TBIFilterProperty.Edit;
var tmp : TPersistent;
begin
  tmp:=GetComponent(0);

  if tmp is TBIFilter then
     if TDynamicFilterEditor.Edit(nil,TBIFilter(tmp)) then
        Designer.Modified;
end;

procedure TBISortByProperty.Edit;
var tmp : TQuerySort;
    tmpComp : TPersistent;
begin
  tmpComp:=GetComponent(0);
  tmp:=TQuerySort({$IFNDEF FPC}System.TypInfo.{$ENDIF}GetOrdProp(tmpComp,'SortBy'));

  TSortEditor.Edit(nil,tmp);
  Designer.Modified;
end;

procedure Register;
const
  TeeBI_Palette='TeeBI';

begin
  RegisterComponents(TeeBI_Palette,[
       TBIDataSet,
       TBIQuery,
       TBIGrid,
       {$IFDEF USETEECHART}
       TBIChart,
       {$ENDIF}
       TBITree,
       TBIComposer
       // Deprecated: ,TBIVisual
       // Deprecated: ,TBIWorkflow
  ]);

  RegisterComponents(TeeBI_Palette,[
       TControlImporter,
       TDataDefinition
  ]);

  RegisterComponentEditor(TBIDataSet,TBIDataSetCompEditor);
  RegisterComponentEditor(TBIQuery,TBIQueryCompEditor);
  RegisterComponentEditor(TBIGrid,TBIGridCompEditor);

  {$IFDEF USETEECHART}
  RegisterComponentEditor(TBIChart,TBIChartCompEditor);
  {$ENDIF}

  RegisterComponentEditor(TBITree,TBITreeCompEditor);

  // Deprecated: RegisterComponentEditor(TBIWorkflow,TBIWorkflowCompEditor);

  RegisterComponentEditor(TBIComposer,TBIComposerEditor);
  RegisterComponentEditor(TDataDefinition,TBIDataDefinitionEditor);
  RegisterComponentEditor(TControlImporter,TBIControlImporterEditor);

  RegisterPropertyEditor(TypeInfo(TDataItem), nil, '',TBIDataProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TDataDefinition, 'Strings',TBIDataDefinitionProperty);

  RegisterPropertyEditor(TypeInfo(TFilterItems), nil, '',TBIFilterProperty);

  RegisterPropertyEditor(TypeInfo(TQuerySort), nil, '',TBISortByProperty);

  // DEPRECATED:
  {
  // Dashboards
  RegisterComponentEditor(TBIVisual,TBIVisualEditor);
  RegisterPropertyEditor(TypeInfo(TDashboard), nil, '',TDashboardProperty);
  }

  {$IFNDEF FPC}
  RegisterVisualizer;
  RegisterSplashScreen;
  RegisterAboutBox;
  RegisterDataManagerMenu;
  {$ENDIF}
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
  TBIGridEditor.Edit(nil,TBIGrid(Component));
  Designer.Modified;
end;

{$IFDEF USETEECHART}
{ TBIChartCompEditor }

procedure TBIChartCompEditor.Edit;
begin
  TBIChartEditor.Edit(nil,TBIChart(Component));
  Designer.Modified;
end;
{$ENDIF}

{ TBIComposerEditor }

procedure TBIComposerEditor.Edit;
begin
  TVisualizerEditor.Edit(nil,TBIComposer(Component));
  Designer.Modified;
end;

{ TBIWorkflowCompEditor }

{
procedure TBIWorkflowCompEditor.Edit;
begin
  TBIWorkflowEditor.Edit(nil,TBIWorkflow(Component));
  Designer.Modified;
end;
}

{ TBIDataSetCompEditor }

procedure TBIDataSetCompEditor.Edit;
begin
  if TBIDataSetEditor.Edit(nil,TBIDataSet(Component)) then
     Designer.Modified;
end;

{ TBIQueryCompEditor }

procedure TBIQueryCompEditor.Edit;
begin
  if TBIQueryEditor.Edit(nil,TBIQuery(Component)) then
     Designer.Modified;
end;

{ TBIDataDefinitionEditor }

procedure TBIDataDefinitionEditor.Edit;
var tmp : TDataDefinition;
begin
  tmp:=TDataDefinition(Component);

  if TDataEditor.Edit(nil,tmp) then
  begin
//    tmp.Data.UnloadData;
    Designer.Modified;
  end;
end;

{ TBIDataDefinitionProperty }

procedure TBIDataDefinitionProperty.Edit;
var tmp : TDataDefinition;
begin
  tmp:=TDataDefinition(GetComponent(0));

  if TDataEditor.Edit(nil,tmp) then
  begin
    tmp.Data.UnloadData;
    Designer.Modified;
  end;
end;

{ TBIControlImporterEditor }

procedure TBIControlImporterEditor.Edit;
var tmp : TControlImporter;
    tmpNew : TComponent;
begin
  tmp:=TControlImporter(Component);

  tmpNew:=TDataComponent.Choose(nil,tmp);

  if tmpNew<>nil then
  begin
    tmp.Source:=tmpNew;
    Designer.Modified;
  end;
end;

{$IFNDEF FPC}
type
  TBIDesignSources=class
  public
    FDesigner:IDesigner;
    Proc:TDataComponent.TBIAddComponent;

    procedure Add(const AName:String);
  end;

procedure TBIDesignSources.Add(const AName:String);
var tmp : TComponent;
begin
  if (AName<>'') and Assigned(Proc) then
  begin
    tmp:=FDesigner.GetComponent(AName);

    if tmp<>nil then
       Proc(nil,tmp,AName);
  end;
end;

procedure BIDesignTimeOnGetDesignerNames(AProc:TDataComponent.TBIAddComponent; AOwner:TComponent);
var tmpForm : TCustomForm;
    tmpDesigner : IDesigner;
    tmp : TBIDesignSources;
begin
  if AOwner is TCustomForm then
     tmpForm:=TCustomForm(AOwner)
  else
  if AOwner.Owner is TCustomForm then
     tmpForm:=TCustomForm(AOwner.Owner)
  else
     tmpForm:=nil;

  if Assigned(tmpForm) and Assigned(tmpForm.Designer) then
  begin
    tmpDesigner:=tmpForm.Designer as IDesigner;

    tmp:=TBIDesignSources.Create;
    try
      tmp.Proc:=AProc;
      tmp.FDesigner:=tmpDesigner;

      tmpDesigner.GetComponentNames(GetTypeData(TComponent.ClassInfo),tmp.Add);
    finally
      tmp.Free;
    end;
  end;
end;
{$ENDIF}

{ TBIClassSubProperties }

function TBIClassSubProperties.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties,paDialog];
end;

// DEPRECATED:

(*
{ TBIVisualEditor }

procedure TBIVisualEditor.Edit;
var tmp : TBIVisual;
begin
  tmp:=TBIVisual(Component);

  TTemplateEditor.Edit(nil,tmp.Template);

  // Force always: (no Ok/Cancel yet at Template editor)
  Designer.Modified;
end;

procedure TBIVisualEditor.ExecuteVerb(Index: Integer);
begin
  if Index=GetVerbCount-3 then
     Load
  else
     inherited;
end;

function TBIVisualEditor.GetVerb(Index: Integer): String;
begin
  if Index=GetVerbCount-3 then
     result:='Load...'
  else
     result:=inherited;
end;

function TBIVisualEditor.GetVerbCount: Integer;
begin
  result:=inherited GetVerbCount+1;
end;

procedure TBIVisualEditor.Load;
var tmp : TOpenDialog;
begin
  tmp:=TOpenDialog.Create(nil);
  try
    tmp.Title:=BIMsg_LoadTemplate;
    tmp.DefaultExt:='.json';
    tmp.Options:=tmp.Options+[ofFileMustExist];

    if tmp.Execute then
       TBIVisual(Component).Template:=TTemplateLoader.FromJSONFile(tmp.FileName);
  finally
    tmp.Free;
  end;
end;

{ TDashboardProperty }

function TDashboardProperty.GetAttributes: TPropertyAttributes;
begin
  result:=inherited GetAttributes+
            [paValueList,paSortList,paVolatileSubProperties]-
            [paSubProperties,paReadOnly,paMultiSelect];
end;

function TDashboardProperty.GetValue: String;
var tmp : TBIVisual;
begin
  tmp:=TBIVisual(GetComponent(0));

  if tmp.Dashboard=nil then
     result:=''
  else
     result:=tmp.Dashboard.Name;
end;

procedure TDashboardProperty.GetValues(Proc: TGetStrProc);
var tmp : TBIVisual;
    t : Integer;
begin
  inherited;

  tmp:=TBIVisual(GetComponent(0));

  for t:=0 to tmp.Dashboards.Count-1 do
      Proc(tmp.Dashboards[t].Name);
end;

procedure TDashboardProperty.SetValue(const Value: string);
var tmp : TBIVisual;
begin
  inherited;

  tmp:=TBIVisual(GetComponent(0));

  tmp.Dashboard:=tmp.Dashboards.Find(Value);
end;
*)

{$IFNDEF FPC}
initialization
  TDataComponent.OnGetDesignerNames:=BIDesignTimeOnGetDesignerNames;
finalization
  TDataComponent.OnGetDesignerNames:=nil;
{$ENDIF}
end.
