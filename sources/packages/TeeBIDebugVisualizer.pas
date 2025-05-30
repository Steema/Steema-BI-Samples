unit TeeBIDebugVisualizer;

interface

procedure RegisterVisualizer;

implementation

uses
  DesignIntf, System.Classes, System.SysUtils, ToolsAPI,
  VCL.Forms, System.Actions, VCL.Menus, VCL.ActnList, VCL.ImgList,
  VCL.ComCtrls, System.IniFiles, TeeBIDebugFrame;

type
  TDataItemVisualizer=class(TInterfacedObject, IOTADebuggerVisualizer,
                            IOTADebuggerVisualizerExternalViewer)
  public
    { IOTADebuggerVisualizer }
    procedure GetSupportedType(Index: Integer; var TypeName: string;
                               var AllDescendents: Boolean);
    function GetSupportedTypeCount: Integer;
    function GetVisualizerDescription: string;
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;

    { IOTADebuggerVisualizerExternalViewer }
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string;
                  SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

var
  DataItemVisualizer : IOTADebuggerVisualizer;

procedure RegisterVisualizer;
var LServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, LServices) then
  begin
    DataItemVisualizer:=TDataItemVisualizer.Create;
    LServices.RegisterDebugVisualizer(DataItemVisualizer);
  end;
end;

procedure RemoveVisualizer;
var LServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, LServices) then
  begin
    LServices.UnregisterDebugVisualizer(DataItemVisualizer);
    DataItemVisualizer:=nil;
  end;
end;

{ TDataItemVisualizer }

function TDataItemVisualizer.GetMenuText: string;
begin
  result:='Show Data';
end;

procedure TDataItemVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendents: Boolean);
begin
  TypeName:='TDataItem';
  AllDescendents:=True;
end;

function TDataItemVisualizer.GetSupportedTypeCount: Integer;
begin
  result:=1;
end;

function TDataItemVisualizer.GetVisualizerDescription: string;
begin
  result:='Displays the structure and data arrays of a TDataItem instance';
end;

function TDataItemVisualizer.GetVisualizerIdentifier: string;
begin
  result:=ClassName;
end;

function TDataItemVisualizer.GetVisualizerName: string;
begin
  result:='TDataItem Debug Visualizer for Delphi';
end;

type
  TDataDebugForm=class(TInterfacedObject, INTACustomDockableForm)
  protected
    FExpression: String;
    FViewerFrame: TDataItemDebugFrame;
  public
    { INTACustomDockableForm }

    procedure CustomizePopupMenu(PopupMenu:TPopupMenu);
    procedure CustomizeToolBar(ToolBar:TToolBar);
    function EditAction(Action: TEditAction):Boolean;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetCaption:String;
    function GetEditState:TEditState;
    function GetFrameClass:TCustomFrameClass;
    function GetIdentifier:String;
    function GetMenuActionList:TCustomActionList;
    function GetMenuImageList:TCustomImageList;
    function GetToolbarActionList:TCustomActionList;
    function GetToolbarImageList:TCustomImageList;
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
  end;

function TDataItemVisualizer.Show(const Expression, TypeName,EvalResult: string;
                                  SuggestedLeft,SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var Form: TCustomForm;
    Frame: TDataItemDebugFrame;
    DockForm: INTACustomDockableForm;
    tmp : TDataDebugForm;
begin
  tmp:=TDataDebugForm.Create;
  tmp.FExpression:=Expression;

  DockForm:=tmp as INTACustomDockableForm;

  Form:=(BorlandIDEServices as INTAServices).CreateDockableForm(DockForm);
  Form.Left:=Suggestedleft;
  Form.Top:=SuggestedTop;

  Frame:=tmp.FViewerFrame;

  if Frame=nil then
     result:=nil
  else
  begin
    Frame.IForm:=Form;
    Frame.DoEvaluate(Expression,TypeName,EvalResult);

    result:=Frame as IOTADebuggerVisualizerExternalViewerUpdater;
  end;
end;

{ TDataDebugForm }

procedure TDataDebugForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin // nothing
end;

procedure TDataDebugForm.CustomizeToolBar(ToolBar: TToolBar);
begin // nothing
end;

function TDataDebugForm.EditAction(Action: TEditAction): Boolean;
begin
  result:=False;
end;

procedure TDataDebugForm.FrameCreated(AFrame: TCustomFrame);
begin
  FViewerFrame:=TDataItemDebugFrame(AFrame);
  FViewerFrame.CheckOnCreate;
end;

function TDataDebugForm.GetCaption: String;
begin
  result:=Format('TDataItem: %s', [FExpression]);
end;

function TDataDebugForm.GetEditState: TEditState;
begin
  result:=[];
end;

function TDataDebugForm.GetFrameClass: TCustomFrameClass;
begin
  result:=TDataItemDebugFrame;
end;

function TDataDebugForm.GetIdentifier: String;
begin
  result:='DataItem';
end;

function TDataDebugForm.GetMenuActionList: TCustomActionList;
begin
  result:=nil;
end;

function TDataDebugForm.GetMenuImageList: TCustomImageList;
begin
  result:=nil;
end;

function TDataDebugForm.GetToolbarActionList: TCustomActionList;
begin
  result:=nil;
end;

function TDataDebugForm.GetToolbarImageList: TCustomImageList;
begin
  result:=nil;
end;

procedure TDataDebugForm.LoadWindowState(Desktop: TCustomIniFile; const Section: string);
begin // nothing
end;

procedure TDataDebugForm.SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
begin // nothing
end;

initialization
finalization
  RemoveVisualizer;
end.
