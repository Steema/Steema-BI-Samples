{*********************************************}
{  TeeBI Software Library                     }
{  Internal IDE Design-Time                   }
{  Debug Visualizer Dockable Frame            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit TeeBIDebugFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  Vcl.Graphics, Vcl.Forms, Vcl.Dialogs,
  ToolsAPI, VCLBI.DataViewer, Classes, Controls, StdCtrls;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope);

  TDebugContext=record
    Process: IOTAProcess;
    Thread: IOTAThread;
    Debug: IOTADebuggerServices;
  end;

  TDeferredEvaluate=record
    Result: String;
    Address: LongWord;
    Code : Integer;
  end;

  TDebugEvaluate=record
    Completed : Boolean;
    Deferred : TDeferredEvaluate;
  end;

  TDebugModify=record
    Completed : Boolean;
    Result : String;
    Code : Integer;
  end;

  TDataItemDebugFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater, IOTAThreadNotifier)
    StatusLabel: TLabel;
  private
    { Private declarations }
    FAvailableState: TAvailableState;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FNotifierIndex: Integer;

    FDebug : TDebugEvaluate;
    FModify : TDebugModify;

    IViewer : TDataViewer;
  protected
    procedure {$IFDEF VER340}EvaluateComplete{$ELSE}EvaluteComplete{$ENDIF}(const ExprStr, ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    procedure EvaluateExpression(const Context:TDebugContext; const Expression,TypeName,EvalResult:String);
    function EvaluatePointer(const Context: TDebugContext; Expression:String):Pointer;
    function EvaluateStringValue(const Context: TDebugContext; Expression:String):String;
    procedure SetParent(AParent: TWinControl); override;
  public
    { Public declarations }

    IForm : TCustomForm;

    procedure CheckOnCreate;
    procedure DoEvaluate(const Expression, TypeName, EvalResult:String);

    { IOTADebuggerVisualizerExternalViewerUpdater }
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);

    { IOTAThreadNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    {$IF CompilerVersion>=35} // RAD 11.0 Alexandria and up
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    {$ENDIF}
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
    procedure ThreadNotify(Reason: TOTANotifyReason);
  end;

implementation

{$R *.dfm}

uses
  BI.DataItem, VCLBI.Grid, System.IOUtils, BI.Persist;

{ TDataItemDebugFrame }

procedure TDataItemDebugFrame.AfterSave;
begin // nothing
end;

procedure TDataItemDebugFrame.BeforeSave;
begin // nothing
end;

procedure TDataItemDebugFrame.CloseVisualizer;
begin
  if IForm<>nil then
     IForm.Close;
end;

type
  TDataViewerAccess=class(TDataViewer);

procedure TDataItemDebugFrame.CheckOnCreate;
begin
  if IViewer=nil then
  begin
    IViewer:=TDataViewer.Create(Self);
    TUICommon.AddForm(IViewer,Self);
  end;
end;

procedure TDataItemDebugFrame.Destroyed;
begin // nothing
end;

function TDataItemDebugFrame.EvaluateStringValue(const Context: TDebugContext; Expression:String):String;
var EvalRes: TOTAEvaluateResult;
    ResultStr: Array[0..65535] of Char;
    CanModify: Boolean;
    ResultAddr,
    ResultSize,
    ResultVal: LongWord;
begin
  result:='';

  EvalRes:=Context.Thread.Evaluate(Expression, @ResultStr, Length(ResultStr),
            CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);

  case EvalRes of
    erOK: result:=ResultStr;

    erDeferred: begin
      FDebug.Completed:=False;
      FDebug.Deferred.Result:='';
      FDebug.Deferred.Code:=0;

      FNotifierIndex:=Context.Thread.AddNotifier(Self);

      while not FDebug.Completed do
            Context.Debug.ProcessDebugEvents;

      Context.Thread.RemoveNotifier(FNotifierIndex);

      FNotifierIndex:=-1;

      if FDebug.Deferred.Code=0 then
      begin
        result:=FDebug.Deferred.Result;

        if result='' then
           result:=ResultStr;
      end;
    end;

    erBusy: begin
              Context.Debug.ProcessDebugEvents;
              result:=EvaluateStringValue(Context, Expression);
            end;
  end;
end;

// Unfortunately, Addr cannot be used in Debug Visualizers to return
// the address of an instance. (Does not work across-processes.
function TDataItemDebugFrame.EvaluatePointer(const Context: TDebugContext; Expression:string):Pointer;
var tmp : String;
    tmpAddr : NativeInt;
begin
  tmp:=EvaluateStringValue(Context, 'Addr('+Expression+')');

  if Length(tmp)>0 then
  begin
    if tmp[1]=':' then
       tmp[1]:='$';

    tmpAddr:=StrToInt64Def(tmp,0);
    result:=Pointer(tmpAddr);
  end
  else
    result:=nil;
end;

procedure TDataItemDebugFrame.EvaluateExpression(const Context: TDebugContext; const Expression, TypeName,EvalResult: String);
var //tmp : Pointer;
    tmpData : TDataItem;
    tmpS,
    tmpExp,
    tmpRes : String;
begin
  //tmp:=EvaluatePointer(Context,Expression);
  {
  if tmp=nil then
     tmpData:=nil
  else
     tmpData:=TDataItem(tmp^);
  }

  // Alternative (ugly): save data to a temporary binary file:
  tmpS:=TPath.GetTempFileName;

  tmpExp:='TDataItemPersistence.Save('+Expression+','''+tmpS+''')';

  tmpRes:=EvaluateStringValue(Context, tmpExp);
  try
    (*
    StatusLabel.Caption:='evaluate: '+Expression+#13#10+
                         TypeName+#13#10+
                         tmpExp+#13#10+
                         tmpRes;

    StatusLabel.Visible:=True;
    *)

    tmpData:=TDataItemPersistence.Load(tmpS); // <-- load back from temp file
  finally
    TFile.Delete(tmpS);
  end;

  CheckOnCreate;
  IViewer.Select(tmpData);
end;

procedure TDataItemDebugFrame.{$IFDEF VER340}EvaluateComplete{$ELSE}EvaluteComplete{$ENDIF}(const ExprStr, ResultStr: string;
  CanModify: Boolean; ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
begin
  FDebug.Completed:=True;
  FDebug.Deferred.Result:=ResultStr;
  FDebug.Deferred.Address:=ResultAddress;
  FDebug.Deferred.Code:=ReturnCode;
end;

procedure TDataItemDebugFrame.MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
begin
  StatusLabel.Visible:=True;

  if Reason = ovurProcessRunning then
  begin
    StatusLabel.Caption:='Process not accessible';
    FAvailableState:=asProcRunning
  end
  else
  if Reason = ovurOutOfScope then
  begin
    StatusLabel.Caption:='Out of scope';
    FAvailableState:=asOutOfScope;
  end;
end;

procedure TDataItemDebugFrame.Modified;
begin // nothing
end;

{$IF CompilerVersion>=35} // RAD 11.0 Alexandria and up
procedure TDataItemDebugFrame.EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
    ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
begin // nothing
end;
{$ENDIF}

procedure TDataItemDebugFrame.ModifyComplete(const ExprStr, ResultStr: string;
  ReturnCode: Integer);
begin
  FModify.Completed:=True;
  FModify.Result:=ResultStr;
  FModify.Code:=ReturnCode;
end;

procedure TDataItemDebugFrame.DoEvaluate(const Expression,TypeName, EvalResult: string);
var Context: TDebugContext;
begin
  FAvailableState:=asAvailable;

  if Supports(BorlandIDEServices, IOTADebuggerServices, Context.Debug) then
  begin
    Context.Process:=Context.Debug.CurrentProcess;

    if Context.Process<>nil then
    begin
      Context.Thread:=Context.Process.CurrentThread;

      if Context.Thread<>nil then
         EvaluateExpression(Context,Expression,TypeName,EvalResult);
    end;
  end;
end;

procedure TDataItemDebugFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState:=asAvailable;
  StatusLabel.Visible:=False;
  DoEvaluate(Expression, TypeName, EvalResult);
end;

procedure TDataItemDebugFrame.SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc:=ClosedProc;
end;

procedure TDataItemDebugFrame.SetParent(AParent: TWinControl);
begin
  if AParent=nil then
     if Assigned(FClosedProc) then
        FClosedProc;

  inherited;
end;

procedure TDataItemDebugFrame.ThreadNotify(Reason: TOTANotifyReason);
begin // nothing
end;

end.
