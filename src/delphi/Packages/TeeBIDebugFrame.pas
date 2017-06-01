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
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    procedure Modified;
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
    procedure ThreadNotify(Reason: TOTANotifyReason);
  end;

implementation
