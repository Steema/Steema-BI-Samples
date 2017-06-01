{*********************************************}
{  TeeBI Software Library                     }
{  WorkflowItem Editor Dialog                 }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.WorkflowItem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  BI.Workflow, BI.DataItem, Vcl.ExtCtrls, VCLBI.Editor.Needs;

type
  TWorkflowItemEditor = class(TForm)
    PageControl1: TPageControl;
    TabRename: TTabSheet;
    ItemNames: TComboBox;
    ENewName: TEdit;
    TabDelete: TTabSheet;
    DeleteItemNames: TComboBox;
    TabAdd: TTabSheet;
    EAdd: TEdit;
    CBKind: TComboBox;
    TabQuery: TTabSheet;
    MemoSQL: TMemo;
    TabFilter: TTabSheet;
    BEditFilter: TButton;
    TabSingleRow: TTabSheet;
    Label1: TLabel;
    ERow: TEdit;
    UDRow: TUpDown;
    TBRow: TTrackBar;
    Panel1: TPanel;
    BEditQuery: TButton;
    LError: TLabel;
    TabNeeds: TTabSheet;
    procedure CBKindChange(Sender: TObject);
    procedure ENewNameChange(Sender: TObject);
    procedure EAddChange(Sender: TObject);
    procedure MemoSQLChange(Sender: TObject);
    procedure ItemNamesChange(Sender: TObject);
    procedure BEditQueryClick(Sender: TObject);
    procedure BEditFilterClick(Sender: TObject);
    procedure ERowChange(Sender: TObject);
    procedure TBRowChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;
    INeeds : TProviderNeedsEditor;

    FItem : TWorkflowItem;

    function AnyTabVisible:Boolean;

    class function ProviderOf(const AItem:TWorkflowItem):TDataProvider; static;

    procedure SetAddProperties(const AData:TDataItem);
    procedure SetDeleteProperties(const AData:TDataItem);
    function SetErrorLabel(const Sender:TObject; const Error:String):Boolean;
    procedure SetQueryProperties(const AData:TDataItem);
    procedure SetRenameProperties(const AData:TDataItem);
    procedure SetSingleRowProperties(const AData:TDataItem);
    procedure ShowHideTabs(const AItem:TWorkflowItem);
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl):TWorkflowItemEditor; static;

    function HasContent:Boolean;
    procedure Refresh(const AItem:TWorkflowItem);
  end;

implementation
