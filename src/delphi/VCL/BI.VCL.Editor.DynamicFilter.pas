{*********************************************}
{  TeeBI Software Library                     }
{  Dynamic Filter Editor                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.DynamicFilter;

interface

{
  This dialog fills a tree with all possible Data Values that can be accessed
  from a given AData parameter.

  The tree enables checking and unchecking individual items to build a Filter
  expression that can be then used in for example a BIGrid to display rows that
  match the selected items.

  Example 1:
  Using this editor embedded into a Form:

  uses
    BI.Persist, BI.VCL.Editor.DynamicFilter;

  var IEditor : TDynamicFilterEditor;

  procedure TFormTest.FilterChanged(Sender: TObject);
  begin
    BIGrid1.SetFilter(IEditor.Filter);
  end;

  procedure TFormTest.FormCreate(Sender: TObject);
  begin
    BIGrid1.Data:=TStore.load('SQLite_Demo')['"Order Details"'];

    IEditor:=TDynamicFilterEditor.Embedd(Self,Panel1,BIGrid1.Data);
    IEditor.OnChange:=FilterChanged;
  end;


  Example 2:
  Using this editor as a modal dialog:

  procedure TForm23.Button2Click(Sender: TObject);
  begin
    BIGrid1.SetFilter(TDynamicFilterEditor.Choose(Self,BIGrid1.Data));
  end;

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.DataControl,
  BI.VCL.Tree, BI.Data, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, BI.Expression,
  BI.VCL.Editor.Expression, Vcl.Buttons, BI.VCL.Editor.DateTimeRange;

type
  TDynamicFilterEditor = class(TForm)
    BITree1: TBITree;
    PanelCustom: TPanel;
    Panel2: TPanel;
    CBCustom: TComboBox;
    PanelButtons: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ECustom: TEdit;
    SBCustom: TSpeedButton;
    LError: TLabel;
    PageItem: TPageControl;
    TabDateTime: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure ECustomChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBCustomChange(Sender: TObject);
    procedure SBCustomClick(Sender: TObject);
    procedure PanelCustomResize(Sender: TObject);
    procedure BITree1Change(Sender: TObject);
  private
    { Private declarations }

    IChecking : Boolean;

    ICustom : TLogicalExpression;

    IData,
    IMainData : TDataItem;

    IDateTimeRange : TDateTimeRangeEditor;

    FOnChange: TNotifyEvent;

    procedure AddMapValues(const AParent:TBITreeNode; const AData:TDataItem);
    procedure ChangedDateTime(Sender: TObject);
    procedure DoChanged;
    procedure Expanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    function HasDummy(const ANode:TBITreeNode):Boolean;
    function Resolver(const S:String; IsFunction:Boolean):TExpression;
    procedure TreeChecked(Sender: TObject);
    procedure TryChangeExpression(const AExp:TExpression);
  public
    { Public declarations }

    class function Choose(const AData:TDataItem;
                          const AMain:TDataItem=nil):TLogicalExpression; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AData:TDataItem;
                          const AMain:TDataItem=nil):TDynamicFilterEditor; static;

    function Filter:TLogicalExpression;

    procedure Refresh(const AData:TDataItem; const AMain:TDataItem=nil);

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
