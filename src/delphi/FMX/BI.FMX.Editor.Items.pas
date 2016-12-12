{*********************************************}
{  TeeBI Software Library                     }
{  TDataItems editor dialog (Table Structure) }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Editor.Items;

interface

{
  Editor dialog to modify the structure of a TDataItem that is in "Table" mode.

  Also a BIGrid in read-write mode to enable adding, modifying and removing
  rows.
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Edit,
  BI.Data, BI.FMX.GridForm;

type
  TItemsEditor = class(TForm)
    Layout1: TLayout;
    GroupBox1: TGroupBox;
    LayoutButtons: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    LBFields: TListBox;
    LayoutGrid: TLayout;
    SBAdd: TSpeedButton;
    SBRemove: TSpeedButton;
    Label1: TLabel;
    EName: TEdit;
    LDuplicateName: TText;
    Label2: TLabel;
    CBKind: TComboBox;
    Layout4: TLayout;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    Layout5: TLayout;
    BOK: TButton;
    procedure LBFieldsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBAddClick(Sender: TObject);
    procedure SBRemoveClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure ENameChangeTracking(Sender: TObject);
    procedure ENameChange(Sender: TObject);
    procedure CBKindChange(Sender: TObject);
  private
    { Private declarations }

    IGrid : TBIGridForm;
    Items : TDataItems;

    FOnChanged : TNotifyEvent;

    procedure Changed;

    procedure DataChanged(Sender: TObject);
    function DataOf(const AIndex:Integer): TDataItem;
    function SelectedField:TDataItem;
    function SelectedItems:TDataItems;
    procedure SwapFields(const A,B:Integer);

    class function Valid(const AItems:TDataItems):Boolean; static;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AItems:TDataItems):TItemsEditor; static;

    class function Edit(const AOwner:TComponent;
                        const AItems:TDataItems):TModalResult; static;

    procedure Refresh(const AItems:TDataItems);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation
