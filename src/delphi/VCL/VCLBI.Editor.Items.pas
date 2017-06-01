{*********************************************}
{  TeeBI Software Library                     }
{  TDataItems editor dialog (Table Structure) }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Items;

interface

{
  Editor dialog to modify the structure of a TDataItem that is in "Table" mode.

  Also a BIGrid in read-write mode to enable adding, modifying and removing
  rows.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  BI.DataItem, Vcl.ExtCtrls, VCLBI.DataControl, VCLBI.Grid,
  VCLBI.GridForm;

type
  TItemsEditor = class(TForm)
    PanelButtons: TPanel;
    Panel1: TPanel;
    BOK: TButton;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    LBFields: TListBox;
    PanelGrid: TPanel;
    Panel3: TPanel;
    Label24: TLabel;
    Label25: TLabel;
    LDuplicateName: TLabel;
    CBKind: TComboBox;
    EName: TEdit;
    Panel4: TPanel;
    SBAddField: TSpeedButton;
    SBRemoveField: TSpeedButton;
    Panel5: TPanel;
    SBUpField: TSpeedButton;
    SBDownField: TSpeedButton;
    Splitter1: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure CBKindChange(Sender: TObject);
    procedure ENameChange(Sender: TObject);
    procedure LBFieldsClick(Sender: TObject);
    procedure LBFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SBRemoveFieldClick(Sender: TObject);
    procedure SBUpFieldClick(Sender: TObject);
    procedure SBAddFieldClick(Sender: TObject);
    procedure SBDownFieldClick(Sender: TObject);
  private
    { Private declarations }

    IGrid : TBIGridForm;
    Items : TDataItems;

    function DataOf(const AIndex:Integer):TDataItem;
    function SelectedField:TDataItem;
    function SelectedItems:TDataItems;
    procedure SwapFields(const A,B:Integer);
    class function Valid(const AItems:TDataItems):Boolean; static;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AItems:TDataItems):TItemsEditor; static;

    class function Edit(const AOwner:TComponent;
                        const AItems:TDataItems):Boolean; static;

    procedure Refresh(const AItems:TDataItems);
  end;

implementation
