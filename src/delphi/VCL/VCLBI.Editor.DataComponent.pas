{*********************************************}
{  TeeBI Software Library                     }
{  Component Selector dialog                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.DataComponent;

interface

{
  This dialog can be used at design-time or runtime to select a TComponent
  that can be a provider of "Data".

  The TDataSelector dialog (VCLBI.DataSelect.pas unit) also uses this dialog.

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,

  {$IFDEF FPC}
  BI.FPC, FGL,
  {$ELSE}
  System.Generics.Collections,
  {$ENDIF}

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Grids, BI.DataItem, Vcl.Menus, Vcl.ExtCtrls;

type
  TFilterEvent=procedure(Sender: TComponent; var Valid:Boolean) of object;

  TDataComponent = class(TForm)
    Tree: TTreeView;
    PopupMenu1: TPopupMenu;
    ViewData1: TMenuItem;
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ViewData1Click(Sender: TObject);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    IComponents : {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TComponent>;

    FRoot : TDataItem;

    FOnFilter : TFilterEvent;
    FOnSelected : TNotifyEvent;

    function CalcName(const AComponent:TComponent; const AName:String):String;
    function CanAdd(const AComponent:TComponent):Boolean;
    procedure DoAddData(const AParent:TTreeNode; const AData:TDataItem);
    procedure FillTree;
  protected
    FCurrent : TObject;
    IEdited : TComponent;

    procedure Add(const AParent:TTreeNode; const AComponent:TComponent; const AName:String); overload;
    procedure Add(const AParent,AComponent:TComponent); overload;
    function NodeOf(const AObject:TObject):TTreeNode;
    function SelectedHasData:Boolean;
    procedure TryFreeData;
  public
    { Public declarations }

    type
      TBIAddComponent=procedure(const AParent:TTreeNode; const AComponent:TComponent; const AName:String) of object;

    class
       var OnGetDesignerNames : TProc<TBIAddComponent,TComponent>;

    function Data(const AOwner:TComponent):TDataItem;

    class function Import(const AOwner:TComponent; const AObject:TObject):TDataItem; static;

    class function Choose(const AOwner,AEdited:TComponent;
                          const ACurrent:TComponent=nil):TComponent; overload; static;

    // Select a data item from ARoot
    class function Choose(const AOwner:TComponent; const ARoot:TDataItem;
                          const ACurrent:TDataItem=nil):TDataItem; overload; static;

    // Multi Select an array of data items from ARoot
    class function ChooseMany(const AOwner:TComponent; const ARoot:TDataItem;
                              const ACurrent:TDataItem=nil;
                              const Compatible:Boolean=False):TDataArray; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const ARoot:TDataItem):TDataComponent; static;

    procedure Select(const AObject:TObject);

    function Selected:TComponent;
    function SelectedData:TDataItem;
    function SelectedItems:TDataArray;

    property OnFilter:TFilterEvent read FOnFilter write FOnFilter;
    property OnSelected:TNotifyEvent read FOnSelected write FOnSelected;
  end;

implementation
