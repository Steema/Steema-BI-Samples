{*********************************************}
{  TeeBI Software Library                     }
{  Data Selector dialog                       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.DataSelect;

interface

{
 This dialog enables selecting a "Data" object that can be used for example
 to change a BIGrid.Data property.

 "Data" can be selected from:

 - Any persisted data in a "Store" (disk cache)

 - Any TComponent that is supported, for example a BIQuery, or content from a
   TMemo that will be imported automatically.

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.Data, BI.VCL.DataManager, BI.VCL.Editor.DataComponent, Vcl.Menus,
  BI.Persist, BI.VCL.DataControl;

type
  TDataSelector = class(TForm)
    PageControl1: TPageControl;
    TabStore: TTabSheet;
    TabComponent: TTabSheet;
    PanelButtons: TPanel;
    Panel2: TPanel;
    BOK: TButton;
    BCancel: TButton;
    BClear: TButton;
    PopupMenu2: TPopupMenu;
    Files1: TMenuItem;
    Database1: TMenuItem;
    Web1: TMenuItem;
    Import1: TMenuItem;
    Query1: TMenuItem;
    Panel1: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Query1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Database1Click(Sender: TObject);
    procedure Files1Click(Sender: TObject);
    procedure Web1Click(Sender: TObject);
  private
    { Private declarations }

    IManager : TDataManager;
    IComp : TDataComponent;
    FOnSelect: TNotifyEvent;

    IFilterOwner,
    IEdited : TComponent;

    procedure FilterSelf(Sender: TComponent; var Valid:Boolean);
    procedure FinishAddNew(const AComponent:TComponent);
    function IsEmbedded:Boolean;
    function NewOwner:TComponent;
    function SelectedHasData:Boolean;
    procedure SelectedItem(Sender: TObject);
    procedure TryAddImport(const AKind:TDataDefinitionKind);
  protected
    procedure SetEdited(const AEdited:TComponent);
  public
    { Public declarations }

    class procedure Choose(const AOwner:TComponent;
                           const AEdited:TBIDataControl); overload; static;

    class function Choose(const AOwner:TComponent;
                          const AEdited:TComponent;
                          out AData:TDataItem;
                          const AFilterOwner:TComponent=nil):Boolean; overload; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AEdited: TComponent):TDataSelector; static;

    procedure RefreshTree(const AFilter:TDataManagerFilter);
    procedure Select(const AData:TDataItem);
    function Selected:TDataItem;

    property ComponentSelector:TDataComponent read IComp;
    property Manager:TDataManager read IManager;

    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation
