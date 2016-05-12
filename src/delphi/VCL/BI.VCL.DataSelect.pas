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
  BI.Data, BI.VCL.DataManager, BI.VCL.Editor.DataComponent;

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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }

    IManager : TDataManager;
    IComp : TDataComponent;
    FOnSelect: TNotifyEvent;

    procedure FilterSelf(Sender: TComponent; var Valid:Boolean);
    function SelectedHasData:Boolean;
    procedure SelectedItem(Sender: TObject);
  public
    { Public declarations }

    class function Choose(const AOwner:TComponent;
                          const AEdited:TComponent;
                          out AData:TDataItem):Boolean; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AEdited: TComponent):TDataSelector; static;

    procedure Select(const AData:TDataItem);
    function Selected:TDataItem;

    property ComponentSelector:TDataComponent read IComp;
    property Manager:TDataManager read IManager;

    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation
