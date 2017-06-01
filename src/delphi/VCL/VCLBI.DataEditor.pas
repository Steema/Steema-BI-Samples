{*********************************************}
{  TeeBI Software Library                     }
{  DataEditor VCL                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.DataEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.DataViewer, Data.DB, BI.Dataset,
  Vcl.Menus, Vcl.Grids, VCLBI.DataControl, VCLBI.Grid, Vcl.DBCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  BI.DataItem;

type
  TDataItemEditor = class(TDataViewer)
    PanelButtons: TPanel;
    Panel7: TPanel;
    BOK: TButton;
    BCancel: TButton;
    PopupNode: TPopupMenu;
    Rename1: TMenuItem;
    N2: TMenuItem;
    AddField1: TMenuItem;
    AddTable1: TMenuItem;
    Kind1: TMenuItem;
    Integer32bit1: TMenuItem;
    Integer64bit1: TMenuItem;
    FloatSingle1: TMenuItem;
    FloatDouble1: TMenuItem;
    FloatExtended1: TMenuItem;
    Text1: TMenuItem;
    DateTime1: TMenuItem;
    Boolean1: TMenuItem;
    AddFolder1: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Rename1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure PopupNodePopup(Sender: TObject);
    procedure AddTable1Click(Sender: TObject);
    procedure AddFolder1Click(Sender: TObject);
    procedure DataSource2UpdateData(Sender: TObject);
    procedure ItemsBeforeDelete(DataSet: TDataSet);
    procedure AddField1Click(Sender: TObject);
    procedure Boolean1Click(Sender: TObject);
  private
    { Private declarations }

    FOriginal : TDataItem;

    IModified : Boolean;

    procedure AskSave;
    function ConvertKind(const AItem:TDataItem; const AKind:TDataKind):Boolean;
    procedure EditButtonClick(Sender: TObject);
    procedure Modified;
    procedure SaveData;
    procedure SelectedEdited(Sender: TObject; Node: TTreeNode; var S: string);
    function SelectedItem:TDataItem;
    procedure UpdatedData(Sender: TObject);
    procedure TryAddNewItem(const ATitle,APrefix:String;
                            const IsTable:Boolean;
                            const AKind:TDataKind);
  protected
    procedure TryAddInfoEditors(const AGrid:TObject); override;
  public
    { Public declarations }

    class function Edit(const AOwner: TComponent; const AData: TDataItem):Boolean; static;
  end;

implementation
