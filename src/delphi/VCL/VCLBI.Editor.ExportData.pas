{*********************************************}
{  TeeBI Software Library                     }
{  TDataItem Export Dialog                    }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.ExportData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BI.DataItem, BI.DataSource, Vcl.ExtCtrls, VCLBI.Editor.ListItems;

type
  TExportData = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    LSize: TLabel;
    LBFormat: TListBox;
    BCopy: TButton;
    BSave: TButton;
    CBSchema: TCheckBox;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    Label3: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    BClose: TButton;
    procedure BSaveClick(Sender: TObject);
    procedure LBFormatClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BCopyClick(Sender: TObject);
  private
    { Private declarations }

    Data : TDataItem;

    IItems : TFormListItems;

    FExports : Array of TBIExport;

    function Current:TBIExport;
    function CurrentExtension:String;
    function DialogFilter:String;
    class function GetExportFormat(const AFile:String):TBIExport; static;
    function Prepare(const AExport:TBIExport; const AData:TDataItem):TBIExport;
    procedure ShowSize(const ASize:UInt64);
  public
    { Public declarations }

    class function HasAnyFormat:Boolean; static;

    class procedure SaveToFile(const AData:TDataItem; const AFile:String); overload; static;
    procedure SaveToFile(const AFile:String); overload;

    class procedure Show(const AOwner:TComponent; const AData:TDataItem); static;
  end;

implementation
