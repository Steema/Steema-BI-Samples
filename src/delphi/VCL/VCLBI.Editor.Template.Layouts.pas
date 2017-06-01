{*********************************************}
{  TeeBI Software Library                     }
{  TLayouts Dashboard Editor                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Template.Layouts;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  VCLBI.Dashboard, BI.Dashboard.Layouts;

type
  TLayoutGallery = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BOK: TButton;
    BCancel: TButton;
    BIVisual1: TBIVisual;
    Panel3: TPanel;
    LBLayouts: TListBox;
    Panel4: TPanel;
    CBCurrent: TCheckBox;
    Splitter1: TSplitter;
    Button1: TButton;
    procedure LBLayoutsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBCurrentClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    ICurrent : TLayoutItem;

    procedure AddList(const ALayouts:TLayouts);
    procedure Preview(const ALayout:TLayoutItem);
    function Selected:TLayoutItem;
  public
    { Public declarations }

    class function Choose(const AOwner:TComponent;
                          out ALayout:String;
                          const ACurrent:TLayoutItem):Boolean; static;
  end;

implementation
