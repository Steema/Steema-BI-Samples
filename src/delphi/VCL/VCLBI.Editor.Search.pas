{*********************************************}
{  TeeBI Software Library                     }
{  Search Editor Panel                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Search;

interface

{
  This form displays the UI necessary to work with TDataSearch.
  TDataSearch is used to find free-text inside TDataItem values.
  TBIGrid uses this form to allow searching text inside grid cells.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Buttons, Vcl.StdCtrls,
  BI.Search, BI.DataSet, BI.Arrays, BI.DataItem;

type
  TGetDataSet=function:TBIDataSet of object;

  TSearchEditor = class(TForm)
    ESearch: TEdit;
    SBMenu: TSpeedButton;
    PopupMenu1: TPopupMenu;
    Casesensitive1: TMenuItem;
    Searchat1: TMenuItem;
    Anywhere1: TMenuItem;
    Start1: TMenuItem;
    End1: TMenuItem;
    Exact1: TMenuItem;
    LHits: TLabel;
    SBClose: TSpeedButton;
    SBDown: TSpeedButton;
    SBUp: TSpeedButton;
    procedure ESearchChange(Sender: TObject);
    procedure Casesensitive1Click(Sender: TObject);
    procedure Exact1Click(Sender: TObject);
    procedure SBMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
  private
    { Private declarations }

    FOnChanged : TNotifyEvent;
    FOnGetDataSet : TGetDataSet;

    procedure SetCurrent(const Value:TInteger);

  public
    { Public declarations }

    CurrentColor,
    HighLightColor : TColor;

    Current,
    Total : TInteger;

    Search : TDataSearch;

    procedure Clear;

    class function Embed(const AOwner:TComponent; const AParent:TWinControl;
                         const AAlign:TAlign):TSearchEditor; static;

    function HasHit(const ARow:Integer;
                    const AItem:TDataItem;
                    out AIndex:TInteger):Boolean;

    function HasHits:Boolean;
    function HitIndex(const ARow:Integer;
                      const AItem:TDataItem):TInteger;

    procedure Reposition(const ALeft:Integer);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
    property OnGetDataSet:TGetDataSet read FOnGetDataSet write FOnGetDataSet;
  end;

implementation
