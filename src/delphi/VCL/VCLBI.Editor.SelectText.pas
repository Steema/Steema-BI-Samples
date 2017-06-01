{*********************************************}
{  TeeBI Software Library                     }
{  Generic dialog to edit string lists        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.SelectText;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  VCLBI.Editor.ListItems, BI.DataItem;

(*
  TSelectTextItems dialog is used to allow the end-user to select one or more
  items from a TDataItem.

  Unique values from a TDataItem are displayed into a checkbox list, using the
  "map" of the TDataItem.
*)

type
  TSelectTextCheckEvent=procedure(const Sender:TObject;
                                  const AText:String;
                                  const IsChecked:Boolean) of object;

  TSelectTextItems = class(TForm)
    PageText: TPageControl;
    TabMultiText: TTabSheet;
    TabSingleText: TTabSheet;
    LBSingleText: TListBox;
    procedure LBSingleTextClick(Sender: TObject);
    procedure PageTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    FData : TDataItem;

    ITextItems : TFormListItems;

    FOnChanged : TNotifyEvent;
    FOnChecked : TSelectTextCheckEvent;

    procedure ChangedText(Sender: TObject);
    procedure CheckedText(Sender: TObject);
    procedure DoChanged;
  public
    { Public declarations }

    function Selected:TStringList;

    procedure Refresh(const AData:TDataItem);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
    property OnChecked:TSelectTextCheckEvent read FOnChecked write FOnChecked;
  end;

implementation
