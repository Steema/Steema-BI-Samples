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

{$R *.dfm}

uses
  VCLBI.Grid;

procedure TSelectTextItems.DoChanged;
begin
  if Assigned(FOnChanged) then
     FOnChanged(Self);
end;

procedure TSelectTextItems.ChangedText(Sender: TObject);
begin
  DoChanged;
end;

procedure TSelectTextItems.CheckedText(Sender: TObject);
var tmp : Integer;
    tmpChecked : Boolean;
    tmpS : String;
begin
  LBSingleText.ItemIndex:=-1;

  if Assigned(FOnChecked) then
  begin
    tmp:=ITextItems.LBItems.ItemIndex;

    tmpChecked:=ITextItems.LBItems.Checked[tmp];
    tmpS:=ITextItems.LBItems.Items[tmp];

    FOnChecked(Self,tmpS,tmpChecked);
  end;
end;

procedure TSelectTextItems.FormCreate(Sender: TObject);
begin
  ITextItems:=TFormListItems.Embed(Self,TabMultiText);

  ITextItems.EnableDrag(False);
  ITextItems.OnChanged:=ChangedText;
  ITextItems.OnChecked:=CheckedText;

  PageText.ActivePage:=TabMultiText;
end;

procedure TSelectTextItems.LBSingleTextClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBSingleText.ItemIndex;

  if tmp<>-1 then
  begin
    ITextItems.CheckAll(False);
    ITextItems.Check(tmp,True);
  end;

  DoChanged;
end;

procedure TSelectTextItems.PageTextChange(Sender: TObject);
begin
  if PageText.ActivePage=TabSingleText then
  begin
    if LBSingleText.Count=0 then
       TFormListItems.AddMap(FData,LBSingleText.Items);
  end
  else
  if PageText.ActivePage=TabMultiText then
     if ITextItems.LBItems.Count=0 then
        ITextItems.AddMap(FData);
end;

procedure TSelectTextItems.Refresh(const AData: TDataItem);
begin
  FData:=AData;

  LBSingleText.Clear;
  ITextItems.LBItems.Clear;

  PageTextChange(Self);
end;

function TSelectTextItems.Selected: TStringList;
var t : Integer;
begin
  result:=TStringList.Create;

  if PageText.ActivePage=TabSingleText then
  begin
    if LBSingleText.ItemIndex<>-1 then
       result.Add(LBSingleText.Items[LBSingleText.ItemIndex]);
  end
  else
  if PageText.ActivePage=TabMultiText then
     for t:=0 to ITextItems.LBItems.Count-1 do
         if ITextItems.LBItems.Checked[t] then
            result.Add(ITextItems.LBItems.Items[t]);
end;

end.
