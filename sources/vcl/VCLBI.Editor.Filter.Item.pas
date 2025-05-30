{*********************************************}
{  TeeBI Software Library                     }
{  TFilterItem editor dialog                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Filter.Item;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  BI.Expression.Filter, VCLBI.Editor.Filter.DateTime,
  VCLBI.Editor.NumericFromTo, VCLBI.Editor.SelectText;

(*
  This dialog is used embedded by TDynamicFilterEditor dialog.

  It can also be used standalone.

  TFilterItem class is used by TBIQuery to configure the "where ..." expressions.
*)

type
  TFilterItemEditor = class(TForm)
    PageItem: TPageControl;
    TabDateTime: TTabSheet;
    TabBoolean: TTabSheet;
    CBTrue: TCheckBox;
    CBFalse: TCheckBox;
    TabNumeric: TTabSheet;
    PageNumeric: TPageControl;
    TabNumericRange: TTabSheet;
    TabNumericSelected: TTabSheet;
    TabText: TTabSheet;
    PageControl2: TPageControl;
    TabTextOptions: TTabSheet;
    Label1: TLabel;
    LBTextStyle: TListBox;
    EText: TEdit;
    CBTextCase: TCheckBox;
    TabIncluded: TTabSheet;
    TabExcluded: TTabSheet;
    procedure CBTextCaseClick(Sender: TObject);
    procedure CBTrueClick(Sender: TObject);
    procedure CBFalseClick(Sender: TObject);
    procedure LBTextStyleClick(Sender: TObject);
    procedure ETextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;
    Item : TFilterItem;
    FOnChange: TNotifyEvent;

    INumericFromTo,
    INumericSelected : TNumericFromTo;

    ITextInclude,
    ITextExclude   : TSelectTextItems;
    IDateTime : TDateTimeFilterEditor;

    procedure ChangedDateTime(Sender: TObject);
    procedure ChangedNumeric(Sender: TObject);
    procedure ChangedText(Sender: TObject);
    procedure CheckedText(const Sender: TObject; const AText:String; const IsChecked:Boolean);
    procedure DoChanged;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AItem:TFilterItem):TFilterItemEditor; static;

    procedure Refresh(const AItem:TFilterItem);

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.dfm}

uses
  BI.DataItem, VCLBI.Grid, BI.CollectionItem;

procedure TFilterItemEditor.DoChanged;
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

procedure TFilterItemEditor.CBFalseClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Item.BoolFilter.IncludeFalse:=CBFalse.Checked;
    DoChanged;
  end;
end;

procedure TFilterItemEditor.CBTextCaseClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Item.Text.CaseSensitive:=CBTextCase.Checked;
    DoChanged;
  end;
end;

procedure TFilterItemEditor.CBTrueClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Item.BoolFilter.IncludeTrue:=CBTrue.Checked;
    DoChanged;
  end;
end;

procedure TFilterItemEditor.ChangedDateTime(Sender: TObject);
begin
  if not IChanging then
     DoChanged;
end;

type
  TFilterItemAccess=class(TFilterItem);

procedure TFilterItemEditor.ChangedText(Sender: TObject);
var tmp : TStringList;
begin
  if not IChanging then
  begin
    TFilterItemAccess(Item).BeginUpdate;
    try
      tmp:=ITextInclude.Selected;
      try
        Item.Included:=tmp;
      finally
        tmp.Free;
      end;

      tmp:=ITextExclude.Selected;
      try
        Item.Excluded:=tmp;
      finally
        tmp.Free;
      end;
    finally
      TFilterItemAccess(Item).EndUpdate;
    end;

    DoChanged;
  end;
end;

procedure TFilterItemEditor.ChangedNumeric(Sender: TObject);
var tmp : TNumericFilter;
begin
  if not IChanging then
  begin
    TFilterItemAccess(Item).BeginUpdate;
    try
      tmp:=Item.Numeric;

      tmp.FromValue.Value:=INumericFromTo.FromValue;
      tmp.FromValue.Enabled:=INumericFromTo.EnabledFrom;
      tmp.FromValue.Equal:=INumericFromTo.FromEqual;

      tmp.ToValue.Value:=INumericFromTo.ToValue;
      tmp.ToValue.Enabled:=INumericFromTo.EnabledTo;
      tmp.ToValue.Equal:=INumericFromTo.ToEqual;

      tmp.Selected.Value:=INumericSelected.FromValue;
      tmp.Selected.Enabled:=INumericSelected.EnabledFrom;
    finally
      TFilterItemAccess(Item).EndUpdate;
    end;

    DoChanged;
  end;
end;

procedure TFilterItemEditor.CheckedText(const Sender: TObject; const AText:String; const IsChecked:Boolean);
begin
  if Sender=ITextInclude then
     Item.IncludeText(AText,IsChecked)
  else
     Item.ExcludeText(AText,IsChecked);

  DoChanged;
end;

type
  TNumericFromToAccess=class(TNumericFromTo);

procedure TFilterItemEditor.Refresh(const AItem:TFilterItem);

  procedure ShowUniqueTab(const ATab:TTabSheet; const AData:TDataItem);
  var tmp : TPageControl;
      t : Integer;
  begin
    ATab.Caption:=AData.Name;

    tmp:=ATab.PageControl;

    for t:=0 to tmp.PageCount-1 do
        tmp.Pages[t].TabVisible:=tmp.Pages[t]=ATab;
  end;

  procedure ShowDateTime;
  begin
    if IDateTime=nil then
       IDateTime:=TDateTimeFilterEditor.Embed(Self,TabDateTime,ChangedDateTime);

    IDateTime.Refresh(AItem);

    ShowUniqueTab(TabDateTime,AItem.Data);
  end;

  procedure ShowNumeric;
  begin
    if INumericFromTo=nil then
       INumericFromTo:=TNumericFromTo.Embedd(Self,TabNumericRange,ChangedNumeric);

    INumericFromTo.Refresh(AItem.Data);

    INumericFromTo.FromEqual:=AItem.Numeric.FromValue.Equal;
    INumericFromTo.ToEqual:=AItem.Numeric.ToValue.Equal;

    if AItem.Numeric.FromValue.Enabled then
       INumericFromTo.FromValue:=AItem.Numeric.FromValue.Value;

    if AItem.Numeric.ToValue.Enabled then
       INumericFromTo.ToValue:=AItem.Numeric.ToValue.Value;

    if INumericSelected=nil then
    begin
      INumericSelected:=TNumericFromTo.Embedd(Self,TabNumericSelected,ChangedNumeric);
      TNumericFromToAccess(INumericSelected).HideTo;
    end;

    INumericSelected.Refresh(AItem.Data);

    ShowUniqueTab(TabNumeric,AItem.Data);
  end;

  procedure ShowBoolean;
  begin
    ShowUniqueTab(TabBoolean,AItem.Data);

    CBTrue.Checked:=AItem.BoolFilter.IncludeTrue;
    CBFalse.Checked:=AItem.BoolFilter.IncludeFalse;
  end;

  function NewSelectText(const AData:TDataItem):TSelectTextItems;
  begin
    result:=TSelectTextItems.Create(Self);
    result.OnChanged:=ChangedText;
    result.OnChecked:=CheckedText;

    result.Refresh(AData);
  end;

  procedure ShowText;
  begin
    if ITextInclude=nil then
    begin
      ITextInclude:=NewSelectText(AItem.Data);
      TUICommon.AddForm(ITextInclude,TabIncluded);
    end;

    if ITextExclude=nil then
    begin
      ITextExclude:=NewSelectText(AItem.Data);
      TUICommon.AddForm(ITextExclude,TabExcluded);
    end;

    LBTextStyle.ItemIndex:=Ord(AItem.Text.Style);
    EText.Text:=AItem.Text.Text;
    CBTextCase.Checked:=AItem.Text.CaseSensitive;

    ShowUniqueTab(TabText,AItem.Data);
  end;

begin
  Item:=AItem;

  PageItem.Visible:=(Item<>nil) and (Item.Data<>nil) and (Item.Data.Kind<>TDataKind.dkUnknown);

  if PageItem.Visible then
  begin
    IChanging:=True;
    try
      case AItem.Data.Kind of
        dkInt32,
        dkInt64,
        dkSingle,
        dkDouble,
        dkExtended: ShowNumeric;
            dkText: ShowText;
        dkDateTime: ShowDateTime;
         dkBoolean: ShowBoolean;
      end;
    finally
      IChanging:=False;
    end;
  end;
end;

class function TFilterItemEditor.Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AItem:TFilterItem):TFilterItemEditor;
begin
  result:=TFilterItemEditor.Create(AOwner);
  TUICommon.AddForm(result,AParent);

  result.Refresh(AItem);
end;

procedure TFilterItemEditor.ETextChange(Sender: TObject);
begin
  if not IChanging then
  begin
    Item.Text.Text:=EText.Text;
    DoChanged;
  end;
end;

procedure TFilterItemEditor.FormCreate(Sender: TObject);
begin
  PageNumeric.ActivePage:=TabNumericRange;
end;

procedure TFilterItemEditor.LBTextStyleClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Item.Text.Style:=TTextFilterStyle(LBTextStyle.ItemIndex);
    DoChanged;
  end;
end;

end.
