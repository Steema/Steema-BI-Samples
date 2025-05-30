{*********************************************}
{  TeeBI Software Library                     }
{  Generic dialog to edit numeric ranges      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.NumericFromTo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  BI.DataItem, Vcl.ExtCtrls;

(*
  TNumericFromTo dialog is used by TDynamicFilterEditor dialog to edit
  numeric ranges.

  It can also be used standalone without a TDataItem.

*)

type
  TNumericFromTo = class(TForm)
    PanelTracks: TPanel;
    TBFrom: TTrackBar;
    TBTo: TTrackBar;
    EFrom: TEdit;
    LErrorFrom: TLabel;
    LErrorTo: TLabel;
    ETo: TEdit;
    CBFrom: TCheckBox;
    CBTo: TCheckBox;
    CBFromEqual: TComboBox;
    CBToEqual: TComboBox;
    procedure TBFromChange(Sender: TObject);
    procedure TBToChange(Sender: TObject);
    procedure PanelTracksResize(Sender: TObject);
    procedure EFromChange(Sender: TObject);
    procedure EToChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBFromClick(Sender: TObject);
    procedure CBToClick(Sender: TObject);
    procedure CBFromEqualChange(Sender: TObject);
    procedure CBToEqualChange(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    Min,
    Max,
    Range : Extended;

    FOnChange : TNotifyEvent;

    function AsString(const AValue:Extended):String;
    procedure DoChanged;
    procedure EnableFrom;
    procedure EnableTo;
    function GetFrom: Extended;
    function GetTo: Extended;
    function PositionOf(const AValue:Extended):Integer;
    procedure SetFrom(const Value: Extended);
    procedure SetTo(const Value: Extended);
    function TrySetTrack(const S:String; const ATrack:TTrackBar):Boolean;
    function ValueOf(const APosition:Integer):Extended;
    function GetFromEqual: Boolean;
    function GetToEqual: Boolean;
    procedure SetFromEqual(const Value: Boolean);
    procedure SetToEqual(const Value: Boolean);
  protected
    procedure HideTo;
  public
    { Public declarations }

    Float : Boolean;
    DateTime : Boolean;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const OnChange:TNotifyEvent):TNumericFromTo; static;

    function EnabledFrom:Boolean;
    function EnabledTo:Boolean;

    property FromEqual:Boolean read GetFromEqual write SetFromEqual;
    property FromValue:Extended read GetFrom write SetFrom;

    property ToEqual:Boolean read GetToEqual write SetToEqual;
    property ToValue:Extended read GetTo write SetTo;

    procedure Refresh(const AData:TDataItem); overload;
    procedure Refresh(const AMin,AMax,AFrom,ATo:Extended); overload;
  end;

implementation

{$R *.dfm}

uses
  BI.Arrays, VCLBI.Grid, BI.Info, BI.Languages.English;

{ TNumericFromTo }

function TNumericFromTo.TrySetTrack(const S:String; const ATrack:TTrackBar):Boolean;
var tmp : Extended;
    tmpPos : Integer;
begin
  result:=False;

  if TryStrToFloat(S,tmp) then
  begin
    tmpPos:=PositionOf(tmp);

    if (tmpPos>=ATrack.Min) and (tmpPos<=ATrack.Max) then
    begin
      ATrack.Enabled:=True;
      ATrack.Position:=tmpPos;
    end
    else
    begin
      ATrack.Enabled:=False;
      DoChanged;
    end;

    result:=True;
  end;
end;

procedure TNumericFromTo.EToChange(Sender: TObject);
begin
  if not IChanging then
  begin
    if TrySetTrack(ETo.Text,TBTo) then
       EnableTo
    else
       LErrorTo.Caption:='Wrong number';
  end;
end;

procedure TNumericFromTo.EFromChange(Sender: TObject);
begin
  if not IChanging then
  begin
    if TrySetTrack(EFrom.Text,TBFrom) then
       EnableFrom
    else
       LErrorFrom.Caption:='Wrong number';
  end;
end;

class function TNumericFromTo.Embedd(const AOwner: TComponent;
                      const AParent: TWinControl;
                      const OnChange: TNotifyEvent): TNumericFromTo;
begin
  result:=TNumericFromTo.Create(AOwner);
  result.FOnChange:=OnChange;
  TUICommon.AddForm(result,AParent);
end;

procedure TNumericFromTo.FormCreate(Sender: TObject);
begin
  Float:=True;
end;

function TNumericFromTo.GetFromEqual: Boolean;
begin
  result:=CBFromEqual.ItemIndex=0;
end;

function TNumericFromTo.GetFrom: Extended;
begin
  if TBFrom.Enabled then
     result:=ValueOf(TBFrom.Position)
  else
     result:=StrToFloatDef(EFrom.Text,Min);
end;

function TNumericFromTo.GetTo: Extended;
begin
  if TBTo.Enabled then
     result:=ValueOf(TBTo.Position)
  else
     result:=StrToFloatDef(ETo.Text,Max);
end;

function TNumericFromTo.GetToEqual: Boolean;
begin
  result:=CBToEqual.ItemIndex=0;
end;

procedure TNumericFromTo.HideTo;
begin
  CBFrom.Caption:=BIMsg_Enabled;
  TBTo.Hide;
  LErrorTo.Hide;

  CBTo.Hide;
  ETo.Hide;

  CBFrom.Caption:='&Enabled';
  CBFrom.Width:=EFrom.Left-CBFrom.Left-1;

  CBToEqual.Hide;
  CBFromEqual.Hide;
end;

procedure TNumericFromTo.PanelTracksResize(Sender: TObject);
begin
  TBFrom.Width:=PanelTracks.Width-2*TBFrom.Left;
  TBTo.Width:=TBFrom.Width;
end;

function TNumericFromTo.PositionOf(const AValue: Extended): Integer;
begin
  if Range=0 then
     result:=0
  else
     result:=Round(100*(AValue-Min)/Range);
end;

procedure TNumericFromTo.Refresh(const AData: TDataItem);
var tmpMin,
    tmpMax : Extended;
begin
  DateTime:=AData.Kind=TDataKind.dkDateTime;
  Float:=(AData.Kind<>TDataKind.dkInt32) and (AData.Kind<>TDataKind.dkInt64);

  TDataInfo.GetMinMax(AData,tmpMin,tmpMax);
  Refresh(tmpMin,tmpMax,tmpMin,tmpMax);
end;

procedure TNumericFromTo.Refresh(const AMin, AMax, AFrom, ATo: Extended);
begin
  IChanging:=True;
  try
    Min:=AMin;
    Max:=AMax;
    Range:=(AMax-AMin);

    if not Float then
       if Range<>0 then
       begin
         TBFrom.Frequency:=100 div Round(Range);
         TBFrom.LineSize:=1;
         TBFrom.PageSize:=1;

         if TBTo.Visible then
         begin
           TBTo.Frequency:=TBFrom.Frequency;
           TBTo.LineSize:=1;
           TBTo.PageSize:=1;
         end;
       end;

    TBFrom.Position:=PositionOf(AFrom);
    EFrom.Text:=AsString(FromValue);

    if TBTo.Visible then
    begin
      TBTo.Position:=PositionOf(ATo);
      ETo.Text:=AsString(ToValue);
    end;
  finally
    IChanging:=False;
  end;
end;

procedure TNumericFromTo.SetFrom(const Value: Extended);
begin
  TBFrom.Position:=PositionOf(Value);
  EnableFrom;
end;

procedure TNumericFromTo.SetFromEqual(const Value: Boolean);
begin
  if Value then
     CBFromEqual.ItemIndex:=0
  else
     CBFromEqual.ItemIndex:=1;
end;

procedure TNumericFromTo.SetTo(const Value: Extended);
begin
  TBTo.Position:=PositionOf(Value);
  EnableTo;
end;

procedure TNumericFromTo.SetToEqual(const Value: Boolean);
begin
  if Value then
     CBToEqual.ItemIndex:=0
  else
     CBToEqual.ItemIndex:=1;
end;

procedure TNumericFromTo.CBFromClick(Sender: TObject);
begin
  if not IChanging then
  begin
    if not CBFrom.Checked then
    begin
      IChanging:=True;
      try
        TBFrom.Enabled:=True;
        TBFrom.Position:=PositionOf(Min);
        EFrom.Text:=AsString(Min);
      finally
        IChanging:=False;
      end;
    end;

    DoChanged;
  end;
end;

procedure TNumericFromTo.CBFromEqualChange(Sender: TObject);
begin
  DoChanged;
end;

procedure TNumericFromTo.CBToClick(Sender: TObject);
begin
  if not IChanging then
  begin
    if not CBTo.Checked then
    begin
      IChanging:=True;
      try
        TBTo.Enabled:=True;
        TBTo.Position:=PositionOf(Max);
        ETo.Text:=AsString(Max);
      finally
        IChanging:=False;
      end;
    end;

    DoChanged;
  end;
end;

procedure TNumericFromTo.CBToEqualChange(Sender: TObject);
begin
  DoChanged;
end;

procedure TNumericFromTo.DoChanged;
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

function TNumericFromTo.AsString(const AValue:Extended):String;
begin
  if DateTime then
     if Range>1 then
        result:=DateToStr(AValue)
     else
        result:=DateTimeToStr(AValue)
  else
  if Float then
     result:=FormatFloat('#,##0.###',AValue)
  else
     result:=IntToStr(Round(AValue));
end;

function TNumericFromTo.EnabledFrom: Boolean;
begin
  result:=CBFrom.Checked;
end;

function TNumericFromTo.EnabledTo: Boolean;
begin
  result:=CBTo.Checked;
end;

procedure TNumericFromTo.EnableFrom;
begin
  CBFrom.Checked:=True;
  LErrorFrom.Caption:='';
end;

procedure TNumericFromTo.EnableTo;
begin
  CBTo.Checked:=True;
  LErrorTo.Caption:='';
end;

procedure TNumericFromTo.TBFromChange(Sender: TObject);
begin
  if not IChanging then
  begin
    EFrom.Text:=AsString(FromValue);
    EnableFrom;
    DoChanged;
  end;
end;

procedure TNumericFromTo.TBToChange(Sender: TObject);
begin
  if not IChanging then
  begin
    ETo.Text:=AsString(ToValue);
    EnableTo;
    DoChanged;
  end;
end;

function TNumericFromTo.ValueOf(const APosition: Integer): Extended;
begin
  result:=Min+0.01*(APosition*Range);

  if not Float then
     result:=Round(result);
end;

end.
