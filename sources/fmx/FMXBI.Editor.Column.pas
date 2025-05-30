{*********************************************}
{  TeeBI Software Library                     }
{  Data Column Editor                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Editor.Column;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.StdCtrls, BI.DataItem, FMX.ListBox;

type
  TColumnEditor = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CBKind: TComboBox;
    Button1: TButton;
    Label4: TLabel;
    CBMaster: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBKindChange(Sender: TObject);
    procedure CBMasterChange(Sender: TObject);
  private
    { Private declarations }
    Data : TDataItem;
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const AData:TDataItem); static;
  end;

implementation

{$R *.fmx}

uses
  BI.Arrays, BI.UI, BI.Convert;

{ TColumnEditor }

type
  TDataItemAccess=class(TDataItem);

procedure TColumnEditor.Button1Click(Sender: TObject);
var tmp : TDataKind;
begin
  tmp:=TDataKind(CBKind.ItemIndex);

  if TDataKindConvert.Convert(Data,tmp) then
  begin
    // Save ?
    // Data.Parent.UnLoadData;
    // TPersist.Save
    // TDataPersistence.Save...

    Button1.Enabled:=False;
    Label2.Text:='Kind: '+Data.Kind.ToString;
  end
  else
    raise EBIException.Create('Error: Cannot convert data: '+Data.FullName+' to: '+ tmp.ToString);
end;

procedure TColumnEditor.CBKindChange(Sender: TObject);
begin
  Button1.Enabled:=CBKind.ItemIndex<>Ord(Data.Kind);
end;

procedure TColumnEditor.CBMasterChange(Sender: TObject);
begin
  if CBMaster.ItemIndex=-1 then
     Data.Master:=nil
  else
     Data.Master:=TDataItem(CBMaster.Items.Objects[CBMaster.ItemIndex]);
end;

class procedure TColumnEditor.Edit(const AOwner: TComponent;
  const AData: TDataItem);
begin
  with TColumnEditor.Create(AOwner) do
  try
    Data:=AData;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TColumnEditor.FormShow(Sender: TObject);
var tmpCols,
    tmpData : TDataItem;
begin
  if Data<>nil then
  begin
    Label1.Text:='Item: '+Data.FullName;
    Label2.Text:='Kind: '+Data.Kind.ToString;

    TCommonUI.AddKinds(CBKind.Items);
    CBKind.ItemIndex:=Ord(Data.Kind);

    CBMaster.Clear;

    tmpCols:=Data.Parent;

    if tmpCols<>nil then
    if tmpCols.Parent<>nil then
    begin
      tmpData:=tmpCols.Parent;

      if tmpData<>nil then
      begin
        TCommonUI.AddItems(tmpData,CBMaster.Items);

        if TDataItemAccess(Data).HasMaster then
           CBMaster.ItemIndex:=CBMaster.Items.IndexOfObject(Data.Master);
      end;
    end;
  end;
end;

end.
