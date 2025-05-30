unit FMXBI.GridForm;

interface

{
  A generic reusable / embeddable form with a BIGrid and a Navigator toolbar
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types,

  FMX.Controls, FMX.Forms,

  {$IF CompilerVersion>25}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, Data.Bind.Controls, Data.DB, FMX.Layouts,
  Fmx.Bind.Navigator, FMX.Grid, FMXBI.Grid, BI.DataItem, Data.Bind.Components,
  Data.Bind.DBScope, FMX.StdCtrls;

type
  TBIGridForm = class(TForm)
    Layout1: TLayout;
    BindNavigator1: TBindNavigator;
    Layout2: TLayout;
    LRow: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    FOnDataChange : TNotifyEvent;

    procedure DataChange(Sender: TObject; Field: TField);
    procedure SetDataItem(const AData:TDataItem);
  public
    { Public declarations }

    Grid : TBIGrid;

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AData:TDataItem):TBIGridForm; static;

    procedure MakeEditable;

    class function Present(const AOwner:TComponent; const AData:TDataItem):TModalResult; overload;

    property OnDataChange:TNotifyEvent read FOnDataChange write FOnDataChange;
  end;

implementation

{$R *.fmx}

uses
  Data.Bind.Grid, FMX.Bind.Grid, BI.UI, FMXBI.Grid.Grid;

procedure TBIGridForm.DataChange(Sender: TObject;
  Field: TField);
var tmp : TDataSet;
begin
  tmp:=Grid.Plugin.DataSource.DataSet;
  LRow.Text:=IntToStr(tmp.RecNo)+'/'+IntToStr(tmp.RecordCount);

  if Assigned(FOnDataChange) then
     FOnDataChange(Self);
end;

type
  TGridAccess=class(TBIFMXGrid);

procedure TBIGridForm.SetDataItem(const AData:TDataItem);
var tmp : TDataSource;
    tmpControl : TObject;
begin
  Grid.BindTo(AData);

  tmpControl:=Grid.Plugin.GetObject;

  if tmpControl is TBIFMXGrid then
  begin
    BindNavigator1.DataSource:=TGridAccess(tmpControl).BindSource;
    tmp:=TGridAccess(tmpControl).BindSource.DataSource;

    if tmp<>nil then
    begin
      tmp.OnDataChange:=DataChange;
      DataChange(tmp,nil);
    end;
  end;
end;

class function TBIGridForm.Embedd(const AOwner: TComponent;
  const AParent: TControl; const AData: TDataItem): TBIGridForm;
begin
  result:=TBIGridForm.Create(AOwner);
  result.SetDataItem(AData);
  TUICommon.AddForm(result,AParent);
end;

procedure TBIGridForm.FormCreate(Sender: TObject);
begin
  Grid:=TBIGrid.Embedd(Self,Self);
end;

procedure TBIGridForm.MakeEditable;
begin
  if Grid.DataSet<>nil then
     Grid.DataSet.ReadOnly:=False;

  Grid.ReadOnly:=False;

  BindNavigator1.VisibleButtons:=BindNavigator1.VisibleButtons+
      [
        TBindNavigateBtn.nbInsert,
        TBindNavigateBtn.nbDelete,
        TBindNavigateBtn.nbEdit,
        TBindNavigateBtn.nbPost
      ];
end;

class function TBIGridForm.Present(const AOwner:TComponent; const AData: TDataItem): TModalResult;
begin
  with TBIGridForm.Create(AOwner) do
  try
    Caption:=AData.Name;
    SetDataItem(AData);
    result:=ShowModal;
  finally
    Free;
  end;
end;

end.
