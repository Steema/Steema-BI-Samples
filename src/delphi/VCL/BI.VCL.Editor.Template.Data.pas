{*********************************************}
{  TeeBI Software Library                     }
{  TVisualData Dashboard Editor               }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Template.Data;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.Dashboard, BI.VCL.DataControl, BI.VCL.Grid;

type
  TDataGallery = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BOK: TButton;
    BCancel: TButton;
    LBData: TListBox;
    BIGrid1: TBIGrid;
    Splitter1: TSplitter;
    procedure LBDataClick(Sender: TObject);
  private
    { Private declarations }

    FData : TVisualDataItems;
  public
    { Public declarations }

    class procedure AddData(const AItems:TStrings; const AData:TVisualDataItems); static;

    function Current:TVisualData;

    class function Choose(const AOwner:TComponent;
                          const ADataItems:TVisualDataItems;
                          out AData:TVisualData):Boolean; static;

    procedure Refresh(const ADataItems:TVisualDataItems);
  end;

implementation
