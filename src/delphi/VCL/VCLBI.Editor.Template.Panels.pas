{*********************************************}
{  TeeBI Software Library                     }
{  TBIPanel Dashboard Editor                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Template.Panels;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, VCLBI.Dashboard,
  Vcl.StdCtrls, BI.Dashboard;

type
  TPanelGallery = class(TForm)
    Panel1: TPanel;
    LBPanels: TListBox;
    BIVisual1: TBIVisual;
    Panel2: TPanel;
    BOK: TButton;
    BCancel: TButton;
    Splitter1: TSplitter;
    procedure LBPanelsClick(Sender: TObject);
  private
    { Private declarations }

    FPanels : TPanels;
  public
    { Public declarations }

    class procedure AddPanels(const AItems:TStrings; const APanels:TPanels); static;

    function Current:TBIPanel;

    class function Choose(const AOwner:TComponent;
                          const APanels:TPanels;
                          out APanel:TBIPanel):Boolean; static;

    procedure Refresh(const APanels:TPanels);
  end;

implementation
