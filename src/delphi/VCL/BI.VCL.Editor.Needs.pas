{*********************************************}
{  TeeBI Software Library                     }
{  TDataProviderNeeds Editor                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Needs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BI.Algorithm, Vcl.ComCtrls,
  BI.Data, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TProviderNeedsEditor = class(TForm)
    PageNeeds: TPageControl;
    PanelBottom: TPanel;
    BApply: TButton;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    procedure BApplyClick(Sender: TObject);
  private
    { Private declarations }

    FOptions : TCustomForm;
    INeeds : TDataProviderNeeds;

    procedure ChangedCombo(Sender:TObject);
    procedure ChangedItems(Sender:TObject);
  public
    { Public declarations }
    class function Embed(const AOwner:TComponent;
                         const AParent: TWinControl;
                         const ANeeds:TDataProviderNeeds;
                         const ASource:TDataItem):TProviderNeedsEditor; static;

    procedure EnableApply;

    procedure RefreshNeeds(const ANeeds:TDataProviderNeeds; const ASource:TDataItem);

    property Needs:TDataProviderNeeds read INeeds;
    property Options:TCustomForm read FOptions;
  end;

  TDataProviderClass=class of TDataProvider;

  TRegisteredProvider=record
  public
    ProviderClass : TDataProviderClass;
    EditorClass : TCustomFormClass;
  end;

  TRegisteredProviders=Array of TRegisteredProvider;

  TRegisteredProvidersHelper=record helper for TRegisteredProviders
  public
    function Count:Integer; inline;

    function EditorOf(const AOwner:TComponent; const AProvider:TDataProvider):TCustomForm;

    function IndexOf(const AClass:TDataProviderClass):Integer;

    class procedure RegisterProvider(const AClass:TDataProviderClass;
                                     const AEditor:TCustomFormClass); static;

    procedure UnRegisterProvider(const AClass:TDataProviderClass);
  end;

var
  RegisteredProviders : TRegisteredProviders=nil;

implementation
