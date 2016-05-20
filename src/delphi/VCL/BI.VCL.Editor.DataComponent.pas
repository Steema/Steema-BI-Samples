{*********************************************}
{  TeeBI Software Library                     }
{  Component Selector dialog                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.DataComponent;

interface

{
  This dialog can be used at design-time or runtime to select a TComponent
  that can be a provider of "Data".

  The TDataSelector dialog (BI.VCL.DataSelect.pas unit) also uses this dialog.

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Grids, BI.Data, Vcl.Menus;

type
  TFilterEvent=procedure(Sender: TComponent; var Valid:Boolean) of object;

  TDataComponent = class(TForm)
    Tree: TTreeView;
    PopupMenu1: TPopupMenu;
    ViewData1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ViewData1Click(Sender: TObject);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    IComponents : TList<TComponent>;

    //Dummy : TComponent;

    FOnFilter : TFilterEvent;
    FOnSelected : TNotifyEvent;

    function CalcName(const AComponent:TComponent; const AName:String):String;
    function CanAdd(const AComponent:TComponent):Boolean;
    procedure FillTree;
  protected
    FCurrent : TObject;

    procedure Add(const AParent:TTreeNode; const AComponent:TComponent; const AName:String); overload;
    procedure Add(const AParent,AComponent:TComponent); overload;
    function NodeOf(const AObject:TObject):TTreeNode;
    function SelectedHasData:Boolean;
    procedure TryFreeData;
  public
    { Public declarations }

    type
      TBIAddComponent=procedure(const AParent:TTreeNode; const AComponent:TComponent; const AName:String) of object;

    class
       var OnGetDesignerNames : TProc<TBIAddComponent,TComponent>;

    var
      Edited : TComponent;

    function Data(const AOwner:TComponent):TDataItem;

    class function Import(const AOwner:TComponent; const AObject:TObject):TDataItem; static;

    class function Choose(const AOwner:TComponent;
                    const ACurrent:TComponent=nil):TComponent; static;

    procedure Select(const AObject:TObject);

    function Selected:TComponent;
    function SelectedData:TDataItem;

    property OnFilter:TFilterEvent read FOnFilter write FOnFilter;
    property OnSelected:TNotifyEvent read FOnSelected write FOnSelected;
  end;

implementation
