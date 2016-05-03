unit BI.VCL.Editor.DataComponent;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Generics.Collections, System.Rtti,
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
  private
    { Private declarations }

    Rtti : TRttiContext;
    tmpDataSource : TRttiType;

    IComponents : TList<TComponent>;

    //Dummy : TComponent;

    FCurrent : TComponent;

    FOnFilter : TFilterEvent;
    FOnSelected : TNotifyEvent;

    procedure Add(const AParent:TTreeNode; const AComponent:TComponent; const AName:String);
    procedure FillTree;
  public
    { Public declarations }

    type
      TBIAddComponent=procedure(const AParent:TTreeNode; const AComponent:TComponent; const AName:String) of object;

    class
       var OnGetDesignerNames : TProc<TBIAddComponent,TComponent>;

    var
      Edited : TComponent;

    function Data:TDataItem;

    class function Import(const AObject:TObject):TDataItem; static;

    class function Select(const AOwner:TComponent;
                    const ACurrent:TComponent=nil):TComponent; static;

    function Selected:TComponent;
    function SelectedData:TDataItem;

    property OnSelected:TNotifyEvent read FOnSelected write FOnSelected;

    property OnFilter:TFilterEvent read FOnFilter write FOnFilter;
  end;

implementation
