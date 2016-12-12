unit BI.VCL.Menus;

// Menu related helper classes

interface

uses
  System.Classes, VCL.Controls, VCL.Menus;

type
  TBIMenu=record
  public
    class function Add(const AMenu:TMenu; const ACaption:String;
                       const AClick:TNotifyEvent=nil):TMenuItem; overload; static;

    class function Add(const AItem:TMenuItem; const ACaption:String;
                       const AClick:TNotifyEvent=nil):TMenuItem; overload; static;

    class function AddSeparator(const AMenu:TMenu):TMenuItem; static;

    class procedure Popup(const APopup:TPopupMenu; const AParent:TControl); static;

    class function NewItem(const AOwner:TComponent;
                           const ACaption:String;
                           const AClick:TNotifyEvent):TMenuItem; static;
  end;

implementation
