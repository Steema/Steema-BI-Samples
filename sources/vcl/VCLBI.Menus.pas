unit VCLBI.Menus;

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

uses
  System.Types;

{ TBIMenu }

class procedure TBIMenu.Popup(const APopup: TPopupMenu; const AParent: TControl);
var P : TPoint;
begin
  P.X:=AParent.ClientRect.Left;
  P.Y:=AParent.ClientRect.Bottom;

  P:=AParent.ClientToScreen(P);
  APopup.Popup(P.X,P.Y);
end;

class function TBIMenu.Add(const AMenu:TMenu; const ACaption:String;
                           const AClick:TNotifyEvent):TMenuItem;
begin
  result:=NewItem(AMenu,ACaption,AClick);
  AMenu.Items.Add(result);
end;

class function TBIMenu.NewItem(const AOwner:TComponent;
                               const ACaption: String;
                               const AClick: TNotifyEvent): TMenuItem;
begin
  result:=TMenuItem.Create(AOwner);
  result.Caption:=ACaption;
  result.OnClick:=AClick;
end;

class function TBIMenu.Add(const AItem: TMenuItem; const ACaption: String;
  const AClick: TNotifyEvent): TMenuItem;
begin
  result:=NewItem(AItem,ACaption,AClick);
  AItem.Add(result);
end;

class function TBIMenu.AddSeparator(const AMenu:TMenu):TMenuItem;
begin
  result:=Add(AMenu,'-');
end;

end.
