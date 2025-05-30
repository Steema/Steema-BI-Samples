unit TeeBIRegisterMenu;

interface

procedure RegisterDataManagerMenu;

implementation

uses
  System.Classes, System.SysUtils, VCL.Menus, ToolsApi,
  TeeBIAbout, VCLBI.DataManager, VCLBI.Grid, BI.DataItem, VCLBI.Menus;
  // DEPRECATED BI.Delphi.Generator, TeeBIRegisterCreator;

type
  TDummyMenu=class
  public
    procedure AboutAction(Sender:TObject);
    procedure DataManagerAction(Sender:TObject);
    procedure DemosAction(Sender:TObject);
    procedure DocumentationAction(Sender:TObject);
    // DEPRECATED procedure GenerateAction(Sender:TObject);
  end;

{ TDummyMenu }

procedure TDummyMenu.AboutAction(Sender:TObject);
begin
  TAboutBI.Show(nil);
end;

procedure TDummyMenu.DataManagerAction(Sender:TObject);
begin
  TDataManager.Edit(nil);
end;

procedure TDummyMenu.DemosAction(Sender:TObject);
begin
  TUICommon.GotoURL(nil,'https://github.com/Steema/BI/tree/master/demos/delphi');
end;

procedure TDummyMenu.DocumentationAction(Sender:TObject);
begin
  TUICommon.GotoURL(nil,'http://www.teechart.net/docs/teebi/tutorials');
end;

{
// DEPRECATED
procedure TDummyMenu.GenerateAction(Sender:TObject);
var tmp : TDataItem;
    tmpStrings : TStrings;
    tmpName : String;
begin
  tmp:=TDataManager.Choose(nil,nil,True);

  if tmp<>nil then
  begin
    if GetActiveProject<>nil then
    begin
      tmpStrings:=TBIDelphiGenerator.FromData(tmp);
      try
        tmpName:=TBIDelphiGenerator.DelphiName(tmp,0);

        (BorlandIDEServices as IOTAModuleServices).CreateModule(
            TGeneratedCreator.Create(tmpStrings.Text,'BI.Generated.'+tmpName+'.pas'));
      finally
        tmpStrings.Free;
      end;
    end;
  end;
end;
}

function FindMenu(const AParent:TMenuItem; const AName:String):TMenuItem;
var t : Integer;
    tmp : TMenuItem;
begin
  result:=nil;

  for t:=0 to AParent.Count-1 do
  begin
    tmp:=AParent.Items[t];

    if SameText(tmp.Name,AName) then
       Exit(tmp)
    else
    begin
      result:=FindMenu(tmp,AName);
      if result<>nil then
         Exit;
    end;
  end;
end;

var
  Dummy : TDummyMenu=nil;
  BIMenu : TMenuItem=nil;

procedure RegisterDataManagerMenu;

  procedure CreateItem(const ACaption,AHint:String; const AClick:TNotifyEvent);
  var MenuItem : TMenuItem;
  begin
    MenuItem:=TBIMenu.NewItem(nil,ACaption,AClick);
    MenuItem.Hint:=AHint;

    BIMenu.Add(MenuItem);
  end;

var NTAServices : INTAServices;
begin
  if Supports(BorlandIDEServices, INTAServices, NTAServices) then
  begin
    if FindMenu(NTAServices.MainMenu.Items,'TeeBI')=nil then
    begin
      BIMenu.Free;

      BIMenu:=TBIMenu.NewItem(nil,'TeeBI',nil);
      BIMenu.Name:='TeeBI';

      CreateItem('Data Manager...','Show the TeeBI Data Manager dialog',
                 Dummy.DataManagerAction);

      // DEPRECATED
      {
      CreateItem('Generate Delphi Unit...',
               'Select data and generate a Delphi unit with its structure',
               Dummy.GenerateAction);
      }

      CreateItem('-','',nil);

      CreateItem('Documentation...','Online documentation',
               Dummy.DocumentationAction);

      CreateItem('Demos...','Online demos',
               Dummy.DemosAction);

      CreateItem('-','',nil);

      CreateItem('About TeeBI...','Show TeeBI About dialog',
               Dummy.AboutAction);

      NTAServices.AddActionMenu('ToolsMenu',nil,BIMenu);

      //AddKeyBinding([ShortCut(Ord('B'), [ssCtrl])], Callback,nil, 0, '', 'MenuItem');
    end;
  end;
end;

initialization
  Dummy:=TDummyMenu.Create;
finalization
  BIMenu.Free;
  BIMenu:=nil;

  Dummy.Free;
  Dummy:=nil;
end.
