{*********************************************}
{  TeeBI Software Library                     }
{  TBITemplate Dashboard Loader methods       }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Dashboard.Loader;

interface

(*
 The purpose of TTemplateLoader methods are to import a JSON file or string,
 into a TBITemplate object.

 Example:

 TTemplateLoader.FromJSONFile('c:\mytemplate.json', BIVisual1.Template);

 Once the template has been loaded, we can edit properties, or display an
 existing dashboard in the template:

 BIVisual1.Dashboard:=BIVisual1.Dashboards[0];

 BIVisual1.Dashboard:='Dashboard 1';

*)

uses
  {$IFDEF FPC}
  BI.FPC, Graphics,
  {$ELSE}
  System.UITypes,
  {$ENDIF}
  BI.DataItem, BI.Dashboard, BI.Dashboard.Layouts;

type
  TTemplateLoader=record
  private
  type
    TBaseList=record
    private
      IList : TDataItem;
      IOwnsList : Boolean;

      Index : Integer;

      function GetItem(const AName: String): String; inline;
      procedure SetItem(const AName,AValue: String);
      function GetBoolean(const AName:String; const ADefault:Boolean):Boolean; overload; inline;
      function GetBoolean(const AList:TDataItem; const AName:String; const ADefault:Boolean):Boolean; overload;
      function GetColor(const AName:String):TAlphaColor; overload; inline;
      function GetColor(const AList:TDataItem; const AName:String):TAlphaColor; overload;
      function GetString(const AList:TDataItem; const AName:String; const AIndex:Integer):String; overload;
    public
      function FindItem(const AName:String):TDataItem;
      function FindSubItem(const AName,ASub:String; out AValue:String):Boolean;

      property Item[const AName:String]:String read GetItem write SetItem; default;
    end;

    class function CreateDashboard(const ATemplate:TBITemplate;
                                   const AList:TBaseList):TDashboard; static;
    class function CreateData(const ATemplate:TBITemplate;
                              const AList: TBaseList):TVisualData; static;
    class function CreateItem(const ATemplate:TBITemplate;
                              const ADashboard:TDashboard;
                              const AList:TBaseList):TDashboardItem; static;

    class function CreateLayout(const ATemplate: TBITemplate;
                                const ALayouts:TLayouts;
                                const AList: TBaseList):TLayoutItem; static;

    class function CreatePanel(const ATemplate:TBITemplate;
                               const AList:TBaseList):TBIPanel; static;

    class procedure LoadTemplate(const AData: TDataItem; const ATemplate:TBITemplate); static;

  public
    class procedure FromJSON(const AJSON:String; const ATemplate:TBITemplate); static;

    class function FromJSONFile(const AFile:String):TBITemplate; overload; static;
    class procedure FromJSONFile(const AFile:String; const ATemplate:TBITemplate); overload; static;

    class function ImportJSON(const AText:String):TDataItem; static;

    class procedure ImportLayouts(const ATemplate:TBITemplate;
                                  const AData: TDataItem;
                                  const ALayouts:TLayouts); static;
  end;

implementation
