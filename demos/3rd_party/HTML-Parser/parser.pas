{==============================================================================|
| Project : Delphi HTML/XHTML parser module                      | 1.1.2       |
|==============================================================================|
| Content:                                                                     |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.0 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ |
|                                                                              |
| Software distributed under the License is distributed on an "AS IS" basis,   |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for |
| the specific language governing rights and limitations under the License.    |
|==============================================================================|
| Initial Developers of the Original Code are:                                 |
|   Sandbil (Russia) sandbil@ya.ru                                             |
| All Rights Reserved.                                                         |
|   Last Modified:                                                             |
|     25.10.2014, Sandbil                                                      |
|==============================================================================|
| History: see README                                                          |
|==============================================================================|}


unit parser;

interface

uses
  System.Classes, System.RegularExpressionsCore, System.Generics.Collections,
  System.Contnrs, System.StrUtils, System.SysUtils;



type
  TNodeList = class;
  TChildList=class;
  TDomTreeNode = class;



  TDomTree = class
  private
    FCount: Integer;
    fParseErr: TStringList;
    fRootNode: TDomTreeNode;
  public
    constructor Create;
    destructor destroy; override;
    property Count: Integer read fCount;
    property RootNode: TDomTreeNode read fRootNode;
    property ParseErr: TStringList read fParseErr;
  end;

  TDomTreeNode = class(TObject)
  private
   fTag: string;
   fAttributesTxt: string;
   fAttributes: TDictionary<string, string>;
   fText: string;
   fTypeTag: string;
   fChild: TChildList;
   fParent: Pointer;
   fOwner: TDomTree;
  public
    property Tag: string read fTag;
    property AttributesTxt: string read fAttributesTxt;
    property Attributes: TDictionary<string, string> read fAttributes;
    property Text: string read fText;
    property TypeTag: string read fTypeTag;
    property Child: TChildList read fChild;
    property Parent: Pointer read fParent;
    property Owner: TDomTree read fOwner;

    constructor create(hOwner: TDomTree; hParent: Pointer; hTag, hAttrTxt: string; hAttr:
        TDictionary<string, string>; hTypeTag, hText: string);
    destructor destroy; override;
    function FindNode(hNameTag: string; hIndex:integer; hAttrTxt: String;
        hAnyLevel: Boolean; dListNode: TNodeList): Boolean;
    function FindTagOfIndex(hNameTag: String; hIndex:integer; hAnyLevel:
        Boolean; dListNode: TNodeList): Boolean;
    function FindXPath(hXPathTxt: String; dListNode: TNodeList;
        dListValue:TStringList): Boolean;
    function GetAttrValue(hAttrName:string): string;
    function GetComment(hIndex: Integer): string;
    function GetTagName: string;
    function GetTextValue(hIndex:Integer): string;
    function GetXPath(hRelative:boolean): string;
    function RunParse(HtmlTxt: String): Boolean;
  end;

  TChildList = class(TList)
  private
    function Get(Index: Integer): TDomTreeNode;
  public
    destructor Destroy; override;
    property Items[Index: Integer]: TDomTreeNode read Get; default;
  end;

  TNodeList = class(TList)
  private
    function Get(Index: Integer): TDomTreeNode;
  public
    property Items[Index: Integer]: TDomTreeNode read Get; default;
  end;


  PPrmRec=^TPrmRec;
  TPrmRec = record
    TagName: string;
    ind: Integer;
    Attr: string;
    AnyLevel: Boolean;
  end;

  TPrmRecList = class(TList)
  private
    function Get(Index: Integer): PPrmRec;
  public
    destructor Destroy; override;
    property Items[Index: Integer]: PPrmRec read Get; default;
  end;





implementation

{ TDomTree }

{
*********************************** TDomTree ***********************************
}
constructor TDomTree.Create;
begin
  fParseErr:= TStringList.Create;
  fRootnode:= TDomTreeNode.Create(self,self,'Root','',nil,'','');
  FCount:=0;

end;

destructor TDomTree.destroy;
begin
  FreeAndNil(fParseErr);
  FreeAndNil(fRootNode);
  inherited;
end;


{ TChildList }

{
********************************** TChildList **********************************
}
destructor TChildList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
   self[i].Free;
  inherited;
end;


function TChildList.Get(Index: Integer): TDomTreeNode;
begin
   Result := TDomTreeNode(inherited Get(Index));
end;

{ TNodeList }

function TNodeList.Get(Index: Integer): TDomTreeNode;
begin
    Result := TDomTreeNode(inherited Get(Index));
end;


{ TPrmRecList }

{
********************************* TPrmRecList **********************************
}
destructor TPrmRecList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    FreeMem(Items[i]);
  inherited;
end;



function TPrmRecList.Get(Index: Integer): PPrmRec;
begin
  Result := PPrmRec(inherited Get(Index));
end;

{ TDomTreeNode }

{
********************************* TDomTreeNode *********************************
}
constructor TDomTreeNode.create(hOwner: TDomTree; hParent: Pointer; hTag, hAttrTxt: string;
    hAttr: TDictionary<string, string>; hTypeTag, hText: string);
begin
  fChild := TChildList.create;
  fParent := hParent;
  fTag := hTag;
  fAttributesTxt := hAttrTxt;
  fAttributes := hAttr;
  fTypeTag:= hTypeTag;
  fText := hText;
  fOwner:=hOwner;
  inc(hOwner.FCount);
end;

destructor TDomTreeNode.destroy;
begin
FreeAndNil(fAttributes);
FreeAndNil(fChild);
  inherited;
end;

//***********FindAttr*************
//  hNameTag - name Tag
//  hIndex - number of a tag one after another (0 - all tag, 1 - each first ..)
//  hAttrTxt - attribute. ex. alt=1
//  hAnyLevel - true - all levels after start node; false - only one child level after start node
//  dListNode - return TNodeList of TDomTreeNode

function TDomTreeNode.FindNode(hNameTag: string; hIndex:integer; hAttrTxt:
    String; hAnyLevel: Boolean; dListNode: TNodeList): Boolean;
var
RegEx: TPerlRegEx;
i,a: integer;
TagNodeList:TNodeList;
tValue: string;

  Function FindAttrChildNode(aNode:TDomTreeNode;AttrName,AttrValue: String):TNodeList;
  var
   aValue: String;
   j: integer;
  begin
    for j := 0 to aNode.Child.Count - 1 do
    begin
      if aNode.Child[j].Attributes <> nil then
        if aNode.Child[j].Attributes.ContainsKey(AttrName) then
          if aNode.Child[j].Attributes.TryGetValue(AttrName, aValue) then
            if AttrValue = aValue then  dListNode.Add(aNode.Child[j]);
      if hAnyLevel then
      FindAttrChildNode(aNode.Child[j], AttrName, AttrValue);
    end;
    result:=dListNode;
  end;

begin
    RegEx:=nil;
 try
    result:=false;
    RegEx := TPerlRegEx.create;
    RegEx.Subject := hAttrTxt;
    RegEx.RegEx   :='([^\s]*?[^\S]*)=([^\S]*".*?"[^\S]*)|'+
                    '([^\s]*?[^\S]*)=([^\S]*#39.*?#39[^\S]*)|'+
                    '([^\s]*?[^\S]*)=([^\S]*[^\s]+[^\S]*)|'+
                    '(autofocus[^\S]*)()|'+
                    '(disabled[^\S]*)()|'+
                    '(selected[^\S]*)()';

    if (not (hAttrTxt = '')) and (RegEx.Match) then
       begin
        for i := 1 to RegEx.GroupCount do
           if trim(RegEx.Groups[i]) <> '' then break;
        if hNameTag = '' then
           begin
              if FindAttrChildNode(self,RegEx.Groups[i],RegEx.Groups[i+1]).Count>0
                 then result:=true;
           end
        else
          begin
             TagNodeList:=TNodeList.Create;
             if FindTagOfIndex(hNameTag,hIndex,hAnyLevel,TagNodeList) then
                for a := 0 to TagNodeList.Count - 1 do
                  if TagNodeList[a].Attributes <> nil then
                     if TagNodeList[a].Attributes.ContainsKey(RegEx.Groups[i]) then
                       if TagNodeList[a].Attributes.TryGetValue(RegEx.Groups[i], tValue) then
                       //There was a strong compareson of values of attribute
                       // if RegEx.Groups = tValue)
                          if pos(RegEx.Groups[i+1],tValue)>0
                             then
                              begin
                                dListNode.Add(TagNodeList[a]);
                                result:=true;
                              end;
             TagNodeList.Free;
          end;
       end
       else
           if hAttrTxt = '' then
             begin
               TagNodeList:=TNodeList.Create;
               if FindTagOfIndex(hNameTag,hIndex,hAnyLevel,TagNodeList) then
                  for a := 0 to TagNodeList.Count - 1 do
                                begin
                                  dListNode.Add(TagNodeList[a]);
                                  result:=true;
                                end;
               TagNodeList.Free;
             end
           else raise Exception.create('Attribute not found: '+ hAttrTxt );

 finally
   RegEx.free
 end;
end;

//***********FindTagOfIndex*************
//  hNameTag - name Tag (* - any tag, except text tag)
//  hIndex - number of a tag one after another (0 - all tag, 1 - each first ..)
//  hAnyLevel - true - all level after start node; false - only one child level after start node
//  dListNode - return TNodeList of TDomTreeNode

function TDomTreeNode.FindTagOfIndex(hNameTag: String; hIndex:integer;
    hAnyLevel: Boolean; dListNode: TNodeList): Boolean;

  function SubStringOccurences(const subString, sourceString : string; caseSensitive : boolean) : integer;
var
   pEx: integer;
   sub, source : string;
begin
   if caseSensitive then
   begin
     sub := subString;
     source := sourceString;
   end
   else
   begin
     sub := LowerCase(subString);
     source := LowerCase(sourceString);
   end;

   result := 0;
   pEx := PosEx(sub, source, 1);
   while pEx <> 0 do
   begin
     Inc(result);
     pEx := PosEx(sub, source, pEx + Length(sub));
   end;
end;

  Function FindChildTagOfIndex(aNode:TDomTreeNode):TNodeList;
  var
   countNode,j: integer;
   enumTags:string;
  begin
   countNode:=0;
   for j := 0 to aNode.Child.Count - 1 do
     begin
      if hNameTag <> '*' then
          begin
            if ((AnsiUpperCase(aNode.Child[j].Tag) = AnsiUpperCase(hNameTag)) and (aNode.Child[j].TypeTag <> '</%s>'))
            or ((AnsiUpperCase(aNode.Child[j].Tag) = '') and (AnsiUpperCase(hNameTag)='TEXT()') and (aNode.Child[j].Text <> ''))
            or ((LeftStr(AnsiUpperCase(aNode.Child[j].Tag),4) = '<!--') and (AnsiUpperCase(hNameTag)='COMMENT()'))
               then
               begin
                 Inc(countNode);
                 if (countNode =  hIndex ) or (hIndex = 0) then dListNode.Add(aNode.Child[j])
               end;
            if (hAnyLevel) and (aNode.Child.Count > 0) then  FindChildTagOfIndex(aNode.Child[j]) ;
          end
      else
          begin
            if (aNode.Child[j].TypeTag <> '</%s>')  then
               begin
                 enumTags:=enumTags + AnsiUpperCase(aNode.Child[j].Tag)+',';

                 if (SubStringOccurences(AnsiUpperCase(aNode.Child[j].Tag)+',',enumTags, false) =  hIndex ) or (hIndex = 0) then dListNode.Add(aNode.Child[j])
               end;
            if (hAnyLevel) and (aNode.Child.Count > 0) then  FindChildTagOfIndex(aNode.Child[j]) ;
          end;
     end;
   result:=dListNode;
  end;


begin
   result:=false;
   if FindChildTagOfIndex(self).Count > 0
      then result:=true;
end;

function TDomTreeNode.FindXPath(hXPathTxt: String; dListNode: TNodeList;
    dListValue:TStringList): Boolean;
  var
  RegExXPath, RegExXPathElmt: TPerlRegEx;
  i: integer;
  NextAnyLevel:boolean;
  PrmXPath:TPrmRecList;
  PrmXPathSTR: String;
  PrmCount:integer;
  procedure MatchXpath(Context,mTxtElmt:string)  ;
  var
  Prm: PPrmRec;
  begin
     if (Context='/') and (trim(mTxtElmt)='') then NextAnyLevel:=true
     else if (Context='/') and (trim(mTxtElmt)='..') then
       begin
         New(prm);
         Prm.TagName:='..';
         Prm.ind:=0;
         Prm.Attr:='';
         Prm.AnyLevel:=false;
         PrmXPath.Add(Prm);
       end
     else
       begin
         RegExXPathElmt.Options := [preCaseLess];
         RegExXPathElmt.Subject:=trim(mTxtElmt);
         RegExXPathElmt.RegEx:='^([\.\*@A-Z][-A-Z0-9\(\)]*)\[?([0-9]*)\]?\[?@?([^\]]*)';
         if RegExXPathElmt.Match  then
            begin
              New(prm);
              Prm.TagName:=RegExXPathElmt.Groups[1];
              if not TryStrToInt( RegExXPathElmt.Groups[2], Prm.ind ) then Prm.ind:=0;
              Prm.Attr:=RegExXPathElmt.Groups[3];
              Prm.AnyLevel:=NextAnyLevel;
              if (Context='/') then NextAnyLevel:=False;
              PrmXPath.Add(Prm);
            end
         else
            raise Exception.create('XPath is not correct '+ Context + mTxtElmt );
     end;
  end;

  Function FindWithPrm(cPrm:integer; CurNode:TDomTreeNode; dListNode: TNodeList) : boolean;
  var
  i: integer;
  cLNode: TNodeList;
  begin
     result:=false;
     if PrmXPath[cPrm].TagName = '..' then
          FindWithPrm(cPrm + 1,CurNode.Parent, dListNode)
     else
     begin
       cLNode:=TNodeList.Create;
       if CurNode.FindNode(PrmXPath[cPrm].TagName,PrmXPath[cPrm].ind,PrmXPath[cPrm].Attr,PrmXPath[cPrm].AnyLevel,cLNode) then
          for I := 0 to cLNode.Count - 1 do
              if cPrm < PrmCount then
                  FindWithPrm(cPrm + 1,cLNode[i], dListNode)
              else  dListNode.Add(cLNode[i]) ;
        cLNode.free;
     end;
     if dListNode.Count > 0 then result:=true
  end;
begin
   PrmXPath:=nil;
   RegExXPath:=nil;
   RegExXPathElmt:=nil;
try
    result:=false;
    NextAnyLevel:=false;
    PrmXPath:=TPrmRecList.Create;
    PrmXPathSTR:='';
    RegExXPath := TPerlRegEx.create;
    RegExXPathElmt := TPerlRegEx.create;

    RegExXPath.Subject:= hXPathTxt;
    RegExXPath.RegEx:='(/)([\*@]?[^/]*)';
    if RegExXPath.Match then
      begin
        MatchXpath(RegExXPath.Groups[1],RegExXPath.Groups[2]);
        while RegExXPath.MatchAgain do
           MatchXpath(RegExXPath.Groups[1],RegExXPath.Groups[2]);
        for i := 0 to PrmXPath.Count-1 do
            PrmXPathSTR:=PrmXPathSTR + PrmXPath[i].TagName +',' + inttostr(PrmXPath[i].ind) +',' + PrmXPath[i].Attr+',' + BoolToStr(PrmXPath[i].AnyLevel,True)+chr(13)+chr(10);

        if PrmXPath.Count > 0 then
          begin
            if (PrmXPath[PrmXPath.Count-1].TagName[1]='@')
               then
                 begin
                    PrmCount:= PrmXPath.Count - 2;
                    PrmXPath[PrmXPath.Count-1].TagName:=AnsiReplaceStr(PrmXPath[PrmXPath.Count-1].TagName,'@','');
                    if FindWithPrm(0,self,dListNode) then
                       begin
                         for I := 0 to dListNode.Count-1 do
                             if dListNode[i].GetAttrValue(PrmXPath[PrmXPath.Count-1].TagName)<>''  then
                                 dListValue.Add(dListNode[i].GetAttrValue(PrmXPath[PrmXPath.Count-1].TagName));
                         if dListValue.Count > 0 then result:= true
                         else result:=false;
                       end
                    else result:=false;
                 end
               else
                 begin
                    PrmCount:= PrmXPath.Count - 1;
                    result:= FindWithPrm(0,self,dListNode);
                    if   (AnsiLowerCase(PrmXPath[PrmXPath.Count-1].TagName)='comment()')
                      or (AnsiLowerCase(PrmXPath[PrmXPath.Count-1].TagName)='text()') then
                         for I := 0 to dListNode.Count-1 do
                         begin
                           if (AnsiLowerCase(PrmXPath[PrmXPath.Count-1].TagName)='text()')
                              then dListValue.Add(dListNode[i].Text)
                           else    dListValue.Add(TDomTreeNode(dListNode[i]).Tag) ;

                         end;
                 end;
          end
        else raise Exception.create('XPath is not correct or empty.');
      end
    else raise Exception.create('XPath is not correct or empty.');
finally
    PrmXPath.Free;
    RegExXPath.Free;
    RegExXPathElmt.Free;
end;

end;

function TDomTreeNode.GetAttrValue(hAttrName:string): string;
begin
   result:='';
   if self.Attributes <> nil then
      if self.Attributes.ContainsKey(hAttrName) then
         if not self.Attributes.TryGetValue(hAttrName, result) then
            result:='';

end;

function TDomTreeNode.GetComment(hIndex: Integer): string;
var
   countNode,j: integer;
begin
   result:='';
   countNode:=0;
   for j := 0 to self.Child.Count - 1 do
      if (LeftStr(self.Child[j].Tag,4) = '<!--') and
            (self.Child[j].TypeTag = '%s') and
                 (self.Child[j].Text = '')
          then
             begin
                 Inc(countNode);
                 if (countNode =  hIndex ) or (hIndex = 0) then
                    begin
                       result:= self.Child[j].Tag;
                       break;
                    end;
             end;
end;

function TDomTreeNode.GetTagName: string;
begin
 if self.TypeTag='</%s>' then
     result:= format(AnsiReplaceStr(self.TypeTag,'/',''),[self.Tag  + ' ' +  self.AttributesTxt] )
 else
     result:= format(self.TypeTag,[self.Tag  + ' ' +  self.AttributesTxt] );
end;

function TDomTreeNode.GetTextValue(hIndex:Integer): string;
var
   countNode,j: integer;
begin
   result:='';
   countNode:=0;
   for j := 0 to self.Child.Count - 1 do
      if (self.Child[j].Tag = '') and
            (self.Child[j].TypeTag = '') and
                 (self.Child[j].Text <> '')
          then
             begin
                 Inc(countNode);
                 if (countNode =  hIndex ) or (hIndex = 0) then
                    begin
                      result:= self.Child[j].Text;
                      break;
                    end;
             end;
end;

function TDomTreeNode.GetXPath(hRelative:boolean): string;

function GetCountTag(Node: TDomTreeNode): string;
var
CountNode, nNode, i: integer;
begin
  CountNode:=0;
  result:= '';
  if TObject(Node.Parent) is TDomTreeNode then
     begin
       for i:=0 to TDomTreeNode(Node.Parent).Child.Count - 1 do
          begin
            if (Node.Tag = TDomTreeNode(Node.Parent).Child[i].Tag)
              or ((LeftStr(Node.Tag,4)='<!--') and (LeftStr(TDomTreeNode(Node.Parent).Child[i].Tag,4)='<!--'))
                then
               inc(CountNode);
            if Node = TDomTreeNode(Node.Parent).Child[i]  then
               nNode:= CountNode;
          end;
       if (CountNode <> nNode) or ((CountNode = nNode) and (CountNode > 1)) then
           result:= format('[%d]',[nNode]);
     end;
end;

function GetParent(Node: TDomTreeNode): string;
begin
  if TObject(Node.Parent) is TDomTreeNode then
     begin
        if (hRelative) and (TDomTreeNode(Node.Parent).GetAttrValue('id') <>'') then
                result:=format('//*[@id=%s]',[TDomTreeNode(Node.Parent).GetAttrValue('id')])+
                         '/' + result
             else
               result:=GetParent(Node.Parent)+
                       TDomTreeNode(Node.Parent).Tag + GetCountTag(Node.Parent) + '/' + result
     end
  else result:='.'+result;
end;


begin
  if (LeftStr(self.Tag,2) <> '<?') and (LeftStr(self.Tag,9) <> '<!DOCTYPE') then
  begin
     if LeftStr(self.Tag,4) = '<!--' then result:='comment()'
     else if self.Tag <> '' then result:=self.Tag
           else  result:='text()';
     result:=GetParent(self) +  result + GetCountTag(self);
     if result[1]='.' then
        result:='.'+RightStr(result, length(result)-pos('/',result,1)+1);

  end
  else  result:='';

end;

function TDomTreeNode.RunParse(HtmlTxt: String): Boolean;
var
  RegExHTML, RegExTag: TPerlRegEx;
  prev, ErrParseHTML, ind: integer;
  ChildTree: TDomTreeNode;
  HtmlUtf8, RegExException: string;
  tag_txt: TArray<String>;

  function getAttr(mAttrTxt: string): TDictionary<string, string>;
  var
  CheckAttr: String;
    procedure MatchAttr;
    var
      i: integer;
    begin
      CheckAttr := StuffString(CheckAttr,RegExTag.MatchedOffset+1, RegExTag.MatchedLength, StringOfChar(' ',RegExTag.MatchedLength));
      for i := 1 to RegExTag.GroupCount do
        if trim(RegExTag.Groups[i]) <> '' then
        begin
          try
            result.Add(trim(RegExTag.Groups[i]), trim(RegExTag.Groups[i + 1]));
          except
            on E: Exception do
              Owner.fParseErr.Add('Warning: not add Attributtes ' +
                E.ClassName + ' : ' + E.Message + 'Sourse string: ' + mAttrTxt +
                ';' + chr(13)+chr(10)+' attributtes: ' + RegExTag.Groups[i]);
          end;
          break;
        end;
    end;

  begin
    try
      result := TDictionary<string, string>.create;
      if trim(mAttrTxt) <> '' then
      begin
        RegExTag.Subject := mAttrTxt;
        CheckAttr :=   mAttrTxt;
        RegExTag.Options := [preCaseLess, preMultiLine, preSingleLine];
        RegExTag.Replacement:='';
        // here RegExp for processing attributes of tags
        // First not Empty - attribute, next - value
        RegExTag.RegEx :='([^\s]*?[^\S]*)=([^\S]*".*?"[^\S]*)|'+
                         '([^\s]*?[^\S]*)=([^\S]*'#39'.*?'#39'[^\S]*)|'+
                         '([^\s]*?[^\S]*)=([^\S]*[^\s]+[^\S]*)|'+
                         '(allowTransparency[^\S]*)()|'+
                         '(allowfullscreen[^\S]*)()|'+
                         '(novalidate[^\S]*)()|'+
                         '(autofocus[^\S]*)()|'+
                         '(itemscope[^\S]*)()|'+
                         '(disabled[^\S]*)()|'+
                         '(readonly[^\S]*)()|'+
                         '(selected[^\S]*)()|'+
                         '(checked[^\S]*)()|'+
                         '(pubdate[^\S]*)()|'+
                         '(nowrap[^\S]*)()|'+
                         '(hidden[^\S]*)()|'+
                         '(async[^\S]*)()';
        if RegExTag.Match then
        begin
          MatchAttr;
          while RegExTag.MatchAgain do
            MatchAttr;
          // ***Start Check Parsing Tag Attributes Error****
          if Length(Trim(CheckAttr)) > 0 then
            Owner.fParseErr.Add('Warning: parsed not all attributes, ' +
              'sourse string: ' + mAttrTxt + chr(13)+chr(10)+
              'not parsed string: ' + Trim(CheckAttr));
          // ***End Check Parsing Tag Attributes Error************
        end
        else
          Owner.fParseErr.Add('Attributtes not found - ' +
            'Sourse string: ' + mAttrTxt);
      end;
    except
      on E: Exception do
        Owner.fParseErr.Add('Attributtes - ' + E.ClassName + ' : ' +
          E.Message + 'Sourse string: ' + mAttrTxt);
    end;
  end;

  function getTagTxt(mTxt: string): TArray<String>;
  begin
    try
      SetLength(result, 4);
      result[0] := ''; // name tag
      result[1] := ''; // text attributes
      result[2] := ''; // text value following for tag
      result[3] := ''; // type tag
        if LeftStr(trim(mTxt),2) = '</'         then result[3] :='</%s>'  //close
          else if RightStr(trim(mTxt),2) = '/>' then result[3] :='<%s/>'  //selfclose
          else if LeftStr(trim(mTxt),2) = '<!'  then result[3] :='%s'
          else if LeftStr(trim(mTxt),2) = '<?'  then result[3] :='%s'
               else                                  result[3] :='<%s>';  // open
      RegExTag.Subject := mTxt;
      RegExTag.Options := [preCaseLess, preMultiLine, preSingleLine];
      // here RegExp for processing HTML tags
      // Group 1- tag, 2- attributes, 3- text
      RegExTag.RegEx := '<([/A-Z][:A-Z0-9]*)\b([^>]*)>([^<]*)';
      if RegExTag.Match then
         begin
          // ****************Start Check Parsing HTML Tag Error************
          if mTxt <> '<' + RegExTag.Groups[1] + RegExTag.Groups[2] + '>' +  RegExTag.Groups[3] then
                   Owner.fParseErr.Add('Check error Tags parsing - ' + 'Sourse string: ' + mTxt);
          // ****************End Check Parsing HTML Tag Error************
          result[0] := trim(RegExTag.Groups[1]);
          if trim(RegExTag.Groups[2])<> '' then
             if RightStr(trim(RegExTag.Groups[2]),1)= '/' then
                result[1] := leftStr(trim(RegExTag.Groups[2]),length(trim(RegExTag.Groups[2]))-1)
             else  result[1] := trim(RegExTag.Groups[2]);
          result[2] := trim(RegExTag.Groups[3]);
      end
      else
        result[0] := trim(mTxt);
    except
      on E: Exception do
        Owner.fParseErr.Add('Tags - ' + E.ClassName + ' : ' + E.Message +
          'Sourse string: ' + mTxt);
    end;
  end;

  function getPairTagTxt(mTxt, mPattern: string): TArray<String>;
  begin
    try
      SetLength(result, 4);
      result[0] := ''; // name tag
      result[1] := ''; // text attributes
      result[2] := ''; // text value following for tag
      result[3] := ''; // close tag

      RegExTag.Subject := mTxt;
      RegExTag.Options := [preCaseLess, preMultiLine, preSingleLine];
      // here RegExp for processing HTML tags
      // Group 1- tag, 2- attributes, 3- text
      RegExTag.RegEx := mPattern;
      if RegExTag.Match then
         begin
          // ****************Start Check Parsing HTML Tag Error************
          if trim(mTxt) <> '<' + RegExTag.Groups[1] + RegExTag.Groups[2] + '>' +  RegExTag.Groups[3] +  '<' +RegExTag.Groups[4] +'>' then
                   Owner.fParseErr.Add('Check error Exception Tags parsing - ' + 'Sourse string: ' + mTxt);
          // ****************End Check Parsing HTML Tag Error************
          result[0] := trim(RegExTag.Groups[1]);
          result[1] := trim(RegExTag.Groups[2]);
          result[2] := trim(RegExTag.Groups[3]);
          result[3] := trim(RegExTag.Groups[4]);
      end
      else
        result[0] := mTxt;
    except
      on E: Exception do
        Owner.fParseErr.Add('Exception Tags - ' + E.ClassName + ' : ' + E.Message +
          'Sourse string: ' + mTxt);
    end;
  end;

  Function CheckParent(aChildTree: TDomTreeNode; tTag: string):TDomTreeNode;
  var
    ParentTag: string;
  begin
    result := aChildTree.Parent;
    if tTag = '<%s>' then
       result := aChildTree
    else if tTag = '</%s>' then
      if TObject(TDomTreeNode(aChildTree.Parent).Parent) is TDomTreeNode then
      begin
        ParentTag := TDomTreeNode(aChildTree.Parent).Tag;
        if ParentTag = RightStr(aChildTree.Tag, length(aChildTree.Tag) - 1) then
          result := TDomTreeNode(aChildTree.Parent).Parent
      end;
  end;

  procedure MatchTag(mTxtMatch:string);
    var
    ExceptTag:  string;
  begin
    // tag without close tag
      ExceptTag :=
      ',META,LINK,IMG,COL,AREA,BASE,BASEFONT,ISINDEX,BGSOUNDCOMMAND,PARAM,INPUT,EMBED,FRAME,BR,WBR,HR,TRACK,';

     if (leftstr(mTxtMatch, 4) = '<!--') then
        begin
          tag_txt[0] := trim(mTxtMatch);
          tag_txt[1] := '';
          tag_txt[2] := '';
          tag_txt[3] := '%s';
           ChildTree.Child.Add(TDomTreeNode.create(ChildTree.Owner,ChildTree, tag_txt[0], '', nil, '%s','')) ;
        end
     else if (AnsiUpperCase(leftstr(mTxtMatch, 7)) = '<TITLE>')        // tag with any symbol
          or (AnsiUpperCase(leftstr(mTxtMatch, 10)) = '<PLAINTEXT>')
          or (AnsiUpperCase(leftstr(mTxtMatch, 5)) = '<XMP>')
          or (AnsiUpperCase(leftstr(mTxtMatch, 7)) = '<SCRIPT')
          or (AnsiUpperCase(leftstr(mTxtMatch, 9)) = '<TEXTAREA')
          //or (AnsiUpperCase(leftstr(mTxtMatch, 4)) = '<PRE')
          then
        begin
           tag_txt := getPairTagTxt(mTxtMatch,'<([A-Z][A-Z0-9]*)\b([^>]*?)>(.*)<(/\1)>');
           ind:=ChildTree.Child.Add(TDomTreeNode.create(ChildTree.Owner,ChildTree, tag_txt[0], tag_txt[1], getAttr(tag_txt[1]), '<%s>','')) ;
           if tag_txt[2] <> '' then ChildTree.Child[ind].Child.Add(TDomTreeNode.create(ChildTree.Owner,ChildTree.Child[ind], '', '', nil, '', tag_txt[2]));
           ChildTree.Child[ind].Child.Add(TDomTreeNode.create(ChildTree.Owner,ChildTree.Child[ind], tag_txt[3], '', nil, '</%s>','')) ;
        end
     else
        begin
          tag_txt := getTagTxt(mTxtMatch);
          ind := ChildTree.Child.Add(TDomTreeNode.create(ChildTree.Owner,ChildTree, tag_txt[0], tag_txt[1], getAttr(tag_txt[1]), tag_txt[3],''));
          if (pos(',' + AnsiUpperCase(trim(tag_txt[0])) + ',', ExceptTag) = 0)
             and (LeftStr(tag_txt[0],2) <> '<?')
             and (LeftStr(tag_txt[0],2) <> '<!') then
             ChildTree := CheckParent(ChildTree.Child[ind],tag_txt[3]);
          if tag_txt[2] <> '' then
             ChildTree.Child.Add(TDomTreeNode.create(ChildTree.Owner,ChildTree, '', '', nil, '',tag_txt[2]));

        end;
  end;
// ***************************  START PARSE HTML*************************
begin
   RegExHTML:=nil;
   RegExTag:=nil;
  try
    HtmlUtf8 := HtmlTxt;
    RegExHTML := TPerlRegEx.create;
    RegExTag := TPerlRegEx.create;
    ErrParseHTML:=0;
    RegExHTML.Options := [preCaseLess, preMultiLine, preSingleLine];
    ChildTree := self;
    with RegExHTML do
    begin


      // *********RegExp for parsing HTML**************
      // (<title>.*</title>[^<]*) - title
      // (<\!--.+?-->[^<]*)       - comment
      // (<script.*?</script>[^<]*) - script
      // (<[^>]+>[^<]*)           - all remaining  tags
      // [^<]*                   - text
      RegExException :='(<PLAINTEXT>.*?</PLAINTEXT>[^<]*)|'+
                       '(<title>.*?</title>[^<]*)|'+
                       '(<xmp>.*?</xmp>[^<]*)|'+
                       '(<script.*?</script>[^<]*)|'+
                       '(<textarea.*?</textarea>[^<]*)|'+
//                       '(<pre.*?</pre>[^<]*)|'+
                       '(<!--.+?-->[^<]*)|';
      RegEx := RegExException + '(<[^>]+>[^<]*)'; // all teg and text
      Subject := HtmlUtf8;
      if Match then
      begin
        MatchTag(RegExHTML.MatchedText);
        prev := MatchedOffset + MatchedLength;
        while MatchAgain do
        begin
          MatchTag(RegExHTML.MatchedText);
          // *****Start Check Parsing HTML Error************
          if MatchedOffset - prev > 0 then
          begin
            Owner.fParseErr.Add(IntToStr(ErrParseHTML) + '- Check error found after HTML parsing');
            inc(ErrParseHTML)
          end;
          prev := MatchedOffset + MatchedLength;
          // *****End Check Parsing HTML  Error************
        end;
        // ***********End RegExp match cycle************
      end
      else
        raise Exception.create('Input text not contain HTML tags');
      // *************End RegExp match ************
    end;

  Finally
    RegExHTML.Free;
    RegExTag.Free;
    if Owner.FCount>0 then
    result := True
    else result := False ;
  end;

end;

end.
