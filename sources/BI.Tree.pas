// david@steema.com
// https://github.com/davidberneda/GenericTree
unit BI.Tree;

{
 Generic Tree structure
 ======================

 Basic usage:

 var Root : TNode<String>;

 Root := TNode<String>.Create;
 try
   Root.Add('Hello').Add('World !');
 finally
   Root.Free;
 end;

 The generic type can be pre-declared for easier typing:

 type
   TFloatTree = TNode<Single>;

 var Tree1 : TFloatTree;

 Features:

 Adding nodes using the Add method, returns the new created node:

 var Node : TNode<String>;
 Node := Root.Add('abc');

"Data" property is your own custom data at each node:

 var Node : TNode<TDateTime>;
     When : TDateTime;
 Node := Root.Add(Now);
 When := Node.Data;
 Node.Data := Tomorrow;

  "Count" returns the number of child nodes for a given node:

 var t : Integer;
 t:=Node.Count;

 "Empty" returns True when the "Count" of children nodes is zero:

 var b : Boolean;
 b:=Node.Empty;

 Destroying a node removes it from its parent:

 Node.Free;

 Nodes can be accessed using the default array property:

 Node := Root[3];

 "Clear" removes and destroys all children nodes of a given node (recursively):

 Node.Clear;

 "Index" returns the position of a node in its parent children array, or -1
 if the node is a "root" node.

 var t : Integer;
 t:=Node.Index;

 "Parent" property returns the node that is the parent of a given node, or
 nil if the node is a "root" node.

 var tmp : TNode<String>;
 tmp:=Node.Parent;

 A node can be "detached" from its parent (without destroying it), setting the
 Parent property to nil:

 Node.Parent:=nil;

 A node can also be removed and destroyed using its Parent Delete method:

 Root.Delete(3); // removes and destroys the 4th child of Root

 Traversing nodes (recursively or not) using the ForEach method:

 var Total:Integer;
 Total:=0;
 Root.ForEach(procedure(const Item:TNode<String>) begin Inc(Total); end);

 The "Level" property returns the depth of a node, (the number of parent->parent->...),
 being zero for root nodes that have no parent.

 var t : Integer;
 t:=Node.Level;

}

interface

type
  TInteger=NativeInt;

  TNode<T>=class
  private
    FItems : TArray<TNode<T>>;

    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FParent : TNode<T>;

    procedure Adopt(const Item:TNode<T>);
    function Get(Index:TInteger):TNode<T>; inline;
    function GetIndex:TInteger;
    function GetLevel:TInteger;
    procedure Orphan;
    procedure RemoveItems(const Index, ACount: TInteger);
    procedure SetParent(const Value:TNode<T>);
  protected
    property Items:TArray<TNode<T>> read FItems;
  public
  type
    TNodeProc={$IFNDEF FPC}reference to{$ENDIF} procedure(const Item:TNode<T>);

  var
    Data : T;

    Destructor Destroy; override;

    function Add(const AData:T):TNode<T>; overload;
    function Add:TNode<T>; overload;
    procedure Clear; inline;
    function Count:TInteger; inline;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1);
    function Empty:Boolean; inline;
    procedure ForEach(const AProc:TNodeProc; const Recursive:Boolean=True);

    property Index:TInteger read GetIndex;
    property Item[Index:TInteger]:TNode<T> read Get; default;
    property Level:TInteger read GetLevel;
    property Parent:TNode<T> read FParent write SetParent;
  end;

implementation

{ TNode<T> }

destructor TNode<T>.Destroy;
begin
  Clear;
  Orphan;
  inherited;
end;

function TNode<T>.Add(const AData: T): TNode<T>;
begin
  result:=TNode<T>.Create;
  result.Data:=AData;
  Adopt(result);
end;

procedure TNode<T>.Clear;
begin
  Delete(0,Count);
  FItems:=nil;
end;

function TNode<T>.Count: TInteger;
begin
  result:=Length(FItems);
end;

procedure TNode<T>.RemoveItems(const Index, ACount: TInteger);
begin
  {$IF CompilerVersion>27}
  System.Delete(FItems,Index,ACount); // RAD XE8
  {$ELSE}
  System.Move(FItems[Index+1],FItems[Index],SizeOf(TObject)*ACount);
  SetLength(FItems,Count-1);
  {$ENDIF}
end;

procedure TNode<T>.Delete(const Index, ACount: TInteger);
var t : TInteger;
begin
  if (ACount>0) and (Count>=(Index+ACount)) then
  begin
    for t:=Index to Index+ACount-1 do
    begin
      FItems[t].FParent:=nil;
      FItems[t].Free;
    end;

    RemoveItems(Index,ACount);
  end;
end;

function TNode<T>.Empty: Boolean;
begin
  result:=Count=0;
end;

procedure TNode<T>.ForEach(const AProc: TNodeProc; const Recursive: Boolean);
type
  TTypeItem=TNode<T>;

var t : TInteger;
    N : TTypeItem;
begin
  for t:=0 to Count-1 do
  begin
    N:=FItems[t];
    AProc(N);

    if Recursive then
       N.ForEach(AProc);
  end;
end;

function TNode<T>.Get(Index: TInteger): TNode<T>;
begin
  result:=FItems[Index];
end;

function TNode<T>.GetIndex: TInteger;
var t : Integer;
begin
  if FParent<>nil then
     for t:=0 to FParent.Count-1 do
         if FParent[t]=Self then
            Exit(t);

  result:=-1;
end;

function TNode<T>.GetLevel: TInteger;
begin
  if FParent=nil then
     result:=0
  else
     result:=FParent.Level+1;
end;

function TNode<T>.Add: TNode<T>;
begin
  result:=TNode<T>.Create;
  Adopt(result);
end;

procedure TNode<T>.Adopt(const Item:TNode<T>);
var L: TInteger;
begin
  Item.FParent:=Self;

  // Pending: Capacity
  L:=Count;
  SetLength(FItems,L+1);
  FItems[L]:=Item;
end;

procedure TNode<T>.Orphan;
begin
  if FParent<>nil then
     FParent.RemoveItems(Index,1);
end;

procedure TNode<T>.SetParent(const Value: TNode<T>);
begin
  if FParent<>Value then
  begin
    Orphan;

    FParent:=Value;

    if FParent<>nil then
       FParent.Adopt(Self);
  end;
end;

end.
