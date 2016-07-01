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
