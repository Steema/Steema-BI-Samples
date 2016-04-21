unit BI.Algorithm.TeeFunctions;

interface

uses
  BI.Data, VCLTee.TeEngine, BI.Algorithm.Model;

type
  TDataFunction=class(TBaseAlgorithm)
  private
    FFunction : TTeeFunction;
    FFunctionClass : TTeeFunctionClass;
    FSource : TDataItem;
    procedure SetSource(const Value: TDataItem);
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor CreateFunction(const AFunctionClass:TTeeFunctionClass); overload;
    Constructor CreateFunction(const AFunction:TTeeFunction); overload;

    procedure Calculate; override;

    property Source:TDataItem read FSource write SetSource;
  end;

implementation
