unit BI.Expressions.Samples;

interface

type
  TSampleExpressions=record
  public
    class function Text:String; static;
  end;

implementation

const
  CRLF=#13#10;

  Expressions=
'0           -> 0'+CRLF+
'1           -> 1'+CRLF+
'True        -> True'+CRLF+
'False       -> False'+CRLF+
'-1          -> -1'+CRLF+
'+CRLF+1          -> 1'+CRLF+
'-1E20       -> -1E20'+CRLF+
'-2E3        -> -2000'+CRLF+
'-5.1e-3     -> -0.0051'+CRLF+
'2e10        -> 20000000000'+CRLF+
'1.234       -> 1.234'+CRLF+
'"abc"       -> abc'+CRLF+
'''ab"c''      -> ab"c'+CRLF+
'"a"         -> a'+CRLF+
'"x" + "y"   -> xy'+CRLF+
'''zzzz''      -> zzzz'+CRLF+
'1234        -> 1234'+CRLF+
'-1234       -> -1234'+CRLF+
'123.456e4   -> 1234560'+CRLF+
'1+1         -> 2'+CRLF+
'1.2 + 3.4   -> 4.6'+CRLF+
'1.2+3.4     -> 4.6'+CRLF+
'-3-2        -> -5'+CRLF+
'-3+ -1      -> -4'+CRLF+
'2*2         -> 4'+CRLF+
'6/3         -> 2'+CRLF+
'5/2         -> 2.5'+CRLF+
'(123)       -> 123'+CRLF+
'(1+1)*2     -> 4'+CRLF+
'(2*3)-(2-1) -> 5'+CRLF+
'5^3         -> 125'+CRLF+
'2 = 2       -> True'+CRLF+
'3 = 5       -> False'+CRLF+
'3 > 2       -> True'+CRLF+
'-1>=-2      -> True'+CRLF+
'12.6<=12.6  -> True'+CRLF+
'12.6<12.6   -> False'+CRLF+
'12.6<>12.7  -> True'+CRLF+
'"xx"<>"yy"  -> True'+CRLF+
'"xx"<>"xx"  -> False'+CRLF+
'5<10        -> True'+CRLF+
'"a" + "bc"  -> abc'+CRLF+
'7 >= 9      -> False'+CRLF+
'6 <=2       -> False'+CRLF+
'(1<3) and (4>2) -> True'+CRLF+
'True and True -> True'+CRLF+
'True or false -> True'+CRLF+
'true and not false -> True'+CRLF+
'false or not false -> True'+CRLF+
'not true   -> False'+CRLF+
'not FALSE  -> True'+CRLF+
'''x'' in []  -> False'+CRLF+
'''x'' in ["123"] -> False'+CRLF+
'''x'' in [ ''x'' ] -> True'+CRLF+
'''x'' in ["x"] -> True'+CRLF+
'''x'' in ['''',''a'',''x''] -> True'+CRLF+
'''x'' in [''y'',''z'',''w''] -> False'+CRLF+
'not (23 in [14,15,23,65]) -> False'+CRLF+
'not (''a''=''b'') -> True'+CRLF+
'not (23 in [14.2]) -> True'+CRLF+
'1 in [1,2,3] -> True'+CRLF+
'1 in [4] -> False'+CRLF+
'PI    -> 3.14159265358979'+CRLF+
'abs(3) -> 3'+CRLF+
'abs(1.3) -> 1.3'+CRLF+
'abs(-3) -> 3'+CRLF+
'abs(-1.3) -> 1.3'+CRLF+
'cos(pi) -> -1'+CRLF+
'Sin(0.52359878) -> 0.500000003811985'+CRLF+
'sign(0) -> 0'+CRLF+
'sign(-3) -> -1'+CRLF+
'sign(3) -> 1'+CRLF+
'sqr(5) -> 25'+CRLF+
'Exp( 10 ) -> 22026.4657948067'+CRLF+
'Ln( 6.8 ) -> 2.76553474636298'+CRLF+
'log( 10000) -> 4'+CRLF+
'(5*4) + sqr(3) - sqrt(36) -> 23'+CRLF+
'Date("5/5/2015") -> 5/5/2015'+CRLF+   // <-- Warning, Variant To Date swaps month and day
'Millennium(''11/7/2016'') -> 3'+CRLF+
'Century(''11/7/2016'') -> 21'+CRLF+
'Year("11/7/2016") -> 2016'+CRLF+
'Month("11/7/2016") -> 11'+CRLF+
'Day(''11/7/2016'') -> 7'+CRLF+
'WeekOfYear(''11/7/2016'') -> 45'+CRLF+
'WeekDay(''11/7/2016'') -> 1'+CRLF+
// 'Now ->'+CRLF+ <-- Not possible to test
'Time("10:23:45") -> 10:23:45'+CRLF+ // <-- Warning, Variant AM/PM is not returned
'Hour("10:23:45") -> 10'+CRLF+
'Minute("10:23:45") -> 23'+CRLF+
'Second("10:23:45") -> 45'+CRLF+
'"ABC" contains "B" -> True'+CRLF+
'"" contains "" -> False'+CRLF+
'"ABC" contains ''X'' -> False'+CRLF+
'''Hello World'' contains ''W'' -> True'+CRLF+
'''Hello World'' starts ''He'' -> True'+CRLF+
'''Hello World'' starts ''Wo'' -> False'+CRLF+
'''Hello World'' ends ''ld'' -> True'+CRLF+
'''Hello World'' ends ''Wo'' -> False'+CRLF+
'Lower("AbC") -> abc'+CRLF+
'Upper("aBC") -> ABC'+CRLF+
'Lower("A" + "B") -> ab'+CRLF+
'Length("Hello World") -> 11'+CRLF+
'Trim(" XYZ ") -> XYZ'+CRLF+
'Lower(Trim(" aBC    ")) -> abc'+CRLF+
'IsEmpty("") -> True'+CRLF+
'IsEmpty(Upper("abc")) -> False'+CRLF+
'trunc(1.2) -> 1'+CRLF+
'trunc(1.9) -> 1'+CRLF+
'trunc(-2.2) -> -2'+CRLF+
'round(0.5) -> 0'+CRLF+
'round(0.50001) -> 1'+CRLF+
'round(0.4) -> 0'+CRLF+
'round(0.9) -> 1'+CRLF+
'Power(2) -> 100'+CRLF+
'Power(10,2) -> 100'+CRLF+
'Power(3,3) -> 27'+CRLF+
'Power(1+4,1+(1)) -> 25'+CRLF+
'IndexOf("","abc") -> 0'+CRLF+
'IndexOf("abc","") -> 0'+CRLF+
'IndexOf("a","abc") -> 1'+CRLF+
'IndexOf("b","abc") -> 2'+CRLF+
'IndexOf("c","abc") -> 3'+CRLF+
'IndexOf("bc","abc") -> 2'+CRLF+
'IndexOf("ac","abc") -> 0'+CRLF+
'IndexOf("B","abc",true) -> 0'+CRLF+
'Insert("abd","c",3) -> abcd'+CRLF+
'Remove("abcd",3) -> abd'+CRLF+
'Remove("abcd",3,2) -> ab'+CRLF+
'Replace("abcd","c","C") -> abCd'+CRLF+
'Replace("abBcd","bb","X") -> aXcd'+CRLF+
'Replace("abBcd","bb","X",true) -> abBcd'+CRLF+
'Count("","") -> 0'+CRLF+
'Count("","a") -> 0'+CRLF+
'Count("a","") -> 0'+CRLF+
'Count("a","a") -> 1'+CRLF+
'Count(" X X X ","X") -> 3'+CRLF+
'Count(Trim(" X X X "),"X") -> 3'+CRLF+
'Count(" X X X ","X ") -> 3'+CRLF+
'Count(Trim(" X X X "),"X ") -> 2'+CRLF+
'Count("Hello World","o") -> 2'+CRLF+
'SubString("abcd",2,1) -> b'+CRLF+
'SubString("abcd",2,2) -> bc'+CRLF+
'SubString("abcd",4,10) -> d'+CRLF+
'SubString("abcd",5) ->'+CRLF+
'SubString("abcd",2) -> bcd'+CRLF+
'Split("1,2,3,4",",") -> [1,2,3,4]'+CRLF+
'Split("1 2 3 4") -> [1,2,3,4]';

{ TSampleExpressions }

class function TSampleExpressions.Text: String;
begin
  result:=Expressions;
end;

end.
