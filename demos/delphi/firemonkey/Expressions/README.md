## TExpression Tests and Benchmarks

This example project (made with Firemonkey), shows several test cases of TExpression capabilities.

![TExpression Test Demo](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_TExpression_Tests.png)

The list of available tests is:

0           -&gt; 0

1           -&gt; 1

True        -&gt; True

False       -&gt; False

-1          -&gt; -1

+1          -&gt; 1

-1E20       -&gt; -1E20

-2E3        -&gt; -2000

-5.1e-3     -&gt; -0.0051

2e10        -&gt; 20000000000

1.234       -&gt; 1.234

"abc"       -&gt; abc

'ab"c'      -&gt; ab"c

"a"         -&gt; a

"x" + "y"   -&gt; xy

'zzzz'      -&gt; zzzz

1234        -&gt; 1234

-1234       -&gt; -1234

123.456e4   -&gt; 1234560

1+1         -&gt; 2

1.2 + 3.4   -&gt; 4.6

1.2+3.4     -&gt; 4.6

-3-2        -&gt; -5

-3+ -1      -&gt; -4

2*2         -&gt; 4

6/3         -&gt; 2

5/2         -&gt; 2.5

(123)       -&gt; 123

(1+1)*2     -&gt; 4

(2*3)-(2-1) -&gt; 5

5^3         -&gt; 125

2 = 2       -&gt; True

3 = 5       -&gt; False

3 &gt; 2       -&gt; True

-1&gt;=-2      -&gt; True

12.6&lt;=12.6  -&gt; True

12.6&lt;12.6   -&gt; False

12.6&lt;&gt;12.7  -&gt; True

"xx"&lt;&gt;"yy"  -&gt; True

"xx"&lt;&gt;"xx"  -&gt; False

5&lt;10        -&gt; True

"a" + "bc"  -&gt; abc

7 &gt;= 9      -&gt; False

6 &lt;=2       -&gt; False

(1&lt;3) and (4&gt;2) -&gt; True

True and True -&gt; True

True or false -&gt; True

true and not false -&gt; True

false or not false -&gt; True

not true   -&gt; False

not FALSE  -&gt; True

'x' in []  -&gt; False

'x' in ["123"] -&gt; False

'x' in [ 'x' ] -&gt; True

'x' in ["x"] -&gt; True

'x' in ['','a','x'] -&gt; True

'x' in ['y','z','w'] -&gt; False

not (23 in [14,15,23,65]) -&gt; False

not ('a'='b') -&gt; True

not (23 in [14.2]) -&gt; True

1 in [1,2,3] -&gt; True

1 in [4] -&gt; False

PI    -&gt; 3.14159265358979

cos(pi) -&gt; -1

Sin(0.52359878) -&gt; 0.500000003811985

sqr(5) -&gt; 25

Exp( 10 ) -&gt; 22026.4657948067

Ln( 6.8 ) -&gt; 2.76553474636298

log( 10000) -&gt; 4

(5*4) + sqr(3) - sqrt(36) -&gt; 23

Date('5/6/2015') -&gt; 5/6/2015

Millennium('11/7/2016') -&gt; 3

Century('11/7/2016') -&gt; 21

Year('11/7/2016') -&gt; 2016

Month('11/7/2016') -&gt; 11

Day('11/7/2016') -&gt; 7

Now -&gt;

Time("10:23:45") -&gt; 10:23:45

Hour("10:23:45") -&gt; 10

Minute("10:23:45") -&gt; 23

Second("10:23:45") -&gt; 45

"ABC" contains "B" -&gt; True

"" contains "" -&gt; False

"ABC" contains 'X' -&gt; False

'Hello World' contains 'W' -&gt; True

'Hello World' starts 'He' -&gt; True

'Hello World' starts 'Wo' -&gt; False

'Hello World' ends 'ld' -&gt; True

'Hello World' ends 'Wo' -&gt; False

Lower("AbC") -&gt; abc

Upper("aBC") -&gt; ABC

Lower("A" + "B") -&gt; ab

Length("Hello World") -&gt; 11

Trim(" XYZ ") -&gt; XYZ

Lower(Trim(" aBC    ")) -&gt; abc

IsEmpty("") -&gt; True

IsEmpty(Upper("abc")) -&gt; False

trunc(1.2) -&gt; 1

trunc(1.9) -&gt; 1

trunc(-2.2) -&gt; -2

round(0.5) -&gt; 0

round(0.50001) -&gt; 1

round(0.4) -&gt; 0

round(0.9) -&gt; 1
