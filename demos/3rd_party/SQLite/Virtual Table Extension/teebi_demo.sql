.load ./teebi
CREATE VIRTUAL TABLE temp.t1 USING teebi(data='DBDemos.Customer');
SELECT * FROM t1;
