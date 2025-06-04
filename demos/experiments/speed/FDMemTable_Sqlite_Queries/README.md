## Experimental test project

### Comparing the speed of FireDAC LocalSQL (SQLite) memory tables and TeeBI

This test project creates an in-memory table, adds 1.000.000 rows (no indexes) and performs a simple query 10 times:

```sql
select * from Persons where Name='Alex' 
```

Tested on an Intel i9 13900HK cpu, compiled with Embarcadero Delphi Athens 12.3, Win32 cpu release mode.

Trying to obtain the best speed for both FireDAC and TeeBI, current results are:


### FireDAC LocalSQL (SQLite)
Create: 45016 msec

Query 10 times: 14831 msec

### TeeBI (pure Pascal)
Create: 20 msec

Query 10 times: 787 msec


In this example, TeeBI is 20x times faster.

