## Speed benchmark tests

This project evaluates several common TeeBI features to determine their individual speeds and find potential speed degradations over releases.

The results below were obtained using a 2014 Intel Haswell cpu i7-4770, with no overclocking, single-cpu single-thread only, under Windows 10 x64.

Project compiled using Embarcadero RAD Studio 10 Seattle, for Windows 32bit platform, default Release mode.


| Description | Times | Milliseconds | Times per Second |
| :-- | --: | --: | --: |
| Create and Destroy Table | 100000 | 96 | 1041667 |
| Add Records | 100000 | 2 | 50000000 |
| Delete last Records | 10000 | 1 | 10000000 |
| Delete random Records | 10000 | 345 | 28986 |
| SQL: Select * from Persons where Name="Alex" | 1 | 20 | 50 |
| SQL: Select Average(Salary) from Persons group by Name | 1 | 23 | 43 |
| SQL: Select ID,Salary from Persons order by Salary DESC | 1 | 34 | 29 |
| Save 80000 rows table to stream | 10 | 179 | 56 |
| Load 80000 rows table from stream | 10 | 168 | 60 |

The SQL queries are executed against a 80000 rows table.
