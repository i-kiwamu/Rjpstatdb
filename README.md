Rjpstatdb
=========

R interface to statistical database organized by Japanese government (http://statdb.nstac.go.jp/)

How to use it?
--------------

1. Search statistial data

    \> getStatsList("北海道 AND 人口", surveyYears = 2008)

2. Get data

    \> hpop20 <- getStatsData("0003009759")

3. Show the data

    \> print(hpop20)

    \> head(hpop20$data[1])
