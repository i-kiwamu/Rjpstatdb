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

Warnings
--------
このサービスは、次世代統計利用システムのAPI機能を使用していますが、サービスの内容は総務省統計局又は独立行政法人統計センターによって保証されたものではありません。

This services use the API of Gateway to Advanced and User-friendly Statistics Service. The contents of this service are not guranteened by Ministry of Internal Affairs and Communications nor National Statistics Center. 
