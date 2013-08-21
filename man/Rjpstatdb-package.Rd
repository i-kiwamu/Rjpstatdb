\name{Rjpstatdb-package}
\alias{Rjpstatdb-package}
\alias{Rjpstatdb}
\docType{package}
\encoding{UTF-8}
\title{
Rjpstatdb
}
\description{
R interface to statistical database organized by Japanese government
(http://statdb.nstac.go.jp/)  
}
\details{
\tabular{ll}{
Package: \tab Rjpstatdb\cr
Type: \tab Package\cr
Version: \tab 0.10\cr
Date: \tab 2013-08-21\cr
License: \tab GPL-2\cr
}
You can search and get data from http://statdb.nstac.go.jp/
}
\author{
Kiwamu Ishikura <ishikura.kiwamu@gmail.com>
}
\references{
http://statdb.nstac.go.jp/
}
\keyword{ Rjpstatdb }
\keyword{ Gateway to Advanced and User-friendly Statistics Services }
\examples{
getStatsList("北海道 AND 人口", surveyYears = 2008)
hpop20 <- getStatsData("0003009759")
head(hpop20[[1L]])
}
