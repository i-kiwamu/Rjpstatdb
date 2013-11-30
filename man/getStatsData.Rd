\name{getStatsData}
\alias{getStatsData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get statistical data from http://statdb.nstac.go.jp/
}
\description{
Get statistical data that you specify the data ID from ``Gateway to
Advanced and User-friendly Statistics Service''
(http://statdb.nstac.go.jp/) organized by Japanese government.
}
\usage{
getStatsData(statsDataId = NULL, dataSetId = NULL, limit = NULL,
             lvTab = "", cdTab = NULL, lvTime = "", cdTime = NULL,
             lvArea = "", cdArea = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statsDataId}{
statistical data ID (character)
}
  \item{dataSetId}{
dataset ID you made (character)
}
  \item{limit}{
Maximum number of data to download
}
  \item{lvTab}{
Level of tables to download
}
  \item{cdTab}{
Code of tables to download (comma separated characters)
}
  \item{lvTime}{
Level of survey date to download
}
  \item{cdTime}{
Code of survey date to download (comma separated characters)
}
  \item{lvArea}{
Level of area to download
}
  \item{cdArea}{
Code of area to download (comma separated characters)
}
}
\details{
Data includes some levels of tables, survey dates and areas. For
example, data ID 0003009759 includes 1 table (the number of buildings),
1 survey date (year 2008) and 112 areas (prefectures and cities). Level
2 of area shows prefectures, level 3 shows the city parts of each
prefecture, and level 4 shows the major cities (e.g. Sapporo, Yokohama,
Nagoya, etc.). You can specify the level of table, time and area to
download data. If you don't specify the level, all levels are included
to data.
}
\value{
  It returns the ``jpstat'' object, which includes
  \item{ID}{data ID}
  \item{Stat name}{statistics name of government}
  \item{Government}{goverment (ministry) that has the authority of data}
  \item{Statistics name}{provided statistics name}
  \item{Title}{title of data}
  \item{Survey date}{survey date}
  \item{Tables}{data body}
  Tables is a list that contains tables, and each table is data frame.
}
\references{
http://statdb.nstac.go.jp/system-info/api/api-spec/
}
\author{
Kiwamu Ishikura <ishikura.kiwamu@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\donttest{getStatsList("Hokkaido AND population", surveyYears = 2008)}
\donttest{hpop20 <- getStatsData("0003009759")}
\donttest{head(hpop20@data[[1L]])}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~getStatsData }
\keyword{ ~getStatsList }% __ONLY ONE__ keyword per line
