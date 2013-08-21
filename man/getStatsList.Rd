\name{getStatsList}
\alias{getStatsList}
\encoding{UTF-8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search and get a list of statistical data from http://statdb.nstac.go.jp/
}
\description{
Searcn and get a list of statistical data from ``Gateway to
Advanced and User-friendly Statistics Service''
(http://statdb.nstac.go.jp/) organized by Japanese government.
}
\usage{
getStatsList(searchWord = "", surveyYears = "", openYears = "", statsField = NULL, statsCode = NULL, searchKind = 1, statsNameList = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{searchWord}{
search words (you can use AND OR operations)
}
  \item{surveyYears}{
years to survey data (format: yyyy, yyyymm or yyyymm-yyyymm)
}
  \item{openYears}{
years to open data (same format as \code{surveyYears})
}
  \item{statsField}{
field of statistics (2-digits number: search from major division;
4-digits: search from minor division)
}
  \item{statsCode}{
code of statistics (5-digits number: search by the author government;
8-digits number: search by government statistics code)
}
  \item{searchKind}{
    1: statistics information (default)
    2: regional mesh
    3: statistics system of society and population
}
  \item{statsNameList}{
    Y: get statistical survey name (not statistics information)
}
}
\value{
  Dataframe of search results 
}
\references{
  http://statdb.nstac.go.jp/system-info/api/api-spec/
}
\author{
Kiwamu Ishikura <ishikura.kiwamu@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
getStatsList("北海道 AND 人口", surveyYears = 2008)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~getStatsList }
