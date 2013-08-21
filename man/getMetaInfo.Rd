\name{getMetaInfo}
\alias{getMetaInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get meta information of specified statistical ID of http://statdb.nstac.go.jp/
}
\description{
Get meta information of specified statistical ID of http://statdb.nstac.go.jp/
}
\usage{
getMetaInfo(statsDataId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statsDataId}{
statistical data ID (character)
}
}
\references{
http://statdb.nstac.go.jp/system-info/api/api-spec/
}
\author{
Kiwamu Ishikura <ishikura.kiwamu@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
getMetaInfo("0003009759")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~getMetaInfo }
