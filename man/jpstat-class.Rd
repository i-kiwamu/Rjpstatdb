\name{jpstat-class}
\Rdversion{0.1}
\docType{class}
\alias{jpstat-class}
%%\alias{show,jpstat-method}

\title{Class \code{"jpstat"}}
\description{
Data object from Gateway to Advanced and User-friendly Statistics
Service (http://statdb.nstac.go.jp/)
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("jpstat", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"list"} which contains dataframes.}
    \item{\code{id}:}{Object of class \code{"character"} of data ID}
    \item{\code{stat.name}:}{Object of class \code{"character"} of
      statistics name of government}
    \item{\code{gov}:}{Object of class \code{"character"} of government
      (ministry) that has the authority of data}
    \item{\code{statistics.name}:}{Object of class \code{"character"} of
    provided statistics name}
    \item{\code{title}:}{Object of class \code{"character"} of title of data}
    \item{\code{survey.date}:}{Object of class \code{"character"} of
      survey date}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "jpstat")}: ... }
	 }
}
\author{
Kiwamu Ishikura <ishikura.kiwamu@gmail.com>
}
\examples{
showClass("jpstat")
}
\keyword{classes}
