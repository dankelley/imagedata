\name{getdata}
\alias{getdata}
\title{Digitize data on a curve.}
\usage{
getdata(X, Y, S, U, verbose = TRUE)
}
\arguments{
  \item{X}{a list with items 'a' and 'b', calculated from
  xaxis()}

  \item{Y}{a list with items 'a' and 'b', calculated from
  yaxis()}

  \item{S}{a list with items 'x' and 'y', calculated from
  topright()}

  \item{U}{a list with items 'x' and 'y', calculated from
  bottomleft()}

  \item{verbose}{a logical indicating whether to indicate
  data as clicked}
}
\value{
list of 'x' and 'y' values
}
\description{
getdata invites the user to click points on the graph.  To
end the process, click to the right or above the topright
corner of the plotting area.
}

