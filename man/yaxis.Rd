\name{yaxis}
\alias{yaxis}
\title{A helper function that compute 'ya' and 'yb' for the y axis.}
\usage{
yaxis(values)
}
\arguments{
  \item{values}{a vector of values on the y axis, typically
  labelled ticks}
}
\value{
list containing 'a' and 'b'
}
\description{
yaxis takes a vector of values along the y axis (commonly
at tick marks) and instructs the user to click the indicate
points.  It then calls locator(n) with n set to the length
of that vector, and uses lm() to find a linear mapping
between values and locations on the graphics device.  The
mapping is stored in global variables ya and yb.
}

