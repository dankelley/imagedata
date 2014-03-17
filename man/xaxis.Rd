\name{xaxis}
\alias{xaxis}
\title{A helper function that compute 'xa' and 'xb' for the x axis.}
\usage{
xaxis(values, U = NULL)
}
\arguments{
  \item{values}{a vector of values on the x axis, typically
  labelled ticks}

  \item{U}{an "undo" list as returned by bottomleft().}
}
\value{
list containing 'a' and 'b', for the linear mapping, and
'angle'
}
\description{
xaxis takes a vector of values along the y axis (commonly
at tick marks) and instructs the user to click the indicate
points.  It then calls idlocator(n) with n set to the
length of that vector, and uses lm() to find a linear
mapping between values and locations on the graphics
device.  The mapping is stored in global variables xa and
xb.
}

