\name{imagedata}
\alias{imagedata}
\title{Digitize a linear-scale plot that is shown in a graphics window.}
\usage{
imagedata(image, xaxis, yaxis)
}
\arguments{
  \item{image}{name of image file (must be PNG)}

  \item{xaxis}{values along the x axis, typically at tick
  marks}

  \item{yaxis}{values along the y axis, typically at tick
  marks}
}
\value{
a list containing 'x' and 'y' values, which are the
digitized points, 'X' and 'Y', which are lists containing
'a' and 'b' values that hold the axis transformations, 'S',
a list containing 'x' and 'y' defining the lower-left
corner of the stop zone, and 'U', a list containing the 'x'
and 'y' defining the upper-right corner of the undo zone.
}
\description{
'imagedata' is the main function of the 'imagedata'
package.  It invites the user to click certain points along
the axes, and then to click the desired digitization points
within the plot area. Clicking to the lower-left of the
plot area undoes the last digitized point, and clickint to
the upper-right finishes the digitization.  To digitize a
second curve with the same axes, use 'getdata', supplying
it with values returned by 'imagedata'.
}
\examples{
\dontrun{
# Create a PNG file
x <- 1:10
y <- 1 + x + x \%\% 2
png("test.png")
par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
plot(x, y)
dev.off()

xy <- imagedata("test.png", c(2, 10), c(4, 10))
}
}

