\name{imagedata}
\alias{imagedata}
\title{Digitize a linear-scale plot that is shown in a graphics window.}
\usage{
imagedata(image, xaxis, yaxis, rotated = FALSE)
}
\arguments{
  \item{image}{name of image file (must be PNG)}

  \item{xaxis}{values along the x axis, typically at tick
  marks}

  \item{yaxis}{values along the y axis, typically at tick
  marks}

  \item{rotated}{a logical indicating whether the axes are
  rotated}
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
'imagedata', the main function of the 'imagedata' package,
digitizes points from images.  As of now, the images must
be in PNG format, and the axes must be linear.
}
\details{
If 'rotated' is TRUE, then the first step is to display the
image with a red diagonal line and red dotted horizontal
and vertical lines, inviting the user to click above or
below the diagonal line to alter the rotation angle.  Any
click that is to the left of the centre of the image will
end this step, storing the inferred angle. The idea of this
interactive method is to help in cases of flawed documents,
e.g. those created with a photocopier.

Next, the user asked to click on the x axis, at values
given by the 'xaxis' argument.  These values are normally
at tick marks.  The same is then done for the y axis.

Then the user is asked to click at the top-right of the
plotting area.  Actually, it can be a bit outside the area,
if desired, or inside it.  This establishes a "stop" zone,
which will be drawn on the plot.  Once the actual
digitization is started, clicking in this zone ends the
processing.

Then the user is asked to click to the lower-left of the
plot to establish an "undo" zone.  Clicking in that zone
removes the most recently acquired point.

Finally, the actual digitization process begins, with the
user clicking within the plot region, using the "undo" zone
to correct mistakes, and the "stop" zone to finish.

Points that are clicked are indicated on the plot.

To digitize a second curve with the same axes, one may use
'getdata()', supplying it with 'X', 'Y', 'S' and 'U' values
returend by 'imagedata'.
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

# Test with a non-rotated image (e.g. screen capture)
file <- system.file("extdata", "test.png", package="imagedata")
xy <- imagedata(file, c(2, 10), c(4, 10))

# Test with a rotated image (e.g. photocopy)
file <- system.file("extdata", "testr.png", package="imagedata")
xy <- imagedata(file, c(2, 10), c(4, 10), rotated=TRUE)
}
}

