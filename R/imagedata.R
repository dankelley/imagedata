#' Digitize a linear-scale plot that is shown in a graphics window.
#'
#' @description
#' \code{imagedata}, the main function of the 'imagedata' package,  
#' digitizes points from images.  As of now, the images must
#' be in PNG format, and the axes must be linear. Also, at
#' least on OS X machines, the plot device type must be 
#' quartz, since x11 does not properly buffer text in the console
#' when combined with clicks in the plot window.
#'
#' @details
#' If \code{rotated} is TRUE, then the first step is to display the image
#' with a red diagonal line and red dotted horizontal and vertical lines,
#' inviting the user to click above or below the diagonal line to 
#' alter the rotation angle.  Any click that is to the left of the
#' centre of the image will end this step, storing the inferred angle.
#' The idea of this interactive method is to help in cases of flawed
#' documents, e.g. those created with a photocopier.
#'
#' Then the user is asked to click somewhere at the bottom-left of 
#' image, to set up an "UNDO" zone, and then the top-right, to
#' set up a "STOP" zone. Normally these zones are placed outside
#' the axis; they only have to be large enough to be easily clicked
#' within.
#' 
#' Next, the user asked to click on the x axis, at values given
#' by the \code{xaxis} argument.  These values are normally at tick
#' marks.  The same is then done for the y axis.
#'
#' Then it's time to digitize image data. If a mistake is made,
#' click in the UNDO zone. To stop digitizing, click in the STOP
#' zone.  The results of \code{imagedata} are a list, of which
#' the two most interesting components are \code{x} and \code{y},
#' the coordinates of the clicked points, translated to the 
#' coordinates of the plot (i.e. using the axis information).
#' 
#' @param image name of image file (must be PNG)
#' @param xaxis values along the x axis, typically at tick marks
#' @param yaxis values along the y axis, typically at tick marks
#' @param rotated a logical indicating whether the axes are rotated
#' @param col colour of points, as for \code{\link{par}}
#' @param pch type of points, as for \code{\link{par}}
#' @param cex size of points, as for \code{\link{par}}
#' @return \code{imagedata} returns a list containing \code{x} and \code{y} values, which are the
#' digitized points, \code{X} and \code{Y}, which are lists containing \code{a}
#' and \code{b} values that hold the axis transformations, \code{S}, a
#' list containing \code{x} and \code{y} defining the lower-left corner
#' of the STOP zone, and \code{U}, a list containing the \code{x} and \code{y}
#' defining the upper-right corner of the UNDO zone.
#'
#' @import png
#' @examples
#' \dontrun{
#' # Create a PNG file
#' x <- 1:10
#' y <- 1 + x + x %% 2
#' png("test.png")
#' par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#' plot(x, y)
#' dev.off()
#'
#' # Test with a non-rotated image (e.g. screen capture)
#' xy <- imagedata("test.png", c(2, 10), c(4, 10))
#'
#' # Test with a rotated image (e.g. photocopy)
#' xy <- imagedata("testr.png", c(2, 10), c(4, 10), rotated=TRUE)
#' }
#' @export imagedata

imagedata <- function(image, xaxis, yaxis, rotated=FALSE, col="red", pch=20, cex=1)
{
    image <- readPNG(image)
    par(mar=rep(0, 4))
    plot(0:1, 0:1, type='n', asp=1)
    rasterImage(image, 0, 0, 1, 1)
    if (rotated) {
        cat("Repeatedly click near solid blue line to rotate; later, click near dotted portion to end.\n")
        flush.console()
        angle <- getangle(image)
        cat("The final rotation angle was", round(angle, 2), " deg counterclockwise.\n")
    }
    cat("Click outside the bottom-left plot corner to create an UNDO region.\n")
    flush.console()
    U <- bottomleft()
    flush.console()
    showbottomleft(U)
    flush.console()
    cat("Click outside the top-right plot corner to create a STOP region.\n")
    flush.console()
    S <- topright()
    showtopright(S)
    xx <- NULL
    for (x in xaxis) {
        cat("Click on the x axis where x =", x, "\n")
        flush.console()
        xx <- c(xx, idlocator(bell=TRUE, type="p", col="red", pch=1, cex=1)$x)
    }
    ab <- as.numeric(coef(lm(xaxis ~ xx)))
    X <- list(a=ab[1], b=ab[2])
    cat("    (X transformation: x=", X$a, " + ", X$b, "*USRx)\n")
    flush.console()
    yy <- NULL
    for (y in yaxis) {
        cat("Click on the y axis where y =", y, "\n")
        flush.console()
        yy <- c(yy, idlocator(bell=TRUE, type="p", col="red", pch=1, cex=1)$y)
    }
    ab <- as.numeric(coef(lm(yaxis ~ yy)))
    Y <- list(a=ab[1], b=ab[2])
    cat("    (Y transformation: y=", Y$a, " + ", Y$b, "*USRy)\n")
    flush.console()
    x <- y <- NULL
    n <- 0
    cat("Click on data points, 'UNDO' to ignore last a click; click 'STOP' to end.\n")
    flush.console()
    ## FIXME: put loop here
    while (TRUE) {
        xy <- idlocator(X=X, Y=Y, S=S, U=U, col=col, pch=pch, cex=cex)
        if (xy$status=="STOP")
            break
        if (xy$status=="UNDO") {
            if (length(x)) {
                cat("Removed datum #", n, "with x=", tail(x, 1), " and y=", tail(y, 1), "\n")
                flush.console()
                x <- head(x, -1)
                y <- head(y, -1)
                n <- n - 1
                flush.console()
                ## redraw
                rasterImage(image, 0, 0, 1, 1)
                showbottomleft(U)
                showtopright(S)
                ## FIXME: undo and stop
                xusr<-(x - X$a) /X$b
                yusr<-(y - Y$a) /Y$b
                points(xusr, yusr, col=col, pch=pch, cex=cex)
            }
        } else if (xy$status=="DATA") {
            x <- c(x, xy$x)
            y <- c(y, xy$y)
            n <- n + 1
            cat("Added datum #", n, "with x=", tail(x, 1), " and y=", tail(y, 1), "\n")
        }
    }
    list(x=x, y=y, X=X, Y=Y, S=S, U=U)
}

