#' Find the angle of a plot
#'
#' 'getangle' finds the counterclockwise angle at which the plot has
#' been rotated.  It asks the user to click on two ends of the x axis,
#' then two ends of the y axis.
#'
#' @param image, the image to be worked with
#'
#' @return counterclockwise angle in degrees
getangle <- function(image)
{
    message("click to upper-right of centre, at an angle to the red line")
    angle <- 0
    while (TRUE) {
        plot(0:1, 0:1, type='n', axes=FALSE, asp=1)
        rasterImage(image, 0, 0, 1, 1, angle=angle)
        abline(0, 1, col='red', lwd=2)
        abline(h=seq(0, 1, 0.1), col='red', lty='dotted', lwd=2)
        abline(v=seq(0, 1, 0.1), col='red', lty='dotted', lwd=2)
        xy <- idlocator(1)
        if (xy$x < 0.5 && xy$y < 0.5) {
            return(angle)
        }
        angle <- angle + 180 / pi * atan2(xy$y, xy$x) - 45
        rasterImage(image, 0, 0, 1, 1, angle=angle)
    }
}
