#' Find the angle of a plot
#'
#' 'getangle' finds the counterclockwise angle at which the plot has
#' been rotated, determining the value from the user via pointer clicks.
#'
#' @param image, the image to be worked with
#'
#' @return counterclockwise angle in degrees
getangle <- function(image)
{
    message("Click at an angle to thick part of the blue line; click near thin part to end.")
    angle <- 0
    while (TRUE) {
        plot(0:1, 0:1, type='n', axes=FALSE, asp=1)
        rasterImage(image, 0, 0, 1, 1, angle=angle)
        abline(h=seq(0, 1, 0.1), col='#FACC2E', lty='dotted', lwd=1)
        lines(c(0, 0.5), c(0, 0.5), col='blue', lwd=1)
        lines(c(0.5, 1), c(0.5, 1), col='blue', lwd=4)
        abline(v=seq(0, 1, 0.1), col='#FACC2E', lty='dotted', lwd=2)
        xy <- idlocator(1)
        if (xy$x < 0.5 && xy$y < 0.5) {
            return(angle)
        }
        angle <- angle + 180 / pi * atan2(xy$y, xy$x) - 45
        rasterImage(image, 0, 0, 1, 1, angle=angle)
    }
}
