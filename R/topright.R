#' A helper function to specify the stop region.
#'
#' topright invites the user to click the top-right corner of the plotting
#' area, and stores the x and y values (in plot units) as xstop and ystop.
#' These values are used by data().
#'
#' @return a list with x and y page coordinates
#' @export topright

topright <- function()
{
    message("Click at or beyond the top-right corner of the plot box to make a STOP region.\n")
    xy <- idlocator(1, col="#F7819F")
    xy
}

showtopright <- function(xy)
{
    par <- par('usr')
    right <- par[2]
    top <- par[4]
    polygon(c(xy$x, right, right, xy$x), c(xy$y, xy$y, top, top), col='#F7819F')
    text(0.5*(xy$x + right), 0.5 * (xy$y + top), "STOP", cex=1/2)
}

