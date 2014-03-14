#' A helper function to specify the undo region.
#'
#' bottomleft invites the user to click the bottom-leftcorner of the plotting
#' area, and stores the x and y values (in plot units) as xundo and yundo.
#' These values are used by data().
#'
#' @return a list with x and y page coordinates
#' @export bottomleft

bottomleft <- function()
{
    flush.console()
    cat("click at or beyond the bottom-left corner of the plot box\n")
    flush.console()
    dev.flush()
    xy <- locator(1)
    dev.flush()
    xy
}

showbottomleft <- function(xy)
{
    par <- par('usr')
    left <- par[1]
    bottom <- par[3]
    polygon(c(left, xy$x, xy$x, left), c(bottom, bottom, xy$y, xy$y), col='#FACC2E')
    text(0.5*(xy$x + left), 0.5 * (xy$y + bottom), "UNDO", cex=1/2)
}

