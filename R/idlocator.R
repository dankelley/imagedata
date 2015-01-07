#' Digitize a point, or UNDO, or STOP.
#'
#' @param S information on the STOP region
#' @param U information on the UNDO region
#' @param X information on the x scale
#' @param Y information on the y scale
#' @param bell TRUE to ring the bell with mouse clicks
#' @param verbose TRUE to print more information
#' @param type 'p' for points, 'n' for none, etc as for \code{\link{plot}}
#' @param col colour (ignored unless plotting) as for \code{\link{par}}
#' @param lwd line width of symbols as for \code{\link{par}}
#' @param pch symbol code as for \code{\link{par}}
#' @param cex symbol size expansion factor as 0for \code{\link{par}}
#'
#' @return a list containing digitized values \code{x} and \code{y} along with \code{status}, which may be \code{"DATA"} for data, \code{"AXIS"} for axis information, \code{"STOP"} for a click in the STOP region, or \code{"UNDO"} for a click in the UNDO region.
#' plus other properties 
#' @export idlocator
idlocator <- function(S=NULL, U=NULL,
                      X=NULL, Y=NULL,
                      bell=TRUE, verbose=TRUE,
                      type='p', col='red', lwd=par("lwd"), pch=20, cex=1/2)
{
    xy <- locator(1, type='n', col=col, pch=pch, cex=cex)
    if (bell) alarm()
    if (!is.null(S) && (xy$x > S$x && xy$y > S$y))
        return(list(x=xy$x, y=xy$y, status="STOP"))
    if (!is.null(U) && (xy$x < U$x && xy$y < U$y))
        return(list(x=NULL, y=NULL, status="UNDO"))
    # OK, must be a data point, but it could be actual data, or axis data.
    if (type == "p") 
        points(xy$x, xy$y, col=col, pch=pch, cex=cex)
    if (!is.null(X) && !is.null(Y)) {
        return(list(x=as.numeric(X$a + X$b * xy$x),
                    y=as.numeric(Y$a + Y$b * xy$y), status="DATA"))
    }
    return(list(x=xy$x, y=xy$y, status="AXIS"))
}
