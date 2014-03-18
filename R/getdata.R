#' Digitize data on a curve.
#'
#' getdata invites the user to click points on the graph.  To end the process,
#' click to the right or above the topright corner of the plotting area.
#'
#' @param X a list with items 'a' and 'b', calculated from xaxis()
#' @param Y a list with items 'a' and 'b', calculated from yaxis()
#' @param S a list with items 'x' and 'y', calculated from topright()
#' @param U a list with items 'x' and 'y', calculated from bottomleft()
#' @param verbose a logical indicating whether to indicate data as clicked
#' @return list of 'x' and 'y' values
#' @export getdata

getdata <- function(X, Y, S, U, verbose=TRUE)
{
    message("Click on data points, or 'UNDO' to ignore last a click; click 'STOP' to end.")
    xy <- idlocator(100, S=S, U=U, X=X, Y=Y, verbose=verbose)
    return(list(x=xy$x, y=xy$y))
}

