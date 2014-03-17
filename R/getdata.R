#' Digitize data on a curve.
#'
#' getdata invites the user to click points on the graph.  To end the process,
#' click to the right or above the topright corner of the plotting area.
#'
#' @param X a list with items 'a' and 'b', calculated from xaxis()
#' @param Y a list with items 'a' and 'b', calculated from yaxis()
#' @param S a list with items 'x' and 'y', calculated from topright()
#' @param U a list with items 'x' and 'y', calculated from bottomleft()
#' @return list of 'x' and 'y' values
#' @export getdata

getdata <- function(X, Y, S, U)
{
    message("Click on data points, or 'UNDO' to ignore last a click; click 'STOP' to end.\n")
    xy <- idlocator(100, S=S, U=U)
    x <- X$a + X$b * xy$x 
    y <- Y$a + Y$b * xy$y 
    return(list(x=x, y=y))
}

