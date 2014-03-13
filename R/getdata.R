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
    message("Click on data points, or 'UNDO' to ignore last a click; click 'STOP' to end.")
    x <- y <- NULL
    i <- 1
    while (TRUE) {
        xy <- idlocator(1)
        xx <- X$a + X$b * xy$x 
        yy <- Y$a + Y$b * xy$y 
        if (xy$x > S$x || xy$y > S$y) {
            return(list(x=x, y=y))
        }
        if (xy$x < U$x || xy$y < U$y) {
            cat("x[", i, "] = ", tail(x, 1), " and y[", i, "] = ", tail(y, 1), " REMOVED\n", sep="")
            x <- head(x, -1)
            y <- head(y, -1)
            i <- i - 1
        } else {
            cat("x[", i, "] = ", xx, " and y[", i, "] = ", yy, " ADDED\n", sep="")
            x <- c(x, xx)
            y <- c(y, yy)
            i <- i + 1
        }
    }
}

