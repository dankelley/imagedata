#' A helper function that compute 'ya' and 'yb' for the y axis.
#'
#' yaxis takes a vector of values along the y axis (commonly at tick marks)
#' and instructs the user to click the indicate points.  It then calls
#' idlocator(n) with n set to the length of that vector, and uses lm() to
#' find a linear mapping between values and locations on the graphics
#' device.  The mapping is stored in global variables ya and yb.
#'
#' @param values a vector of values on the y axis, typically labelled ticks
#' @param U an "undo" list as returned by bottomleft().
#' @return list containing 'a' and 'b', for the linear mapping, and 'angle'
#' @export yaxis

yaxis <- function(values, U=NULL)
{
    y <- NULL
    for (value in values) {
        y <- c(y, idlocator(U=U, col='blue')$y)
    }
    if (length(y) != length(values))
        stop("You must click each requested point")
    m <- lm(values ~ y)
    C <- as.numeric(coef(m))
    a <- C[1]
    b <- C[2]
    list(a=a, b=b)
}

