#' A helper function that compute 'xa' and 'xb' for the x axis.
#'
#' xaxis takes a vector of values along the y axis (commonly at tick marks)
#' and instructs the user to click the indicate points.  It then calls
#' idlocator(n) with n set to the length of that vector, and uses lm() to
#' find a linear mapping between values and locations on the graphics
#' device.  The mapping is stored in global variables xa and xb.
#'
#' @param values a vector of values on the x axis, typically labelled ticks
#' @param U an "undo" list as returned by bottomleft().
#' @return list containing 'a' and 'b', for the linear mapping, and 'angle'
#' @export xaxis

xaxis <- function(values, U=NULL)
{
    x <- NULL
    for (value in values) {
        message("Click on the x axis where x=", value, "\n")
        x <- c(x, idlocator(1, U=U, col='blue')$x)
    }
    if (length(x) != length(values))
        stop("you must click each requested point")
    m <- lm(values ~ x)
    C <- as.numeric(coef(m))
    a <- C[1]
    b <- C[2]
    list(a=a, b=b)
}

