#' A helper function that compute 'ya' and 'yb' for the y axis.
#'
#' yaxis takes a vector of values along the y axis (commonly at tick marks)
#' and instructs the user to click the indicate points.  It then calls
#' locator(n) with n set to the length of that vector, and uses lm() to
#' find a linear mapping between values and locations on the graphics
#' device.  The mapping is stored in global variables ya and yb.
#'
#' @param values a vector of values on the y axis, typically labelled ticks
#' @return list containing 'a' and 'b'
#' @export yaxis

yaxis <- function(values)
{
    n <- length(values)
    message("click on the y axis at places where x=", paste(values, collapse=","))
    xy <- idlocator(n)
    m <- lm(values ~ xy$y)
    C <- as.numeric(coef(m))
    list(a=C[1], b=C[2])
}

