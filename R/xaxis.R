#' A helper function that compute 'xa' and 'xb' for the x axis.
#'
#' xaxis takes a vector of values along the y axis (commonly at tick marks)
#' and instructs the user to click the indicate points.  It then calls
#' locator(n) with n set to the length of that vector, and uses lm() to
#' find a linear mapping between values and locations on the graphics
#' device.  The mapping is stored in global variables xa and xb.
#'
#' @param values a vector of values on the x axis, typically labelled ticks
#' @return list containing 'a' and 'b', for the linear mapping, and 'angle'
#' @export xaxis

xaxis <- function(values)
{
    flush.console()
    cat("Click on the x axis at places where x=", paste(values, collapse=","), '\n')
    flush.console()
    n <- length(values)
    dev.flush()
    xy <- locator(n)
    dev.flush()
    m <- lm(values ~ xy$x)
    C <- as.numeric(coef(m))
    a <- C[1]
    b <- C[2]
    list(a=a, b=b)
}

