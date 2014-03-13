idlocator <- function(n, S=NULL, U=NULL, col='red', pch=20, cex=1/2)
{
    ## FIXME: catch 'stop' and 'edit' here, not elsewhere
    x <- NULL
    y <- NULL
    for (i in 1:n) {
        xy <- locator(1)
        alarm()
        ## if 'stop', do not record, and return what we have so far
        if (!is.null(S) && (xy$x > S$x || xy$y > S$y))
            break
        ## if 'undo', just don't record it
        if (is.null(U) || !(xy$x < U$x || xy$y < U$y)) {
            x <- c(x, xy$x)
            y <- c(y, xy$y)
            if (!is.null(col))
                points(xy$x, xy$y, col=col, pch=pch, cex=cex)
        }
    }
    list(x=x, y=y)
}
