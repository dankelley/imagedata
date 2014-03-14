idlocator <- function(S=NULL, U=NULL, col='red', pch=20, cex=1/2)
{
    ## FIXME: catch 'stop' and 'edit' here, not elsewhere
    x <- NULL
    y <- NULL
    for (i in 1:10) {                  # for caution, only permit 10 trials
        xy <- locator(1, type='p', col=col, pch=pch, cex=cex)
        ## if 'stop', do not record, and return what we have so far
        cat("xy$x", xy$x, "xy$y", xy$y, "S$x", S$x, "S$y", S$y, "\n")
        if (!is.null(S) && (xy$x > S$x && xy$y > S$y))
            break
        ## if 'undo', just don't record it
        if (is.null(U) || !(xy$x < U$x && xy$y < U$y)) {
            x <- c(x, xy$x)
            y <- c(y, xy$y)
        }
    }
    list(x=x, y=y)
}
