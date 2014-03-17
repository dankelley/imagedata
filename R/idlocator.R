## Find data, with undo and stop regions.
idlocator <- function(n=1, S=NULL, U=NULL, col='red', pch=20, cex=1/2)
{
    ## FIXME: catch 'stop' and 'edit' here, not elsewhere
    x <- NULL
    y <- NULL
    i <- 1
    str(U)
    while (i <= n) {
        xy <- locator(1, type='p', col=col, pch=pch, cex=cex)
        alarm()
        x <- c(x, xy$x)
        y <- c(y, xy$y)
        i <- i + 1
        ## if 'stop', do not record, and return what we have so far
        ##cat("xy$x", xy$x, "xy$y", xy$y, "S$x", S$x, "S$y", S$y, "\n")
        if (!is.null(S) && (xy$x > S$x && xy$y > S$y)) { # STOP: detect condition in calling code
            x <- head(x, -1)
            y <- head(y, -1)
            break
        }
        if (!is.null(U) && (xy$x < U$x && xy$y < U$y)) { # UNDO
            cat(" undoing.  first, x:", x, "\n")
            x <- head(x, -2)
            y <- head(y, -2)
            i <- i - 2
            cat(" done undoing.  now x:", x, "\n")
        }
    }
    list(x=x, y=y)
}
