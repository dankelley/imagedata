## Find data, with undo and stop regions.
idlocator <- function(n=1, S=NULL, U=NULL,
                      X=NULL, Y=NULL,
                      col='red', pch=20, cex=1/2,
                      bell=TRUE, verbose=TRUE)
{
    ## FIXME: catch 'stop' and 'edit' here, not elsewhere
    x <- y <- xu <- yu <- NULL
    i <- 1L
    while (i <= n) {
        xy <- locator(1, type='p', col=col, pch=pch, cex=cex)
        if (bell)
            alarm()
        ## if 'stop', do not record, and return what we have so far
        if (!is.null(S) && (xy$x > S$x && xy$y > S$y)) { # STOP: detect condition in calling code
            if (verbose) message(sprintf("  digitized %d points", i-1))
            break
        }
        ## if "UNDO", remove last point
        if (!is.null(U) && (xy$x < U$x && xy$y < U$y)) { # UNDO
            if (i > 1) {
                if (verbose)
                    message(sprintf("  removed x[%d]=%.4g and y[%d]=%.4g ", i-1, xu[i-1], i-1, yu[i-1]))
                x <- tail(x, -1)
                y <- tail(y, -1)
                i <- i - 1L
            }
        } else {
            ## OK, it's not STOP and not UNDO.  Add to the output vectors.
            if (!is.null(X) && !is.null(Y)) {
                xx <- X$a + X$b * xy$x
                yy <- Y$a + Y$b * xy$y
            } else {
                xx <- xy$x
                yy <- xy$y
            }
             if (verbose) {
                message(sprintf("  added   x[%d]=%.4g and y[%d]=%.4g", i, xx, i, yy))
            }
            x <- c(x, xy$x)
            y <- c(y, xy$y)
            xu <- c(xu, xx)
            yu <- c(yu, yy)
            i <- i + 1L
        }
    }
    list(x=xu, y=yu)
}
