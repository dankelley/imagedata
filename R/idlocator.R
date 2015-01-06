## Find data, with undo and stop regions.
idlocator <- function(S=NULL, U=NULL,
                      X=NULL, Y=NULL,
                      col='red', pch=20, cex=1/2,
                      bell=TRUE, verbose=TRUE)
{
    xy <- locator(1, type='p', col=col, pch=pch, cex=cex)
    if (bell) alarm()
    if (!is.null(S) && (xy$x > S$x && xy$y > S$y))
        return(list(x=xy$x, y=xy$y, status="STOP"))
    if (!is.null(U) && (xy$x < U$x && xy$y < U$y))
        return(list(x=NULL, y=NULL, status="UNDO"))
    # OK, must be a data point, but it could be actual data, or axis data.
    if (!is.null(X) && !is.null(Y))
        return(list(x=as.numeric(X$a + X$b * xy$x),
                    y=as.numeric(Y$a + Y$b * xy$y), status="DATA"))
    return(list(x=xy$x, y=xy$y, status="AXIS"))
}
