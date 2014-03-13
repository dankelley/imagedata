imagedata is an R package for digitizing data from images.  The present version
only works with PNG images, and only if the x and y axes are linear.  Features
will be added to imagedata as they show their worth through real-life use.

The main function is imagedata(), and its documentation should be enough to get
people started.
    
Installing imagedata is a two-step process

    library(devtools)
    install_github('oce', 'dankelley', 'develop')

after which using it is a single function call e.g.

    library(imagedata)
    xy <- imagedata("test.png", c(2, 10), c(4, 10))

for a non-rotated image (e.g. from a screen capture) or e.g.

    xy <- imagedata("testr.png", c(2, 10), c(4, 10), rotated=TRUE)

for a rotated image (e.g. from a photocopy).

