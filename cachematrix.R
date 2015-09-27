## These functions are to be used with the following format as based on items in the discussion 
##   board. The first function will convert vectors with 4, 9, or other square number of elements
##   into a square matrix. Note that inverted matrices may feature scientific notation.
##
##   * a <- <square matrix or eligible vector for conversion to matrix> 
##   * b <- makeCacheMatrix(a)
##   * cacheSolve(b)
##
##
## makeCacheMatrix has two functions:
##
##   * makeCacheMatrix$make converts displays the matrix, converting it if need be back into a square
##       matrix
##   * makeCacheMatrix$invert includes the functions of x$make but also inverts the entered matrix
 
makeCacheMatrix <- function(x = matrix()) { 
    c <- NULL
    d <- NULL
    make <- function() {
        z <- sqrt(length(x))
        c <<- matrix((x), nrow = z, ncol = z)
    }
    invert <- function() {
        z <- sqrt(length(x))
        c <<- matrix((x), nrow = z, ncol = z)
        d <<- solve(c)
    }
    list(make = make, invert = invert)
} 

## This seeks out displays the inverse of the matrix, with the if statement seeking the cached
##   value from the makeCacheMatrix$invert function.

cacheSolve <- function(x, ...) {
    d <- x$invert()
    if(!is.null(d)) {
        message("getting cached data")
        return(d)
    }
    c <- x$make()
    d <- solve(c)
    return(d)
}
