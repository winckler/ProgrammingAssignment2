## Specialized Matrix Framework to cache inverse calculation

## Solution 1 - Following the Vector/Mean template
##  from the assignment description

## Creates a specialized Matrix Object that can store
##  a cache of inverse calculation
makeCacheMatrix <- function(x = matrix()) {
    # variable to hold the cached data
    inverse <- NULL

    # function to update the matrix values (and invalidate cache)
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    # function to get the matrix values
    get <- function() x

    # function to set the inverse cached value
    set.inverse <- function(value) inverse <<- value

    # function to set the inverse cached value
    get.inverse <- function() inverse

    # return attributes
    list(
        set = set, get = get,
        set.inverse = set.inverse,
        get.inverse = get.inverse
    )
}

## Operates in a specialized Matrix Object that can
##  cache the inverse calculation
cacheSolve <- function(x, ...) {
    inverse <- x$get.inverse()
    if (is.null(inverse)) {
        # Cache missing. Calculate and Store
        message("cache miss: no previous value found.")
        data <- x$get()
        inverse <- solve(data, ...)
        x$set.inverse(inverse)
    }
    # return
    inverse
}

## Solution 2 - My take, more object oriented
##  Makes cacheSolve() almost useless
##  (commented out because differs very much from the template)

# makeCacheMatrix <- function(x = matrix()) {
#     inverse <- NULL
#     set <- function(y) {
#         x <<- y
#         inverse <<- NULL
#     }
#     get <- function() x
#
#     get.inverse <- function(...) {
#         if (is.null(inverse)) {
#             message("cache miss: no previous value found.")
#             inverse <<- solve(x, ...)
#             return(inverse)
#         }
#         inverse
#     }
#
#     list(set = set, get = get, get.inverse = get.inverse)
# }
#
# cacheSolve <- function(x, ...) {
#     x$get.inverse(...)
# }
