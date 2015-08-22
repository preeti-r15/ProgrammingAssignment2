
#Prog2 (WEEK 3): Inverse of Matrix
#create vector to cache matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
#calculate or return inverse of matrix
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached inverse of matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
