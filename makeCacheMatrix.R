## This is to store data
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL     #initializing inverse as Null
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() {x}       #function to get matrix x
    SetInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## This is used to get Cache data
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {             #checking whether inverse is Null
        message("getting cached data")
        return(inv)  #returns inverse value
    }
    mat <- x$get()
    inv <- solve(mat, ...)          #calculates inverse value
    x$setInverse(inv)
    inv
}
