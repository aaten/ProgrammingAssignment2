The first function, makeCacheMatrix does the following :
# 1- Verifies that the matrix passed is squared. If not reports an error message 
# 2- set the value of the matrix
# 2- get the value of the matrix
# 3- set the value of the inverse of the square matrix passed
# 4- get the value of the inverse of the square matrix passed

makeCacheMatrix <- function(x = matrix()) {
    r<-dim(x)
    if(r[1]!=r[2]){
        message("matrix not squared will not proceed")
    }
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


# The next function calculates the inverse of the square matrix that is created with the previous function. Befroe it
#does so, it first checks to see if has already been calculated. In this case, it retrieves the inverse matrix from 
#the cache and avoids the computation. Otherwise, it calculates the inverse matrix  and sets the value recalculated 
#in the cache via the setmean function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

r<-c(100.77006,100.90486,101.59568,98.95958,99.54370,99.73224,100.75817,98.10560,100.46816)
b<-matrix(data=r,nrow=3,ncol=3)
maw<-makeCacheMatrix(b)
cacheSolve(maw)
cacheSolve(maw)

