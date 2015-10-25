#as a suggestion perform these items
#x <- rbind(c(1,4), c(4,1)) 
#then take the results of x and call the makecachematrix function

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        setf <- function(y){
                x <<- y
                m <<- NULL
        }
        getf <- function () x
        setinv <- function(invr) m <<- invr
        getinv <- function() m
        list(setf = setf, getf = getf, setinv =setinv, getinv = getinv)
}

cacheSolve <- function(x, ...){
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data! :-D")
                return(m)
        }
        data <- x$getf()
        m <- solve(data)
        x$setinv(m)
        m
}
