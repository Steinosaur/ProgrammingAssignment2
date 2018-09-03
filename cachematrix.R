##The functions in this script cache the inverse of a matrix




## The "makeCacheMatrix" function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        
        get <- function() {
                x
        }
        
        
        ## Set the inverse of the matrix
        setInv <- function(inverse) {
                i <<- inverse
        }
        ## Get the inverse of the matrix
        getInv <- function() {
                m
        }
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## The function "cacheSolve" calculates the inverse of the "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInv(m)
        m      
}