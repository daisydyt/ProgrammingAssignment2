## Cache the inverse of a matrix 




## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inversematrix<-NULL
        
        getinverse <- function() inversematrix
        setinverse<- function(m){
                inversematrix <<- m 
        }
        get <- function() x
        set <- function(y){
                x<<- y
                inversematrix<<-NULL
        }
        list(getinverse = getinverse, setinverse = setinverse, get = get, set=set)
}





## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        n<- x$getinverse()
        if (!is.null(n)){
                message("Getting cached data")
                return(n)
        }
        
        data<-x$get()
        n<-solve(data)
        x$setinverse(n)
        n
}
