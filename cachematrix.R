## Functions below will enable efficient inversion of a matrix. 
## If the inverse already exists, the calculation of inverse will not
## be called thus realizing efficiency
## Two functions will realize this logic
## Function makeCacheMatrix will create a special object
## Function cacheSolve will calculate the inverse


makeCacheMatrix  <- function(x = matrix()) {

        ## initialze a matrix to hold an inverse
        
        inx <- matrix()
        
        ## set values to hold matrix and its inverse
        
        set <- function(y) {
                x <<- y
                inx <<- NULL                
        }
        
        ## get x when needed
        
        get <- function() x 
        
        ## set inverse of function
        
        setinverse <- function(inverse) inx <<- inverse
        
        ## calculate inverse of function
        
        getinverse <- function() inx
        
        list(set=set ,get=get , setinverse=setinverse, getinverse=getinverse)
        
        
}


## below function will calculate inverse of a matrix in an efficient manner

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        if(is.null(i))
        {
                message ("getting catched data")
                return(i)
        }
        
        data <- x$get()
        
        y <- solve(data)
        
        x$setinverse(y)
        
        y
        ## Return a matrix that is the inverse of 'x'
}
