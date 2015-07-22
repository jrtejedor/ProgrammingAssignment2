makeCacheMatrix <- function(x = matrix()) { ## The makeCacheMatrix is a function thnt contains 
                                            ##other functions (set,get,setmatrix,getmatrix).
        
        inverse_m <- NULL  ## the default value of the inverse matrix in the main function is null.
        
        set <- function(y){ ##This function sets the value of the matrix stored in the main function.
                            ## makeCacheMAtrix
                
                x <<- y  ## The use of <<- assigns a value to an object y (the input) in an 
                         ## environment that is different from the current environment. Otherwise it 
                         ## would change x only in the set function and not in the main one. 
                
                inverse_m <<- NULL  ## Restores the inverse of the new matrix to null if the vector 
                                    ## is different from the first one. As stated before, it changes 
                                    ## inverse_m in a different environment from the set function, so 
                                    ## not only  changes inverse_m in the set function but resets the
                                    ## main function as well. The new mean is calculated through the
                                    ## function cacheSolve.
        }
        
        get <- function() x  ## This function returns the matrix x stored in the main function.
                             ## (makeVector).
        
        setinvmat <- function(solve) inverse_m <<- solve  ## This function does not calculate the
                                                          ## inverse, but stores the value of the 
                                                          ## input in a variable inverse_m from 
                                                          ## the main function (makeCacheMatrix).
        
        getinvmat <- function() inverse_m  ## This function returns the value inverse_m of the 
                                           ## setinvmat function obtained in the previous step.
        
        list(set=set, get=get, setinvmat=setinvmat, getinvmat=getinvmat)
        ## This command generates and stores a list with the parameters obtained from the previous 
        ## four functions. 
}

cacheSolve <- function(x=matrix(), ...) {  ## This function returns a matrix that is the inverse of 
                                           ## x. It takes as an input the object were makeCacheMatrix
                                           ## was stored.
        
        inverse_m <- x$getinvmat()  ## This line assigns the value of getinvmat obtained with the 
                                    ## makeCacheMatrix function to inverse_m. It can be null or not, 
                                    ## depending if the inverse matrix was or was not calculated 
                                    ## before.
        
        
        if(!is.null(inverse_m)){                ## This IF statement verifies if inverse_m exists and
                message("getting cached data")  ## / or was calculated before. If inverse_m IS NOT 
                return(inverse_m)               ## null, the program returns a message and returns 
                                                ## the value of the inverse matrix (inverse_m) that 
                                                ## was stored in the cache (and ends the function). 
                                                ## If inverse_m is null, the program skips this step.
        }
        
        matrix <- x$get() ## This line gets the matrix stored with the previous main function 
                          ## (makeCacheMatrix)
        
        inverse_m <- solve(matrix, ...)  ## This line calculates and assigns to inverse_m the value 
                                         ## of the inverse matrix obtained from the input matrix.
        
        x$setinvmat(inverse_m) ## This line stores the new value of inverse_m (inverse of the matrix)
                               ##  in the cache of the previous main function (makeCacheMatrix)
        
        inverse_m  ## returns the value of inverse_m (the inverse of the initial matrix)
}