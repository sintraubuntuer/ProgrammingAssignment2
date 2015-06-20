## the makeCacheMatrix function  creates a special matrix object
## that can calculate and 'store'/cache its inverse matrix for later (more efficient)  use

##  cacheSolve returns the inverse of a 'special' Cache Matrix 'x' passed as an argument
##  starts by looking if the inverse has already been calculated and stored inside the 'x' object
##  and calculates only if necessary


makeCacheMatrix <- function(x = matrix()) {
        # the makeCacheMatrix function  creates a special matrix object
        # that calculates and 'stores'/caches its inverse for later (more efficient)  use

        
        theMat <- x        # theMat -  internal variable that will hold the matrix passed as an argument to the function 
        invMat <- NULL     # invMat -  internal variable that is going to hold the inverse matrix of theMat

        
        set <- function(matY) {
               # function used in case we want to change the initial matrix 
               #
               # Args :
               #    matY  - the matrix we want to set as the 'main' matrix of makeCacheMatrix function  

                theMat <<- matY      #  the <<- operator is used because we're assigning to a variable that isn't defined inside the set function
                invMat <<- NULL       #  the <<- operator is used because we're assigning to a variable that isn't defined inside the set function
                
         }

        
        get <- function() {
                # function used to retrieve the matrix  ( theMat )
                # takes no arguments
                # returns 'theMat'

                theMat
        }

        setinverse <- function(mInv=solve(theMat)) { 
                #  function used to 'store' ( in the variable invMat ) the matrix  passed as an argument  -
                #  which should be the inverse of the initial matrix -
                #  that can  afterwards be retrieved  using the getinverse function .
                #  if no argument is passed , the function automatically calculates the inverse of theMat and stores it in invMat

                invMat <<- mInv
        }

        
        getinverse <- function() {
                # function used to get the inverse matrix 
                # takes no arguments
                # returns the inverse matrix

                invMat
        }


        # the makeCacheMatrix function returns a list whose elements are its 'inner' functions. 
        # The list labels are equal to the function names , so that they are acessible from the outside with the same names they have inside
        # it's an elegant way of making the inner functions accessible to the outside , making  makeCacheMatrix similar to a class
        # ( which it actually is since R functions are also classes ) with public methods : set , get , setinverse and getinverse
     
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## cacheSolve returns the inverse of a 'special' Cache Matrix 'x' passed as an argument
         # starts by looking if the inverse has already been calculated and stored inside the 'x' object
         # and calculates only if necessary
         
        # uses the getinverse function of the makeCacheMatrix object (passed as an argument) and tests wether the inverse has already been calculated
        mat <- x$getinverse()      
        if(!is.null(mat)) {         
                message("getting cached data")
                return(mat)        # if it has already been calculated simply returns it
        }
        mat <- x$get()         # if the inverse matrix hasn't yet been calculated gets the matrix 'theMat' using the get function of the makeCacheMatrix object  
        mInv <- solve(mat, ...)   # then calculates the inverse
        x$setinverse(mInv)        # and stores it in the makeCacheMatrix object ( using the setinverse function) for later use 
 
       # returns the inverse matrix
       mInv

}
