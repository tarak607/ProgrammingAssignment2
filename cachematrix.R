##There are two functions here makeCacheMatrix() and cacheSolve() in this assignment.
##Using both of them I have got the output to print out the inverse of a valid matrix entered for the first time.Once the inverse is cached it is printed from the cache.
##This also will ignore the values/matrices which don't have a determinant or determinant is equal to zero as they dont have inverses.
##I have taken care of error handling well to print appropriate messages. 
##---------------------------------------------------------------------------------------------------------------------------

## makeCacheMatrix() -- Function
## The function will return the below list of 4 functions with the input of a matrix which needs to be captured in a variable. 
## 1.set() - This function resets the value of makeCacheMatrix's original argument.This also sets the inverse to null as this a new matrix you are passing as input.
## 2.get() - Returns the value of orignal argument or set argument passed.
## 3.setinv() - Using this function you can input the inverse directly into inverse value if it is valid.I have done error handling here such that you can only input 
##              matrices which have inverse.If you pass any strings/non-matrices/singular matrices this will print out messages and exits.
##              This is the most important function here which sets the inverse if it is a valid input.
## 4.getinv() - This gets the inverse value from the function.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    
    setinv <- function(data = matrix()) {
        e1 = 0
        
        determinant <- tryCatch( det(data),error = function(e) e1 )
        
        if (determinant != 0) {
            
            message("Success!Matrix Inverse valid!")
            
            inverse <<- data 
            
        } else if (determinant == 0) {
            message("Incorrect!Set a square and invertible matrix!")
            inverse <<- NULL
            x <<- NULL
        }
              
    }    
    getinv <- function() {
        inverse
    }
    list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## cacheSolve() -- Function
## The function prints the inverse with the object of makeCacheMatrix() function as input.
## If the inverse was calculated in the last step then it will print from the cache with appropriate message.
## If the matrix entered is singular/non-matrix/string then this will print appropriate messages that the inverse cannot be calculated

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinv()
    
    
    if(!is.null(inverse)) {
        message("Getting data from cache!")
        return(inverse)
    }
    
    data <- x$get()
    
    e1 = 0
    
    determinant <- tryCatch( det(data),error = function(e) e1 )
    
    if (determinant != 0) {
        
        inverse <- solve(data)
        x$setinv(inverse)
        inverse 
        
    } else if (determinant == 0) {
        message("The matrix you have entered is not square or not invertible.Enter a valid new one!")
    }
    
}