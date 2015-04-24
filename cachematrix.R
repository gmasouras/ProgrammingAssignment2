## Put comments here that give an overall description of what your
## functions do

## This function gives a list of four functions. The getters and setters of the argument type of matrix that we pass in
## the function and the getters and setters of the inversed type of matrix. 

makeCacheMatrix <- function(inputmatrix = matrix()) {

        if (is.matrix(inputmatrix)) {
    
	inverse <- NULL

	setMatrix <- function (m1) {
		inputmatrix <<- m1
		inverse <<- NULL
	}

	getMatrix <- function () inputmatrix

	setInverseMatrix <- function (inversematrix) {
			inverse <<- inversematrix
	}	
	
	getInverseMatrix <- function () inverse

	list(smx = setMatrix, gmx = getMatrix, sImx = setInverseMatrix, gImx = getInverseMatrix )
  
  }else{
    
    message("You must input an argument type of matrix!")
    
  }
}


## This function returns the inverse of a matrix. After the first time of calculation, if it is recalled it would
## return the cached type of the inverse matrix without losing time recalculating him.
## The argument passed is list type which derives from our first function called

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$gImx()
	
	if(!is.null(inverse)) {
    
		message("getting cache inverse matrix")
		inverse
    
	}

	data <- x$gmx()
	inverse <- solve(data, ...)
	x$sImx(inverse)
	inverse	
}
