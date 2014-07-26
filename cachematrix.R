##The functions bellow, work with a concept of caching results of 
##potentially time-consuming computations, which is in this case, matrix inversion. 
##To take advantage of those function you have to use makeCacheMatrix function firstly in order to create a special type of matrix which have caching capabilities, and then use 
##cacheResolve function to in fact, calculate de inverse matrix or use a cache 
##if it has already been calculated.


## this function creates a special type of matrix with caching capability from a matrix given
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL;
    set <- function(newMatrix){
        x <<- newMatrix;
        inverseMatrix <- NULL;    
    }
    get <- function() x;
    setInverseMatrix <- function (newInverseMatrix) inverseMatrix <<- newInverseMatrix;
    getInverseMatrix <- function() inverseMatrix;

    list( set = set, 
          get = get, 
          setInverseMatrix = setInverseMatrix, 
          getInverseMatrix = getInverseMatrix);
}


## This function calculates a inverse matrix and cache it in the given special matrix object.
## If you call this function again with the same special matrix as parameter it will 
## return the cache of the previous calculation
cacheSolve <- function(x, ...) {
    currentInverseMatrix <- x$getInverseMatrix();
    if (is.null(currentInverseMatrix)){
        currentMatrix <- x$get();
        currentInverseMatrix <- solve(currentMatrix, ...);
        x$setInverseMatrix(currentInverseMatrix);
    }
    
    currentInverseMatrix;
}
