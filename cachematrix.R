## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Setter and Getter methods for the matri and its inverse
## setMatrix()  --> Sets the matrix
## getMatrix()  --> Gets the matrix
## setInverse() --> Sets the inverse of the matrix
## getInverse() --> Gets the inverse of the matrix

## Usage : 

## a = makeCacheMatrix(matrix(c(2,3,4,5,6,7,8,9,1),3,3))
## a$get()
## cacheSolve(a) 
## a$getInverse()


makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        setMatrix <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        getMatrix <- function() x
        setMatrixInverse <- function(mi) matinv <<- mi
        getMatrixInverse <- function() matinv
		list(set = setMatrix, get = getMatrix, setInverse = setMatrixInverse, getInverse = getMatrixInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix. It first searches the cache, if found 
## gets it from the cache, else computes it.

cacheSolve <- function(x = matrix(),...) {
		matinv <- x$getInverse()
		if (!is.null(matinv))	{
			return(matinv)
		}
		matinv <- solve(x$get())
		x$setInverse(matinv)
		matinv
}

