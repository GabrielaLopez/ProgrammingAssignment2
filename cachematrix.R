## Assignment Week 3. Gabriela Lopez-Gonzalez
## The functions work using a square matrix. Once the Matrix  is called the inverse is calculated and then cached. If the solution is cached the solution is called.


## The Matrix function creates a matrix object where the inverse can be stored and then retrieved by the cacheSolve function.

##For testing purposes I used a code that generates  3 square matrices.


md <-  matrix ({i <- 1:5 ; 5 / outer ((5 - i), i, "+")}, nrow=5, ncol=5)
md2 <- matrix ({i <- 1:6 ; 6 / outer ((6 - i), i, "+")}, nrow=6, ncol=6)
md3 <- matrix ({i <- 1:3 ; 3 / outer ((6 - i), i, "+")}, nrow=3, ncol=3)


## "makeCacheMatrix" function. This function takes a matrix (x). 
## If a Matrix is first submitted to cachesolve, the solucion is assigned to "inv" in the cachesolve function and is set in the makeCacheMatrix function.
## The new solution can be retrieved using the gesolve function.

makeCacheMatrix <- function (x = matrix ()) { 
        inv <- NULL
        ## the set function is not called in the cachesolve function but can be used to update the matrix and test the functionality.
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setsolve <- function (solve) inv <<- solve
        getsolve <- function () inv
        list (set = set, get = get, 
              setsolve = setsolve,
              getsolve = getsolve)
        
        
}


## The cachesolve function first gets the matrix cached in makeCacheMatrix. If the solution is available the solucion is used.
## If the matrix is new it applies "solve" to the new Matrix. The new solution is assigned to "inV.



cacheSolve <- function(x, ...) {
        inv <- x$getsolve ()
        if ( !is.null (inv)){
                message ("Getting cached data")
                return (inv)
        }
        data <- x$get ()
        message ("Using new matrix")
        inv <- solve (data, ...)
        x$setsolve(inv)
        inv
        
}
