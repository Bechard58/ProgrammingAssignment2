        ##	x: a square invertible matrix
        ##	return a list containing functions to
        ##	1. set the matrix (variable Conjunto (set))
        ##	2. get the matrix (variable Obtener(get))
        ##	3. set the inverse (variable Inversa)
        ##	4. get the inverse (variable Inversa)

        ##	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (x = matrix()) { 
                
        Inversa = NULL
        Conjunto <- function (y) {  # use “<<-“ to assign a value to an object in an environment different from the current environment

                x <<- y
                Inversa <<- NULL
        }

        Obtener = function () x
        Conjunto_inversa = function (inverse) Inversa <<- inverse 
        Obtener_inversa = function () Inversa
        list (Conjunto = Conjunto, Obtener = Obtener, Conjunto_inversa = Conjunto_inversa, Obtener_inversa = Obtener_inversa)
}


         ##	cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
         ##	If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
         ##	retrieve the inverse from the cache.

cacheSolve <- function (x, ...) { 

        ## x: output of makeCacheMatrix ()
        ## return: inverse of the original matrix input to makeCacheMatrix ()
        
        Inversa = x$Obtener_inversa ()
        
        if (!is.null (Inversa)) {  ##	if the inverse has already been calculated
                message ("Getting Cached Data")
                return (Inversa)
        }
        mat.data = x$Obtener ()
        Inversa = solve (mat.data, ...)
        ##	sets the value of the inverse in the cache via the Conjunto_inversa function.
        x$Conjunto_inversa (Inversa)
        
        return (Inversa)
}

