makeCacheMatrix <- function(mat = matrix()) { ##creation of smat object
	 inv <- NULL   ##inicialization of inv mat.
	set <- function(y) {  ##function of set matrix's values.##if matrix changed, then  inverse is initialized a new
		mat <<- y
		inv <<- NULL   
	}
	 get <- function() mat	 ##return mat
	setinv <- function(inverse) inv <<- inverse  ##set inv mat.
	getinv <- function() inv ##return inverse of matrix, if matrix inverse hasn't calculated yet then return null
       	list(set = set,get = get, ##list of functions
		setinv = setinv,
		getinv = getinv)
}

cacheSolve <- function(mat) { ## this function calcule and return the inverse of special matrix 'mat'
	inv <- mat$getinv() ## first get the inv matrix.
        if(!is.null(inv)) { ## if inverse has already calculated then return it.
                message("getting cached data")
                return(inv)
        }
         data <- mat$get() ##if not, then get data of mat
         inv <- solve(data) ##calcule inverse of adquired data
        mat$setinv(inv) ##set the inverse of object mat
        inv	##return the inverse
}
