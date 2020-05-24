## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix returns list of functions set,get,setInv,getInv
makeCacheMatrix <- function(x = matrix()){
	Inv <- NULL # The required inverse of matrix x
	set <- function(y) {#This sets the matrix (x's) value to y and as inverse can change it's set to null
		x <<- y
		Inv <<- NULL
	}
	get <- function() x #Returns the matrix (x's) value
	setInv <- function(i) Inv <<- i #inverse Inv is set to i by this
	getInv <- function() Inv # Inverse Inv is returned by this
	list(set = set, get = get,setInv = setInv,getInv = getInv)#this returns list of functions as our new cache-matrix
}


## Write a short comment describing this function
#cacheSolve returns Inverse of matrix created by makeCacheMatrix 
#it checks if Inv is already calculated else its calculated and stored
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	Inv <- x$getInv()#gets Inverse corresponding to x from this
	if(!is.null(Inv)){#if null then calculate again else return Inv
		message("getting cached data")
		return(Inv)
	}
	#Here => Inv is null 
	data <- x$get()#data is matrix whose inverse we are interested in
	Inv <- solve(data, ...)#Calculating Inverse of data with Solve function
	x$setInv(Inv)#Set the inverse corresponding to x as Inv
	Inv#return the value of inverse
}
