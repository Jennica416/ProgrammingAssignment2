## These two commands create a matrix from object x with the 
## number of rows specifed by nrow and the number of columns by 
## ncol. They then take the inverse of this matrix. The answer is 
## stored so future work with object X in the cacheSolve function
## will display the stored value without having to re-calculate
## it.  

## The first command will create a matrix based on the object x,
## with a specific number of rows (nrow) and columns (ncol). 
## If the matrix is not square (a prerequisite for 

makeCacheMatrix <- function(x, nrow, ncol) {
	x<- matrix(x, nrow, ncol)
	m <- NULL
	set<-function(y){
		x <<-y
		m <<-NULL
	}
	get <- function()x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, 
	getinverse=getinverse)
}

## This takes the matrix from above and calculates the matrix's
## inverse. It then stores this value so next time the same object
## is called upon in the below function, it displays the 
## stored answer without re-calculating it 

cacheSolve <- function(x, ...) {
      m <-x$getinverse()
	if(!is.null(m)){
		message("Getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setinverse(m)
	m
}
