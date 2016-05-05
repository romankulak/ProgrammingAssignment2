## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### in this function code is simmilar to the example
#### This function creates a matrix
makeCacheMatrix <- function(x=matrix()) {
  #initialize empty variable for invert matrix
  invert <- NULL
  
  # function that sets value to x in global environment (using: "<<-" )
  set <- function(y){
    x<<- y
    invert <<-NULL 
  }
  get <- function() x
  setinvert <- function(solve) invert <<- solve
  getinvert <- function() invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invert <- x$getinvert()
    
    # if var. "invert" contains the data it returns the value:
    if(!is.null(invert)) {
      message("getting cached data")
      return(invert)
    }# but if there is no invert computed, then it skips the "if" block and computes it: 
    
    data <- x$get()
    invert <- solve(data, ...) #calls the solve function to invert the matrix
    x$setinvert(invert)
    invert #inverted matrix returned
}

##====== Testing ==============

my_matrix <- makeCacheMatrix(matrix(1:4,nrow = 2,ncol=2))
my_matrix$get()
# Output:
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
cacheSolve(my_matrix)
#Output:
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# Call cacheSolve again, and in returns cached data with the message:
cacheSolve(my_matrix)
#Output:
#getting cached data
#   [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5



