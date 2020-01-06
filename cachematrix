#
# Parsons - R Programming - Week 3 - Project 2
#
#***********************************************************************************************************************
# Assignment:
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of
# a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will 
# not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#   
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
#
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square 
# invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.
#
#
#***********************************************************************************************************************
# Begin Code

## Put comments here that give an overall description of what your functions do:
#
# These functions creates a special "matrix" object that can cache its invers and computes the inverse of the special "matrix".
#
#
## Write a short comment describing this function:
#
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# Note: Must create a square matrix and pass to arguement as seen in example below
#
# mat1 <- matrix(1:4,2) #Creates a square  2x2 matrix using elements 1,2,3,4
# aMatrix <- makeCacheMatrix(mat1) #Passess mat1 to makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) { # Define makeCacheMatrix function
  
  minv <- NULL
  
  set <- function(y) { #Define set function
    
    x <<- y # Assigne the input argument to the x object in the parent environment
    
    minv <<- NULL # Assign the value of NULL to the m object in the parent environment. This clears an value of m that had been cached by a prior execution of cache matrix inverse.
    
  } #End set function
  
  
  get <- function() x #Define get function. x is not defined it get. R retrieves x from the parent environment.
  
  setminv <- function(solve) minv <<- solve #Define setinv function.
  
  getminv <- function() minv #Define getminv function.
  
  
  list(set = set,           # gives the name 'set' to the set() function defined above
       get = get,           # gives the name 'get' to the get() function defined above
       setminv = setminv,   # gives the name 'setminv' to the setminv() function defined above
       getminv = getminv)   # gives the name 'getminv' to the getminv() function defined above
  
  #Returns functions to the parent environment
  

  
    
} #END makeCacheMatrix function









## Write a short comment describing this function
#
#  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.




cacheSolve <- function(x, ...) { #Define function cacheSolve
  ## Return a matrix that is the inverse of 'x'

  minv <- x$getminv() #calls function getminv(). 1st attempt to retrieve a matrix inverse from the object passed in as an arguement.
  
  if(!is.null(minv)) { #Determine if m is not NULL
    
    message("getting cached data")
    
    return(minv) #Return the value of m if m != NULL
    
  } #END if statement
  
  
  data <- x$get() #Gets data using get function
  
  minv <- solve(data, ...) #Calculates matrix inverse of the data
  
  x$setminv(minv) #sets matrix inverse of the data using setminv
  
  minv #Returns minv
  
} #END cacheSolve


# Example commands and output
#
#> mat1 <- matrix(1:4,2)
#> aMatrix <- makeCacheMatrix(mat1)
#>
#> aMatrix$get()
#  [,1] [,2]
#  [1,]    1    3
#  [2,]    2    4
#> aMatrix$getminv()
#  NULL
#>
#> mat2 <- matrix(5:8, 2)
#> aMatrix$set(mat2)
#> aMatrix$set(mat1)
#> > cacheSolve(aMatrix)
#  [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5
#> aMatrix$getminv()
#  [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5




#***********************************************************************************************************************
# Reference Links for this assignment:
#
# Explanation of makeVector and cacheMean
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
#
# Explanation of matrix calculations
# https://www.r-bloggers.com/quick-review-of-matrix-algebra-in-r/
#
#
#***********************************************************************************************************************
# Notes taken for this assignment:
#
# From Advanced R, Functions: http://adv-r.had.co.nz/Functions.html
#
# 3 Parts of a function: body, formals, environment
#
# 4 principles of lexical scoping: name masking, functions vs. variables, a fresh start, dynamic lookup
#
# Everything in R is an object. Everything that happens in R is a function call. - John Chambers
#
# Define a matrix:  mat1 <- matrix(1:4,2)
#
# Inverse of a matrix: solve(mat1)



