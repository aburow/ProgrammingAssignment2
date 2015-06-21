## Test script

q <- matrix(c(-1, -2, 1, 1), 2,2)
q
tst <- makeCacheMatrix(q)
inv <- cacheSolve(tst)
inv
inv <- cacheSolve(tst)

