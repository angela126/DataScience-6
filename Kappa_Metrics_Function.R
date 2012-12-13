#====================  Kappa Metrics Function =====================================================================
kappa.metrics.fun <- function(rater, set.num){
    #browser()
    
    #rater <- r
    #print(class(rater))
    switch(set.num,  #check the number of set
{n = 11
 rater = rater - 1},  #check the N possible ratings for the set 
{n = 5
 rater = rater + 1},   #and correct the ratings from 1
{n = 4
 rater = rater + 1},
{n = 4
 rater = rater + 1},
{n = 5
 rater = rater + 1})
    
    N <- dim(rater)[1]  #number of essays of the set
    raterA <- rater[1]  #rating of human
    raterB <- rater[2]  #rating of automated scoring system
    
    #Calculate O
    O <- matrix(0, nrow = n, ncol = n)
    
    for(j in 1:N){
        #browser()
        O[rater[j, 1], rater[j, 2]] <- O[rater[j, 1], rater[j, 2]] +1
    }
    sum(O)    
    
    #W
    W <- matrix(0, nrow = n, ncol = n)
    W <- ((row(W) - col(W)) ^ 2) / ((n - 1) ^ 2)
    
    #E
    E <- matrix(0, nrow = n, ncol = n)
    
    table.A <- vector("numeric", n)
    table.B <- vector("numeric", n)
    for(j in 1:n){
        table.A[j] <- sum(raterA == j)
        table.B[j] <- sum(raterB == j)
    }
    
    E.table <- as.matrix(table.A) %*% t(as.matrix(table.B)) 
    
    E <- E.table
    E <- E * sum(O) / sum(E)
    
    
    #
    k <- 1 - (sum(W * O) / sum(W * E))
    return(k)
}

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
