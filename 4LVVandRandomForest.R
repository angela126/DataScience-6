#Read in data
essay.grader.data <- read.delim(file = "E:/Program Files/RStudio/my/Homework/FinalProject/Data/train.tsv", header=TRUE, sep= "\t" , stringsAsFactors= FALSE)
save(essay.grader.data, file = "essayGraderData.rda")

class(essay.grader.data)
names(essay.grader.data)
head(essay.grader.data)

#============== Calculate the number of words in each essay  =============
require(stringr)
require(plyr)
require(tau)
require(randomForest)

#Train essays for each essay set
kappa.metrics <- vector("numeric", 5)
for(i in 1 : 5){  
    
    #Train the i essay set
    data.for.each.set <- essay.grader.data[essay.grader.data$set == i, ]  
    if(sum(is.na(data.for.each.set$grade)) != 0){
        data.for.each.set <- data.for.each.set[-which(is.na(data.for.each.set$grade)), ]  # delete the essay which has no grade
    }
    
    #generate the training data and testing data
    the.order <- sample(dim(data.for.each.set)[1])
    train.data <- data.for.each.set[the.order[1: (0.7*(dim(data.for.each.set)[1]))], ]
    test.data <- data.for.each.set[the.order[(0.7*(dim(data.for.each.set)[1]) + 1) : (dim(data.for.each.set)[1])], ]
    
    #Calculate the features of each eaasy
    words.of.train <- PEGFuction(train.data$essay)
    words.of.test <- PEGFuction(test.data$essay)
    
    words.of.train <- cbind(words.of.train, train.data$grade)
    names(words.of.train)[4] <- "grade"
    words.of.test <- cbind(words.of.test, test.data$grade)
    names(words.of.test)[4] <- "grade"
    
    n <- dim(words.of.train)[1]
    words.data.frame <- rbind(words.of.train, words.of.test)
    
    ## RandomForest
    #formular = train.data$grade ~ random.set$length.sig + random.set$variance + random.set$vocabulary
    modForest.LVV <- randomForest(grade ~., data = words.data.frame[1:n, ], importance=TRUE, proximity=TRUE, na.action=na.omit)
    summary(modForest.LVV)

    #test
    #test <- as.data.frame(cbind(essay.test.length.sig, essay.test.variance, essay.test.vocabulary))
    #test <- as.matrix(test)
    pre.forest <- predict(modForest.LVV, words.data.frame[(n + 1): dim(words.data.frame)[1], 1:3])
    pre.forest <- round(pre.forest)
    
    plot(pre.forest ~ words.of.test$grade)
    
    round(importance(modForest.LVV), 2)
        
    #Calculate the number of correctly graded essays
    n.pred.right <- sum(pre.forest == words.of.test$grade)
    size.test <- dim(words.of.test)[1]
    print(c(size.test, n.pred.right, n.pred.right/size.test))  #print the number of test set, the number of correctly graded passige and their ratio
    
    #Use the kappa metrics to evaluate
    Rater<- data.frame(words.of.test$grade, pre.forest)
    kappa.metrics[i] <- kappa.metrics.fun(Rater, i)
}

#
print(kappa.metrics)
kappa.metrics[kappa.metrics > 0.999] <- 0.999

z.kappa.metrics <- 0.5 * log((1+kappa.metrics)/(1-kappa.metrics))
z <- mean(z.kappa.metrics)

Kappa <- (exp(2*z) - 1) / (exp(2*z) + 1)
print(Kappa)

#========== My function =====================

PEGFuction <- function(x){
    #browser()
    #print(class(x))
    num.of.words <- vector("numeric", length(x))
    var.of.words <- vector("numeric", length(x))
    vocabulary <- vector("numeric", length(x))
    
    num.of.words <- ldply(x, cal.num)  #计算单词数
    num.of.words.sig <- num.of.words ^ 0.25  #单词数的四次方根
    
    var.of.words <- ldply(x, cal.word.var)  #计算方差
    vocabulary <- ldply(x, cal.Vocabulary)  #计算单词量
    
    words.feature <- cbind(num.of.words, var.of.words, vocabulary)
    names(words.feature) <- c("length", "variance", "vocabulary")
    
    return(words.feature)
}

#============单词数
cal.num <- function(essay){
    #print("Calculating number of words.")
    
    #words number
    sentence <- unlist(strsplit(essay, "\\.|\\!|\\?|\\,"))  #split each essay into sentence 
    x <- unlist(strsplit(sentence, ' '))  #split the sentence into words
    words <- remove_stopwords(x, words = c("","\\"))  #remove stopwords
    num.of.words <- length(words)
    
    return(num.of.words)    
}

#=========方差
cal.word.var <- function(essay){
    
    #words number
    sentence <- unlist(strsplit(essay, "\\|\\.|\\!|\\?|\\,"))  #split each essay into sentence 
    x <- unlist(strsplit(sentence, ' '))  #split the sentence into words
    words <- remove_stopwords(x, words = c("","\\"))  #remove stopwords
    
    #words variance
    if(any(!is.na(str_extract(string=words, pattern="^@.+$")))){
        words <- words[-which(!is.na(str_extract(string=words, pattern="^@.+$")))]
    }
    
    words <- tolower(words)
    words <- unique(words)  #########
    
    num.words <- str_length(words)
    var.words <- var(num.words)
    
    return(var.words)
}

#================== 单词量
cal.Vocabulary <- function(essay){
    #browser()
    #print("Calculating number of words.")
    #essay <- train.data$essay[2]  ######
    
    #words number
    sentence <- unlist(strsplit(essay, "\\|\\.|\\!|\\?|\\,"))  #split each essay into sentence 
    x <- unlist(strsplit(sentence, ' '))  #split the sentence into words
    words <- remove_stopwords(x, words = c("","\\"))  #remove stopwords
    
    #words variance
    if(any(!is.na(str_extract(string=words, pattern="^@.+$")))){
        words <- words[-which(!is.na(str_extract(string=words, pattern="^@.+$")))]
    }
    
    words <- tolower(words)
    cal.Vocabulary <- length(unique(words))
    return(cal.Vocabulary)
}

#=========================================================================================
kappa.metrics.fun <- function(rater, set.num){
    #browser()
    
    #rater <- r
    print(class(rater))
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
    
    #
    O <- matrix(0, nrow = n, ncol = n)
    
    for(j in 1:N){
        O[rater[j, 1], rater[j, 2]] <- O[rater[j, 1], rater[j, 2]] +1
    }
    sum(O)    
    
    #
    W <- matrix(0, nrow = n, ncol = n)
    W <- ((row(W) - col(W)) ^ 2) / ((n - 1) ^ 2)
    
    #
    E <- matrix(0, nrow = n, ncol = n)
    
    table.A <- vector("numeric", n)
    table.B <- vector("numeric", n)
    for(j in 1:n){
        table.A[j] <- sum(raterA == j)
        table.B[j] <- sum(raterB == j)
    }
    
    E.table <- as.matrix(table.A) %*% t(as.matrix(table.B)) 
    
    E <- E.table
    #mean.E <- mean(E)
    #var.E <- var(E)
    #E <- (E - mean.E) / var.E
    #sum(E) 
    #min.E <- min(E)
    #max.E <- max(E)
    #E <- (E - min.E) / (max.E - min.E)
    #E <- E * 100
    #sum(E)
    E <- E * sum(O) / sum(E)
    
    
    #
    k <- 1 - (sum(W * O) / sum(W * E))
    return(k)
}

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
