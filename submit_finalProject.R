#Read in data
essay.grader.data.train <- read.delim(file = "E:/Program Files/RStudio/my/Homework/FinalProject/Data/train.tsv", header=TRUE, sep= "\t" , stringsAsFactors= FALSE)
save(essay.grader.data.train, file = "essayGraderDataTrain.rda")
class(essay.grader.data.train)
names(essay.grader.data.train)
head(essay.grader.data.train)

# test set
essay.grader.data.test <- read.delim(file = "E:/Program Files/RStudio/my/Homework/FinalProject/Data/test.tsv", header=TRUE, sep= "\t" , stringsAsFactors= FALSE)
class(essay.grader.data.test)
names(essay.grader.data.test)
head(essay.grader.data.test)
save(essay.grader.data.test, file = "essayGraderDataTest.rda")
#============== Calculate the number of words in each essay  _  train the model =============
require(stringr)
require(plyr)
require(tau)

#Train essays for each essay set

test.pred.grade <- matrix(0,1,1)
for(i in 1 : 5){  
    
    #Train the i essay set
    data.for.each.set <- essay.grader.data.train[essay.grader.data.train$set == i, ]  
    if(sum(is.na(data.for.each.set$grade)) != 0){
        data.for.each.set <- data.for.each.set[-which(is.na(data.for.each.set$grade)), ]  # delete the essay which has no grade
    }
    
    #generate the training data and testing data
    #the.order <- sample(dim(data.for.each.set)[1])
    #train.data <- data.for.each.set[the.order[1: (0.7*(dim(data.for.each.set)[1]))], ]
    #test.data <- data.for.each.set[the.order[(0.7*(dim(data.for.each.set)[1]) + 1) : (dim(data.for.each.set)[1])], ]
    train.data <- data.for.each.set
    test.data <- essay.grader.data.test[essay.grader.data.test$set == i, ]
    
    #Calculate the number of words in each eaasy
    words.num.of.train <- calculateWordsNum(train.data$essay)
    words.num.of.test <- calculateWordsNum(test.data$essay)
    
    #Train the Linear Regression
    lm.numOfWord <- lm(train.data$grade ~ t(as.data.frame(words.num.of.train)))
    summary(lm.numOfWord)
    
    #coefficient of the linear model
    coeff.numOfWord <- as.matrix(coef(lm.numOfWord))
    test <- t(as.data.frame(words.num.of.test))
    test <- data.frame(1, test)
    test <- as.matrix(test)
    
    pred.grad.wordNum <- test %*% coeff.numOfWord
    
    #Plot the original grade and our predicted grade
    plot(words.num.of.test,pred.grad.wordNum)
    
    switch(i,  #check the number of set
           {up.n = 12
            low.n = 2},  #check the N possible ratings for the set
           {up.n = 4
            low.n = 0},   #and correct the ratings from 1
           {up.n = 3
            low.n = 0},
           {up.n = 3
            low.n = 0},
           {up.n = 4
            low.n = 0})
    
    pred.grad.wordNum[pred.grad.wordNum > up.n] <- up.n
    pred.grad.wordNum[pred.grad.wordNum < low.n] <- low.n
    pred.grad <- round(pred.grad.wordNum)
    #a<- data.frame(pred.grad, test.data$grade)
    plot(words.num.of.test, pred.grad)
    hist(train.data$grade)
    hist(pred.grad)
    
    #Calculate the number of correctly graded essays
    #n.pred.right <- sum(pred.grad == test.data$grade)
    #size.test <- dim(test.data)[1]
    #print(c(size.test, n.pred.right, n.pred.right/size.test))  #print the number of test set, the number of correctly graded passige and their ratio
    
    #Use the kappa metrics to evaluate
    #Rater<- data.frame(test.data$grade, pred.grad)
    #kappa.metrics[i] <- kappa.metrics.fun(Rater, i)
    
    test.pred.grade <- rbind(test.pred.grade,pred.grad)
}

test.pred.grade <- test.pred.grade[-1]

essay.grader.data.test <- essay.grader.data.test[, -3]
essay.grader.data.test <- cbind(essay.grader.data.test, weight = 1)
essay.grader.data.test <- cbind(essay.grader.data.test, grade = test.pred.grade)
row.names(essay.grader.data.test)


write.csv(essay.grader.data.test, file = "submission_1.csv", row.names = FALSE)
write.csv(essay.grader.data.test, file = "submission_2.csv", row.names = FALSE)




#======================================================================================================



#========== My function =====================

calculateWordsNum <- function(x){
    #browser()
    #print(class(x))
    num.of.words <- vector("numeric", length(x))
    num.of.words <- lapply(x, cal.num)
    return(num.of.words)
}

cal.num <- function(essay){
    #print("Calculating number of words.")
    
    sentence <- unlist(strsplit(essay, "\\.|\\!|\\?|\\,"))  #split each essay into sentence 
    x <- unlist(strsplit(sentence, ' '))  #split the sentence into words
    words <- remove_stopwords(x, words = "")  #remove stopwords
    num.of.words <- length(words)
    
    return(num.of.words)    
}
