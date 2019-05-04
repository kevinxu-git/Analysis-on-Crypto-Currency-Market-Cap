rm(list=ls())
# read our raw data file. 
data <- read.csv("result.csv")
#check whether data file is saved successfully or not.
View(data)
#explore our data. then check whether our data file needs to be preprocessed or not 
head(data)
dim(data)
summary(data)

#preprocessing : 결측치가 있는지 확인하여본다.
data<- na.omit(data)
#print the list in  a useful column format
t(t(names(data)))
n = dim(data)[1]
#data partition : train 50% valid 30% test 20% 
set.seed(1)
train.index <- sample(1:n, n*0.5)

valid.index <- sample(setdiff(1:n, train.index), n*0.3)

test.index <- setdiff(1:n, union(train.index, valid.index))



train.data <- data[train.index, ]

valid.data <- data[valid.index, ]

test.data <- data[test.index, ]

#DATA REDUCTION  : 1st : use Summary about data. 
#before do some numerical summary , let's find a variables that maybe not important for numerical summary, like a x variable, which is just an order of an 
#rows.

data.pre <- data[,-1]

train.pre <- train.data[,-1]
valid.pre <- valid.data[,-1]
test.pre <- test.data[,-1]

# now we do numerical data summary for our data file. we use summary() in R 

data.summary <- data.frame(mean = sapply(data.pre, mean), 
                                 sd = sapply(data.pre, sd), 
                                 min = sapply(data.pre, min),
                                 max = sapply(data.pre, max), 
                                 median = sapply(data.pre, median), 
                                 length = sapply(data.pre, length), 
                                 miss.val = sapply(data.pre, function(x) 
                                   sum(length(which(is.na(x)))))) 

#이 요약에 대한 설명 이 가능한 부분 
#find relation of two variables in our datafile
round(cor(data.pre),2)

#next we use PCA : because our variable's unit is same and there is meaningful correlation between bariables. also our variables are all numerical
pcs <- prcomp(data.pre)
summary(pcs)
pcs$rot[,1:5]
#maybe need to normalization , because even if variable's unit is same, variable's scale is so different

pcs.cor <- prcomp(data.pre, scale. = T)
summary(pcs.cor)
pcs.cor$rot[,1:5]

pcs.cor <- prcomp(data.pre, scale. = T)
summary(pcs.cor)
pcs.cor$rot[,1:5]

# now we get meaningful information , but our purpose is predict market cap . don't forget it 
# so next step , we will predict and evaluate performance with lm , or knn(for numerical variable) and during lm process also we can consider data reduction 
#by exhaustive search or local search 