# 19/05/01

# Group

# Team Project



rm(list=ls())



# Data import

data <- read.csv("data.csv")



# 3. Explore, clean, and pre-process data :

n <- dim(data)[1]
#p is really need?
p <- dim(data)[2]

# View(data)

head(data)

dim(data)

summary(data)

t(t(names(data)))

# plot(data)
#this is done by hyeonjae 's python code which is upladed in github, form of html files.


## Missing values ?

data <- na.omit(data)



## Data visualisation

### Time series

library(forecast)

market.ts <- ts(data[, c(2, 8)]$Market.Cap..., start = c(2015, 1), end = c(2019, 200), freq = 365)

plot(market.ts, xlab = "year", ylab = "Market.Cap (in $)")



# 4. Data dimension reduction

## Data summary for understanding each variables.

data.pre <- data[,-c(1,2)]

data.summary <- data.frame(mean = sapply(data.pre, mean), 
                           
                           sd = sapply(data.pre, sd), 
                           
                           min = sapply(data.pre, min),
                           
                           max = sapply(data.pre, max), 
                           
                           median = sapply(data.pre, median), 
                           
                           length = sapply(data.pre, length), 
                           
                           miss.val = sapply(data.pre, function(x) 
                             
                             sum(length(which(is.na(x)))))) 







# maybe we need some understanding about each variables.

# find relation between two variables in our datafile

round(cor(data.pre), 2)

# data.pre <- data[, -4]                                            



## PCA

pcs <- prcomp(data.pre, scale. = TRUE) 

summary(pcs)

pcs$rot[,1:5]                                                   

plot(pcs)

library(factoextra)

fviz_eig(pcs)

fviz_pca_var(pcs,
             
             col.var = "contrib", # Color by contributions to the PC
             
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             
             repel = TRUE     # Avoid text overlapping
             
)



# maybe we need some copied data named data.pre for using lm analysis and other method related only numerical variables. 

# so I declare Another data frame named ' data.pre ' : deleting "x" and "tx_date" column   



# We are going to remove the variables CUM and tx_count because of high correlations

data.pre <- data[, -c(3, 10)]





# 6. Data Partition

## We remove the variables "X", "tx_date" in order to fit the models

data.pre <- data.pre[, -c(1, 2)]

set.seed(123)  # set seed for reproducing the partition

train.index <- sample(1:n, n*0.4)

valid.index <- sample(setdiff(1:n, train.index), n*0.4)

test.index <- setdiff(1:n, union(train.index, valid.index))



train.df <- data.pre[train.index, ]

valid.df <- data.pre[valid.index, ]

test.df <- data.pre[test.index, ]





# 7, 8, 9

# Models

## Linear model --> maybe we replae this linear regression model with hyeonjae's python code named LinearRegression.py 
## But also this linear regression model maybe need gor the comparison to neural net in R, comparing MSE.
lm.full <- lm(Market.Cap... ~ ., data = train.df)

plot(lm.full)

options(scipen = 999)



summary(lm.full)

pr.lm <- predict(lm.full,test.df)

MSE.lm <- sum((pr.lm -test.df$Market.Cap...)^2)/nrow(test.df)

# summary(lm(Market.Cap... ~ ., data = data[train.index, -c(1,2)]))





### Evaluating Predictive Performance of linear model

library(forecast) 

lm.full.pred <- predict(lm.full, valid.df)

accuracy(lm.full.pred, valid.df$Market.Cap...)



all.residuals <- valid.df$Market.Cap... - lm.full.pred

hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")





#### Lift chart

library(gains)

gain <- gains(valid.df$Market.Cap..., lm.full.pred)

options(scipen=999) 

plot(c(0, gain$cume.pct.of.total*sum(valid.df$Market.Cap...)) ~ c(0, gain$cume.obs), 
     
     xlab="# cases", ylab="Cumulative Market.Cap...", main="LiftChart", 
     
     type="l")

lines(c(0, sum(valid.df$Market.Cap...)) ~ c(0, dim(valid.df)[1]), col = "gray", lty = 2)





#### Decile-wise lift chart

barplot(gain$mean.resp/mean(valid.df$Market.Cap...), names.arg = gain$depth,
        
        xlab = "Percentile", ylab = "Mean Response", 
        
        main = "Decile-wise lift chart")





# 9. 

## Exhaustive Search 

library(leaps)

search <- regsubsets(Market.Cap... ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")

summary(search)


## Stepwise

lm.full.step <- step(lm.full, direction = "both",intercept = FALSE)

summary(lm.full.step)

### Best model = with 0 predictors ...



## KNN
### Data Normalization
predictors <- c(1:4, 6) 
train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df <- test.df


library(caret)
norm.values <- preProcess(train.df[, predictors], method = c("center", "scale"))
train.norm.df[, predictors] <- predict(norm.values, train.df[, predictors])
valid.norm.df[, predictors] <- predict(norm.values, valid.df[, predictors])
test.norm.df[, predictors] <- predict(norm.values, test.df[, predictors])


### Fit model
library(class)
library(forecast)
kMax <- 10
accuracy.df <- data.frame(k = seq(1, kMax, 1), accuracy = rep(0, kMax))
set.seed(123)  # set seed for reproducing the partition

for(i in 1:kMax) {
  knn.pred <- class::knn(train.norm.df[, predictors], valid.norm.df[, predictors], cl = train.norm.df[, 5], k = i)
  accuracy.df[i, 2] <- accuracy(as.numeric(knn.pred), valid.norm.df$Market.Cap...)[2]
}
accuracy.df
k <- which.min(accuracy.df[,2])
k
accuracy.df[k,2]


### Evaluating Predictive Performance of KNN on validation set
knn.pred <- class::knn(train.norm.df[, predictors], valid.norm.df[, predictors], cl = train.norm.df[, 5], k = 3)
knn.pred <- as.numeric(as.character(knn.pred))
accuracy(knn.pred, valid.norm.df$Market.Cap...)

#### Graph real vs predicted
par(mfrow=c(1,1))
options(scipen=1) 
plot(valid.df$Market.Cap..., knn.pred, xlab = "Real values", ylab = "Predicted value", col='red', main='Real vs predicted values - Validation Set - KNN', pch=18, cex=0.7)
abline(0, 1, lwd=2)
legend('bottomright',legend=c('Predicted value'),pch=18,col=c('red'))


#### Lift chart
gain <- gains(valid.df$Market.Cap..., knn.pred)
options(scipen=1) 
plot(c(0, gain$cume.pct.of.total*sum(valid.df$Market.Cap...)) ~ c(0, gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Market.Cap...", main="LiftChart - Validation Set - KNN", 
     type="l")
lines(c(0, sum(valid.df$Market.Cap...)) ~ c(0, dim(valid.df)[1]), col = "gray", lty = 2)
legend('bottomright', legend = c("Cumulative using predicted values", 
                                      "Cumulative using average"), col = c(1, "gray"), lty = c(1, 2))

#### Decile-wise lift chart
barplot(gain$mean.resp/mean(valid.df$Market.Cap...), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", 
        main = "Decile-wise lift chart - Validation Set - KNN")


### Evaluating Predictive Performance of KNN on test set
knn.pred <- class::knn(test.norm.df[, predictors], test.norm.df[, predictors], cl = test.norm.df[, 5], k = 3)
knn.pred <- as.numeric(as.character(knn.pred))
accuracy(knn.pred, test.norm.df$Market.Cap...)

#### Graph real vs predicted
par(mfrow=c(1,1))
plot(test.df$Market.Cap..., knn.pred, xlab = "Real values", ylab = "Predicted value", col='red', main='Real vs predicted values - Test Set - KNN', pch=18, cex=0.7)
abline(0, 1, lwd=2)

#### Lift chart
gain <- gains(test.df$Market.Cap..., knn.pred)
options(scipen=1) 
plot(c(0, gain$cume.pct.of.total*sum(test.df$Market.Cap...)) ~ c(0, gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Market.Cap...", main="LiftChart - Test Set - KNN", 
     type="l")
lines(c(0, sum(test.df$Market.Cap...)) ~ c(0, dim(test.df)[1]), col = "gray", lty = 2)
legend(120, 1500000000000, legend = c("Cumulative using predicted values", 
                                      "Cumulative using average"), col = c(1, "gray"), lty = c(1, 2))

#### Decile-wise lift chart
barplot(gain$mean.resp/mean(test.df$Market.Cap...), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", 
        main = "Decile-wise lift chart - Test Set - KNN")







### neuralnet 

library(neuralnet)

library(nnet)

library(caret)



## for using MSE from neuralnet , data partition with training 75% , validation 25%



# we save market Cap's max ,min value 

# After normalization ->neural net result -> we restore market cap <- market * (max-min) +min to so that predict our result with actual value



maxs <- apply(data.pre,2,max);

mins <- apply(data.pre,2,min);

scaled <- as.data.frame(scale(data.pre, center =mins, scale = maxs - mins))

train_ <- scaled[train.index,];

valid_ <- scaled[valid.index,];

test_ <- scaled[test.index,]



#after preprocessing max-min method and scale the data in the interval [0,1], use neuralnet(): target - marketcap 
mse.df <- matrix(0, kmax, 2)
  
kmax <- 10
for ( i in 1:kmax){
train.nn <- neuralnet(Market.Cap... ~ ., data = train_, hidden =i ,linear.output = F)

plot(train.nn, rep = "best")

pr.nn <- compute(train.nn,test_[,-5])

pr.nn_ <- pr.nn$net.result*(max(data.pre$Market.Cap...)-min(data.pre$Market.Cap...))+min(data.pre$Market.Cap...)

test.r <- (test_$Market.Cap...)*(max(data.pre$Market.Cap...)-min(data.pre$Market.Cap...))+min(data.pre$Market.Cap...)

MSE.nn <- sum((test.r- pr.nn_)^2)/nrow(test_)
mse.df[i,2] <- MSE.nn;
}
mse.df
                                             
#3 is best for hidden
#re assign hedden = 3                                             
train.nn <- neuralnet(Market.Cap... ~ ., data = train_, hidden =3 ,linear.output = F)

plot(train.nn, rep = "best")

pr.nn <- compute(train.nn,valid_[,-5])

pr.nn_ <- pr.nn$net.result*(max(data.pre$Market.Cap...)-min(data.pre$Market.Cap...))+min(data.pre$Market.Cap...)

valid.r <- (valid_$Market.Cap...)*(max(data.pre$Market.Cap...)-min(data.pre$Market.Cap...))+min(data.pre$Market.Cap...)

MSE.nn <- sum((valid.r- pr.nn_)^2)/nrow(valid_)

pr.lm <- predict(lm.full,train.df)
MSE.lm <- sum((valid.df$Market.Cap...- pr.lm)^2)/nrow(valid.df)

par(mfrow=c(1,2))

plot(valid.df$Market.Cap...,pr.nn_,col='red',main='valid vs predicted NN',pch=18,cex=0.7)

abline(0,1,lwd=2)

legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(valid.df$Market.Cap...,pr.lm,col='blue',main='valid vs predicted lm',pch=18, cex=0.7)

abline(0,1,lwd=2)

legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)



par(mfrow = c(1,1))
plot(valid.df$Market.Cap...,pr.nn_,col='red',main='d',pch=18,cex=0.7, xlab = "Real Market Cap", ylab = "Predicted Market Cap")

points(valid.df$Market.Cap...,pr.lm,col='blue',pch=18,cex=0.7)

abline(0,1,lwd=2)

legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
#### Lift chart
val.pred <- compute(valid.nn,valid_[,-5])
val.pred_ <- val.pred$net.result*(max(data.pre$Market.Cap...)-min(data.pre$Market.Cap...))+min(data.pre$Market.Cap...)
library(gains)

gain <- gains(valid.df$Market.Cap..., val.pred_)

options(scipen=999) 

plot(c(0, gain$cume.pct.of.total*sum(valid.df$Market.Cap...)) ~ c(0, gain$cume.obs), 
     
     xlab="# cases", ylab="Cumulative Market.Cap...", main="LiftChart", 
     
     type="l")

lines(c(0, sum(valid.df$Market.Cap...)) ~ c(0, dim(valid.df)[1]), col = "gray", lty = 2)





#### Decile-wise lift chart

barplot(gain$mean.resp/mean(valid.df$Market.Cap...), names.arg = gain$depth,
        
        xlab = "Percentile", ylab = "Mean Response", 
        
        main = "Decile-wise lift chart")

