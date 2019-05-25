# 19/05/01
# Group
# Team Project

rm(list=ls())

# Data import
data <- read.csv("data.csv")


# 3. Explore, clean, and pre-process data :
n <- dim(data)[1]
p <- dim(data)[2]
# View(data)
head(data)
dim(data)
summary(data)
t(t(names(data)))
# plot(data)

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
train.index <- sample(1:n, n*0.5)
valid.index <- sample(setdiff(1:n, train.index), n*0.3)
test.index <- setdiff(1:n, union(train.index, valid.index))

train.df <- data.pre[train.index, ]
valid.df <- data.pre[valid.index, ]
test.df <- data.pre[test.index, ]


# 7, 8, 9

# Models
## Linear model
lm.full <- lm(Market.Cap... ~ ., data = train.df)
plot(lm.full)
options(scipen = 999)
summary(lm.full)
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
lm.full.step <- step(lm.full, direction = "both")
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

for(i in 1:kMax) {
  knn.pred <- class::knn(train.norm.df[, predictors], valid.norm.df[, predictors], cl = train.norm.df[, 5], k = i)
  accuracy.df[i, 2] <- accuracy(as.numeric(knn.pred), valid.norm.df$Market.Cap...)[2]
}
# accuracy.df
k <- which.min(accuracy.df[,2])
k
accuracy.df[k,2]

### Evaluating Predictive Performance of KNN
knn.pred <- class::knn(train.norm.df[, predictors], valid.norm.df[, predictors], cl = train.norm.df[, 5], k = 3)
knn.pred <- as.numeric(as.character(knn.pred))

accuracy(knn.pred, valid.norm.df$Market.Cap...)

#### Lift chart
gain <- gains(valid.df$Market.Cap..., knn.pred)
options(scipen=999) 
plot(c(0, gain$cume.pct.of.total*sum(valid.df$Market.Cap...)) ~ c(0, gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Market.Cap...", main="LiftChart", 
     type="l")
lines(c(0, sum(valid.df$Market.Cap...)) ~ c(0, dim(valid.df)[1]), col = "gray", lty = 2)

#### Decile-wise lift chart
barplot(gain$mean.resp/mean(valid.df$Market.Cap...), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", 
        main = "Decile-wise lift chart")

