# 19/05/01
# Group
# Team Project

rm(list=ls())

# Data import
data <- read.csv("data.csv")

# 3. Explore, clean, and pre-process data :
n <- dim(data)[1]
p <- dim(data)[2]
View(data)
head(data)
dim(data)
summary(data)

t(t(names(data)))

plot(data)

## Missing values ?
data <- na.omit(data)

## Data visualisation

### Time series
data.ts <- data[, c(2, 8)]
library(forecast)
market.ts <- ts(data.ts$Market.Cap..., start = c(2015, 1), end = c(2019, 200), freq = 365)
plot(market.ts, xlab = "year", ylab = "Market.Cap (in $)")


# Scatter Plot, Histograms, Box Plot, Head Maps ...

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
#maybe we need some understanding about each variables.
#find relation of two variables in our datafile
round(cor(data.pre),2)
                                                   
## PCA
pcs <- prcomp(data[, -2], scale. = TRUE) 
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


# 6. Data Partition
train.index <- sample(1:n, n*0.5)
valid.index <- sample(setdiff(1:n, train.index), n*0.3)
test.index <- setdiff(1:n, union(train.index, valid.index))

train.data <- data[train.index, ]
valid.data <- data[valid.index, ]
test.data <- data[test.index, ]
#maybe we need some copied data named data.pre for using lm analysis and other method related only numerical variables. 
#so I declare Another data frame named ' data.pre ' : deleting "x" and "tx_date" column       
