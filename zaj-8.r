library("rpart")
library("dplyr")
library("party")
data <- as.data.frame(matrix(c(1000, 1300, 1000, 1300, 1200, 1300, 1400, 1200, 1300, 1100,
                               7, 6, 5, 6, 7, 6, 9, 7, 6, 6.5,
                               42, 53, 60, 42, 60, 53, 42, 60, 60, 54,
                               1199, 1399, 799, 1599, 1249, 1249, 3199, 1899, 1079, 1599), 10, 4))
labels <- c("1", "2", "3", "4", "5")
result <- c(1, 4, 5, 4, 3, 4, 1, 2, 3, 2)
factors <- factor(result, labels)
data <- cbind(data, factors)
colnames(data) <- c("Speed", "Capacity", "Size", "Price","Rank")

mydata <- c("training", "test") %>% sample(nrow(data), replace=T) %>% split(data, .)
rtree_fit <- rpart(Rank ~ ., mydata$training, control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
par(mar = rep(0.2, 4))
plot(rtree_fit, uniform = TRUE)
text(rtree_fit, use.n = TRUE)
