library(ggplot2)
library(class)
library(caret)
library(dplyr)
library(reshape2)
library(gridExtra)
library(GGally)

data <- read.csv("D:/college/sem 6/R-lab/datasets/Mall_Customers.csv")

data$Gender <- as.factor(data$Gender)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data_norm <- as.data.frame(lapply(data[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], normalize))
colnames(data_norm) <- c("Age", "Income", "Score")

data_norm$Gender <- data$Gender

set.seed(123)
data_norm$Segment <- kmeans(data_norm[, 1:3], centers = 4, nstart = 25)$cluster
data_norm$Segment <- as.factor(data_norm$Segment)

index <- createDataPartition(data_norm$Segment, p = 0.7, list = FALSE)
train_data <- data_norm[index, ]
test_data <- data_norm[-index, ]

knn_pred <- knn(train = train_data[, 1:3],
                test = test_data[, 1:3],
                cl = train_data$Segment,
                k = 5)

confusionMatrix(knn_pred, test_data$Segment)

windows()
print(
ggplot(data_norm, aes(x = Income, y = Score, color = Segment)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Customer Segments by Income & Spending Score", x = "Normalized Income", y = "Spending Score") +
  theme_minimal()
)

library(plotly)
print(
plot_ly(data_norm, x = ~Age, y = ~Income, z = ~Score,
        color = ~Segment, colors = c('#FF5733','#33FF57','#3357FF','#FF33A1'),
        type = 'scatter3d', mode = 'markers')
)