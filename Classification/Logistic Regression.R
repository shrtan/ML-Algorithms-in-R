library(tidyverse)
bc <- read_csv("breast-cancer-wisconsin.data", col_names = FALSE)

colnames(bc) <- c("ID", "Clump Thickness", "Cell Size", "Cell Shape", 
                  "Marginal Adhesion", "Single Epithelial Cell Size", "Bare Nuclei",
                  "Bland Chromatin", "Normal Nucleoli", "Mitoses", "Class")

bc$`Clump Thickness` <- as.factor(bc$`Clump Thickness`)
bc$`Cell Size` <- as.factor(bc$`Cell Size`)
bc$`Cell Shape` <- as.factor(bc$`Cell Shape`)
bc$`Marginal Adhesion` <- as.factor(bc$`Marginal Adhesion`)
bc$`Single Epithelial Cell Size` <- as.factor(bc$`Single Epithelial Cell Size`)
bc$`Bare Nuclei` <- as.factor(bc$`Bare Nuclei`)
bc$`Bland Chromatin` <- as.factor(bc$`Bland Chromatin`)
bc$`Normal Nucleoli` <- as.factor(bc$`Normal Nucleoli`)
bc$`Mitoses` <- as.factor(bc$`Mitoses`)
bc$`Class` <- as.factor(bc$`Class`)


library(caret)
library(caTools)

split <- sample.split(bc$Class, SplitRatio = 0.8)
train <- subset(bc, split == TRUE)
test <- subset(bc, split == FALSE)

logistic_classifier <- train(Class ~ ., data = train, 
                             trControl = trainControl(method = "cv", number = 10),
                             method = "glm",
                             family = "binomial")


y_hat <- predict(logistic_classifier, test)

cm <- table(unlist(test[11]), as.factor(y_hat))
mean(unlist(test[11]) == as.factor(y_hat))



