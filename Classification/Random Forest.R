library(ggplot2)
#improves some of ggplot2's settings
library(cowplot)
library(randomForest)

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)

#name the columns after the ones found on the UCI website
colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment 
  "ca",   # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  "hd" # (the predicted attribute) - diagnosis of heart disease 
)

str(data)

## First, replace "?"s with NAs.
data[data == "?"] <- NA

## Now add factors for variables that are factors and clean up the factors that had missing data
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca) 
# since this column had "?"s in it (which I converted to NAs) R thinks that the levels for the factor 
# are strings, but I  know they are integers, so convert the strings to integers
data$ca <- as.factor(data$ca)  # then convert the integers to factor levels

data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)

## replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor


## NOTE: For most machine learning methods, I need to divide the data
## manually into a "training" set and a "test" set. This allows me to train 
## the method using the training data, and then test it on data it was not
## originally trained on. 
##
## In contrast, Random Forests split the data into "training" and "test" sets 
## for me. This is because Random Forests use bootstrapped
## data, and thus, not every sample is used to build every tree. The 
## "training" dataset is the bootstrapped data and the "test" dataset is
## the remaining samples. The remaining samples are called
## the "Out-Of-Bag" (OOB) data.

## impute any missing values in the training set using proximities
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)
## NOTE: iter = the number of iterations to run. Breiman says 4 to 6 iterations is usually good enough. 
## With this dataset, when I set iter=6, OOB-error bounces around between 17% and 18%. 

model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)

#format error rate information
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))

#plotting error rates
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

## MDS Plot

## Convert the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
