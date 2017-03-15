# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2017-03-15 14:48:35 x86_64-w64-mingw32 

# Rattle version 4.1.0 user 'tthul'

# This log file captures all Rattle interactions as R commands. 

Export this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2017-03-15 15:02:49 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- ds

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2017-03-15 15:02:50 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 10000 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 7000 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 1500 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 1500 observations

# The following variable selections have been noted.

crs$input <- c("Age", "Gender", "Diabetes", "BMI",
     "HHIncome")

crs$numeric <- c("Age", "BMI")

crs$categoric <- c("Gender", "Diabetes", "HHIncome")

crs$target  <- "PhysActive"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2017-03-15 15:05:47 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 10000 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.8*crs$nobs) # 8000 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.2*crs$nobs) # 2000 observations
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("Age", "Gender", "BMI", "HHIncome",
     "PhysActive")

crs$numeric <- c("Age", "BMI")

crs$categoric <- c("Gender", "HHIncome", "PhysActive")

crs$target  <- "Diabetes"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2017-03-15 15:09:35 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

# The 'Hmisc' package provides the 'describe' function.

library(Hmisc, quietly=TRUE)

# Generate a description of the dataset.

describe(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

# The 'basicStats' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Generate a description of the numeric data.

lapply(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)][,c(1, 3)], basicStats)

#============================================================
# Rattle timestamp: 2017-03-15 15:12:35 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Age

library(dplyr) # Provides select().
library(ggplot2) # Provides ggplot(), aes(), geom_density(), xlab(), ggtitle(), labs().

# Generate the plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  select(Age, Diabetes) %>%
  ggplot(aes(x=Age)) +
  geom_density(lty=3) +
  geom_density(aes(fill=Diabetes, colour=Diabetes), alpha=0.55) +
  xlab("Age\n\nRattle 2017-Mar-15 15:12:35 tthul") +
  ggtitle("Distribution of Age (sample)\nby Diabetes") +
  labs(fill="Diabetes", y="Density")

# Display the plots.

library(gridExtra) # Provides grid.arrange().

grid.arrange(p01)

#============================================================
# Rattle timestamp: 2017-03-15 15:18:21 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(Diabetes ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(crs$glm)[1],
            attr(logLik(crs$glm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            crs$glm$null.deviance-crs$glm$deviance,
            crs$glm$df.null-crs$glm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(crs$glm$null.deviance-crs$glm$deviance,
                   crs$glm$df.null-crs$glm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
             cor(crs$glm$y, crs$glm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(crs$glm, test="Chisq"))
cat("\n")

# Time taken: 0.36 secs

#============================================================
# Rattle timestamp: 2017-03-15 15:22:03 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

crs$pr <- as.vector(ifelse(predict(crs$glm, type="response", newdata=crs$dataset[crs$validate, c(crs$input, crs$target)]) > 0.5, "Yes", "No"))

# Generate the confusion matrix showing counts.

table(crs$dataset[crs$validate, c(crs$input, crs$target)]$Diabetes, crs$pr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset[crs$validate, c(crs$input, crs$target)]$Diabetes, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-03-15 15:23:48 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Diabetes ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.14 secs

#============================================================
# Rattle timestamp: 2017-03-15 15:24:05 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 10000 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.8*crs$nobs) # 8000 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.2*crs$nobs) # 2000 observations
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("Age", "Gender", "BMI", "PhysActive")

crs$numeric <- c("Age", "BMI")

crs$categoric <- c("Gender", "PhysActive")

crs$target  <- "Diabetes"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- "HHIncome"
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2017-03-15 15:24:08 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Diabetes ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.03 secs

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)

#============================================================
# Rattle timestamp: 2017-03-15 15:25:08 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 10000 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.8*crs$nobs) # 8000 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.2*crs$nobs) # 2000 observations
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("Age", "Gender", "BMI", "HHIncome",
     "PhysActive")

crs$numeric <- c("Age", "BMI")

crs$categoric <- c("Gender", "HHIncome", "PhysActive")

crs$target  <- "Diabetes"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2017-03-15 15:25:12 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Diabetes ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.08 secs

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)

#============================================================
# Rattle timestamp: 2017-03-15 15:26:15 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree ds $ Diabetes")

#============================================================
# Rattle timestamp: 2017-03-15 15:26:40 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

table(crs$dataset[crs$validate, c(crs$input, crs$target)]$Diabetes, crs$pr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset[crs$validate, c(crs$input, crs$target)]$Diabetes, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-03-15 15:28:36 x86_64-w64-mingw32 

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
crs$rf <- randomForest::randomForest(Diabetes ~ .,
      data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
      ntree=500,
      mtry=4,
      importance=TRUE,
      na.action=randomForest::na.roughfix,
      replace=FALSE)

# Generate textual output of 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted))

# Calculate the AUC Confidence Interval.

pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted))

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 4.17 secs

#============================================================
# Rattle timestamp: 2017-03-15 15:30:07 x86_64-w64-mingw32 

# Plot the relative importance of the variables.

randomForest::varImpPlot(crs$rf, main="")
title(main="Variable Importance Random Forest ds",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Display tree number 1.

printRandomForests(crs$rf, 1)

# Plot the error rate against the number of trees.

plot(crs$rf, main="")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest ds",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Plot the OOB ROC curve.

library(verification)
aucc <- verification::roc.area(as.integer(as.factor(crs$dataset[crs$sample, crs$target]))-1,
                 crs$rf$votes[,2])$A
verification::roc.plot(as.integer(as.factor(crs$dataset[crs$sample, crs$target]))-1,
         crs$rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Random Forest ds",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-03-15 15:32:42 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

table(crs$dataset[crs$validate, c(crs$input, crs$target)]$Diabetes, crs$pr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset[crs$validate, c(crs$input, crs$target)]$Diabetes, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

table(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$Diabetes, crs$pr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$Diabetes, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-03-15 15:33:30 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

table(crs$dataset[crs$validate, c(crs$input, crs$target)]$Diabetes, crs$pr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset[crs$validate, c(crs$input, crs$target)]$Diabetes, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

table(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$Diabetes, crs$pr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$Diabetes, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))
