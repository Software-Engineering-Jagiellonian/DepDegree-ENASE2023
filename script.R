setwd("DEFINE THE WORING DIRECTORY HERE")

library(Rnalytica)
library(corrplot)
library(caret)
library(randomForest)


#############################
# Load and prepare the data #
#############################

d <- read.csv("github-UBD.csv")

#dependent variable 'bug' as factor
d$bug <- as.factor(d$bug)
levels(d$bug) <- c("bug","clean")

#log(x+1) except for density metrics
d[,-c(1, 2, 4, 8, 9, 21, 22, 27, 66, 67)] <- log(d[,-c(1, 2, 4, 8, 9, 21, 22, 27, 66, 67)]+1)

# define sets of variables
indVars <- names(d)[c(3:63)]    # names of independent variables
indDFvars <- names(d)[c(64:65)] # names of independent data-flow variables
depVar <- names(d)[67]          # name of dependent variable


###############################
# Functions used in the study #
###############################

splitdata <- function(d, pr) {
  trainRowNumbers <- createDataPartition(d$bug, p=pr, list=FALSE)
  trainData <- d[trainRowNumbers,]
  testData <- d[-trainRowNumbers,]
  rv <- list("trainData"=trainData, "testData"=testData)
  return(rv)
}

autoSp <- function(d, features, thr, vif) {
  res <- AutoSpearman(dataset = d, metrics = features, 
                             spearman.threshold = thr, 
                             vif.threshold = vif, 
                             groups = FALSE, 
                             verbose = F)
  return(res)
}

# Matthew's correlation coefficient
mcc <- function(tab)
{
  if(nrow(tab) != 2 | ncol(tab) != 2) stop("A 2x2 table is needed")
  tp <- tab[1, 1]
  tn <- tab[2, 2]
  fp <- tab[1, 2]
  fn <- tab[2, 1]
  d1 <- tp + fp
  d2 <- tp + fn
  d3 <- tn + fp
  d4 <- tn + fn
  if(d1 == 0 | d2 == 0 | d3 == 0 | d4 == 0) return(0)
  ((tp * tn) - (fp * fn))/(sqrt(d1)*sqrt(d2)*sqrt(d3)*sqrt(d4))
}


##############################################
# Correlation analysis                       #
##############################################

res <- cor(as.matrix(d[, c(indVars, "DD", "DDD")]))
plot(res[1:(nrow(res)-2),c("DD", "DDD")], xlim=c(0, 1), ylim=c(0,1))
abline(a=0, b=1)

VARS <- autoSp(d[,c(indVars, indDFvars, depVar)], c(indVars, indDFvars), thr=0.7, vif=5)
print(VARS)


##############################################
# Feature importance                         #
##############################################

# Model agnostic approach
fvi <- filterVarImp(d[,3:65], d$bug)
fvi$names <- rownames(fvi)
fvi <- fvi[,c(1,3)]
fvi <- fvi[order(fvi$bug, decreasing=TRUE),]
print(fvi)

#with RandomForest built-in 'importance' method
spl <- splitdata(d[,c(indVars, indDFvars, depVar)], 0.6)
trainData <- spl[["trainData"]]
testData <- spl[["testData"]]
trD <- trainData
tsD <- testData

for (i in 1:100) {
  cat(i,"\n")
  RF <- randomForest(x = trD[,-64], y = trD$bug, 
                     xtest = tsD[,-64], ytest = tsD$bug, 
                     importance = TRUE, data = trD)
  if (i == 1) {im <- importance(RF)} else {im <- im + importance(RF)}
}

mdGini <- sort(im[,"MeanDecreaseGini"]/100, decreasing = TRUE)
pic <- data.frame(metric = names(mdGini[1:20]), imp = mdGini[1:20])
rownames(pic) <- c(1:20)

ggplot(data=pic, aes(x=reorder(metric, -imp), y=imp)) +
  labs(x = "metric", y = "importance") +
  geom_bar(stat="identity", fill="black")+
  geom_text(aes(label=round(imp,1)), angle = 90, vjust=0.4, hjust=1.5, color="white", size=3.5)+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle = 90, vjust = 0.5))


###############################################
# Within-Project Defect Prediction Experiment #
###############################################

# control parameters for caret train() function
fitControl <- trainControl(
  method = 'boot632',              # .632 bootstrap
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = TRUE,               # should class probabilities be returned
  summaryFunction=twoClassSummary  # results summary function
) 

MODELS <- c('glm', 'knn', 'naive_bayes', 'rf', 'rpart')
PROJECTS <- c('broadleaf-3.0.10','ceylon-ide-eclipse-1.1.0','elasticsearch-0.90.11','hazelcast-3.3','mapdb-0.9.6',
              'mcMMO-1.4.06','netty','oryx','titan-0.5.1',)  

filename <- 'results.csv'             # output file (variant 1 of the experiment, 
                                      # with AutoSpearman selecting the variables 
                                      # in each iteration) 
filename <- 'results-VarImpVars.csv'  # output file (variant 2 of the experiment, 
                                      # with fixed set of metrics from 'importance' 
                                      # function built in the RandomForest procedure) 

N <- 100                              # number of iterations

cat("Iter;",sep="",file=filename,append=TRUE)
cat(indVars,sep=";",file=filename,append=TRUE)
cat(";Project;Model;F1X;F1D2;F1D3;F1D4;PrecX;PrecD2;PrecD3;PrecD4;RecallX;RecallD2;RecallD3;RecallD4;AUCX;AUC2;AUC3;AUC4;SENX;SEN2;SEN3;SEN4;SPCX;SPC2;SPC3;SPC4;MCC;MCC2;MCC3;MCC4\n",sep="",file=filename,append=TRUE)

for (p in PROJECTS) {
  
  # filter by project and remove any non-variable column
  d2 <- d[d$ProjectName==p,c(indVars, indDFvars, depVar)]
  
  nbugs <- nrow(d2[d2$bug=="bug",])
  nclean <- nrow(d2[d2$bug=="clean",])
  
  for (i in 1:N) {
    cat("proj=",p," clean=",nclean," bugs=",nbugs, " iter=",i, "\n",sep="")
  
    # generate training and testing samples
    spl <- splitdata(d2, 0.6)
    trainData <- spl[["trainData"]]
    testData <- spl[["testData"]]
  
  # SELECT ONE OF THESE: variables with AutoSpearman...
  VARS <- autoSp(trainData, indVars, thr=0.7, vif=5)  # VARS is denoted by "X" in the paper
  # ... OR fixed set of 10 top features with the highest Gini index  
  #VARS <- c("McCC", "WMC", "CBO", "RFC", "NOI", "NOA", "TNOS", "TLOC", "TLLOC", "LOC")
  
  # other combinations for the analysis (DD, DDD, DD+DDD)
  VARS2 <- c(VARS, "DD")
  VARS3 <- c(VARS, "DDD")
  VARS4 <- c(VARS, "DD", "DDD")
  
  #construct defect models for selected variables with and without a given data flow metric
  for (m in MODELS) {
  cat("  model=",m,"\n",sep="")

    # mdl = X. mdl2 = DD. mdl3 = DDD. mdl4 = DD+DDD
    mdl = train(x = as.data.frame(trainData[,VARS]), y = trainData$bug,  
                method=m, tuneLength = 5, metric='ROC', trControl = fitControl)
    mdl2 = train(x = as.data.frame(trainData[,VARS2]), y = trainData$bug,  
                 method=m, tuneLength = 5, metric='ROC', trControl = fitControl)
    mdl3 = train(x = as.data.frame(trainData[,VARS3]), y = trainData$bug,  
                 method=m, tuneLength = 5, metric='ROC', trControl = fitControl)
    mdl4 = train(x = as.data.frame(trainData[,VARS4]), y = trainData$bug,  
                 method=m, tuneLength = 5, metric='ROC', trControl = fitControl)
    
    ROCpredicted <- predict(mdl, testData, type='prob')
    ROCpredicted2 <- predict(mdl2, testData, type='prob')
    ROCpredicted3 <- predict(mdl3, testData, type='prob')
    ROCpredicted4 <- predict(mdl4, testData, type='prob')
    
    predicted <- predict(mdl, testData)
    predicted2 <- predict(mdl2, testData)
    predicted3 <- predict(mdl3, testData)
    predicted4 <- predict(mdl4, testData)
    
    k <-  data.frame(obs=testData$bug, pred=predicted, bug=ROCpredicted[,'bug'], clean=ROCpredicted[,'clean'])
    k2 <- data.frame(obs=testData$bug, pred=predicted2, bug=ROCpredicted2[,'bug'], clean=ROCpredicted2[,'clean'])
    k3 <- data.frame(obs=testData$bug, pred=predicted3, bug=ROCpredicted3[,'bug'], clean=ROCpredicted3[,'clean'])
    k4 <- data.frame(obs=testData$bug, pred=predicted4, bug=ROCpredicted4[,'bug'], clean=ROCpredicted4[,'clean'])
    
    AUCX <- twoClassSummary(k, lev = levels(testData$bug))[[1]] #ROC AUC for model w/o DD
    AUC2 <- twoClassSummary(k2, lev = levels(testData$bug))[[1]] #ROC AUC for model w DD
    AUC3 <- twoClassSummary(k3, lev = levels(testData$bug))[[1]] #ROC AUC for model w DDD
    AUC4 <- twoClassSummary(k4, lev = levels(testData$bug))[[1]] #ROC AUC for model w DD and DDD

    SENX <- twoClassSummary(k, lev = levels(testData$bug))[[2]] 
    SEN2 <- twoClassSummary(k2, lev = levels(testData$bug))[[2]] 
    SEN3 <- twoClassSummary(k3, lev = levels(testData$bug))[[2]] 
    SEN4 <- twoClassSummary(k4, lev = levels(testData$bug))[[2]] 
    
    SPCX <- twoClassSummary(k, lev = levels(testData$bug))[[3]] 
    SPC2 <- twoClassSummary(k2, lev = levels(testData$bug))[[3]] 
    SPC3 <- twoClassSummary(k3, lev = levels(testData$bug))[[3]] 
    SPC4 <- twoClassSummary(k4, lev = levels(testData$bug))[[3]] 

    CM <- as.matrix(confusionMatrix(predicted, testData$bug, positive="bug", mode="everything")[[2]])
    CM2 <- as.matrix(confusionMatrix(predicted2, testData$bug, positive="bug", mode="everything")[[2]])
    CM3 <- as.matrix(confusionMatrix(predicted3, testData$bug, positive="bug", mode="everything")[[2]])
    CM4 <- as.matrix(confusionMatrix(predicted4, testData$bug, positive="bug", mode="everything")[[2]])
    mccCoeff <- mcc(CM)
    mccCoeff2 <- mcc(CM2)
    mccCoeff3 <- mcc(CM3)
    mccCoeff4 <- mcc(CM4)
    
    F1X <- confusionMatrix(predicted, testData$bug, positive="bug", mode="everything")[[4]]["F1"]
    F1D2 <- confusionMatrix(predicted2, testData$bug, positive="bug", mode="everything")[[4]]["F1"]
    F1D3 <- confusionMatrix(predicted3, testData$bug, positive="bug", mode="everything")[[4]]["F1"]
    F1D4 <- confusionMatrix(predicted4, testData$bug, positive="bug", mode="everything")[[4]]["F1"]
    PrecX <- confusionMatrix(predicted, testData$bug, positive="bug", mode="everything")[[4]]["Precision"]
    PrecD2 <- confusionMatrix(predicted2, testData$bug, positive="bug", mode="everything")[[4]]["Precision"]
    PrecD3 <- confusionMatrix(predicted3, testData$bug, positive="bug", mode="everything")[[4]]["Precision"]
    PrecD4 <- confusionMatrix(predicted4, testData$bug, positive="bug", mode="everything")[[4]]["Precision"]
    RecallX <- confusionMatrix(predicted, testData$bug, positive="bug", mode="everything")[[4]]["Recall"]
    RecallD2 <- confusionMatrix(predicted2, testData$bug, positive="bug", mode="everything")[[4]]["Recall"]
    RecallD3 <- confusionMatrix(predicted3, testData$bug, positive="bug", mode="everything")[[4]]["Recall"]
    RecallD4 <- confusionMatrix(predicted4, testData$bug, positive="bug", mode="everything")[[4]]["Recall"]
    
    cat(i,";",sep="",file=filename,append=TRUE)
    # remember which features were selected in a given iteration 
    # (important for the AutoSpearman variant of the experiment)
    cat(1*(indVars %in% VARS),sep=";",file=filename,append=TRUE)

    cat(";",p,sep="",file=filename,append=TRUE)
    cat(";",m,";",sep="",file=filename,append=TRUE)
    cat(F1X,";",F1D2,";",F1D3,";",F1D4,";",
        PrecX,";",PrecD2,";",PrecD3,";",PrecD4,";",
        RecallX,";",RecallD2,";",RecallD3,";",RecallD4,";",
        AUCX,";",AUC2,";",AUC3,";",AUC4,";",
        SENX,";",SEN2,";",SEN3,";",SEN4,";",
        SPCX,";",SPC2,";",SPC3,";",SPC4,";",mccCoeff,";",mccCoeff2,";",mccCoeff,"3;",mccCoeff4,"\n",
        sep="",file=filename,append=TRUE)
    
  }

}
}


##############################################
# Cross-Project Defect Prediction Experiment #
##############################################

MODELS <- c('glm', 'knn', 'naive_bayes', 'rf', 'rpart')
PROJECTS <- c('broadleaf-3.0.10','ceylon-ide-eclipse-1.1.0','elasticsearch-0.90.11','hazelcast-3.3','mapdb-0.9.6',
              'mcMMO-1.4.06','netty','oryx','titan-0.5.1',)  

# select the file (for variant 1 or variant 2 of the experiment)
filename <- 'results-cross.csv'
#filename <- 'results-cross-VarImpVars.csv'

cat("Iter;",sep="",file=filename,append=TRUE)
cat(indVars,sep=";",file=filename,append=TRUE)
cat(";Project;Model;F1X;F1D2;F1D3;F1D4;PrecX;PrecD2;PrecD3;PrecD4;RecallX;RecallD2;RecallD3;RecallD4;AUCX;AUC2;AUC3;AUC4;SENX;SEN2;SEN3;SEN4;SPCX;SPC2;SPC3;SPC4;MCC;MCC2;MCC3;MCC4\n",sep="",file=filename,append=TRUE)


for (p in PROJECTS) {

  cat("proj=",p," iter=",i, "\n",sep="")

  # generate training and testing samples
  trainData <- d[d$ProjectName==p,]
  testData <- d[d$ProjectName!=p,]
  
  
  # SELECT ONE OF THESE: variables with AutoSpearman...
  VARS <- autoSp(trainData, indVars, thr=0.7, vif=5)  # VARS is denoted by "X" in the paper
  # ... OR fixed set of 10 top features with the highest Gini index  
  #VARS <- c("McCC", "WMC", "CBO", "RFC", "NOI", "NOA", "TNOS", "TLOC", "TLLOC", "LOC")
  
  # other combinations for the analysis (DD, DDD, DD+DDD)
  VARS2 <- c(VARS, "DD")
  VARS3 <- c(VARS, "DDD")
  VARS4 <- c(VARS, "DD", "DDD")

  #construct defect models for selected variables with and without a given data flow metric
  for (m in MODELS) {
    
    # train on a given project p
    # mdl = X. mdl2 = DD. mdl3 = DDD. mdl4 = DD+DDD
    mdl = train(x = as.data.frame(trainData[,VARS]), y = trainData$bug,  
                method=m, tuneLength = 5, metric='ROC', trControl = fitControl)
    mdl2 = train(x = as.data.frame(trainData[,VARS2]), y = trainData$bug,  
                 method=m, tuneLength = 5, metric='ROC', trControl = fitControl)
    mdl3 = train(x = as.data.frame(trainData[,VARS3]), y = trainData$bug,  
                 method=m, tuneLength = 5, metric='ROC', trControl = fitControl)    
    mdl4 = train(x = as.data.frame(trainData[,VARS4]), y = trainData$bug,  
                 method=m, tuneLength = 5, metric='ROC', trControl = fitControl)

    
    # predict on other projects
    
    ROCpredicted <- predict(mdl, testData, type='prob')
    ROCpredicted2 <- predict(mdl2, testData, type='prob')
    ROCpredicted3 <- predict(mdl3, testData, type='prob')
    ROCpredicted4 <- predict(mdl4, testData, type='prob')
    
    predicted <- predict(mdl, testData)
    predicted2 <- predict(mdl2, testData)
    predicted3 <- predict(mdl3, testData)
    predicted4 <- predict(mdl4, testData)

    # calculate cross-project performance for all models 
    
    k <-  data.frame(obs=testData$bug, pred=predicted, bug=ROCpredicted[,'bug'], clean=ROCpredicted[,'clean'])
    k2 <- data.frame(obs=testData$bug, pred=predicted2, bug=ROCpredicted2[,'bug'], clean=ROCpredicted2[,'clean'])
    k3 <- data.frame(obs=testData$bug, pred=predicted3, bug=ROCpredicted3[,'bug'], clean=ROCpredicted3[,'clean'])
    k4 <- data.frame(obs=testData$bug, pred=predicted4, bug=ROCpredicted4[,'bug'], clean=ROCpredicted4[,'clean'])
    
    AUCX <- twoClassSummary(k, lev = levels(testData$bug))[[1]] #ROC AUC for model w/o DD
    AUC2 <- twoClassSummary(k2, lev = levels(testData$bug))[[1]] #ROC AUC for model w DD
    AUC3 <- twoClassSummary(k3, lev = levels(testData$bug))[[1]] #ROC AUC for model w DDD
    AUC4 <- twoClassSummary(k4, lev = levels(testData$bug))[[1]] #ROC AUC for model w DD and DDD
    
    SENX <- twoClassSummary(k, lev = levels(testData$bug))[[2]] 
    SEN2 <- twoClassSummary(k2, lev = levels(testData$bug))[[2]] 
    SEN3 <- twoClassSummary(k3, lev = levels(testData$bug))[[2]] 
    SEN4 <- twoClassSummary(k4, lev = levels(testData$bug))[[2]] 
    
    SPCX <- twoClassSummary(k, lev = levels(testData$bug))[[3]] 
    SPC2 <- twoClassSummary(k2, lev = levels(testData$bug))[[3]] 
    SPC3 <- twoClassSummary(k3, lev = levels(testData$bug))[[3]] 
    SPC4 <- twoClassSummary(k4, lev = levels(testData$bug))[[3]] 
    
    CM <- as.matrix(confusionMatrix(predicted, testData$bug, positive="bug", mode="everything")[[2]])
    CM2 <- as.matrix(confusionMatrix(predicted2, testData$bug, positive="bug", mode="everything")[[2]])
    CM3 <- as.matrix(confusionMatrix(predicted3, testData$bug, positive="bug", mode="everything")[[2]])
    CM4 <- as.matrix(confusionMatrix(predicted4, testData$bug, positive="bug", mode="everything")[[2]])
    mccCoeff <- mcc(CM)
    mccCoeff2 <- mcc(CM2)
    mccCoeff3 <- mcc(CM3)
    mccCoeff4 <- mcc(CM4)
    
    F1X <- confusionMatrix(predicted, testData$bug, positive="bug", mode="everything")[[4]]["F1"]
    F1D2 <- confusionMatrix(predicted2, testData$bug, positive="bug", mode="everything")[[4]]["F1"]
    F1D3 <- confusionMatrix(predicted3, testData$bug, positive="bug", mode="everything")[[4]]["F1"]
    F1D4 <- confusionMatrix(predicted4, testData$bug, positive="bug", mode="everything")[[4]]["F1"]
    PrecX <- confusionMatrix(predicted, testData$bug, positive="bug", mode="everything")[[4]]["Precision"]
    PrecD2 <- confusionMatrix(predicted2, testData$bug, positive="bug", mode="everything")[[4]]["Precision"]
    PrecD3 <- confusionMatrix(predicted3, testData$bug, positive="bug", mode="everything")[[4]]["Precision"]
    PrecD4 <- confusionMatrix(predicted4, testData$bug, positive="bug", mode="everything")[[4]]["Precision"]
    RecallX <- confusionMatrix(predicted, testData$bug, positive="bug", mode="everything")[[4]]["Recall"]
    RecallD2 <- confusionMatrix(predicted2, testData$bug, positive="bug", mode="everything")[[4]]["Recall"]
    RecallD3 <- confusionMatrix(predicted3, testData$bug, positive="bug", mode="everything")[[4]]["Recall"]
    RecallD4 <- confusionMatrix(predicted4, testData$bug, positive="bug", mode="everything")[[4]]["Recall"]
            
    cat(i,";",sep="",file=filename,append=TRUE)
    cat(1*(indVars %in% VARS),sep=";",file=filename,append=TRUE)

    cat(";",p,sep="",file=filename,append=TRUE)
    cat(";",m,";",sep="",file=filename,append=TRUE)
    cat(F1X,";",F1D2,";",F1D3,";",F1D4,";",PrecX,";",PrecD2,";",PrecD3,";",PrecD4,";",RecallX,";",RecallD2,";",RecallD3,";",RecallD4,";",AUCX,";",AUC2,";",AUC3,";",AUC4,";",SENX,";",SEN2,";",SEN3,";",SEN4,";",SPCX,";",SPC2,";",SPC3,";",SPC4,";",mccCoeff,";",mccCoeff2,";",mccCoeff,"3;",mccCoeff4,"\n",sep="",file=filename,append=TRUE)
    
      }
}

# script 'printResults.R' generates LaTeX tables from the csv files (Tables III, IV, V, VI)
