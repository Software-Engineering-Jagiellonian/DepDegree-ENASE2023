setwd("DEFINE THE WORKING DIRECTORY HERE")

# select for which file the results should be generated
d <- read.csv('results.csv', sep=";")
#d <- read.csv('results-VarImpVars.csv', sep=";")

d[d$Project=='elasticsearch-0.90.11',]$Project <- 'el'
d[d$Project=='hazelcast-3.3',]$Project <- 'hz'
d[d$Project=='mapdb-0.9.6',]$Project <- 'md'
d[d$Project=='mcMMO-1.4.06',]$Project <- 'mc'
d[d$Project=='netty',]$Project <- 'ne'
d[d$Project=='broadleaf-3.0.10',]$Project <- 'br'
d[d$Project=='ceylon-ide-eclipse-1.1.0',]$Project <- 'ce'
d[d$Project=='oryx',]$Project <- 'or'
d[d$Project=='titan-0.5.1',]$Project <- 'ti'

d[d$Model=='rpart',]$Model <- 'CT'
d[d$Model=='knn',]$Model <- 'KN'
d[d$Model=='rf',]$Model <- 'RF'
d[d$Model=='naive_bayes',]$Model <- 'NB'
d[d$Model=='glm',]$Model <- 'LR'

r <- aggregate(cbind(F1X, F1D2, F1D3, F1D4, PrecX, PrecD2, PrecD3, PrecD4, 
                     RecallX, RecallD2, RecallD3, RecallD4, AUCX, AUC2, AUC3, AUC4,
                     SENX, SEN2, SEN3, SEN4, SPCX, SPC2, SPC3, SPC4, MCC, MCC2, MCC3, MCC4) ~ Model+Project, data=d, FUN=mean)
r2 <- aggregate(cbind(F1X, F1D2, F1D3, F1D4, PrecX, PrecD2, PrecD3, PrecD4, 
                     RecallX, RecallD2, RecallD3, RecallD4, AUCX, AUC2, AUC3, AUC4,
                     SENX, SEN2, SEN3, SEN4, SPCX, SPC2, SPC3, SPC4, MCC, MCC2, MCC3, MCC4) ~ Model+Project, data=d, FUN=sd)

# select the output file
filename <- 'table.txt'
#filename <- 'table-VarImpVars.txt'

# GENERATING RESULTS FOR THE WITHIN-PROJECT EXPERIMENT (Tables III and IV)

cat("\\begin{tabular}{rrcccc|cccc|cccc}\n",file=filename,append=TRUE)
cat("\\hline \n",file=filename,append=TRUE)
cat("& & \\multicolumn{12}{c}{Performance metrics (averaged over 100 runs)} \\\\ \\hline \n",file=filename,append=TRUE)
cat("Pr & M & \\multicolumn{4}{c}{F1-score} & \\multicolumn{4}{c}{ROC-AUC} & \\multicolumn{4}{c}{Matthew's Correlation Coefficient} \\\\ \\hline \n",file=filename,append=TRUE)
cat("   & & X & DD & DDD & DD+DDD & X & DD & DDD & DD+DDD & X & DD & DDD & DD+DDD \\\\ \\hline \n",file=filename,append=TRUE)

proj <- unique(r$Project)
mdls <- unique(r$Model)

for (p in proj) {
  for (m in mdls) {
    if (m==mdls[1]) cat(p," ",file=filename,append=TRUE)
    cat ("&", " ", m," ",file=filename,append=TRUE)
    m1 <- which.max(r[r$Project==p & r$Model==m, 3:6])+2
    m2 <- which.max(r[r$Project==p & r$Model==m, 15:18]) + 14
    m3 <- which.max(r[r$Project==p & r$Model==m, 27:30]) + 26
    mx <- c(m1, m2, m3)
    
    for (i in c(3,4,5,6,15,16,17,18,27,28,29,30)) {
      a <- sprintf(r[r$Project==p & r$Model==m, i],fmt = '%.3f')
      b <- sprintf(r2[r2$Project==p & r2$Model==m, i],fmt = '%.3f')
      cat ("&",file=filename,append=TRUE)
      if (i %in% mx) cat("\\bf{",file=filename,append=TRUE)
      cat (sub("^0+", "", a), "$\\pm$", sub("^0+", "", b), " ",file=filename,append=TRUE)
      if (i %in% mx) cat("}",file=filename,append=TRUE)
    }
    cat ("\\\\ \n",file=filename,append=TRUE)
    
  }
  cat ("\\hline \n",file=filename,append=TRUE)
}

r <- colMeans(d[,-c(63, 64)],na.rm = TRUE)
r2 <- apply(d[,-c(63,64)],2,sd,na.rm=TRUE)
cat("\\multicolumn{2}{r}{\\bf{all}}",file=filename,append=TRUE)

m1 <- which.max(r[63:66])+62
m2 <- which.max(r[75:78]) + 74
m3 <- which.max(r[87:90]) + 86
mx <- c(m1, m2, m3)

for (i in c(63,64,65,66,75,76,77,78,87,88,89,90)) {
  a <- sprintf(r[i],fmt = '%.3f')
  b <- sprintf(r2[i],fmt = '%.3f')
  cat("&",file=filename,append=TRUE)
  if (i %in% mx) cat("\\bf{",file=filename,append=TRUE)
  cat(sub("^0+", "", a), "$\\pm$", sub("^0+", "", b), " ",file=filename,append=TRUE)
  if (i %in% mx) cat("}",file=filename,append=TRUE)
}
cat("\\\\ \n",file=filename,append=TRUE)
cat("\\hline \n",file=filename,append=TRUE)
cat("\\end{tabular}",file=filename,append=TRUE)


# GENERATING RESULTS FOR CROSS-PROJECT EXPERIMENT (Tables V and VI)

d <- read.csv('results-cross-VarImpVars.csv', sep=";")

d[d$Project=='elasticsearch-0.90.11',]$Project <- 'el'
d[d$Project=='hazelcast-3.3',]$Project <- 'hz'
d[d$Project=='mapdb-0.9.6',]$Project <- 'md'
d[d$Project=='mcMMO-1.4.06',]$Project <- 'mc'
d[d$Project=='netty',]$Project <- 'ne'
d[d$Project=='broadleaf-3.0.10',]$Project <- 'br'
d[d$Project=='ceylon-ide-eclipse-1.1.0',]$Project <- 'ce'
d[d$Project=='oryx',]$Project <- 'or'
d[d$Project=='titan-0.5.1',]$Project <- 'ti'

d[d$Model=='rpart',]$Model <- 'CT'
d[d$Model=='knn',]$Model <- 'KN'
d[d$Model=='rf',]$Model <- 'RF'
d[d$Model=='naive_bayes',]$Model <- 'NB'
d[d$Model=='glm',]$Model <- 'LR'

r <- aggregate(cbind(F1X, F1D2, F1D3, F1D4, PrecX, PrecD2, PrecD3, PrecD4, 
                     RecallX, RecallD2, RecallD3, RecallD4, AUCX, AUC2, AUC3, AUC4,
                     SENX, SEN2, SEN3, SEN4, SPCX, SPC2, SPC3, SPC4, MCC, MCC2, MCC3, MCC4) ~ Model, data=d, FUN=mean)
r2 <- aggregate(cbind(F1X, F1D2, F1D3, F1D4, PrecX, PrecD2, PrecD3, PrecD4, 
                      RecallX, RecallD2, RecallD3, RecallD4, AUCX, AUC2, AUC3, AUC4,
                      SENX, SEN2, SEN3, SEN4, SPCX, SPC2, SPC3, SPC4, MCC, MCC2, MCC3, MCC4) ~ Model, data=d, FUN=sd)


# Select the output file
filename <- 'table-cross.txt'
#filename <- 'table-cross-VarImpVars.txt'

cat("\\begin{tabular}{rcccc}\n",file=filename,append=TRUE)
cat("\\hline \n",file=filename,append=TRUE)
cat("\\multicolumn{5}{c}{Performance metrics -- F1 score} \\\\ \\hline \n",file=filename,append=TRUE)
cat("Model  & X & DD & DDD & DD+DDD \\\\ \\hline \n",file=filename,append=TRUE)

mdls <- unique(r$Model)

  for (m in mdls) {
    cat (m," ",file=filename,append=TRUE)
    m1 <- which.max(r[r$Model==m, 2:5])+1
    m2 <- which.max(r[r$Model==m, 14:17]) + 13
    m3 <- which.max(r[r$Model==m, 26:29]) + 25
    mx <- c(m1, m2, m3)
    
    for (i in c(2,3,4,5)) {
      a <- sprintf(r[r$Model==m, i],fmt = '%.3f')
      b <- sprintf(r2[r2$Model==m, i],fmt = '%.3f')
      cat ("&",file=filename,append=TRUE)
      if (i %in% mx) cat("\\bf{",file=filename,append=TRUE)
      cat (sub("^0+", "", a), "$\\pm$", sub("^0+", "", b), " ",file=filename,append=TRUE)
      if (i %in% mx) cat("}",file=filename,append=TRUE)
    }
    cat ("\\\\ \n",file=filename,append=TRUE)
    
  }
  cat ("\\hline \n",file=filename,append=TRUE)


r <- colMeans(d[,-c(63, 64)],na.rm = TRUE)
r2 <- apply(d[,-c(63,64)],2,sd,na.rm=TRUE)
cat("\\bf{all}",file=filename,append=TRUE)

m1 <- which.max(r[63:66]) + 62
m2 <- which.max(r[75:78]) + 74
m3 <- which.max(r[87:90]) + 86
mx <- c(m1, m2, m3)

for (i in c(63,64,65,66)) {
  a <- sprintf(r[i],fmt = '%.3f')
  b <- sprintf(r2[i],fmt = '%.3f')
  cat("&",file=filename,append=TRUE)
  if (i %in% mx) cat("\\bf{",file=filename,append=TRUE)
  cat(sub("^0+", "", a), "$\\pm$", sub("^0+", "", b), " ",file=filename,append=TRUE)
  if (i %in% mx) cat("}",file=filename,append=TRUE)
}
cat("\\\\ \n",file=filename,append=TRUE)
cat("\\hline \n",file=filename,append=TRUE)
cat("\\end{tabular}",file=filename,append=TRUE)

#################

r <- aggregate(cbind(F1X, F1D2, F1D3, F1D4, PrecX, PrecD2, PrecD3, PrecD4, 
                     RecallX, RecallD2, RecallD3, RecallD4, AUCX, AUC2, AUC3, AUC4,
                     SENX, SEN2, SEN3, SEN4, SPCX, SPC2, SPC3, SPC4, MCC, MCC2, MCC3, MCC4) ~ Model, data=d, FUN=mean)
r2 <- aggregate(cbind(F1X, F1D2, F1D3, F1D4, PrecX, PrecD2, PrecD3, PrecD4, 
                      RecallX, RecallD2, RecallD3, RecallD4, AUCX, AUC2, AUC3, AUC4,
                      SENX, SEN2, SEN3, SEN4, SPCX, SPC2, SPC3, SPC4, MCC, MCC2, MCC3, MCC4) ~ Model, data=d, FUN=sd)

cat("\\begin{tabular}{rcccc}\n",file=filename,append=TRUE)
cat("\\hline \n",file=filename,append=TRUE)
cat("\\multicolumn{5}{c}{Performance metrics -- ROC-AUC} \\\\ \\hline \n",file=filename,append=TRUE)
cat("Model  & X & DD & DDD & DD+DDD \\\\ \\hline \n",file=filename,append=TRUE)

mdls <- unique(r$Model)

for (m in mdls) {
  cat (m," ",file=filename,append=TRUE)
  m1 <- which.max(r[r$Model==m, 2:5])+1
  m2 <- which.max(r[r$Model==m, 14:17]) + 13
  m3 <- which.max(r[r$Model==m, 26:29]) + 25
  mx <- c(m1, m2, m3)
  
  for (i in c(14,15,16,17)) {
    a <- sprintf(r[r$Model==m, i],fmt = '%.3f')
    b <- sprintf(r2[r2$Model==m, i],fmt = '%.3f')
    cat ("&",file=filename,append=TRUE)
    if (i %in% mx) cat("\\bf{",file=filename,append=TRUE)
    cat (sub("^0+", "", a), "$\\pm$", sub("^0+", "", b), " ",file=filename,append=TRUE)
    if (i %in% mx) cat("}",file=filename,append=TRUE)
  }
  cat ("\\\\ \n",file=filename,append=TRUE)
  
}
cat ("\\hline \n",file=filename,append=TRUE)


r <- colMeans(d[,-c(63, 64)],na.rm = TRUE)
r2 <- apply(d[,-c(63,64)],2,sd,na.rm=TRUE)
cat("\\bf{all}",file=filename,append=TRUE)

m1 <- which.max(r[63:66]) + 62
m2 <- which.max(r[75:78]) + 74
m3 <- which.max(r[87:90]) + 86
mx <- c(m1, m2, m3)

for (i in c(75,76,77,78)) {
  a <- sprintf(r[i],fmt = '%.3f')
  b <- sprintf(r2[i],fmt = '%.3f')
  cat("&",file=filename,append=TRUE)
  if (i %in% mx) cat("\\bf{",file=filename,append=TRUE)
  cat(sub("^0+", "", a), "$\\pm$", sub("^0+", "", b), " ",file=filename,append=TRUE)
  if (i %in% mx) cat("}",file=filename,append=TRUE)
}
cat("\\\\ \n",file=filename,append=TRUE)
cat("\\hline \n",file=filename,append=TRUE)
cat("\\end{tabular}",file=filename,append=TRUE)

#################

r <- aggregate(cbind(F1X, F1D2, F1D3, F1D4, PrecX, PrecD2, PrecD3, PrecD4, 
                     RecallX, RecallD2, RecallD3, RecallD4, AUCX, AUC2, AUC3, AUC4,
                     SENX, SEN2, SEN3, SEN4, SPCX, SPC2, SPC3, SPC4, MCC, MCC2, MCC3, MCC4) ~ Model, data=d, FUN=mean)
r2 <- aggregate(cbind(F1X, F1D2, F1D3, F1D4, PrecX, PrecD2, PrecD3, PrecD4, 
                      RecallX, RecallD2, RecallD3, RecallD4, AUCX, AUC2, AUC3, AUC4,
                      SENX, SEN2, SEN3, SEN4, SPCX, SPC2, SPC3, SPC4, MCC, MCC2, MCC3, MCC4) ~ Model, data=d, FUN=sd)

cat("\\begin{tabular}{rcccc}\n",file=filename,append=TRUE)
cat("\\hline \n",file=filename,append=TRUE)
cat("\\multicolumn{5}{c}{Performance metrics -- Matthew's Correlation Coefficient} \\\\ \\hline \n",file=filename,append=TRUE)
cat("Model  & X & DD & DDD & DD+DDD \\\\ \\hline \n",file=filename,append=TRUE)

mdls <- unique(r$Model)

for (m in mdls) {
  cat (m," ",file=filename,append=TRUE)
  m1 <- which.max(r[r$Model==m, 2:5])+1
  m2 <- which.max(r[r$Model==m, 14:17]) + 13
  m3 <- which.max(r[r$Model==m, 26:29]) + 25
  mx <- c(m1, m2, m3)
  
  for (i in c(26,27,28,29)) {
    a <- sprintf(r[r$Model==m, i],fmt = '%.3f')
    b <- sprintf(r2[r2$Model==m, i],fmt = '%.3f')
    cat ("&",file=filename,append=TRUE)
    if (i %in% mx) cat("\\bf{",file=filename,append=TRUE)
    cat (sub("^0+", "", a), "$\\pm$", sub("^0+", "", b), " ",file=filename,append=TRUE)
    if (i %in% mx) cat("}",file=filename,append=TRUE)
  }
  cat ("\\\\ \n",file=filename,append=TRUE)
  
}
cat ("\\hline \n",file=filename,append=TRUE)


r <- colMeans(d[,-c(63, 64)],na.rm = TRUE)
r2 <- apply(d[,-c(63,64)],2,sd,na.rm=TRUE)
cat("\\bf{all}",file=filename,append=TRUE)

m1 <- which.max(r[63:66]) + 62
m2 <- which.max(r[75:78]) + 74
m3 <- which.max(r[87:90]) + 86
mx <- c(m1, m2, m3)

for (i in c(87,88,89,90)) {
  a <- sprintf(r[i],fmt = '%.3f')
  b <- sprintf(r2[i],fmt = '%.3f')
  cat("&",file=filename,append=TRUE)
  if (i %in% mx) cat("\\bf{",file=filename,append=TRUE)
  cat(sub("^0+", "", a), "$\\pm$", sub("^0+", "", b), " ",file=filename,append=TRUE)
  if (i %in% mx) cat("}",file=filename,append=TRUE)
}
cat("\\\\ \n",file=filename,append=TRUE)
cat("\\hline \n",file=filename,append=TRUE)
cat("\\end{tabular}",file=filename,append=TRUE)
