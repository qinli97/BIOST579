life <- read.csv("~/Desktop/Biostat 579/Life Expectancy Data.csv", sep =",", header=TRUE)

library(knitr)
library(tidyverse)
library(mice)
library(Amelia)
library(missMDA)


y <- life$Life.expectancy
X <- life[,-4]


summary(life)
 
missing.sum <- sapply(life, function(x) sum(is.na(x)))

life.missing <- na.omit(life.pre)

life.pca <- prcomp(life.missing[,c(2,4:21)], center = TRUE,scale. = TRUE)
summary(life.pca)

life.pre <- life[,-4]
library(glmnet)
Y = life$Life.expectancy

lasso.mod <- glmnet(life.pre, Y, alpha = 1, lambda = 0.5)
lasso.coef <- round(coef(lasso.mod),3)

## Multiple imputation

# # PCA with missing data
# nb <- estim_ncpPCA(life[,c(2,5:21)],method.cv = "Kfold", verbose = FALSE)
# nb$ncp
# plot(0:5, nb$criterion, xlab = "nb dim", ylab = "MSEP")



# imp.mice <- mice(life, m = 10, defaultMethod = "norm.boot") 
# # the variability of the parameters is obtained 




# md.pattern(life)
# library(VIM)
# aggr_plot <- aggr(life, col=c('navyblue','red'), numbers=TRUE, 
#                   sortVars=TRUE, labels=names(data), cex.axis=.7, 
#                   gap=3, ylab=c("Histogram of missing data","Pattern"))


# impmethod <- character(ncol(X))
# names(impmethod) <- colnames(X)
# impmethod["extrav"] <- "2l.norm"
# impmethod
# 
# pm <- make.predictorMatrix(X)
# pm$extrav <- rep("2l.norm")
# pm[,c(colnames(X))] <- c(-2,0,0,1,0,1,0,1,0,1,0,1,1,1,0,1,1,1,1,1,1)
# pm[,c(colnames(X))] <- c(-2,0,0,1,0,1,0,1,0,1,0,1,1,1,0,1,1,1,1,1,1)
# 
# pred <- make.predictorMatrix(X)
# pred[, "hyp"] <- 0
# pred[c("hyp", "chl"), ] <- 0
# meth <- c("", "pmm", "", "")
# nhanes.miced <- mice(nhanes, m=30, maxit=30, 
#                      method = meth,
#                      predictorMatrix = pred, 
#                      seed=2016)
# 
# imp <- mice(life, m = 3, print=F)

mis <- c("Adult.Mortality","Alcohol","Hepatitis.B","BMI","Polio","Total.expenditure",
         "Diphtheria","GDP","Population","thinness..1.19.years",
         "thinness.5.9.years","Income.composition.of.resources","Schooling")
uniq.country <- unique(X$Country)

part.x <- c("Hepatitis.B","GDP","Population")
name.country <- c()

excld <- function(df,predtor){
  mis <- c("Adult.Mortality","Alcohol","Hepatitis.B","BMI","Polio","Total.expenditure",
           "Diphtheria","GDP","Population","thinness..1.19.years",
           "thinness.5.9.years","Income.composition.of.resources","Schooling")
  uniq.country <- unique(X$Country)
  newdf <- df
  for (c in uniq.country){
    j = rep(0,16)
    ind <- which(df$Country %in% c)
    j = ifelse(is.na(df[ind,predtor]),j+1,j)
    # sub_x <-df[ind,]
    #print(j)
    if (sum(j)>7){
      name.country <- rbind(name.country,c)
    }
    else{
      for (i in 1:length(mis)){
        df[ind,][,mis[i]][is.na(df[ind,][,mis[i]])] <- rep(mean(df[ind,][,mis[i]],na.rm = T),
                                                           length(df[ind,][,mis[i]][is.na(df[ind,][,mis[i]])]))
      }

    }
  }
  newdf <- df[-which(df$Country %in% c(name.country)),]
  return(newdf)
}
newdf4 <- excld(newdf3,)
lm <- c("Hungary","Iceland","Norway","Iceland")
ind <- which(df$Country %in% lm)

newdf3 <- excld(newdf,"Hepatitis.B")


func.missing <- function(df){
  mis <- c("Adult.Mortality","Alcohol","Hepatitis.B","BMI","Polio","Total.expenditure",
           "Diphtheria","GDP","Population","thinness..1.19.years",
           "thinness.5.9.years","Income.composition.of.resources","Schooling")
  uniq.country <- unique(df$Country)
  for (country in uniq.country){
    temp_subject_index = which(X$Country %in% country) # get all index of current subject
    sub_x <-X[temp_subject_index,]
    for (i in 1:length(mis)){
      sub_x[,mis[i]][is.na(sub_x[,mis[i]])] <- rep(mean(sub_x[,mis[i]],na.rm = T),
                                                   length(sub_x[,mis[i]][is.na(sub_x[,mis[i]])]))
    }
  }

}

df = newdf3
for (country in uniq.country){
  ind <- which(df$Country %in% c)

  for (i in 1:length(mis)){
    
    df[ind,][,mis[i]][is.na(df[ind,][,mis[i]])] <- rep(mean(df[ind,][,mis[i]],na.rm = T),
                                                       length(df[ind,][,mis[i]][is.na(df[ind,][,mis[i]])]))

  }
  
}

###############################################################
df<- na.omit(df)
life.pca <- prcomp(df[,c(4:21)], center = TRUE,scale. = TRUE)
summary(life.pca)

expectancy <- life$Life.expectancy
Life <- cbind(expectancy,X)

Life <- na.omit(Life)
x <- Life[,-1]

library(glmnet)

mod <- cv.glmnet(as.matrix(x), Life$expectancy, alpha=1)

