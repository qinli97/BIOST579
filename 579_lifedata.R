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


mis <- c("Alcohol","Hepatitis.B","BMI","Total.expenditure")

uniq.country <- unique(life$Country)

part.x <- c("Hepatitis.B","GDP","Population")


excld <- function(df,predtor){
  name.country <- c()
  uniq.country <- unique(df$Country)
  newdf <- df
  for (c in uniq.country){
    j = rep(0,16)
    ind <- which(df$Country %in% c)
    j = ifelse(is.na(df[ind,predtor]),j+1,j)
    # sub_x <-df[ind,]
    #print(j)
    if (sum(j)>5){
      name.country <- rbind(name.country,c)
    }
      
  }
  newdf <- df[-which(df$Country %in% c(name.country)),]
  return(newdf)
}


new.df <- excld(life, "Population")
new.df2 <- excld(new.df,"GDP")
new.df3 <- excld(new.df2,"Hepatitis.B")

lm <- c("Hungary","Iceland","Norway","Denmark","Slovenia")
ind <- which(new.df3$Country %in% c(lm))


new.df3 <- new.df3[-ind,]


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
mis <- c("Adult.Mortality","Alcohol","Hepatitis.B","BMI","Polio","Total.expenditure",
         "Diphtheria","GDP","Population","thinness..1.19.years",
         "thinness.5.9.years","Income.composition.of.resources","Schooling")
df = new.df3
for (country in uniq.country){
  ind <- which(df$Country %in% country)
  
  for (i in 1:length(mis)){
    df[ind,][,mis[i]][is.na(df[ind,][,mis[i]])] <- rep(mean(df[ind,][,mis[i]],na.rm = T),
                                                       length(df[ind,][,mis[i]][is.na(df[ind,][,mis[i]])]))
    print(rep(mean(df[ind,][,mis[i]],na.rm = T),
              length(df[ind,][,mis[i]][is.na(df[ind,][,mis[i]])])))
    }

  }
  
}




###############################################################
df<- na.omit(df)
df$Status <- ifelse(df$Status=="Developing",0,1)
life.pca <- prcomp(df[,c(3,5:22)], center = TRUE,scale. = TRUE)

summary(life.pca)

#compute standard deviation of each principal component
std_dev <- life.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
lines(x = seq(1,19), y = rep(0.8,19),col = "red")

expectancy <- df$Life.expectancy

x <- model.matrix(Life.expectancy~., data = df[,-1])
x <- x[,-1]
lasso_model <- cv.glmnet(
  x = x,
  y = df$Life.expectancy,
  alpha = 1)

as.matrix(round(coef(lasso_model, lasso_model$lambda.min)),3)

library('randomForest')
# Using random forest for variable selection
rfModel <-randomForest(Life.expectancy ~ ., data = df)
importance(rfModel)



glmnet1<-cv.glmnet(x=x,y=df$Life.expectancy,type.measure='mse')

c<-round(coef(glmnet1,s='lambda.min',exact=TRUE),3)
inds<-which(c!=0)
inds
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']

stepAIC(lm1, k = 2)
mod <- cv.glmnet(as.matrix(x), expectancy, alpha=1)
lm1 <- lm(Life.expectancy ~ . , data = df[,-1])
AIC(lm1)
library(uwIntroStats)
lm1.null <- regress("mean", Life.expectancy ~Year + Status + Adult.Mortality + infant.deaths + 
                Alcohol + Measles + BMI + under.five.deaths, data = df)
lm1.null %>% coef %>% round(3)

lm1.alt1 <- lm(Life.expectancy ~Year + Status + Adult.Mortality + infant.deaths + 
                                  Alcohol + BMI + under.five.deaths, data = df)

lm1.alt2 <- lm(Life.expectancy ~Year + Status + Adult.Mortality + infant.deaths + 
                      Alcohol + Measles + BMI + under.five.deaths + Hepatitis.B, data = df)
anova(lm1.alt1,lm1.alt2)
df2 <- df
df2$Year <- as.factor(df2$Year)
