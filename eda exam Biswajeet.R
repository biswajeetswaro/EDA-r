concerete_df<-read.csv('concrete.csv',header = TRUE)
library(dplyr)
names(concerete_df)
str(concerete_df)
nrow(concerete_df)
ncol(concerete_df)
depth_vs_breath_ratio<- nrow(concerete_df)/ncol(concerete_df)
depth_vs_breath_ratio
summary(concerete_df$cement)
summary(concerete_df$age)
summary(concerete_df$slag)
summary(concerete_df$ash)
summary(concerete_df$water)
summary(concerete_df$superplastic)
summary(concerete_df$coarseagg)
summary(concerete_df$fineagg)
summary(concerete_df$strength)
par(mfrow = c(3,3))
hist(concerete_df$cement)
hist(concerete_df$slag)
hist(concerete_df$ash)
hist(concerete_df$water)
hist(concerete_df$superplastic)
hist(concerete_df$coarseagg)
hist(concerete_df$fineagg)
hist(concerete_df$age)
hist(concerete_df$strength)

par(mfrow = c(3,3))
boxplot(concerete_df$cement,main = 'cement',horizontal = TRUE)
boxplot(concerete_df$slag,main = 'slag', horizontal = TRUE)
boxplot(concerete_df$ash,main = 'ash',horizontal = TRUE)
boxplot(concerete_df$water, main = 'water', horizontal = TRUE)
boxplot(concerete_df$superplastic,main = 'superplastic',horizontal = TRUE)
boxplot(concerete_df$coarseagg, main = 'coarseagg', horizontal = TRUE)
boxplot(concerete_df$fineagg,main = 'fineagg', horizontal = TRUE)
boxplot(concerete_df$age, main = 'age', horizontal = TRUE)
boxplot(concerete_df$strength,main = 'strength',horizontal = TRUE)

library(psych)
pairs.panels(concerete_df)

attach(concerete_df)
par(mfrow = c(3,3))
plot(strength,strength)
plot(strength,cement)
plot(strength,slag)
plot(strength,ash)
plot(strength,water)
plot(strength,superplastic)
plot(strength,coarseagg)
plot(strength,fineagg)
plot(strength,age)

concerete_df$slag[concerete_df$slag == 0]<- "NA"
concerete_df$ash[concerete_df$ash == 0]<- "NA"

concerete_df$slag <- as.numeric(concerete_df$slag)
concerete_df$ash <- as.numeric(concerete_df$ash)

#after inducing na values
pairs.panels(concerete_df)

library(mice)
pMiss <- function(x){sum(is.na(x)) / length(x) * 100}  
apply(concerete_df, 2, pMiss)  

md.pattern(concerete_df)

library(VIM)
aggr_plot <- aggr(concerete_df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(concerete_df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(concerete_df[c(1,2)])

micedata <- mice(concerete_df,m=5,maxit=50,method='pmm',seed=500)
summary(micedata)

micedata$imp$slag
micedata$imp$ash

completedData <- complete(micedata,1)
summary(completedData)


attach(concerete_df)
par(mfrow = c(3,3))
plot(slag,strength)
plot(slag,cement)
plot(slag,slag)
plot(slag,ash)
plot(slag,water)
plot(slag,superplastic)
plot(slag,coarseagg)
plot(slag,fineagg)
plot(slag,age)

attach(concerete_df)
par(mfrow = c(3,3))
plot(ash,strength)
plot(ash,cement)
plot(ash,slag)
plot(ash,ash)
plot(ash,water)
plot(ash,superplastic)
plot(ash,coarseagg)
plot(ash,fineagg)
plot(ash,age)
