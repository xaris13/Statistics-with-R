#reading data that has been transformed locally in weka
#for question 2 and above we make it manually to present how it is done with R
datast <- read.table(file.choose(),header = TRUE,sep = ";")
#delete singularites columns,columns with NAs
#target variables except ours
# the first 5 columns which are not interest us
#delete also the rows from rapes perPop contain NA
#93*103 dataframe : datast
datast[,1:103]<-lapply(datast[,1:103], as.numeric)
#summary statistics

######################### Question 1####################################3
summary(datast)
#qqplot for our response
library("car")
qqPlot(datast$rapesPerPop)
qqnorm(datast$rapesPerPop, pch = 1, frame = FALSE)
qqline(datast$rapesPerPop, col = "steelblue", lwd = 2)
#all corellations with our response
corr<-as.data.frame(cor(datast$rapesPerPop,datast[,1:102]))
sort_corr<-sort(corr)
#split to negative and postive correlations most significant
negative_cor<-sort_corr[,c(1:8)]
positive_cor<-sort_corr[,c(94:102)]
#identify the positive and negative correlations most significant
nd<-datast[c("rapesPerPop","pctKids-4w2Par" ,"pctKids2Par","pct2Par","medIncome","medFamIncome", "pctPersOwnOccup","pctWdiv","ownHousLowQ"
)]
#pairwise comparisons
pairs(nd)
pd<-datast[c("rapesPerPop","pctBlack","pctPoverty","pctKidsBornNevrMarr","pctVacantBoarded","pctSmallHousUnits",
             "pctHousWOphone","pctFemDivorc","pctMaleDivorc","pctAllDivorc")]
#correlations plots between positive and negative
corrplots<-c(nd,pd)
pairs(pd)
#correlations
corrplots<-datast[c("rapesPerPop","pctKids-4w2Par" ,"pctKids2Par","pct2Par","medIncome","medFamIncome", "pctPersOwnOccup","pctWdiv","ownHousLowQ","rapesPerPop","pctBlack","pctPoverty","pctKidsBornNevrMarr","pctVacantBoarded","pctSmallHousUnits","pctHousWOphone","pctFemDivorc","pctMaleDivorc","pctAllDivorc")]
#correleogram
res <- cor(corrplots)
round(res, 2)
library(corrplot)
#in use correologram
corrplot(cor(corrplots$rapesPerPop,corrplots[2:19]), method = "number")
#pairs of postive correlatiosn
pairs(pd[,2:7], pch = 19,  cex = 0.5,
      lower.panel=NULL)

par(mfrow=c(5,5))
library(car)
#scatterplots for response and divorces
plot(datast$rapesPerPop, datast$pctAllDivorc, main="Scatterplot ",
     xlab="Rapes Percentage ", ylab="Percentage of Divorces ", pch=19)

plot(datast$rapesPerPop, datast$`pctKids-4w2Par`, main="Scatterplot ",
     +      xlab="Rapes Percentage ", ylab="Percentage of Kids living with 2 parents ", pch=19)
par(mfrow=c(4,4)); n <- nrow(datast)
#histograms
hist(datast$pctBlack)
hist(datast$rapesPerPop)
hist(negative_cor[,2], main=names(negative_cor)[2])
hist(negative_cor[,3], main=names(negative_cor)[4])
hist(negative_cor[,4], main=names(negative_cor)[4])
plot(table(datast[,1])/n, type='h', xlim=range(datast[,1])+c(-1,1), main=names(datast)[3], ylab='Relative frequency')
plot(table(negative_cor[,2])/n, type='h', xlim=range(negative_cor[,2])+c(-1,1), main=names(negative_cor)[4], ylab='Relative frequency')
plot(table(negative_cor[,3])/n, type='h', xlim=range(negative_cor[,3])+c(-1,1), main=names(negative_cor)[3], ylab='Relative frequency')
plot(table(negative_cor[,4])/n, type='h', xlim=range(negative_cor[,4])+c(-1,1), main=names(negative_cor)[4], ylab='Relative frequency')

########################### Question 2 ##########################
#####################data preparation and cleaning###################
############# in this part we are doing it manually in R ########################
setwd("/Users/xarismallios/Desktop")
crimes_28<- read.csv(file="crime_28.csv", header=TRUE, sep=",")
df<-crimes_28
#We dont need first 4 columns refering states and community
df[,1:4]<-NULL
#we drop columns with many NAs appeared
df[,130:143]<-NULL
df[,126:128]<-NULL
dar<-df
dar[,127]<-NULL
dar[,125]<-NULL
dar[,120:123]<-NULL
dar[,100:116]<-NULL
#delete rows from response where there is NA
dat28<-subset(dar, !is.na(dar$rapesPerPop))

#full model with all variables
fullmodel <- lm(rapesPerPop~.,data=dat28)
#drop the singularities which cause NAs
dat28$ownHousQrange<-NULL
dat28$rentQrange<-NULL
dat28$pctBornStateResid<-NULL
dat28$pctSameHouse.5<-NULL
dat28$pctSameState.5 <-NULL
dat28$landArea<-NULL
dat28$pctUsePubTrans<-NULL
dat28$pctSameCounty.5<-NULL
dat28$popDensity<-NULL
dat28$pctOfficDrugUnit<-NULL
#full model without singularities
fullmodel_nona <- lm(rapesPerPop~.,data=datas28)
datas28<-dat28
datas28[,1:93]<-lapply(dat28[,1:93], as.numeric)

#lasso to fullmodel without singularities
require(glmnet)
X <- model.matrix(fullmodel_nona)[,-1]
lasso1 <- glmnet(X, dat28$rapesPerPop)
lasso1$lambda
#cross validation lasso methos
lasso1 <- cv.glmnet(X, dat28$rapesPerPop)
# lasso with 1 standard error λ
lasso1$lambda.1se
# lasso with min λ
lasso1$lambda.min
coef(lasso1, s = "lambda.1se")
coef(lasso1, s = "lambda.min")
variablesoflasso1se<-coef(lasso1, s = "lambda.1se")
variablesoflassomin<-coef(lasso1, s = "lambda.min")
#get variables screening
names(which(variablesoflasso1se[,1]>0))
names(which(variablesoflassomin[,1]>0))
#plot lasso
plot(lasso1)
plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)

# model lasso with 1 SE
fullmodel_lasso_1se<-lm(rapesPerPop~pctAllDivorc+pctMaleDivorc,data=dat28)
# model lasso with min lamda
fullmodel_lasso_min<-lm(rapesPerPop~+persHomeless+pctHousWOphone+pctVacant6up+pctVacantBoarded+pctAllDivorc+pctMaleDivorc,data=dat28)
#summary for both
summary(fullmodel_lasso_1se)
summary(fullmodel_lasso_min)

# model lasso with min lamda and BIC
fullmodel_stepbic_lasso_min<-step(fullmodel_lasso_min,direction = "both",k=log(93))
# model lasso with 1 SE and BIC
fullmodel_stepbic_lasso_1se<-step(fullmodel_lasso_1se,direction = "both",k=log(93))
#summary for both
summary(fullmodel_stepbic_lasso_min)
summary(fullmodel_stepbic_lasso_1se)
# model lasso with min lamda and AIC
fullmodel_stepaic_lasso_min<-step(fullmodel_lasso_min,direction = "both")
# model lasso with 1 SE and AIC
fullmodel_stepaic_lasso_1se<-step(fullmodel_lasso_1se,direction = "both")
#summary for both
summary(fullmodel_stepaic_lasso_min)
summary(fullmodel_stepaic_lasso_1se)
#final best model we select
final<-lm(rapesPerPop ~ + pctHousWOphone + pctVacant6up +
            + pctMaleDivorc,data=dat28)
summary(final)
########################### Question 3 ##########################

#assumptions tests
library(nortest)
library(car)
#linearity test, Tukey
residualPlot(final, type="rstudent")
residualPlots(final, plot=F)
plot(rstudent(final), type="l")
#normality test Kolmogorov-Smirnov
lillie.test(residuals(final))
#constant variance test homoscedsticity
ncvTest(final)
library(lmtest)
#independence test
dwtest(final)
########################### Question 4 ##########################

#cross validation using 10 folds
library(caret)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
modelt <- train(rapesPerPop ~ pctHousWOphone + pctVacant6up +
                  + pctMaleDivorc, data = dat28, method = "lm",
                trControl = train.control)
# Summarize the results
print(modelt)
########################### Question 5,6,7 ##########################

#scatter index
SI<-mean(abs(final$fitted.values - predict(final,crimesdatatest)))
predict(final,dat28)

########################### Question 8##########################

# the three scenarios
typical <-data.frame("pctHousWOphone" = mean(crimesdatatest$pctHousWOphone) ,"pctVacant6up" = mean(crimesdatatest$pctVacant6up) ,"pctMaleDivorc" = mean(crimesdatatest$pctMaleDivorc))
worst <-data.frame("pctHousWOphone" = max(crimesdatatest$pctHousWOphone) ,"pctVacant6up" = max(crimesdatatest$pctVacant6up) ,"pctMaleDivorc" = max(crimesdatatest$pctMaleDivorc))
best <-data.frame("pctHousWOphone" = min(crimesdatatest$pctHousWOphone) ,"pctVacant6up" = min(crimesdatatest$pctVacant6up) ,"pctMaleDivorc" = min(crimesdatatest$pctMaleDivorc))

#3 scenarios of typical profile prediction, max , min
predict(final,typical)
predict(final,worst)
predict(final,best)

# overall estimation and sum
mean(final$fitted.values)
sum(final$fitted.values)
