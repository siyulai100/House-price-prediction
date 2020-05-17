library(tidyverse)
library(scales)
library(car)
library(dplyr)

train <- read_csv('data/train.csv') %>%
  mutate(SalePrice = SalePrice/1000)
test <- read_csv('data/test.csv')


set.seed(1234)


dim(train)
summary(train)
summary(train$PoolQC)
class(train$YearBuilt)

## clean data
names(train)[names(train)=="1stFlrSF"]="FirstSF"
names(train)[names(train)=="2ndFlrSF"]="SecSF"
names(train)[names(train)=="3SsnPorch"]="threeSsnPorch"

train$PoolQC <- !is.na(train$PoolQC)
summary(train)

year <- seq(1870, 2010, 10)


# train %>%
#   mutate(Year.f = cut(YearBuilt,breaks=year)) %>%
#   ggplot(aes(SalePrice, GrLivArea)) +
#   geom_point(aes(colour=Year.f))+
#   stat_smooth()


train$MSSubClass<-train$LotFrontage<-train$MSZoning<-train$Street<-train$Alley<-train$Utilities<-train$LotConfig<-train$LotShape<-
train$LandContour<-train$LandSlope<-train$Condition1<-train$Condition2<-train$MasVnrType<-train$MasVnrArea<-train$Heating<-
train$HeatingQC<-train$BsmtQual<-train$BsmtCond<-train$BsmtExposure<-train$BsmtFinType1<-train$BsmtFinSF1<-train$BsmtFinType2<-
train$BsmtFinSF2<-train$Exterior1st<-train$Exterior2nd<-train$BsmtUnfSF<- train$Electrical<-train$LowQualFinSF<-train$FirstSF<-
train$SecSF<-train$Functional<-train$FireplaceQu<-train$GarageQual<-train$GarageYrBlt<-train$GarageFinish<-train$GarageCond<-
train$PavedDrive<-train$EnclosedPorch<-train$threeSsnPorch<-train$ScreenPorch<-train$PoolArea<-train$Fence<-train$MiscFeature<-
train$MiscVal<-train$SaleType<-train$SaleCondition<-train$Foundation <-train$Id <-NULL


# Reduce skewness
train_log <- train %>%
  mutate(SalePrice = log(SalePrice),
         GrLivArea = log(GrLivArea),
         LotArea = log(LotArea))

summary(train_log)



??stepwise


train_na <- na.omit(train_log)
lmMod <- lm(SalePrice ~ . -Neighborhood , data = train_na)
selectedMod <- step(lmMod)
summary(selectedMod)
vif(selectedMod)

set.seed(1)
spec = c(train = .6, test = .2, validate = .2)

g = sample(cut(
  seq(nrow(train)), 
  nrow(df)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(train, g)

sapply(res, nrow)/nrow(train)
addmargins(prop.table(table(g)))

res$train
res$validate
res$test
