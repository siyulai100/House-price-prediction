#final mode了selec
library(tidyverse)
library(scales)
library(car)
library(dplyr)
library(lubridate)
train <- read_csv('data/train.csv') %>%
  mutate(SalePrice = SalePrice/1000)
test <- read_csv('data/test.csv')

## clean data
names(train)[names(train)=="1stFlrSF"]="FirstSF"
names(train)[names(train)=="2ndFlrSF"]="SecSF"
names(train)[names(train)=="3SsnPorch"]="threeSsnPorch"

names(test)[names(test)=="1stFlrSF"]="FirstSF"
names(test)[names(test)=="2ndFlrSF"]="SecSF"
names(test)[names(test)=="3SsnPorch"]="threeSsnPorch"

train$PoolQC <- !is.na(train$PoolQC)
train$MiscFeature <- !is.na(train$MiscFeature)
train$Fence <- !is.na(train$Fence)
train$Alley <- !is.na(train$Alley)
train$FireplaceQu <- !is.na(train$FireplaceQu)

test$PoolQC <- !is.na(test$PoolQC)
test$MiscFeature <- !is.na(test$MiscFeature)
test$Fence <- !is.na(test$Fence)
test$PoolQC <- !is.na(test$PoolQC)
test$FireplaceQu <- !is.na(test$FireplaceQu)

summary(train)


train$MSSubClass<-train$LotFrontage<-train$Neighborhood<-train$Street<-train$Alley<-train$Utilities<-train$LotConfig<-train$LotShape<-
  train$LandContour<-train$LandSlope<-train$Condition1<-train$Condition2<-train$MasVnrType<-train$MasVnrArea<-train$Heating<-
  train$HeatingQC<-train$BsmtQual<-train$BsmtCond<-train$BsmtExposure<-train$BsmtFinType1<-train$BsmtFinSF1<-train$BsmtFinType2<-
  train$BsmtFinSF2<-train$Exterior1st<-train$Exterior2nd<-train$BsmtUnfSF<- train$Electrical<-train$LowQualFinSF<-train$FirstSF<-
  train$SecSF<-train$Functional<-train$FireplaceQu<-train$GarageQual<-train$GarageYrBlt<-train$GarageFinish<-train$GarageCond<-
  train$PavedDrive<-train$EnclosedPorch<-train$threeSsnPorch<-train$ScreenPorch<-train$PoolArea<-train$Fence<-train$MiscFeature<-
  train$MiscVal<-train$SaleType<-train$SaleCondition<-train$Foundation <-train$Id <- train$ExterCond<-train$FullBath<-
  train$TotRmsAbvGrd<-train$PoolQC<-train$YearRemodAdd<-train$YrSold<-train$WoodDeckSF<-train$MoSold<-train$HouseStyle<-
  RoofStyle<-train$BsmtHalfBath<-train$HalfBath<-train$GarageType<-train$OpenPorchSF<-train$BedroomAbvGr<-train$GarageArea<-
  train$ExterQual<-train$RoofStyle<-train$BldgType<-NULL

test$MSSubClass<-test$LotFrontage<-test$Neighborhood<-test$Street<-test$Alley<-test$Utilities<-test$LotConfig<-test$LotShape<-
  test$LandContour<-test$LandSlope<-test$Condition1<-test$Condition2<-test$MasVnrType<-test$MasVnrArea<-test$Heating<-
  test$HeatingQC<-test$BsmtQual<-test$BsmtCond<-test$BsmtExposure<-test$BsmtFinType1<-test$BsmtFinSF1<-test$BsmtFinType2<-
  test$BsmtFinSF2<-test$Exterior1st<-test$Exterior2nd<-test$BsmtUnfSF<- test$Electrical<-test$LowQualFinSF<-test$FirstSF<-
  test$SecSF<-test$Functional<-test$FireplaceQu<-test$GarageQual<-test$GarageYrBlt<-test$GarageFinish<-test$GarageCond<-
  test$PavedDrive<-test$EnclosedPorch<-test$threeSsnPorch<-test$ScreenPorch<-test$PoolArea<-test$Fence<-test$MiscFeature<-
  test$MiscVal<-test$SaleType<-test$SaleCondition<-test$Foundation <-test$Id <-test$ExterCond<-
  test$FullBath <-test$TotRmsAbvGrd <-test$PoolQC <-test$YearRemodAdd <- test$YrSold<-test$WoodDeckSF<-test$MoSold<-
  test$HouseStyle<-test$RoofStyle<-test$BsmtHalfBath<-test$HalfBath<-test$GarageType<-test$OpenPorchSF<-test$BedroomAbvGr<-
  test$GarageArea<-test$ExterQual<-test$RoofStyle<-test$BldgType<-NULL

# Reduce skewness
train_log <- train %>%
  mutate(SalePrice = log(SalePrice),
         GrLivArea = log(GrLivArea),
         LotArea = log(LotArea))

test_log <- test %>%
  mutate(GrLivArea = log(GrLivArea),
         LotArea = log(LotArea))



train_na <- na.omit(train_log)
selectedMod <- lm(SalePrice ~ . , data = train_na)

summary(selectedMod)
vif(selectedMod)


# Delete NA data
test_na <- na.omit(test_log)

# Apply the model to test set
x <-data.frame(test_log)
x_na <- data.frame(test_na)
pred <- predict(selectedMod,x)
pred_na <- predict(selectedMod,x_na)
summary(pred)

# Create testset with prediction
test_log$SalePrice <- exp(pred)*1000
test_na$SalePrice <- exp(pred_na)*1000
avg_price <- mean(test_na$SalePrice)
dim(test_log)

# Distribution of SalePrice
ggplot(test_na, aes(SalePrice)) +
  geom_density()

# Generate csv file
Id <- seq(1461,2919)
SalePrice <- test_log$SalePrice
SalePrice[is.na(SalePrice)] <- avg_price
sub <- cbind(Id, SalePrice)
dim(sub)
summary(sub)

write.csv(sub,file='submission.csv', quote=F, row.names=F)
