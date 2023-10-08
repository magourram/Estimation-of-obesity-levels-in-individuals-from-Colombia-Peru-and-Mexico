#### Introduction ####
#Libraries and packages required
require(readxl)
require(sandwich)
require(lmtest)
require(expss)
require(MASS)
require(corrplot)
require(dplyr)
require(janitor)
require(data.table)
require(mltools)
require(plyr)
require(caret)
require(tidyr)
require(car)
require(rpart)
require(rpart.plot)
require(tree)
require(partykit)
require(randomForest)
require(nnet)
require(gbm)
require(ggmosaic)
require(grid)
require(gridExtra)
require(parameters)
require(performance)
require(glmnet)
require(Metrics)
require(FactoMineR)
require(factoextra)
require(RColorBrewer)

#Working directory
setwd("~/Desktop/FantaCalcio_Project/Obesity")

#### Data import and cleaning ####
#Data import
data = read.csv(file = "~/Desktop/FantaCalcio_Project/Obesity/data/data.csv")

# 1) Formatting
colnames(data)
data$Age = round(data$Age, 0)
data$Height = round(data$Height, 2)
data$Weight = round(data$Weight, 2)
data$FCVC = round(data$FCVC, 0)
data$NCP = round(data$NCP, 0)
data = data %>% 
  mutate(NPC=ifelse(NCP==4,3,NCP))
data$NCP = data$NPC
data = data[, -18]
data$CH2O = round(data$CH2O, 0)
data$FAF = round(data$FAF, 0)
data$TUE = round(data$TUE,0)

describe_distribution(data)

#2) Subset DF into categorical variables and numerical variables
data_num = data[ , c("Age", "Height", "Weight")]                        
data_cha = data[ , c("Gender", "family_history_with_overweight", "FAVC", "FCVC", "NCP", "CAEC", "SMOKE", "CH2O", "SCC", "FAF", "TUE", "CALC", "MTRANS", "NObeyesdad")]

#3) Distribution of all numerical variables, represented via Histograms
colnames(data_num)
hist(data_num$Age,  main = "Histogram of Age", xlab = "Age", col = "lightblue")
hist(data_num$Height,  main = "Histogram of Height", xlab = "Height", col = "lightblue")
hist(data_num$Weight,  main = "Histogram of Weight", xlab = "Weight", col = "lightblue")

#4) Correlation matrix and plot on numerical variables
cor_n = cor(data_num)
corrplot(cor_n, type = "upper", tl.col = "black", tl.srt = 45, title = "Correlation plot between numerical variables")

#5) Compare 'NObeyesdad' levels across 'Age', 'Height', 'Weight'
ddply(data, "NObeyesdad", summarize,
      Age = round(mean(Age),2), 
      Height = round(mean(Height),2), 
      Weight = round(mean(Weight),2))

#6) Distribution of some variables
#ggplot(data, aes(x = Gender, fill = Gender)) + 
#  geom_bar(stat = "count") + 
#  ggtitle("Distribution for Gender variable") + 
#  xlab("Gender") + ylab("Number of records") + 
# theme_bw() + 
# geom_text(aes(label = ..count..), stat = 'count', vjust = -0.4) + 
# labs(fill = "Obesity level") + 
# theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ggplot(data, aes(x = SMOKE, fill = SMOKE)) + 
# geom_bar(stat = "count") + 
# ggtitle("Distribution for Smoking habits response") + 
# xlab("Smokers") + ylab("Number of records") + 
# theme_bw() + 
# geom_text(aes(label = ..count..), stat = 'count', vjust = -0.4) + 
# labs(fill = "Obesity level") + 
# theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ggplot(data, aes(x = FAF, fill = FAF)) + 
# geom_bar(stat = "count") + 
# ggtitle("Distribution for Physical activity frequency  response") + 
# xlab("Response") + ylab("Number of records") + 
# theme_bw() + 
# geom_text(aes(label = ..count..), stat = 'count', vjust = -0.4) + 
# labs(fill = "Obesity level") + 
# theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ggplot(data, aes(x = TUE, fill = TUE)) + 
# geom_bar(stat = "count") + 
# ggtitle("Distribution for Time using technology devices response") + 
# xlab("Response") + ylab("Number of records") + 
# theme_bw() + 
# geom_text(aes(label = ..count..), stat = 'count', vjust = -0.4) + 
# labs(fill = "Obesity level") + 
# theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ggplot(data, aes(x = NObeyesdad, fill = NObeyesdad)) + 
# geom_bar(stat = "count") + 
# ggtitle("Distribution across different obesity levels") + 
# xlab("Obesity level") + ylab("Number of records") + 
# theme_bw() + 
# geom_text(aes(label = ..count..), stat = 'count', vjust = -0.4) + 
# labs(fill = "Obesity level") + 
# theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# 6) Distribution of some variables; Gender wise distribution for Weight and Height
#ggplot(data, aes(y = Height, color = Gender)) + #fig3
# geom_boxplot() +
# ggtitle("Gender wise distribution of Height") + 
# theme_bw()

#ggplot(data, aes(y = Weight, color = Gender)) + #fig4
# geom_boxplot() +
# ggtitle("Gender wise distribution of Weight") + 
# theme_bw()

# 6) Distribution of some variables: Analyzing the preferred Mode of Transport by Gender
#ggplot(data) +
# geom_bar(aes(y = MTRANS, fill = Gender),position = position_dodge()) + 
# ylab("Mode of Transport") +
# scale_color_manual(values = c("blue", "red"), aesthetics = "fill") +
# ggtitle("Transport preferences by Gender") + 
# theme_mosaic()

# 7) Comparing Compare 'NObeyesdad' levels across categorical variables
tabyl(data, NObeyesdad, Gender)
tabyl(data, NObeyesdad, family_history_with_overweight)
tabyl(data, NObeyesdad, FAVC)
tabyl(data, NObeyesdad, FCVC)
tabyl(data, NObeyesdad, NCP)
tabyl(data, NObeyesdad, CAEC)
tabyl(data, NObeyesdad, SMOKE)
tabyl(data, NObeyesdad, CH2O)
tabyl(data, NObeyesdad, SCC)
tabyl(data, NObeyesdad, MTRANS)

# 8) Outliers detection
#fig_bp1 = ggplot(data, aes(y = Age, color = Age)) + #fig3
# geom_boxplot() +
# ggtitle("Age") + 
# theme_bw()
#fig_bp2 = ggplot(data, aes(y = Height, color = Height)) + #fig3
# geom_boxplot() +
# ggtitle("Height") + 
# theme_bw()
#fig_bp3 = ggplot(data, aes(y = Weight, color = Weight)) + #fig3
# geom_boxplot() +
# ggtitle("Weight") + 
# theme_bw()
#grid.arrange(fig_bp1, fig_bp2, fig_bp3, ncol = 3)

# 9) Check outliers with Interquartile Range: Percentile: Age column
data_out = data[, c("Age", "Height", "Weight")]
out_summary = as.data.frame(apply(data_out,2,summary))

age_p25 = out_summary[2,1]
age_p75 = out_summary[5,1]

# 9) Check outliers with Interquartile Range: Percentile: Weight column
wei_p25 = out_summary[2,3]
wei_p75 = out_summary[5,3]

# 9) Check outliers with Interquartile Range: Inte-rquantile Ranges
IR_age = age_p75 - age_p25
IR_wei = wei_p75 - wei_p25

# 9) Check outliers with Interquartile Range: Determine the lower and upper limit
lower_lim_age = age_p25 - (1.5 * IR_age)
upper_lim_age = age_p75 + (1.5 * IR_age)

lower_lim_weight = wei_p25 - (1.5 * IR_wei)
upper_lim_weight = wei_p75 + (1.5 * IR_wei)

# 9) Check outliers with Interquartile Range: Dropping observations which do not satisfy IQR outliers approach
data_iqr = data
data_iqr = data_iqr[!(data_iqr$Age > upper_lim_age | data_iqr$Age < lower_lim_age),]
data_iqr = data_iqr[!(data_iqr$Weight > upper_lim_weight | data_iqr$Weight < lower_lim_weight),] #same outliers for Weight and Height

# 9) Check outliers with Interquartile Range: boxplot without outliers
#fig_bp4 = ggplot(data_iqr, aes(y = Age, color = Age)) + #fig3
# geom_boxplot() +
# ggtitle("Age") + 
# theme_bw()
#fig_bp5 = ggplot(data_iqr, aes(y = Height, color = Height)) + #fig3
# geom_boxplot() +
# ggtitle("Height") + 
# theme_bw()
#fig_bp6 = ggplot(data_iqr, aes(y = Weight, color = Weight)) + #fig3
# geom_boxplot() +
# ggtitle("Weight") + 
# theme_bw()
#grid.arrange(fig_bp4, fig_bp5, fig_bp6, ncol = 3)

#rm(data_cha, data_num, age_p25, age_p50, age_p75, hei_p25, hei_p50, hei_p75, wei_p25, wei_p50, wei_p75, IR_age, IR_wei, IR_hei,
#   lower_lim_age, lower_lim_height, lower_lim_weight, upper_lim_age, upper_lim_height, upper_lim_weight, outliers,
#  data_out, cor_n, out_summary, find_outliers_age, find_outliers_weight)
#rm(fig_bp1, fig_bp2, fig_bp3, fig_bp4, fig_bp5, fig_bp6)

#### Data preparation for regression\classification ####
# 1) Data Pre-Processing
sapply(data_iqr,class)

# 2) one-hot-encoding for binary categorical variables
data_cha_ohe = data_iqr[, c("Gender", "family_history_with_overweight", "FAVC", "SMOKE", "SCC", "MTRANS")]
dmy = dummyVars(" ~ .", data = data_cha_ohe)
dat_transformed = data.frame(predict(dmy, newdata = data_cha_ohe))

data_iqr = cbind(data_iqr, dat_transformed)
data_iqr = data_iqr[, -c(1, 5, 6, 10, 12, 16 )]
rm(data_cha_ohe, dmy, dat_transformed)

# 3) Ordinal Encoding: used for ordinal categorical variables "CAEC", "CALC", "NObeyesdad"
data_iqr$CAEC = factor(data_iqr$CAEC, levels = c("no", "Sometimes", "Frequently", "Always"), labels = c(1, 2, 3, 4))
data_iqr$CALC = factor(data_iqr$CALC, levels = c("no", "Sometimes", "Frequently", "Always"), labels = c(1, 2, 3, 4))
data_iqr$NObeyesdad = factor(data_iqr$NObeyesdad, levels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"), labels = c(0:6))

col_order = c("GenderFemale", "GenderMale", "Age", "Height", "Weight", "family_history_with_overweightno", "family_history_with_overweightyes", 
              "FAVCno", "FAVCyes", "FCVC", "NCP", "CAEC",
              "SMOKEno", "SMOKEyes", "CH2O", "SCCno", "SCCyes", "FAF", "TUE", 
              "CALC", "MTRANSAutomobile", "MTRANSBike", "MTRANSMotorbike", "MTRANSPublic_Transportation", 
              "MTRANSWalking", "NObeyesdad")
data_iqr = data_iqr[, col_order]
describe_distribution(data_iqr)

dev.off()
boxplot(data_iqr)
title("Comparing boxplot()s")

# 4) Scaling variables
data_iqr$CAEC = as.numeric(levels(data_iqr$CAEC))[data_iqr$CAEC]
data_iqr$CALC = as.numeric(levels(data_iqr$CALC))[data_iqr$CALC]
data_iqr$NObeyesdad = as.numeric(levels(data_iqr$NObeyesdad))[data_iqr$NObeyesdad]
data_iqr_2 = scale(x = as.data.table(data_iqr), center = TRUE, scale = TRUE)

boxplot(data_iqr_2)
title("Comparing boxplot()s, scaled")
summary(data_iqr_2)
colnames(data_iqr)
data_iqr = data_iqr[,-c(2, 6, 8, 13, 16, 25)]

### 5) DF with only categorical variable scaled and numerical ones unchanged
scaled = as.data.frame(scale(x = as.data.table(data_iqr[,-c(2:4, 20)]), center = TRUE, scale = TRUE))

true = data_iqr[, c(2:4, 20)]
sc_data = cbind(scaled, true)

col_order = c("GenderFemale", "Age", "Height", "Weight", "family_history_with_overweightyes",
              "FAVCyes", "FCVC", "NCP", "CAEC",
              "SMOKEyes", "CH2O", "SCCyes", "FAF", "TUE", 
              "CALC", "MTRANSAutomobile", "MTRANSBike", "MTRANSMotorbike", "MTRANSPublic_Transportation", 
              "NObeyesdad")
sc_data = sc_data[, col_order]
rm(true, scaled)

#### before OLS REGRESSION ####
# Training and Test splitting set
set.seed(123)
split_train_test = createDataPartition(y = sc_data$NObeyesdad, p=0.8, list = F) 
train_data = sc_data[split_train_test ,]
test_data = sc_data[-split_train_test ,] 
dim(test_data)
dim(train_data)
#### 0) Part 1: LINEAR REGRESSION ####
lin_mod = lm(formula = NObeyesdad ~ ., data = train_data)
lin_sum = summary(lin_mod)

par(mfrow = c(2,2))
plot(lin_mod)
dev.off()

check_model(lin_mod)
check_normality(lin_mod)
check_heteroscedasticity(lin_mod)
check_autocorrelation(lin_mod) 
check_collinearity(lin_mod)

coeftest(lin_mod, vcov. = vcovHC, type = "HC1")
lin_mod_performance = performance(lin_mod)

# STEPWISE REGRESSION
lin_mod_step = lm(formula = NObeyesdad ~ ., data = train_data)
lin_mod_step = select_parameters(lin_mod_step)
lin_step_sum = summary(lin_mod_step)

coeftest(lin_mod_step ,vcov. = vcovHC , type = "HC1")

check_model(lin_mod_step)
check_normality(lin_mod_step) 
check_heteroscedasticity(lin_mod_step)
check_autocorrelation(lin_mod_step) 
check_collinearity(lin_mod_step)

lin_step_performance = performance(lin_mod_step)

# LASSO REGRESSION
x = as.matrix(train_data[,-20])
y = train_data$NObeyesdad
cv_lasso = cv.glmnet(x,y)
plot(cv_lasso)

coef = coef(cv_lasso , s = cv_lasso$lambda.min)
coefname = coef@Dimnames [[1]][ -1]
coef = coefname[coef@i]
coef

fmla = as.formula(paste("y ~ ", paste(coef, collapse = "+")))

lin_lasso = lm(fmla, data=train_data)
lin_lasso_sum = summary(lin_lasso)
check_model(lin_lasso)

check_normality(lin_lasso)
check_heteroscedasticity(lin_lasso)
check_autocorrelation(lin_lasso)
check_collinearity(lin_lasso)

coeftest(lin_lasso, vcov. = vcovHC, type='HC1')

lin_lasso_performance = performance(lin_lasso)

# compare linear regressions
compare_performance(lin_mod,lin_lasso,lin_mod_step,rank = T)
plot(compare_performance(lin_mod,lin_lasso,lin_mod_step,rank = T))

# Robust lasso estimator
lin_mod_lasso_robust = rlm(fmla,data=train_data, psi = psi.bisquare)

hweights = data.frame(resid = lin_mod_lasso_robust$resid, weight = lin_mod_lasso_robust$w) 
hweights2 = hweights[order(lin_mod_lasso_robust$w),]
hweights2 [1:10 ,]

rob_se_pan = list(sqrt(diag(vcovHC(lin_mod , type = "HC1"))),
                  sqrt(diag(vcovHC(lin_mod_step , type = "HC1"))), 
                  sqrt(diag(vcovHC(lin_lasso , type = "HC1"))),
                  sqrt(diag(vcovHC(lin_mod_lasso_robust , type = "HC1")))) 

#### 1) Part 1: MULTINOMIAL LOGISTIC REGRESSION ####
train_data_log = train_data
test_data_log = test_data
train_data_log$NObeyesdad = factor(train_data_log$NObeyesdad, levels = c(1:7), labels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
test_data_log$NObeyesdad = factor(test_data_log$NObeyesdad, levels = c(1:7), labels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

# Setting the 'reference' level
train_data_log$NObeyesdad = relevel(train_data_log$NObeyesdad, ref = "Insufficient_Weight")
multinom_model = multinom(NObeyesdad ~ ., data = train_data_log) #training the model
summary(multinom_model)
exp(coef(multinom_model)) # Convert the coefficients to odds by taking the exponential of the coefficients.
head(round(fitted(multinom_model), 2))# The predicted values are saved as fitted.values in the model object.

# Predicting & Validating the model: Predicting the values for train dataset
train_data_log$ClassPredicted = predict(multinom_model, newdata = train_data_log, "class")
tab = table(train_data_log$NObeyesdad, train_data_log$ClassPredicted) # Building classification table
tab
round((sum(diag(tab))/sum(tab))*100,2)

# Predicting the class on test dataset.
test_data_log$ClassPredicted = predict(multinom_model, newdata = test_data_log, "class")
tab_1 = table(test_data_log$NObeyesdad, test_data_log$ClassPredicted)# Building classification table
tab_1
round((sum(diag(tab_1))/sum(tab_1))*100,2)

#### 2) Part 1: REGRESSION DECISION TREES####
train_data_log = train_data
test_data_log = test_data
train_data_log$NObeyesdad = factor(train_data_log$NObeyesdad, levels = c(1:7), labels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
test_data_log$NObeyesdad = factor(test_data_log$NObeyesdad, levels = c(1:7), labels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

tree_1 = rpart(NObeyesdad~., data = train_data_log, method = "class")
printcp(tree_1)
summary(tree_1)

cv_tree_1 = plotcp(tree_1) #size of the tree = 10
rpart.plot(tree_1)

prune_tree_1 = prune(tree_1,cp=0.023)
rpart.plot(prune_tree_1)

#prediction on train data
dt_1_yhat = as.numeric(predict(tree_1, newdata=train_data_log, type= "class"))
dt_1_train = as.numeric(train_data_log[,"NObeyesdad"]) #output vector in test data
plot(dt_1_yhat,dt_1_train);abline(0,1, col = "red") #how is the distribution between the final units in the nodes

# Prediction on test data
dt_1_pred = as.numeric(predict(tree_1, test_data, type= "class"))

# Confusion Matrix
confMat = table(test_data[, "NObeyesdad"],dt_1_pred)
dt_1_accuracy = sum(diag(confMat))/sum(confMat)

TREE = c(rmse(test_data$NObeyesdad, dt_1_pred),
         mae(test_data$NObeyesdad, dt_1_pred),
         R2(train_data$NObeyesdad, dt_1_yhat))

tab_2 = table(test_data_log$NObeyesdad,dt_1_pred)
tab_accuracy = sum(diag(tab_2))/sum(tab_2)

#### 3) Part 1: RANDOM FOREST REGRESSION ####
rf_1 = randomForest(NObeyesdad ~ .,data=train_data, mtry = sqrt(19), type = "regression")
plot(rf_1, main = "Random Forest Error")
rf_1_y_hat = as.numeric(predict(rf_1 ,newdata=test_data))
test_data$NObeyesdad = as.numeric(test_data$NObeyesdad)
mean((rf_1_y_hat - test_data$NObeyesdad)^2)

ggplot() + 
  geom_point(aes(x = test_data$NObeyesdad, y = rf_1_y_hat)) +
  geom_abline()

impo = round(importance(rf_1),2)
varImpPlot (rf_1)

oob.err=double(19)
test.err=double(19)
for(mtry in 1:19){
  fit=randomForest(NObeyesdad~.,data=train_data,mtry=mtry,ntree=400) 
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,test_data)
  test.err[mtry]=with(test_data,mean((NObeyesdad-pred)^2))
  cat(mtry," ")
}
oob.err
test.err
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error"); legend("topright",legend=c("Test","OOB"),pch=19,col=c("red","blue"))

RF = c(rmse(test_data$NObeyesdad,rf_1_y_hat),
       mae(test_data$NObeyesdad, rf_1_y_hat),
       R2(test_data$NObeyesdad, rf_1_y_hat))

#### Part 1: COMPARISONS ####
test_pred = predict(lin_mod, newdata = test_data) 
train_pred = predict(lin_mod , newdata = train_data)
OLS =  c(rmse(test_data$NObeyesdad, test_pred),
         mae(test_data$NObeyesdad, test_pred),
         R2(train_data$NObeyesdad, train_pred)) 
test_pred = predict(lin_mod_step , newdata = test_data)
train_pred = predict(lin_mod_step , newdata = train_data)
Stepwise =  c(rmse(test_data$NObeyesdad, test_pred),
              mae(test_data$NObeyesdad, test_pred),
              R2(train_data$NObeyesdad, train_pred))
test_pred = predict(lin_lasso , newdata = test_data) 
train_pred = predict(lin_lasso , newdata = train_data)
LASSO =  c(rmse(test_data$NObeyesdad, test_pred),
           mae(test_data$NObeyesdad, test_pred),
           R2(train_data$NObeyesdad, train_pred)) 
test_pred = predict(lin_mod_lasso_robust , newdata = test_data)
train_pred = predict(lin_mod_lasso_robust , newdata = train_data)
Robust =  c(rmse(test_data$NObeyesdad, test_pred),
            mae(test_data$NObeyesdad, test_pred),
            R2(train_data$NObeyesdad, train_pred))
metrics =  data.frame(OLS,Stepwise,LASSO,Robust,TREE,RF)
row.names(metrics) =  c("RMSE","MAE","R2")
metrics
#### 1) Part 2: INTRODUCTION ####
rm(list = ls())
dev.off()
data = read.csv(file = "~/Desktop/FantaCalcio_Project/Obesity/data/data.csv")

# Data cleaning
data$Age = round(data$Age, 0)
data$Height = round(data$Height, 2)
data$Weight = round(data$Weight, 2)
data$FCVC = round(data$FCVC, 0)
data$NCP = round(data$NCP, 0)
data = data %>% 
  mutate(NPC=ifelse(NCP==4,3,NCP))
data$NCP = data$NPC
data = data[, -18]
data$CH2O = round(data$CH2O, 0)
data$FAF = round(data$FAF, 0)
data$TUE = round(data$TUE,0)

#Replace nominal variables with categorical variables
data["FCVC"][data["FCVC"] == 1] = "Never"
data["FCVC"][data["FCVC"] == 2] = "Sometimes"
data["FCVC"][data["FCVC"] == 3] = "Always"

data["NCP"][data["NCP"] == 1] = "1_or _2_meals"
data["NCP"][data["NCP"] == 2] = "3_meals"
data["NCP"][data["NCP"] == 3] = "More_than_3_meals"

data["CH2O"][data["CH2O"] == 1] = "Less_than_1_liter"
data["CH2O"][data["CH2O"] == 2] = "1_or_2_liters"
data["CH2O"][data["CH2O"] == 3] = "More_than_2_liters"

data["FAF"][data["FAF"] == 0] = "No_activity"
data["FAF"][data["FAF"] == 1] = "1_or_2_days"
data["FAF"][data["FAF"] == 2] = "2_or_3_days"
data["FAF"][data["FAF"] == 3] = "4_or_5_days"

data["TUE"][data["TUE"] == 0] = "0-to_2_hours"
data["TUE"][data["TUE"] == 1] = "3_to_5_hours"
data["TUE"][data["TUE"] == 2] = "More_than_5_hours"

quanti = data[,c(2,3,4)]
quali = data[, c(1, 5:16)]
quali = as.data.frame(unclass(quali),stringsAsFactors = TRUE)
data = cbind(quanti, quali)
rm(quali, quanti)
#### 2) Part 2: MCA ####
#1) Model 
data_active = data[, c(6:9, 11, 12, 15)]
par(mfrow = c(2, 4))
for (i in 1:7) {
  plot(data_active[,i], main=colnames(data_active)[i],
       ylab = "Count", col="beige", las = 2)
}

res.mca = MCA(data_active, graph = TRUE, ncp = 3)

#Outliers detection with Factoinvestigate and removale
outliers = FactoInvestigate::outliers(res.mca)
outliers$ID

data_active = data_active[-outliers$ID, ]

res.mca = MCA(data_active, graph = TRUE, ncp = 3)
summary(res.mca)
#2) Visualization and Interpretation: Eigenvalues-Variance
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

#3) Bi-plot
fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

#4) Correlation between variables and principal dimensions
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

#5) extract the results for variable categories
var = get_mca_var(res.mca)

#6) coordinates of variables to create a scatter plot
round(var$coord, 2)

#7) contains the contributions (in percentage) of the variables to the definition of the dimensions.
round(var$contrib,2)

#8) represents the quality of the representation for variables on the factor map.
round(var$cos2,2)

#9) Color by cos2 values: quality on the factor map:
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

#10) Graph of individuals
ind = get_mca_ind(res.mca)

#11) Coordinates of column points
head(ind$coord)

#12) Quality of representation
head(ind$cos2)

#13) Contributions
head(ind$contrib)

#14) Color individuals by groups
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "FAVC", # color by groups 
             palette = display.brewer.pal(n = 7, name = 'RdBu'),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

#15) Dimension description
res.desc = dimdesc(res.mca, axes = c(1,2))

#16) Description of dimension 1
res.desc[[1]]

#17) Description of dimension 2
res.desc[[2]]

summary(res.mca, nbelements = Inf)

#### 4) Part 2: HCPC ####
res.HCPC=HCPC(res.mca, graph = FALSE)

#1) Dendrogram
fviz_dend(res.HCPC, show_labels = FALSE)

#2) Individuals factor map
fviz_cluster(res.HCPC, geom = "point", main = "Factor map")

#3) Description by variables
head(res.HCPC$desc.var$test.chi2,10)

#4) Description by variable categories
head(res.HCPC$desc.var$category,10)

#5) Description by principal components
res.HCPC$desc.axes

#6) Individuals closest to the centre
res.HCPC$desc.ind$para

#7) Individuals farest from the centre
res.HCPC$desc.ind$dist

res.coph <- cophenetic(res.HCPC)


#8) Plots
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Hierarchical tree on the factor map')