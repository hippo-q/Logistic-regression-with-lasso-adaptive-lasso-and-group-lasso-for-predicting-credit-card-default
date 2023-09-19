library(readxl)
library(plyr)

library(ggplot2)
library(ggpubr)
library(patchwork)
library(scales)
library(ggthemes)

library(dplyr)
library(ggpol)

library(caret)
library(ROCR)
library(MASS)
library(caTools)

library(grid)
library(broom)
library(tidyr)
library(gridExtra)
library(data.table)

library(pROC)
library(glmnet)
library(plotmo)
library(gglasso)
library(grpreg)
setwd("~/Desktop/STAT0036")

default_of_credit_card_clients <- read_excel("default of credit card clients.xls")
original_data <- default_of_credit_card_clients[ ,-1]
head(original_data)
# remove the variable: order 


# We have 24 variables.
ncol(original_data)
# We have 29999 observations.
nrow(original_data)

#需要marriage =0 的删除吗
which(original_data$MARRIAGE == 0)
which(original_data$EDUCATION == 5)
which(original_data$EDUCATION == 6)
which(original_data$PAY_1== 0)
original_data = subset(original_data, original_data$MARRIAGE != 0)
original_data = subset(original_data, original_data$EDUCATION != 0)
original_data = subset(original_data, original_data$EDUCATION != 5)
original_data = subset(original_data, original_data$EDUCATION != 6)
str(original_data)

summary(original_data)

# Change the column name PAY_O into PAY_1 for convenience
colnames(original_data)[colnames(original_data) == "PAY_0"] <- "PAY_1"

str(original_data)

# change some variables into factor
original_data$SEX <- as.factor(original_data$SEX)
original_data$EDUCATION <- as.factor(original_data$EDUCATION)
original_data$MARRIAGE <- as.factor(original_data$MARRIAGE)
original_data$PAY_1 <- as.factor(original_data$PAY_1)
original_data$PAY_2 <- as.factor(original_data$PAY_2)
original_data$PAY_3 <- as.factor(original_data$PAY_3)
original_data$PAY_4 <- as.factor(original_data$PAY_4)
original_data$PAY_5 <- as.factor(original_data$PAY_5)
original_data$PAY_6<- as.factor(original_data$PAY_6)
original_data$`default payment next month`<- as.factor(original_data$`default payment next month`)



str(original_data)

attach(original_data)
str(original_data)
# No missing values
which(is.na(original_data))

table(`default payment next month`)
table(SEX)
table(AGE)
table(MARRIAGE)
table(EDUCATION)

# EDA part
male_default <- original_data %>%
  filter(SEX == "1" & `default payment next month` == "1" )
male_nodefault <- original_data %>%
  filter(SEX == "1" & `default payment next month` == "0" )
female_default <- original_data %>%
  filter(SEX == "2" & `default payment next month` == "1" )
female_nodefault <- original_data %>%
  filter(SEX == "2" & `default payment next month` == "0" )


marrid_default <- original_data %>%
  filter(MARRIAGE == "1" & `default payment next month` == "1" )
singe_default <- original_data %>%
  filter(MARRIAGE == "2" & `default payment next month` == "1" )

gra_default <- original_data %>%
  filter(EDUCATION == "1" & `default payment next month` == "1" )
uni_default <- original_data %>%
  filter(EDUCATION == "2" & `default payment next month` == "1" )
high_default <- original_data %>%
  filter(EDUCATION== "3" & `default payment next month` == "1" )


str(original_data)
table(EDUCATION)
summary(original_data)



# age plot
# "#3399FF" ???, "#CC6666"???
age_plot <- ggplot(original_data, aes(x = AGE, color = `default payment next month`)) + geom_density() + scale_x_continuous(breaks = seq(0, 80, 10))+ theme_minimal() +scale_color_manual(values=c("#3399FF", "#CC6666"))+scale_fill_discrete(labels = c("no default", "default")) + theme(legend.position="bottom") + theme(legend.text=element_text(size=15), legend.title = element_text(size = 15))
ggsave(age_plot, file="age.eps", device="eps", width = 10)

limit_plot <- ggplot(original_data, aes(x = LIMIT_BAL, color = `default payment next month`)) + geom_density() + theme_minimal() +scale_color_manual(values=c("#3399FF", "#CC6666"))+scale_fill_discrete(labels = c("no default", "default")) + theme(legend.position="bottom") + theme(legend.text=element_text(size=15), legend.title = element_text(size = 15))
ggsave(limit_plot, file="limit.eps", device="eps", width = 10)

ggplot(original_data, aes(x = EDUCATION, fill = factor(`default payment next month`)))+ geom_bar(position = "stack")
edu_plot <- ggplot(original_data, aes(x = EDUCATION, fill = factor(`default payment next month`, levels = c("0", "1"), labels = c("no default", "default") ))) + geom_bar(position = "stack") + theme_minimal() + scale_x_discrete(labels = c("1" = "graduate school", "2" = "university", "3" = "high school", "4" = "others")) + labs(y = "Frequency")+ theme(legend.position="bottom")+scale_fill_manual(values=c("steelblue", "#CC6666")) + labs(fill = "Default payment next month") + theme(text = element_text(size = 18)) 
ggsave(edu_plot, file="edu.eps", device="eps")

sex_plot <- ggplot(original_data, aes(x = SEX, fill = factor(`default payment next month`, levels = c("0", "1"), labels = c("no default", "default") ))) + geom_bar(position = "stack") + theme_minimal() + scale_x_discrete(labels = c("1" = "male", "2" = "female")) + labs(y = "Frequency") + theme(legend.position="bottom")+scale_fill_manual(values=c("steelblue", "#CC6666")) + labs(fill = "Default payment next month") + theme(text = element_text(size = 18)) 
ggsave(sex_plot, file = "sex.eps", device = "eps")

marriage_plot <- ggplot(original_data, aes(x = MARRIAGE, fill = factor(`default payment next month`, levels = c("0", "1"), labels = c("no default", "default") ))) + geom_bar(position = "stack") + theme_minimal() + scale_x_discrete(labels = c("1" = "married", "2" = "single", "3" = "others")) + labs(y = "Frequency") + theme(legend.position="bottom")+scale_fill_manual(values=c("steelblue", "#CC6666")) + labs(fill = "Default payment next month") + theme(text = element_text(size = 18))       
ggsave(marriage_plot, file = "marriage.eps", device = "eps")




repay1 <- ggplot(original_data, aes(x = PAY_6, fill = `default payment next month`)) +geom_bar(position = "stack") + theme_minimal() +scale_fill_manual(values=c("steelblue", "#CC6666")) + labs(y = "Frequency", x = "Repayment status in Apr") + theme(legend.position = "none")
repay2 <- ggplot(original_data, aes(x = PAY_5, fill = `default payment next month`)) +geom_bar(position = "stack") + theme_minimal()+ labs(y = "Frequency", x = "Repayment status in May")+ theme(legend.position = "none") +scale_fill_manual(values=c("steelblue", "#CC6666"))
repay3 <-ggplot(original_data, aes(x = PAY_4, fill = `default payment next month`)) +geom_bar(position = "stack")+ theme_minimal()+ labs(y = "Frequency", x = "Repayment status in Jun")+ theme(legend.position = "none") +scale_fill_manual(values=c("steelblue", "#CC6666"))
repay4 <- ggplot(original_data, aes(x = PAY_3, fill = `default payment next month`)) +geom_bar(position = "stack") + theme_minimal()+ labs(y = "Frequency", x = "Repayment status in Jul")+ theme(legend.position = "none") +scale_fill_manual(values=c("steelblue", "#CC6666"))
repay5 <- ggplot(original_data, aes(x = PAY_2, fill = `default payment next month`)) +geom_bar(position = "stack")+ theme_minimal()+ labs(y = "Frequency", x = "Repayment status in Aug")+ theme(legend.position = "none") +scale_fill_manual(values=c("steelblue", "#CC6666"))
repay6 <- ggplot(original_data, aes(x = PAY_1, fill = `default payment next month`)) +geom_bar(position = "stack") + theme_minimal()+ labs(y = "Frequency", x = "Repayment status in Sept")+ theme(legend.position = "none") +scale_fill_manual(values=c("steelblue", "#CC6666"))
repay_plot <- ggarrange(repay1, repay2, repay3, repay4, repay5, repay6, ncol = 2, nrow = 3)
ggsave(repay_plot, file = "repay.eps", device = "eps", width = 10)

bill_1 <- ggplot(original_data, aes(x = BILL_AMT6, color = `default payment next month`)) + geom_density() + theme_minimal() + labs(y = "Density", x = "Amount of bill statement in Apr") + theme(legend.position = "none") + scale_x_continuous(limits=c(0, 100000))+scale_color_manual(values=c("#3399FF", "#CC6666"))
bill_2 <- ggplot(original_data, aes(x = BILL_AMT6, color = `default payment next month`)) + geom_density() + theme_minimal()+ labs(y = "Density", x = "Amount of bill statement in May") + theme(legend.position = "none")+ scale_x_continuous(limits=c(0, 100000))+scale_color_manual(values=c("#3399FF", "#CC6666"))
bill_3 <- ggplot(original_data, aes(x = BILL_AMT6, color = `default payment next month`)) + geom_density() + theme_minimal()+ labs(y = "Density", x = "Amount of bill statement in Jun") + theme(legend.position = "none")+ scale_x_continuous(limits=c(0, 100000))+scale_color_manual(values=c("#3399FF", "#CC6666"))
bill_4 <- ggplot(original_data, aes(x = BILL_AMT6, color = `default payment next month`)) + geom_density() + theme_minimal()+ labs(y = "Density", x = "Amount of bill statement in Jul") + theme(legend.position = "none")+ scale_x_continuous(limits=c(0, 100000))+scale_color_manual(values=c("#3399FF", "#CC6666"))
bill_5 <- ggplot(original_data, aes(x = BILL_AMT6, color = `default payment next month`)) + geom_density() + theme_minimal()+ labs(y = "Density", x = "Amount of bill statement in Aug") + theme(legend.position = "none")+ scale_x_continuous(limits=c(0, 100000))+scale_color_manual(values=c("#3399FF", "#CC6666"))
bill_6 <- ggplot(original_data, aes(x = BILL_AMT6, color = `default payment next month`)) + geom_density() + theme_minimal()+ labs(y = "Density", x = "Amount of bill statement in Sept") + theme(legend.position = "none")+ scale_x_continuous(limits=c(0, 100000))+scale_color_manual(values=c("#3399FF", "#CC6666"))
bill_plot <- ggarrange(bill_1, bill_2, bill_3, bill_4, bill_5, bill_6, ncol = 2, nrow = 3)
ggsave(bill_plot, file = "bill.eps", device = "eps", width = 10)


abill_1 <- ggplot(original_data, aes(x = PAY_AMT6, color = `default payment next month`)) + geom_density() + theme_minimal() + labs(y = "Density", x = "Amount of previous bill statement in Apr") + theme(legend.position = "none")
abill_2 <- qplot(PAY_AMT5, data = original_data, geom = "density",  color = `default payment next month`) + theme_minimal()+ labs(y = "Density", x = "Amount of previous bill statement in May") + theme(legend.position = "none")
abill_3 <- qplot(PAY_AMT4, data = original_data, geom = "density",  color = `default payment next month`) + theme_minimal()+ labs(y = "Density", x = "Amount of previous bill statement in Jun") + theme(legend.position = "none")
abill_4 <- qplot(PAY_AMT3, data = original_data, geom = "density",  color = `default payment next month`) + theme_minimal()+ labs(y = "Density", x = "Amount of previous bill statement in Jul") + theme(legend.position = "none")
abill_5 <- qplot(PAY_AMT2, data = original_data, geom = "density",  color = `default payment next month`) + theme_minimal()+ labs(y = "Density", x = "Amount of previous bill statement in Aug") + theme(legend.position = "none")
abill_6 <- qplot(PAY_AMT1, data = original_data, geom = "density",  color = `default payment next month`) + theme_minimal()+ labs(y = "Density", x = "Amount of previous bill statement in Sept") + theme(legend.position = "none")
abill_plot <- ggarrange(abill_1, abill_2, abill_3, abill_4, abill_5, abill_6, ncol = 2, nrow = 3)
ggsave(abill_plot, file = "previousbill.eps", device = "eps", width = 10)

summary(original_data)
str(original_data)

ggplot(original_data, aes(x = factor(`default payment next month`, labels = c("default", "no-default")), y = BILL_AMT1, color = `default payment next month`)) + geom_boxplot(size = 1, outlier.shape = 1, outlier.color = "black", outlier.size = 3) + geom_boxjitter(alpha = 0.5, width = .2) + scale_y_continuous(label = dollar)
ggplot(original_data, aes(x = `default payment next month`, y = PAY_AMT6 )) + geom_boxplot()


original_data$Order <- seq(1, 29601)
prebill1 <- ggplot(original_data, aes(x = Order, y = PAY_AMT6, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Apr")
prebill2 <- ggplot(original_data, aes(x = Order, y = PAY_AMT5, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "May")
prebill3 <- ggplot(original_data, aes(x = Order, y = PAY_AMT4, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Jun")
prebill4 <- ggplot(original_data, aes(x = Order, y = PAY_AMT3, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Jul")
prebill5 <- ggplot(original_data, aes(x = Order, y = PAY_AMT2, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Aug")
prebill6 <- ggplot(original_data, aes(x = Order, y = PAY_AMT1, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Sept")
prebill_plot <- ggarrange(prebill1, prebill2, prebill3, prebill4, prebill5, prebill6, ncol = 2, nrow = 3)
ggsave(prebill_plot, file = "previousbill_line.eps", device = "eps", width = 10)


bill1 <- ggplot(original_data, aes(x = Order, y = BILL_AMT6, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Apr")
bill2 <- ggplot(original_data, aes(x = Order, y = BILL_AMT5, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "May")
bill3 <- ggplot(original_data, aes(x = Order, y = BILL_AMT4, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Jun")
bill4 <- ggplot(original_data, aes(x = Order, y = BILL_AMT3, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Jul")
bill5 <- ggplot(original_data, aes(x = Order, y = BILL_AMT2, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Aug")
bill6 <- ggplot(original_data, aes(x = Order, y = BILL_AMT1, color = `default payment next month`)) + geom_line()+ theme_minimal() + theme(legend.position = "none")+ labs( x = "Sept")
bill_line_plot <- ggarrange(bill1, bill2, bill3, bill4, bill5, bill6, ncol = 2, nrow = 3)
ggsave(bill_line_plot, file = "bill_line.eps", device = "eps", width = 10)

limit_sexplot <- ggplot(original_data, aes(x = SEX, y = LIMIT_BAL, fill =  `default payment next month`)) + geom_boxplot()+ scale_fill_brewer() + theme_minimal()
ggsave(limit_sexplot, file = "limit_sex.eps", device = "eps", width = 10)
age_sexplot <- ggplot(original_data, aes(x = SEX, y = AGE, fill =  `default payment next month`)) + geom_boxplot()+ scale_fill_brewer() + theme_minimal()
ggsave(age_sexplot, file = "age_sex.eps", device = "eps", width = 10)




## creating new features
# new age group; 0 - low financial pressure; 1 - high financial pressure
original_data = original_data %>% mutate(age_group = case_when(AGE <= 25 ~ "low financial pressure",
                                                               AGE <= 40 ~ "high financial pressure",
                                                               AGE > 40 ~  "low financial pressure"))

# set one trainset and one testset
n <- nrow(original_data)
set.seed(4321)
trainindex <- sample(1:n, size = round(0.8*n), replace = FALSE)
trainset <- original_data[trainindex, ]
testset <- original_data[-trainindex, ]
lrmodel0 <- glm(`default payment next month` ~ . , data = trainset, family = binomial(link = "logit"))
trainset$prediction <- predict( lrmodel0, newdata = trainset, type = "response" )
testset$prediction  <- predict( lrmodel0, newdata = testset , type = "response" )


# plot AUC curve on this one testset
roc(testset$`default payment next month` ~ testset$prediction, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)
# ideal AUC
roc(testset$prediction ~ testset$prediction, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)

## AUC boxplot 
##---------------------------------------------------------------------------------------------------
aucs_log <- list()
aucs_min <- list()
aucs_1se <- list()
aucs_1se_adaptive <- list()
aucs_min_adaptive <- list()
aucs_1se_adaptive_1 <- list()
aucs_min_adaptive_1 <- list()
aucs_min_group <- list()
aucs_1se_group <- list()
aucs_min_adpgrp <- list()
aucs_1se_adpgrp <- list()

train_data <- list()
test_data <- list()
for (i in 1:50){
  trainindex <- sample(1:n, size = round(0.8*n), replace = FALSE)
  train_data[[i]] <- original_data[trainindex, ]
  test_data[[i]] <- original_data[-trainindex, ]
}

for (i in 1:50){
  trainset = train_data[[i]]
  testset = test_data[[i]]
  
  ##logistic regression
  lrmodel0 <- glm(`default payment next month` ~ . , data = trainset, family = binomial(link = "logit"))
  
  lrmodel0$xlevels[["PAY_1"]] <- union(lrmodel0$xlevels[["PAY_1"]], levels(testset$PAY_1))
  lrmodel0$xlevels[["PAY_2"]] <- union(lrmodel0$xlevels[["PAY_2"]], levels(testset$PAY_2))
  lrmodel0$xlevels[["PAY_3"]] <- union(lrmodel0$xlevels[["PAY_3"]], levels(testset$PAY_3))
  lrmodel0$xlevels[["PAY_4"]] <- union(lrmodel0$xlevels[["PAY_4"]], levels(testset$PAY_4))
  lrmodel0$xlevels[["PAY_5"]] <- union(lrmodel0$xlevels[["PAY_5"]], levels(testset$PAY_5))
  lrmodel0$xlevels[["PAY_6"]] <- union(lrmodel0$xlevels[["PAY_6"]], levels(testset$PAY_6))
  ## predicting and assessing the model
  # prediction
  trainset$prediction <- predict( lrmodel0, newdata = trainset, type = "response" )
  testset$prediction  <- predict( lrmodel0, newdata = testset , type = "response" )
  test_roc <- roc(testset$`default payment next month` ~ testset$prediction, plot = FALSE, print.auc = TRUE)
  aucs_log <- append(aucs_log, as.numeric(test_roc$auc))
}

for (i in 1:50){
  ##logistic lasso regression
  trainset = train_data[[i]]
  testset = test_data[[i]]
  
  y.train <- trainset$`default payment next month`
  x.train <- data.matrix(trainset[, c('LIMIT_BAL', 'SEX', 'EDUCATION', 'MARRIAGE', 'AGE', 'PAY_1', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6','BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6', 'age_group')])
  cross_val <- cv.glmnet(as.matrix(x.train), as.matrix(y.train), 
                         family = 'binomial', 
                         type.measure = 'class',
                         alpha = 1, 
                         nlambda = 100)
  fit_1se <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                    family = 'binomial', 
                    alpha = 1, 
                    lambda = cross_val$lambda.1se)
  
  fit_min <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                    family = 'binomial', 
                    alpha = 1, 
                    lambda = cross_val$lambda.min)
  
  y.test <- testset$`default payment next month`
  x.test <- data.matrix(testset[, rownames(fit_min$beta)])
  predictions_1se <- predict(fit_1se, newx = as.matrix(x.test),type = 'response')
  predictions_min <- predict(fit_min, newx = as.matrix(x.test), type = 'response')
  
  ## predicting and assessing the model
  # prediction
  testset$prediction_1se  <- predictions_1se
  testset$prediction_min  <- predictions_min
  
  test_roc_min <- roc(testset$`default payment next month` ~ testset$prediction_min, plot = FALSE, print.auc = TRUE)
  test_roc_1se <- roc(testset$`default payment next month` ~ testset$prediction_1se, plot = FALSE, print.auc = TRUE)
  
  aucs_min <- append(aucs_min, as.numeric(test_roc_min$auc))
  aucs_1se <- append(aucs_1se, as.numeric(test_roc_1se$auc))
}
    
for (i in 1:50){
    trainset = train_data[[i]]
    testset = test_data[[i]]
    
    y.train <- trainset$`default payment next month`
    x.train <- data.matrix(trainset[, c('LIMIT_BAL', 'SEX', 'EDUCATION', 'MARRIAGE', 'AGE', 'PAY_1', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6','BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6', 'age_group')])
    ##logistic adaptive lasso
    #fit MLE
    mle_fit <- glm(y.train ~ x.train, family = binomial(link = 'logit'))
    beta.init <- coef(mle_fit)
    #calculate weight (gamma=0.5)
    gamma <- 0.5
    w <- 1/abs(beta.init)^gamma
    w[-1]
    lasso_cross_val <- cv.glmnet(as.matrix(x.train), as.matrix(y.train), 
                                 family = 'binomial', 
                                 standardize = FALSE,
                                 type.measure = "class",
                                 alpha = 1, 
                                 penalty.factor = w[-1])
    fit_1se_adaptive <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                               family = 'binomial', 
                               alpha = 1, 
                               lambda = lasso_cross_val$lambda.1se,
                               penalty.factor = w[-1])
    fit_min_adaptive <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                               family = 'binomial', 
                               alpha = 1, 
                               lambda = lasso_cross_val$lambda.min,
                               penalty.factor = w[-1])
    y.test <- testset$`default payment next month`
    x.test <- data.matrix(testset[, rownames(fit_min_adaptive$beta)])
    predictions_1se_adaptive <- predict(fit_1se_adaptive, newx = as.matrix(x.test),type = 'response')
    predictions_min_adaptive <- predict(fit_min_adaptive, newx = as.matrix(x.test), type = 'response')
    ## predicting and assessing the model
    # prediction
    testset$prediction_1se_adaptive  <- predictions_1se_adaptive
    testset$prediction_min_adaptive  <- predictions_min_adaptive
    
    test_roc_min_adaptive <- roc(testset$`default payment next month` ~ testset$prediction_min_adaptive, plot = FALSE, print.auc = TRUE)
    test_roc_1se_adaptive <- roc(testset$`default payment next month` ~ testset$prediction_1se_adaptive, plot = FALSE, print.auc = TRUE)
    
    aucs_min_adaptive <- append(aucs_min_adaptive, as.numeric(test_roc_min_adaptive$auc))
    aucs_1se_adaptive <- append(aucs_1se_adaptive, as.numeric(test_roc_1se_adaptive$auc))
}

for (i in 1:50){
    trainset = train_data[[i]]
    testset = test_data[[i]]
    
    ## logistic adaptive lasso 
    y.train <- trainset$`default payment next month`
    x.train <- data.matrix(trainset[, c('LIMIT_BAL', 'SEX', 'EDUCATION', 'MARRIAGE', 'AGE', 'PAY_1', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6','BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6', 'age_group')])
    #calculate weight (gamma = 1)
    gamma1 <- 1
    w <- 1/abs(beta.init)^gamma1
    w[-1]
    lasso_cross_val_1 <- cv.glmnet(as.matrix(x.train), as.matrix(y.train), 
                                   family = 'binomial', 
                                   standardize = FALSE,
                                   type.measure = "class",
                                   alpha = 1, 
                                   penalty.factor = w[-1])
    fit_1se_adaptive_1 <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                                 family = 'binomial', 
                                 alpha = 1, 
                                 lambda = lasso_cross_val_1$lambda.1se,
                                 penalty.factor = w[-1])
    
    fit_min_adaptive_1 <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                                 family = 'binomial', 
                                 alpha = 1, 
                                 lambda = lasso_cross_val_1$lambda.min,
                                 penalty.factor = w[-1])
    
    x.test <- data.matrix(testset[, rownames(fit_min_adaptive$beta)])
    predictions_1se_adaptive_1 <- predict(fit_1se_adaptive_1, newx = as.matrix(x.test),type = 'response')
    predictions_min_adaptive_1 <- predict(fit_min_adaptive_1, newx = as.matrix(x.test), type = 'response')
    ## predicting and assessing the model
    # prediction
    testset$prediction_1se_adaptive_1  <- predictions_1se_adaptive_1
    testset$prediction_min_adaptive_1  <- predictions_min_adaptive_1
    
    test_roc_min_adaptive_1 <- roc(testset$`default payment next month` ~ testset$prediction_min_adaptive_1, plot = FALSE, print.auc = TRUE)
    test_roc_1se_adaptive_1 <- roc(testset$`default payment next month` ~ testset$prediction_1se_adaptive_1, plot = FALSE, print.auc = TRUE)
    
    aucs_min_adaptive_1 <- append(aucs_min_adaptive_1, as.numeric(test_roc_min_adaptive_1$auc))
    aucs_1se_adaptive_1 <- append(aucs_1se_adaptive_1, as.numeric(test_roc_1se_adaptive_1$auc))
}

group_lasso_lambdas <- list(min = list(), lse = list())

for (i in 1:1){
    trainset = train_data[[i]]
    testset = test_data[[i]]
    
    y.train <- trainset$`default payment next month`
    x.train <- data.matrix(trainset[, c('LIMIT_BAL', 'SEX', 'EDUCATION', 'MARRIAGE', 'AGE', 'PAY_1', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6','BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6', 'age_group')])
    ### group lasso
    y.train.gl <- ifelse(y.train == "0", -1, 1)
    group.index <- c(1,2,3,4,5,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,5)
    gglasso_cv <- cv.gglasso(as.matrix(x.train), y.train.gl, group = group.index, 
                             pred.loss = "loss", loss = "logit", nlambda = 20)
    group_lasso_lambdas$min <- append(group_lasso_lambdas$min, gglasso_cv$lambda.min)
    group_lasso_lambdas$lse <- append(group_lasso_lambdas$lse, gglasso_cv$lambda.1se)
    
    fit_min_group <- gglasso(as.matrix(x.train), y.train.gl, group = group.index, 
                             loss = "logit", lambda = gglasso_cv$lambda.min)
    fit_1se_group <- gglasso(as.matrix(x.train), y.train.gl, group = group.index, 
                             loss = "logit", lambda = gglasso_cv$lambda.1se)
    x.test <- data.matrix(testset[, rownames(fit_min_group$beta)])
    predictions_1se_group <- predict(fit_1se_group, newx = as.matrix(x.test),type = 'link')
    predictions_min_group <- predict(fit_min_group, newx = as.matrix(x.test), type = 'link')
    
    ## predicting and assessing the model
    # prediction
    testset$prediction_1se_group  <- predictions_1se_group
    testset$prediction_min_group  <- predictions_min_group
    
    test_roc_min_group <- roc(ifelse(testset$`default payment next month` == "0", -1, 1) ~ testset$prediction_min_group, plot = FALSE, print.auc = TRUE)
    test_roc_1se_group <- roc(ifelse(testset$`default payment next month` == "0", -1, 1) ~ testset$prediction_1se_group, plot = FALSE, print.auc = TRUE)
    
    aucs_min_group <- append(aucs_min_group, as.numeric(test_roc_min_group$auc))
    aucs_1se_group <- append(aucs_1se_group, as.numeric(test_roc_1se_group$auc))
}

for (i in 1:1){
    trainset = train_data[[i]]
    testset = test_data[[i]]
    
    y.train <- trainset$`default payment next month`
    x.train <- data.matrix(trainset[, c('LIMIT_BAL', 'SEX', 'EDUCATION', 'MARRIAGE', 'AGE', 'PAY_1', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6','BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4',
                                        'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6', 'age_group')])
    ### group lasso
    y.train.gl <- ifelse(y.train == "0", -1, 1)
    ## adaptive group lasso (using the same lambda from group lasso)
    # unpenalized MLE estimator of 8 groups
    mle_fit <- glm(y.train ~ x.train, family = binomial(link = 'logit'))
    mle_coef <- mle_fit$coefficients
    mle_coef_tidy <- broom::tidy(mle_fit)
    beta1 <- sqrt(abs(as.numeric(mle_coef_tidy[2, "estimate"])))
    beta2 <- sqrt(abs(as.numeric(mle_coef_tidy[3, "estimate"])))
    beta3 <- sqrt(abs(as.numeric(mle_coef_tidy[4, "estimate"])))
    beta4 <- sqrt(abs(as.numeric(mle_coef_tidy[5, "estimate"])))
    beta5 <- sqrt(abs(as.numeric(mle_coef_tidy[6, "estimate"])) + abs(as.numeric(mle_coef_tidy[25, "estimate"])))
    beta6 <- sqrt(abs(as.numeric(mle_coef_tidy[7, "estimate"])) + abs(as.numeric(mle_coef_tidy[8, "estimate"]))
                  + abs(as.numeric(mle_coef_tidy[9, "estimate"]))+ abs(as.numeric(mle_coef_tidy[10, "estimate"]))
                  + + abs(as.numeric(mle_coef_tidy[11, "estimate"])) + + abs(as.numeric(mle_coef_tidy[12, "estimate"])))
    beta7 <- sqrt(abs(as.numeric(mle_coef_tidy[13, "estimate"])) + abs(as.numeric(mle_coef_tidy[14, "estimate"]))
                  + abs(as.numeric(mle_coef_tidy[15, "estimate"]))+ abs(as.numeric(mle_coef_tidy[16, "estimate"]))
                  + + abs(as.numeric(mle_coef_tidy[17, "estimate"])) + + abs(as.numeric(mle_coef_tidy[18, "estimate"])))
    beta8 <- sqrt(abs(as.numeric(mle_coef_tidy[19, "estimate"])) + abs(as.numeric(mle_coef_tidy[20, "estimate"]))
                  + abs(as.numeric(mle_coef_tidy[21, "estimate"]))+ abs(as.numeric(mle_coef_tidy[22, "estimate"]))
                  + + abs(as.numeric(mle_coef_tidy[23, "estimate"])) + + abs(as.numeric(mle_coef_tidy[24, "estimate"])))
    # 
    
    x.adapt <- as.matrix(x.train)
    y.adapt <- as.matrix(y.train)
    y.adapt <- ifelse(y.train == "0", -1, 1)
    p <- ncol(x.train)
    beta <- c(beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8)
    gamma <- 1
    group.index <- c(1,2,3,4,5,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,5)
    for(i in 1:p){
      x.adapt[,i] <- x.adapt[,i] * beta[group.index[i]]^gamma
    }
    grpreg_cv_adap <- cv.grpreg(x.adapt, y.adapt, group = group.index, family = "binomial", 
                                penalty = "grLasso")
    
    one_se <- min(grpreg_cv_adap[["cve"]]) + sd(grpreg_cv_adap[["cve"]])/length(grpreg_cv_adap[["cve"]])
    lambda_1se <- max(grpreg_cv_adap$lambda[grpreg_cv_adap$lambda <= one_se])
    
    fit_min_adpgrp <- gglasso(x.adapt, as.numeric(y.adapt), group = group.index, 
                              loss = "logit", lambda = grpreg_cv_adap$lambda.min)
    fit_1se_adpgrp <- gglasso(x.adapt, as.numeric(y.adapt), group = group.index, 
                              loss = "logit", lambda = lambda_1se)
    x.test <- data.matrix(testset[, rownames(fit_min_adpgrp$beta)])
    for(i in 1:p){
      x.test[,i] <- x.test[,i] * beta[group.index[i]]^gamma
    }
    predictions_1se_adpgrp <- predict(fit_1se_adpgrp, newx = as.matrix(x.test),type = 'link')
    predictions_min_adpgrp <- predict(fit_min_adpgrp, newx = as.matrix(x.test), type = 'link')
    ## predicting and assessing the model
    # prediction
    testset$prediction_1se_adpgrp  <- predictions_1se_adpgrp
    testset$prediction_min_adpgrp  <- predictions_min_adpgrp
  
    
    test_roc_min_adpgrp <- roc(ifelse(testset$`default payment next month` == "0", -1, 1) ~ as.vector(testset$prediction_min_adpgrp), plot = FALSE, print.auc = TRUE)
    test_roc_1se_adpgrp <- roc(ifelse(testset$`default payment next month` == "0", -1, 1) ~ as.vector(testset$prediction_1se_adpgrp), plot = FALSE, print.auc = TRUE)
    
    aucs_min_adpgrp <- append(aucs_min_adpgrp, as.numeric(test_roc_min_adpgrp$auc))
    aucs_1se_adpgrp <- append(aucs_1se_adpgrp, as.numeric(test_roc_1se_adpgrp$auc))
}



aucs <- as.data.frame(do.call(cbind, list("logistic " = aucs_log, "\n lasso \n (min)" = aucs_min, " lasso \n (1se)" = aucs_1se,
                                          "adp \n (gamma=0.5, min)" = aucs_min_adaptive, "adp \n (gamma=0.5, 1se)" = aucs_1se_adaptive,
                                          "adp \n (gamma=1, min)" = aucs_min_adaptive_1, "adp \n (gamma=1, 1se)" = aucs_1se_adaptive_1,
                                          "grp lasso \n min" = aucs_min_group, "grp lasso \n 1se" = aucs_1se_group, "adpgrp \n min" = aucs_min_adpgrp, "adpgrp \n 1se" = aucs_1se_adpgrp)))
aucs_mat <- data.matrix(aucs)
##---------------------------------------------------------------------------------------------------
par(mfrow = c(1,1))
par(mgp=c(3,2,0))
names(aucs_mat) <- c(paste("logistic regression"))
boxplot(aucs_mat, xlab = "different methods", ylab = "AUC value", main = "AUC value of 50 test sets")


# hypothesis test
t.test(unlist(aucs_min), unlist(aucs_1se))
t.test(unlist(aucs_min_adaptive), unlist(aucs_1se_adaptive))
t.test(unlist(aucs_min_adaptive_1), unlist(aucs_1se_adaptive_1))




 # distribution of the prediction score grouped by known outcome
#negative, 0, not default
negativeset <- trainset[trainset$`default payment next month` == "0", ]
View(negativeset)
nrow(negativeset)
negative_data <- ggplot(negativeset, aes(x = prediction)) + geom_histogram(color = "grey", fill = "#3399FF") + xlab("Prediction score") + ylab("Frequency") + ggtitle("Negative data (0 ,not default) in training set prediction")
positiveset <- trainset[trainset$`default payment next month` == "1", ]
View(positiveset)
nrow(positiveset)
positive_data <- ggplot(positiveset, aes(x = prediction)) + geom_histogram(color = "grey", fill = "#CC6666") + xlab("Prediction score") + ylab("Frequency") + ggtitle("Positive data (1 ,default) in training set prediction")
distribution_his_plot <- negative_data + positive_data
ggsave(distribution_his_plot, file="distribution_his_plot.eps", device="eps", width = 10)


predictmodel0 <- ifelse(predict(lrmodel0, newdata = trainset, type = "response") > 0.5, 1, 0)
confusionMatrix(trainset$`default payment next month`, as.factor(predictmodel0))



get_logistic_pred <- function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5){
  probs = predict(mod, newdata = data, type = "response")
  ifelse (probs > cut, pos, neg)
}







# sensitivity - the percentage of people the model correctly predicted would default
sensitivity(testset$`default payment next month`, as.factor(predictmodel0))
# specificity - the percentage of people the model correctly predicted would not default
specificity(testset$`default payment next month`, as.factor(predictmodel0))
# total misclassification rate - the percentage of total incorrect classifications made by the model
(4340 + 396) / (4340 + 396 + 264 + 920)

## logistic lasso 
y.train <- trainset$`default payment next month`
x.train <- data.matrix(trainset[, c('LIMIT_BAL', 'SEX', 'EDUCATION', 'MARRIAGE', 'AGE', 'PAY_1', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6','BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6', 'age_group')])
cross_val <- cv.glmnet(as.matrix(x.train), as.matrix(y.train), 
                       family = 'binomial', 
                       type.measure = 'class',
                       standardize = TRUE,
                       alpha = 1, 
                       nlambda = 100)
fit_1se <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                  family = 'binomial', 
                  standardize = TRUE,
                  alpha = 1, 
                  lambda = cross_val$lambda.1se)
fit_1se$beta
coef(fit_1se)
coef(cross_val, s = cross_val$lambda.1se)
fit_min <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                  family = 'binomial', 
                  alpha = 1, 
                  lambda = cross_val$lambda.min)
fit_min$beta

y.test <- testset$`default payment next month`
x.test <- data.matrix(testset[, rownames(fit_min$beta)])
predictions_1se <- predict(fit_1se, newx = as.matrix(x.test),type = 'response')
predictions_min <- predict(fit_min, newx = as.matrix(x.test), type = 'response')

# 2 plots of lasso
par(mfrow = c(1,2))
plot(cross_val)
lasso = glmnet(as.matrix(x.train), as.matrix (y.train))
fit_1se$beta
fit_min$beta
plot_glmnet(lasso, xvar = 'lambda')
abline(v = log(cross_val$lambda.min), lty = 3)
abline(v = log(cross_val$lambda.1se), lty = 3)



## adaptive lasso
y.train <- trainset$`default payment next month`
x.train <- data.matrix(trainset[, c('LIMIT_BAL', 'SEX', 'EDUCATION', 'MARRIAGE', 'AGE', 'PAY_1', 'PAY_2', 
                                    'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6','BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 
                                    'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3',
                                    'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6', 'age_group')])
#fit MLE
mle_fit <- glm(y.train ~ x.train, family = binomial(link = 'logit'))
beta.init <- coef(mle_fit)
#calculate weight (gamma=0.5)
gamma <- 0.5
w <- 1/abs(beta.init)^gamma
w[-1]
lasso_cross_val <- cv.glmnet(as.matrix(x.train), as.matrix(y.train), 
                       family = 'binomial', 
                       standardize = FALSE,
                       type.measure = "class",
                       alpha = 1, 
                       penalty.factor = w[-1])

fit_1se_adaptive <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                           family = 'binomial', 
                           alpha = 1, 
                           lambda = lasso_cross_val$lambda.1se,
                           penalty.factor = w[-1])
print(fit_1se_adaptive)
# coefficients of final model
coef(lasso_cross_val, s = lasso_cross_val$lambda.1se)
coef(fit_1se_adaptive, s = lasso_cross_val$lambda.1se)

fit_min_adaptive <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                           family = 'binomial', 
                           alpha = 1, 
                           lambda = lasso_cross_val$lambda.min,
                           penalty.factor = w[-1])
coef(lasso_cross_val, s = lasso_cross_val$lambda.min)
fit_min_adaptive$beta
#2 plots for adaptive lasso
par(mfrow = c(1,2))
plot(lasso_cross_val)
plot_glmnet(lasso_cross_val$glmnet.fit, xvar = 'lambda')
abline(v = log(lasso_cross_val$lambda.min), lty = 3)
abline(v = log(lasso_cross_val$lambda.1se), lty = 3)

# gamma = 1
gamma1 <- 1
w1 <- 1/abs(beta.init)^gamma1
w1[-1]
lasso_cross_val1 <- cv.glmnet(as.matrix(x.train), as.matrix(y.train), 
                             family = 'binomial', 
                             standardize = FALSE,
                             type.measure = "class",
                             alpha = 1, 
                             penalty.factor = w1[-1])

fit_1se_adaptive1 <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                           family = 'binomial', 
                           alpha = 1, 
                           lambda = lasso_cross_val$lambda.1se,
                           penalty.factor = w1[-1])
coef(lasso_cross_val1, s = lasso_cross_val1$lambda.1se)

fit_min_adaptive1 <- glmnet(as.matrix(x.train), as.matrix(y.train), 
                           family = 'binomial', 
                           alpha = 1, 
                           lambda = lasso_cross_val$lambda.min,
                           penalty.factor = w1[-1])
coef(lasso_cross_val1, s = lasso_cross_val1$lambda.min)
#2 plots for adaptive lasso (gamma = 1)
par(mfrow = c(1,2))
plot(lasso_cross_val1)
plot_glmnet(lasso_cross_val1$glmnet.fit, xvar = 'lambda')
abline(v = log(lasso_cross_val1$lambda.min), lty = 3)
abline(v = log(lasso_cross_val1$lambda.1se), lty = 3)




## group lasso
y.train.gl <- ifelse(y.train == "0", -1, 1)
group.index <- c(1,2,3,4,5,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,5)
gglasso_cv <- cv.gglasso(as.matrix(x.train),y.train.gl , group = group.index, pred.loss = 'misclass',
                         loss = 'logit')
plot(gglasso_cv)
fit_gglasso <- gglasso()

  

grpreg_cv <- cv.grpreg(as.matrix(x.train), as.matrix(y.train), group = group.index, family = "binomial", 
                       penalty = "grLasso")
plot(grpreg_cv)
coef(grpreg_cv, s = grpreg_cv$lambda.min)

## adaptive group lasso
# x <- as.matrix(x.train)
# y <- as.matrix(y.train)
# p <- ncol(x)
# x.ols <- cbind(x,rep(1,nrow(x)))
# beta.ols <- solve(t(x)%*%x) %*% t(x) %*% y
# gamma <- 1
# x.adaptive <- t(apply(x, 1, function(t) t*beta.ols[1:p])^(-gamma))
# grpreg_cv_adap <- cv.grpreg(x.adaptive, y, group = group.index, family = "binomial", 
#                        penalty = "grLasso")
# plot(grpreg_cv_adap)
# coef(grpreg_cv_adap, s = grpreg_cv$lambda.min)


## Unbalanced distribution - trainset
# no default data - 18389
sum(trainset$`default payment next month` == "0")
# default data - 5292
sum(trainset$`default payment next month` == "1")
confusionMatrix(trainset$`default payment next month`, trainset$prediction)



