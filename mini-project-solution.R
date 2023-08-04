getwd()
setwd("C:/Users/Raghav/Desktop/CS555 Machine Learning Project")

library(ggplot2)
library(pROC)
library(corrplot)


train_data = read.csv("train - Copy.csv")
View(train_data)

##################################################################

#Taking care of categorical variables

train_data$Gender = factor(train_data$Gender,
                           levels= c('Female','Male'),
                           labels = c(1,0))
train_data$MaritalStatus= factor(train_data$MaritalStatus,
                                 levels= c('Single','Married','Divorced'),
                                 labels = c(1,2,3))
train_data$EducationField = factor(train_data$EducationField,
                                   levels= c('Human Resources','Life Sciences','Marketing','Medical','Other','Technical Degree'),
                                   labels= c(1,2,3,4,5,6))
train_data$BusinessTravel = factor(train_data$BusinessTravel,
                                   levels=c('Non_Travel','Travel_Frequently','Travel_Rarely'),
                                   labels= c(1,2,3))
train_data$OverTime = factor (train_data$OverTime,
                              levels=c('Yes','No'),
                              labels=c(1,0))
train_data$Department= factor(train_data$Department,
                              levels= c('Research & Development','Sales','Human Resources'),
                              labels = c(1,2,3))
train_data$JobRole = factor(train_data$JobRole,
                            levels= c('Healthcare Representative','Human Resources','Laboratory Technician','Manager','Manufacturing Director','Research Director','Research Scientist','Sales Executive','Sales Representative'),labels = c(1,2,3,4,5,6,7,8,9))


View(train_data)

#Taking care of missing data in business travel
train_data$BusinessTravel[is.na(train_data$BusinessTravel)]=1

###################################################################
train_data_df = as.data.frame(sapply(train_data, as.numeric))


corr_mat <- round(cor(train_data_df),2) 
(corr_mat)

corrplot(corr_mat, method = "color", addCoef.col="black", type= "upper", number.cex=0.5)

###################################################################

# 1) Age
summary(train_data$Age)
Age_plot <- cut(train_data$Age, 8, include.lowest = TRUE)
ggplot(train_data, aes(Age_plot, ..count.., 
                       fill = factor(Attrition))) + geom_bar(position="dodge")

# 2) Job Role
table(train_data$JobRole, train_data$Attrition)
ggplot(train_data, aes(JobRole, ..count.., 
                       fill = factor(Attrition))) + geom_bar(position="dodge")

# 3) Total working years
summary(train_data$TotalWorkingYears)
TotalWorkingYears_plot <- cut(train_data$TotalWorkingYears,
                              10, include.lowest = TRUE)
ggplot(train_data, aes(TotalWorkingYears_plot, ..count.., 
                       fill = factor(Attrition))) + geom_bar(position="dodge")

# 4) Overtime
table(train_data$OverTime, train_data$Attrition)
ggplot(train_data, aes(OverTime, ..count.., 
                       fill = factor(Attrition))) + geom_bar(position="dodge")

###################################################################

X=train_data_df[,1]
Y=train_data_df[,-1]
set.seed(123)
train <- train_data_df[1:400,]
test <- train_data_df[401:501,]

#Fitting Logistic Regression to Training Set
model= glm(formula = Attrition ~.,
           family = binomial,
           data = train_data_df)


fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))


cm=table(unlist(test$Attrition),unlist(fitted.results))
print(cm)
Sen=cm[1]/(cm[1] + cm[2])
Sen
Spec=cm[4]/(cm[4]+cm[3])
Spec
Pre=cm[1]/(cm[1]+cm[3])
Pre
Mis=(cm[2]+cm[3])/(cm[1]+cm[2]+cm[3]+cm[4])
Mis
Acc=(cm[1]+cm[4])/(cm[1]+cm[2]+cm[3]+cm[4])
Acc
F1Score=Pre*Sen/Pre+Sen
2*F1Score

###################################################################

summary(model)
anova(model)

auc5 <- roc(as.numeric(test$Attrition), as.numeric(fitted.results))
auc5


