########################################
####        DATA & LIBRARIES        ####
library(pacman)
pacman::p_load(MASS, tidyverse, tidyr, dplyr, stats, ggplot2, gridExtra,
               ggbiplot, boot, ROCR, leaps, glmnet, bestglm, sjPlot)
employee_dataset <- read_csv("employee_dataset.csv")
data <- separate(employee_dataset, col=1, into=c("EmployeeID", "Gender", "Age", "Education", "EducationType", 
                                                 "MaritalStatus", "TotalCompanies", "TotalExperience", "DistanceToOffice", 
                                                 "Department", "Traveltype_last_year", "BillingRate", 
                                                 "MonthlyIncome", "Years_at_Company", "Years_InCurrentRole", "LastSalaryHike",
                                                 "PotentialReview", "PerformanceReview", "SatisfactionScore",
                                                 "JobRole_SatisfactionScore", "Overall_SatisfactionScore"), sep=";")
########################################

########################################
####   GENERAL DATA INFO / SUMMARY   ####
summary(data)
unique(data$EducationType) #4 Majors: Economics, Marketing/Finance, Bio-technology, "Psych/Behaviour Sciences"
unique(data$Education) #Under Graduation OR Graduation OR Masters/PHD
unique(data$Overall_SatisfactionScore) #Detractor(Negative) OR Promoter(Positive) OR Passive(Neither)
unique(data$Department) #ClientSolutions OR Business Development OR Support
unique(data$Traveltype_last_year)
unique(data$PerformanceReview)# Met Expectations OR Exceed Expectations OR Inconsistent
unique(data$PotentialReview) #Very High OR High OR Medium OR Low
unique(data$Traveltype_last_year)
#######################################

#######################################
#########  DATA FORMATTING   ########
data$Male <- ifelse(data$Gender=="Male", 1, 0) 
data$Bachelors <- ifelse(data$Education=="Graduation" | data$Education=="Masters / PHD", 1, 0) 
data$PostGrad <- ifelse(data$Education=="Masters / PHD", 1, 0)
data$Married <- ifelse(data$MaritalStatus=="Married", 1, 0)
data$ShortTravel <- ifelse(data$Traveltype_last_year=="No" | data$Traveltype_last_year=="LongTermProject", 0, 1)
data$LongTravel <- ifelse(data$Traveltype_last_year=="LongTermProject", 1, 0)
data$Overall_Promoter <- ifelse(data$Overall_SatisfactionScore=="Promoter", 1, 0)
data$JobRole_Promoter <- ifelse(data$JobRole_SatisfactionScore=="Promoter", 1, 0)
data$Employer_Promoter <- ifelse(data$SatisfactionScore=="Promoter", 1, 0)
data$Employee_VHighPotential  <- ifelse(data$PotentialReview=="Very High", 1, 0)
data$Employee_HighPotential  <- ifelse(data$PotentialReview=="High", 1, 0)
data$Employee_LowPotential  <- ifelse(data$PotentialReview=="Low", 1, 0)
data$Employee_ExcExpectations <- ifelse(data$PerformanceReview=="Exceed Expectations", 1, 0)
data$Employee_IncExpectations <- ifelse(data$PerformanceReview=="Inconsistent", 1, 0) #not included in dataset
data$Ed_Economics <- ifelse(data$EducationType=="Economics", 1, 0)
data$Ed_Mkting_Fin <- ifelse(data$EducationType=="Marketing / Finance", 1, 0)
data$Ed_BioTech <- ifelse(data$EducationType=="Bio-technology", 1, 0)
data$Dep_BusDev <- ifelse(data$Department=="BusinessDevelopment", 1, 0)
data$Dep_Solutions <- ifelse(data$Department=="Support", 1, 0)
#dummies only created for k-1 possibilities for each variable to avoid dummy variable trap
#######################################
######### NUMERIC DATA SUBSET ####
num_data <- data[c("Age", "TotalCompanies", "TotalExperience", "DistanceToOffice", "BillingRate",
                   "MonthlyIncome", "Years_at_Company", "Years_InCurrentRole", "LastSalaryHike",
                   "Male", "Bachelors", "PostGrad", "Married", "ShortTravel", "LongTravel",
                   "Overall_Promoter", "JobRole_Promoter", "Employer_Promoter",
                   "Employee_VHighPotential", "Employee_HighPotential", "Employee_LowPotential", 
                  "Ed_Economics", "Ed_Mkting_Fin","Ed_BioTech", "Dep_BusDev", "Dep_Solutions",
                  "Employee_ExcExpectations")]
num_data[] <- lapply(num_data, function(x) as.numeric(as.character(x)))
sapply(num_data, class)
summary(num_data)
#######################################

#######################################
#######   Exploration - EDA, PCA   #######
cor(num_data)[16,] #Correlations of Overall_Promoter with Dataset
pairs(num_data) #Numerical data pairplot, not intuitive

pca_data <- data[c("Age", "TotalCompanies", "TotalExperience", "DistanceToOffice", "BillingRate",
                   "MonthlyIncome", "Years_at_Company", "Years_InCurrentRole", "LastSalaryHike",
                   "Male", "PostGrad", "Married", "ShortTravel", "LongTravel",
                   "Overall_Promoter", "Employee_VHighPotential", 
                   "Employee_ExcExpectations", "Ed_Economics",
                  "Dep_BusDev", "Dep_Solutions")]
pca_data[] <- lapply(pca_data, function(x) as.numeric(as.character(x)))
sapply(pca_data, class)

pca4 <- prcomp(pca_data, rank=5, scale=TRUE)
biplot(pca4, col=c("grey", "black"), cex=c(0.2,0.9))
biplot1 <- ggbiplot(pca4, alpha=0.2, var.axes = TRUE, varname.size=4.7)
biplot1 <- biplot1 + xlim(-0.56, 2.25) + ylim(0, 3) + 
  xlab("PC1 - 15.8% Variance Explained") + ylab("PC2 - 9.5% Variance Explained")
print(biplot1)
biplot2 <- ggbiplot(pca4, alpha=0.2, var.axes = TRUE, varname.size=4.7)
biplot2 <- biplot2 + xlim(-0.56, 2.25) + ylim(-3, 0) + 
  xlab("PC1 - 15.8% Variance Explained") + ylab("PC2 - 9.5% Variance Explained")
print(biplot2)
grid.arrange(biplot1, biplot2, ncol=1, nrow=2, heights=c(2.5,2.5))

#PVE via PCA + Cumulative PVE Plot
pca_var <- pca4$sdev^2
pve <- pca_var / sum(pca_var)
cumpve <- cbind(PC=as.numeric(rownames(as.data.frame(cumsum(pve)))), as.data.frame(cumsum(pve)))
plot(cumpve, type='o', col=c("black"),
     xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1))
#######################################

#######################################
#######        Inference        #########
#LOGIT MODEL
logit <- glm(Employee_ExcExpectations ~ ., data=num_data, family=binomial)
summary(logit)
tab_model(logit)
#predict p(X)
logit_phat <- predict(logit, type="response")
#predict Y
logit_yhat <- ifelse(logit_phat>0.5, 1,0)
table(logit_yhat, num_data$Employee_ExcExpectations)
#misclassification rate
((table(logit_yhat, num_data$Employee_ExcExpectations)[2]+
    table(logit_yhat, num_data$Employee_ExcExpectations)[3])/
  sum(table(logit_yhat, num_data$Employee_ExcExpectations)))*100
#######################################

#######################################
####### Twist - Cross Validation #########
#Logit - crossvalidation
cv.error.10 <- rep(0,10)
for (i in 1:10){
  logit.fit <- glm(Employee_ExcExpectations ~ ., data=num_data, family=binomial)
  cv.error.10[i] <- cv.glm(num_data, logit.fit, K=10)$delta[1]
}
cv.error.10
mean(cv.error.10) #avg. misclassification rate, quite low (5.2%)

plot(1:10, cv.error.10, 
     type='l', col='maroon', ylim=c(0.04,0.06),
     xlab='k', ylab='Error') #model doesn't have high variance?
############################################

#######################################
#######  Twist - LASSO   #########
#subset selection skipped as aim is prediction not interpretation
# a lot of parameters insignificant, 
     #### i.e., close to 0, LASSO may help improve prediction / variable selection
# would also help avoid overfitting (not as huge of a concern already as n > p)

#lambda search grid + set up variable matrices
grid <- 10^seq(10,-2,length=100)
x <- model.matrix(Employee_ExcExpectations ~ ., data=num_data)[,-1]
y <- num_data$Employee_ExcExpectations
#train and test subset
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod, xvar="lambda") #coefficient values at different lambda values
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
#Fit model with best lambda
out <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:27,]
lasso.coef
lasso.coef[lasso.coef!=0] #nonzero variables

#LOGIT model with variable subsets
logit.lasso <- glm(Employee_ExcExpectations ~ Years_InCurrentRole + LastSalaryHike + 
                     LongTravel + Employee_VHighPotential, data=num_data, family=binomial)
summary(logit.lasso)
#predict p(X)
logit.lasso_phat <- predict(logit.lasso, type="response")
#predict Y
logit.lasso_yhat <- ifelse(logit.lasso_phat>0.5, 1,0)
table(logit.lasso_yhat, num_data$Employee_ExcExpectations) #95% classification instead of 96%
#misclassification rate
((table(logit.lasso_yhat, num_data$Employee_ExcExpectations)[2]+
    table(logit.lasso_yhat, num_data$Employee_ExcExpectations)[3])/
    sum(table(logit.lasso_yhat, num_data$Employee_ExcExpectations)))*100


cv.error.10.2 <- rep(0,10)
for (i in 1:10){
  logit.lasso.fit <- glm(Employee_ExcExpectations ~ Years_InCurrentRole + LastSalaryHike + 
                            LongTravel + Employee_VHighPotential, data=num_data, family=binomial)
  cv.error.10.2[i] <- cv.glm(num_data, logit.lasso.fit, K=10)$delta[1]
}
cv.error.10.2
mean(cv.error.10.2)
############################################

