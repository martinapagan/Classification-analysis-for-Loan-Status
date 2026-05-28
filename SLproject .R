# clean memory
rm(list=ls())

# set WD
# setwd("G:\\Il mio Drive\\Progetto Statistica nuovo")
setwd("C:\\Users\\Isabella\\OneDrive - unibs.it\\Desktop\\Progetto con Marty")

# loading packages
library(readxl)
library(dplyr)
library(kableExtra)
library(leaps)
library(glmnet)
library(ISLR2)
library(ggplot2)
library(forcats)
library(ggrepel)
library(caret)
library(MASS)
library(ISLR2)
library(e1071)
library(reshape2)
library(ROCR)
library(biotools)
library(BeSS)
library(leaps)

# Loading the dataset
SLproject <- read_excel("Loan_dataset.xlsx")
SLproject <- data.frame(SLproject)
str(SLproject)

# na detection
sum(is.na(SLproject))                   #75
colSums(is.na(SLproject))

# Managing na of Self_Employed
#################################
sum(is.na(SLproject$Self_Employed))     #21

mean_yes <- round(mean(SLproject$ApplicantIncome[SLproject$Self_Employed == "Yes"], na.rm = TRUE), 2)
mean_no <- round(mean(SLproject$ApplicantIncome[SLproject$Self_Employed == "No"], na.rm = TRUE), 2)
mean_table <- data.frame(Self_Employed = c("Yes", "No"),
                         Average = c(mean_yes, mean_no))
kbl(mean_table, centering = TRUE, 
    col.names = c("Self employed", "Average")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                position = "left", full_width = FALSE) 

# We notice that self_employed individuals tends to have an higher ApplicantIncome 
# than no-self_employed individuals. (It can be seen also through a boxplot).
# Under this assumption, we replace the na values of Self_employed with:
# - Yes, if the corrisponding ApplicantIncome is greater than the mean ApplicantIncome
# - No, otherwise

# parameter 
applincome_mean = round(mean(SLproject$ApplicantIncome),2)

# NA replacement according to the parameter
SLproject$Self_Employed <- ifelse(is.na(SLproject$Self_Employed) & SLproject$ApplicantIncome >= applincome_mean, "Yes", SLproject$Self_Employed)
SLproject$Self_Employed <- ifelse(is.na(SLproject$Self_Employed) & SLproject$ApplicantIncome < applincome_mean, "No", SLproject$Self_Employed)
sum(is.na(SLproject$Self_Employed))     #0

# new na detection 
colSums(is.na(SLproject))
sum(is.na(SLproject))       #54, reduction in na number

# Once the number of na has been reduced, we can remove na from the dataset,
# clean it and recode the variables in the dataset. 

# remove na 
SLproject = na.omit(SLproject)

# create factors
SLproject$Gender <- factor(SLproject$Gender,labels=c("Female", "Male"))
SLproject$Married <- factor(SLproject$Married,labels=c("No", "Yes"))
SLproject$Education <- factor(SLproject$Education,labels=c("Graduate", "Not Graduate"))
SLproject$Self_Employed <- factor(SLproject$Self_Employed,labels=c("No", "Yes"))
SLproject$Property_Area <- factor(SLproject$Property_Area,labels=c("Rural", "Semiurban","Urban"))
SLproject$Loan_Status <- factor(SLproject$Loan_Status,labels=c("N", "Y"))
SLproject$Dependents <- factor(SLproject$Dependents, labels=c("0", "1", "2", "3+"))
SLproject$Credit_History <- factor(SLproject$Credit_History, labels=c("0", "1")) # 1=guidelines met
str(SLproject)

# recode factors
SLproject$Loan_Status <- recode_factor(SLproject$Loan_Status, `Y` = "Yes", `N` = "No")
SLproject$Education <- recode_factor(SLproject$Education, `Graduate` = "Graduate", `Not Graduate` = "Not_graduate")
str(SLproject)

# make the variable more clear
# - VARIABLE: Education
names(SLproject)
SLproject = rename(SLproject, Graduate = Education)
SLproject$Graduate = recode_factor(SLproject$Graduate,  "Not_graduate" = "No", "Graduate" = "Yes")

# - VARIABLE: Credit History
names(SLproject)
SLproject = rename(SLproject, Guid_met = Credit_History)
SLproject$Guid_met = recode_factor(SLproject$Guid_met, "0" = "No", "1" = "Yes")

# - VARIABLE: Gender
names(SLproject)
SLproject = rename(SLproject, Male = Gender)
SLproject$Male = recode_factor(SLproject$Male,"Female" = "No", "Male" = "Yes")

# remove the ID 
SLproject$Loan_ID <- NULL

# change variable names
names(SLproject) <- c("Male", "Married", "Dependents", "Graduate", "Self_empl", "Appl_income", "Coappl_income", 
                      "Loan_amount", "Loan_term", "Guid_met", "Prop_area", "Loan_status")


#
##
###
####
##### DESCRIPTIVE STATISTICS
####
###
##
#

attach (SLproject)

# Pie chart Loan Status 
names(SLproject)
table(Loan_status)/length(Loan_status)

df_loan <- data.frame(Loan_status=c("Yes", "No"))

Number_Loan_status <- c(sum(Loan_status == "Yes"), 
                        sum(Loan_status == "No"))
perc = round(Number_Loan_status/sum(Number_Loan_status)*100,2)
dd = data.frame(df_loan, perc) %>% arrange(perc)
dd

df = data.frame(status = dd$Loan_status,
                value = dd$perc)
df2 <- df %>% 
  mutate(cs = rev(cumsum(rev(value))), 
         pos = value/2 + lead(cs, 1),
         pos = if_else(is.na(pos), value/2, pos))

ggplot(df, aes(x = "" , y = value, fill = fct_inorder(status))) +
  geom_col(width=1, color="black") +
  coord_polar(theta = "y", start = 0 ) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(aes(y = pos, label = paste0(value, "%")),
                   data = df2, size=4, show.legend = FALSE, nudge_x = 1) +
  guides(fill = guide_legend(title = "Loan Status")) +
  labs(title = "Loan Status")+
  theme_void()
# It is more probable to obtain a Loan rather than not receiving it. 
# => evaluating balancing?

# ApplicantIncome by Loan_Status
ggplot(SLproject, aes(x=Loan_status, y = Appl_income, fill = Loan_status)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(legend.position="none") +
  scale_y_continuous(n.breaks = 10, name = "Applicant Income") + 
  xlab("Loan Status")    # no great difference

loan_yes = subset(SLproject, Loan_status == "Yes") 
median(loan_yes$Appl_income)

loan_no = subset(SLproject, Loan_status == "No")
median(loan_no$Appl_income)

# Loan_amount by Loan_Status
ggplot(SLproject, aes(x=Loan_status, y = Loan_amount, fill = Loan_status)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 200), n.breaks = 10, name = "Loan amount") + 
  xlab("Loan Status")    # no great difference

loan_yes = subset(SLproject, Loan_status == "Yes") 
median(loan_yes$Loan_amount)

loan_no = subset(SLproject, Loan_status == "No")
median(loan_no$Loan_amount)

# Loan_term by Loan_Status
ggplot(SLproject, aes(x=Loan_status, y = Loan_term, fill = Loan_status)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(legend.position="none") +
  scale_y_continuous(n.breaks = 10, name = "Loan term") + 
  xlab("Loan Status")    # no great difference

# The loan term is mainly 360. Values such as 180, 480, 300 represent outliers. 
loan_yes = subset(SLproject, Loan_status == "Yes") 
median(loan_yes$Loan_term)

loan_no = subset(SLproject, Loan_status == "No")
median(loan_no$Loan_term)

# Loan amount - Applicant income
ggplot(data=SLproject, aes(x = Appl_income, y = Loan_amount, color = Loan_status)) +
  geom_point() +
  labs(x = "Applicant Income", y = "Loan Amount", color = "Loan Status") +
  ggtitle("Relationship between Loan Amount, Applicant Income and Loan Status") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 2000)) +
  facet_grid(Loan_status)

# This graph confirms that the Applicant income and Loan Amount haven't a great 
# relationships. The same is observable by considering the Loan Status. 
# => no great association between Loan Amount, Applicant Income and Loan status. 
# We suppose that to obtain the Loan, it is not important the Applicant income itself, 
# but how the Applicant income is used. So, if the Applicant has dependents and so on. 
# Those are elements that affect by the expenses the Applicant have to bear monthly.  

# Dependents - Applicant income
ggplot(data=SLproject, aes(x = Loan_status, fill = Dependents)) +
  geom_bar(position = "dodge", color="black") +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Loan Status", y = "Count", fill = "Dependants") +
  ggtitle("Distribution of Loan Status by Dependants") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  theme_minimal()
# We notice that the probability of obtaining a loan is higher independently 
# from the dependents. Those who have obtained the loan are mainly families 
# without dependents. We obviously have to consider that there are many people
# with 0 dependents and that people who received the loan are more than those who
# have not received it. 

# Dependents - Appl.income
ggplot(data=SLproject, aes(x = Appl_income, y = Loan_amount, color = Dependents)) +
  geom_point() +
  labs(x = "Applicant Income", y = "Loan Amount", color = "Dependants") +
  ggtitle("Relationship between Loan Amount, Applicant Income and Loan Status") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 30)) +
  scale_x_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 2500)) +
  facet_grid(Loan_status ~ Dependents)
  # no great difference

# Eealtionship between Loan status and other predictors
# Male
ggplot(data=SLproject, aes(x = Loan_status, fill = Male)) +
  geom_bar(position = "dodge", color="black") + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Loan Status", y = "Count", fill = "Male") +
  ggtitle("Distribution of Loan Status by Gender") +
  scale_y_continuous(limits = c(0, 210), breaks = seq(0, 210, by = 25)) +
  theme_minimal()
# People who have received the loan are more Male than Female, but it is also true 
# that the population is made more by Male people rather than Female. 

# Married
ggplot(data=SLproject, aes(x = Loan_status, fill = Married)) +
  geom_bar(position = "dodge", color="black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Loan Status", y = "Count", fill = "Married") +
  ggtitle("Distribution of Loan Status by Married status") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 15)) +
  theme_minimal()

# Pie chart Married  
names(SLproject)
table(Married)/length(Married)

df_loan <- data.frame(Married=c("Yes", "No"))

Number_Married <- c(sum(Married == "Yes"), 
                    sum(Married == "No"))
perc = round(Number_Married/sum(Number_Married)*100,2)
dd = data.frame(df_loan, perc) %>% arrange(perc)
dd

df = data.frame(Married = dd$Married,
                value = dd$perc)
df2 <- df %>% 
  mutate(cs = rev(cumsum(rev(value))), 
         pos = value/2 + lead(cs, 1),
         pos = if_else(is.na(pos), value/2, pos))

ggplot(df, aes(x = "" , y = value, fill = fct_inorder(Married))) +
  geom_col(width=1, color="black") +
  coord_polar(theta = "y", start = 0 ) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(aes(y = pos, label = paste0(value, "%")),
                   data = df2, size=4, show.legend = FALSE, nudge_x = 1) +
  guides(fill = guide_legend(title = "Married")) +
  labs(title = "Married")+
  theme_void()

table(Married)
# People who received the Loan are mainly Married, but the population is mainly 
# characterized by Married people. 

# Self Employed
ggplot(data=SLproject, aes(x = Loan_status, fill = Self_empl)) +
  geom_bar(position = "dodge", color="black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Loan Status", y = "Count", fill = "Self Employed") +
  ggtitle("Distribution of Loan Status by Employed status") +
  scale_y_continuous(limits = c(0, 210), breaks = seq(0, 210, by = 30)) +
  theme_minimal()
# Those whose received the loan are mainly Self employed. 
# However the population is mainly composed by Self employed individuals. 

# Graduate
ggplot(data=SLproject, aes(x = Loan_status, fill = Graduate)) +
  geom_bar(position = "dodge", color="black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Loan Status", y = "Count", fill = "Graduate") +
  ggtitle("Distribution of Loan Status by Graduate status") +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 30)) +
  theme_minimal()
# Those whose received the loan are mainly Graduated. 
# However the population is mainly composed by Graduated individuals.

# Guideline 
ggplot(data=SLproject, aes(x = Loan_status, fill = Guid_met)) +
  geom_bar(position = "dodge", color="black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Loan Status", y = "Count", fill = "Guidelines met") +
  ggtitle("Distribution of Loan Status by Guidelines met") +
  scale_y_continuous(limits = c(0, 240), breaks = seq(0, 240, by = 40)) +
  theme_minimal() 
# Those whose received the loan are mainly those who have met guidelines in the past. 
# However the population is mainly composed by individuals that have met  
# guidelines in the past.

loan_yes = subset(SLproject, Loan_status == "Yes") 
table(loan_yes$Guid_met)

loan_no = subset(SLproject, Loan_status == "No")
table(loan_no$Guid_met)

# Property area
ggplot(data=SLproject, aes(x = Loan_status, fill = Prop_area)) +
  geom_bar(position = "dodge", color="black") +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Loan Status", y = "Count", fill = "Property Area") +
  ggtitle("Approved and rejected loans by Property Area") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 15)) +
  theme_minimal()

table(loan_yes$Prop_area)

table(loan_no$Prop_area)

# Pie chart prop_area  
names(SLproject)
table(Prop_area)/length(Prop_area)

df_loan <- data.frame(Prop_area=c("Rural", "Semiurban", "Urban"))

Number_Prop_area <- c(sum(Prop_area == "Rural"), 
                      sum(Prop_area == "Semiurban"),
                      sum(Prop_area == "Urban"))
perc = round(Number_Prop_area/sum(Number_Prop_area)*100,2)
dd = data.frame(df_loan, perc) %>% arrange(perc)
dd

df = data.frame(Prop_area = dd$Prop_area,
                value = dd$perc)
df2 <- df %>% 
  mutate(cs = rev(cumsum(rev(value))), 
         pos = value/2 + lead(cs, 1),
         pos = if_else(is.na(pos), value/2, pos))

ggplot(df, aes(x = "" , y = value, fill = fct_inorder(Prop_area))) +
  geom_col(width=1, color="black") +
  coord_polar(theta = "y", start = 0 ) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(aes(y = pos, label = paste0(value, "%")),
                   data = df2, size=4, show.legend = FALSE, nudge_x = 1) +
  guides(fill = guide_legend(title = "Property area")) +
  labs(title = "Property area")+
  theme_void()

# Those who have obtained the loan mainly leave in Semiurban area, however 
# the population is mainly composed by individuals living in Semiurban areas.


###############
# ANALYSIS
###############

# REGRESSIONS with all variables
# - Logistic regression 
# - Linear Discriminant Analysis 
# - Quadratic Discriminant Analysis
# - Naive Bayes


#
##
###
####
##### LOGISTIC REGRESSION
####
###
##
#

#recode
SLproject$loan_status.rec <- ifelse(SLproject$Loan_status=="Yes",1,0)

glm.fit <- glm(loan_status.rec ~ .- Loan_status, 
               data = SLproject, family = binomial)
summary(glm.fit)


# classification
glm.probs <- predict(glm.fit, type="response")
glm.pred <- rep("No", length(Loan_status))

# vector of legth(dataset) values 
head(SLproject)   # the observations
head(glm.probs)   # to see the predicted probabilities related to observations conserved in Default dataframe. 

glm.pred[glm.probs > 0.5] = "Yes"   # rule of classification method
head(glm.pred)

M1 <- glm(loan_status.rec ~ 1, data = SLproject, family = binomial) #~1 is a null model???
M2 <- glm(loan_status.rec ~ . -Loan_status, data = SLproject, family = binomial)
M3 <- glm(loan_status.rec ~ Guid_met + Prop_area, data = SLproject, family = binomial)
anova(M1,M2,M3, test="Chisq") 
# The very small p-value indicates that the M2 model explains the variability 
# of the Loan status better than the M1 model. In contrast, the following 
# p-value shows that the M2 still better explains the variability of loan status 
# than the M3 model. The M3 model is created by only considering the significant 
# variables found performing the logistic regression model. 
# As consequence, the best model includes all the variables of the dataset 
# even if they are not significant. It help improving the estimate of the 
# binary response variable. 


#confusion matrix
#training and test sets
n <- nrow(SLproject) #numer of row of the data frame
set.seed(1234) #function that fix a seed for random procedure, we all get the same result and with this i can compare with others
#1234 is casual, i can choose what i want
#sample from integral 1 to n (total of bunch for the sample), a sample with a size 0.75
train.ind <- sample(1:n, size = 0.75*n) #75% is the training and the 25% is the test
train <- SLproject[train.ind,]
str(train)
test <- SLproject[-train.ind,] #the remaining part that is used to test the model, infatti c'è il meno davanti a train
str(test)

#multiple logistic regression model on training data
#before we use the entire dataframe, now only train
glm.fit <- glm(loan_status.rec ~ . - Loan_status, data = train, family = binomial)
summary(glm.fit)

#predictions on test data
#im predictin pgrego hat on test
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- rep("No", nrow(test))
glm.pred[glm.probs > 0.5] = "Yes" #glm.pred are the predicted classes
glm.pred

#confusion matrix, obtain to evaluate our logistic regression model on our data
table(test$Loan_status,glm.pred)
addmargins(table(test$Loan_status,glm.pred))

#Ensure the levels of test$Loan_status and glm.pred are the same and in the same order
levels_ref <- levels(as.factor(test$Loan_status))
glm.pred <- factor(glm.pred, levels = levels_ref)

#install.packages("caret")
confusionMatrix(data=as.factor(glm.pred),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT: 
# P-Value [Acc > NIR] : 0.2610    
# Mcnemar's Test P-Value : 0.2113
# The P-values are greater than 0.05 -> problems

#ROC curve
#install.packages("ROCR")
pred <- prediction(glm.probs,as.factor(test$Loan_status)) #we are considering all the possible threshold (glm.probs), than we considerate the predicted y
perf <- performance(pred,"tpr","fpr")
plot(perf, main="ROC curve", colorize=TRUE)

#AUC
AUC <- performance(pred,"auc")@y.values[[1]] #0.6365915
AUC

#changing the classification threshold 
#predictions on test data
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- rep("No", nrow(test))
glm.pred[glm.probs > 0.6] = "Yes" 
glm.pred #specificity increase

#confusion matrix
table(test$Loan_status,glm.pred)
addmargins(table(test$Loan_status,glm.pred))

#Ensure the levels of test$Loan_status and glm.pred are the same and in the same order
levels_ref <- levels(as.factor(test$Loan_status))
glm.pred <- factor(glm.pred, levels = levels_ref)

#install.packages("caret")
confusionMatrix(data=as.factor(glm.pred),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT
# As specificity does not increase more than sensitivity and 
# accuracy decrease, we decide to keep the threshold at 0.5.

#ROC curve
#install.packages("ROCR")
pred.glm <- prediction(glm.probs,as.factor(test$Loan_status)) #we are considering all the possible threshold (glm.probs), than we considerate the predicted y
perf.glm <- performance(pred.glm,"tpr","fpr")
plot(perf, main="ROC curve", colorize=TRUE)

#AUC
AUC.glm <- performance(pred.glm,"auc")@y.values[[1]] #0.6365915
AUC.glm 

#
##
###
####
##### LINEAR DISCRIMINANT ANALYSIS - LDA
####
###
##
#


###
options(scipen=999)
lda.fit <- lda(Loan_status ~ . - loan_status.rec, data = SLproject)
lda.fit
# COMMENT:
# Prior Yes: the proportion of individual with Y=Yes is about 70%.
# It means that the loan status is not balanced. 

# predictions
lda.pred <- predict(lda.fit)
names(lda.pred)

# predictions - posterior probabilities
lda.post <- lda.pred$posterior

# Probabilities used to predict the class:
# If it is close to 0 -> class Y=No
# If it is close to 1 -> class Y=Yes

# predictions - predicted classes
pred.class.05 <- ifelse(lda.post[,2]>0.5,"Yes", "No")
# Looking at the largest pi and it classify the observations by comparing it,
# using a classification threshold of 0.5. "Assign the observation for the 
# class for which the posterior probability is larger". 

# predictions - scores of observations on linear discriminants 
LD <- lda.pred$x

# plot LD
par(mar = c(4, 4, 2, 2))
ldahist(LD,g=SLproject$Loan_status)
# COMMENT:
# For classification aim, it is better that the values of x tends not to overlap
# for the two groups. Separation between the values of x helps improving
# the prediction of outcome variable. This is not the case.
# As we can see, the plot for the group Yes has values mainly localized 
# in left part of the graph, while the one for group No has values mainly 
# locates both in the right and in the left part of the graph. 

#Confusion matrix 
table(SLproject$Loan_status,pred.class.05)
addmargins(table(SLproject$Loan_status,pred.class.05))

#Ensure the levels of SLproject$Loan_status and pred.class.05 are the same and in the same order
levels_ref <- levels(as.factor(SLproject$Loan_status))
pred.class.05 <- factor(pred.class.05, levels = levels_ref)

#install.packages("caret")
confusionMatrix(data=as.factor(pred.class.05),reference=as.factor(SLproject$Loan_status),positive="Yes")
# COMMENT:
# There is no a clear distinction between group Yes and group No. The two groups
# should be associated with different values of x, but it is not the case. Here, 
# the values of x for the two groups overlap. There is no separation between 
# values of x. The values of x are mainly localized in the left part of the plot 
# for the Yes group while in the left and the right side of the plot for the 
# No group. We expect that the model is not able to predict positive value, 
# but it may be good in predicting negative values. The confusion matrix
# confirms our ideas, indicating a very low Sensitivity (0.012766) and 
# a quite large Specificity (0.537634). Because of this difficulty in predicting 
# values, even the accuracy is low (0.1616).

# Furthermore, the P-Value [Acc > NIR]=1. It is lower than the 5% significance 
# level, indicating that model's predictions are not significantly better than
# chance. 

# To improve the model we can divide the dataset into training and a test set. 
# training and test sets

#LDA on training data
lda.fit <- lda(Loan_status ~ . -loan_status.rec, data = train)
lda.fit

#predictions on test data
lda.pred.test <- predict(lda.fit, newdata = test)
lda.class.test <- lda.pred.test$class

#Confusion matrix (on the test set)
table(test$Loan_status,lda.class.test)
addmargins(table(test$Loan_status,lda.class.test))

#confusion matrix
confusionMatrix(data=as.factor(lda.class.test),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT: 
# using the training and the test test, the model's confusion matrix 
# improves, but still indicates that the model does not work better
# than chance (P-Value [Acc>NIR] = 0.1807). The accuracy reaches the 
# 80%, the sensitivity gets better, while the specificity get worsen. 


#ROC curve 
#install.packages("ROCR")
lda.pred.test <- predict(lda.fit, newdata = test)
lda.post <- lda.pred.test$posterior
pred.lda <- prediction(lda.post[,2],as.factor(test$Loan_status))
perf.lda <- performance(pred.lda,"tpr","fpr")
plot(perf.lda, main="ROC curve", colorize=TRUE)

#AUC of LDA
AUC.lda <- performance(pred.lda,"auc")@y.values[[1]] #0.3575606
AUC.lda

# Comparing Logistic Regression with LDA
plot(perf.lda, main="ROC curve", col = "red")
plot(perf.glm, add =TRUE, col = "blue")

AUC1 <- paste("LDA:",round(AUC.lda,4), sep=" ")
AUC2 <- paste("Logistic:",round(AUC.glm,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2), lty=1, col = c("red","blue"), title="AUC",cex=0.6)

# COMMENT:
# Glm has an higher AUC with respect to the LDA, but both models does not 
# perform better than chance since the p-value [ACC > NIR] > 0.05.


#
##
###
####
##### QUADRATIC DISCRIMINANT ANALYSIS - QDA
####
###
##
#


#QDA on training data
# qda.fit <- qda(Loan_status ~. -loan_status.rec, data = train) # we remove Guid_met because is less heterogeneous
# qda.fit
# Errore in qda.default(x, grouping, ...) : carenza di rango nel gruppo Yes
# the error "carenza di rango nel gruppo Yes" indicates that one or more variables
# may be constant in group "YES". That's why we search for variables with 
# more Yes than No values.
table(SLproject$Guid_met)    # No: 46, Yes: 282 
# As a result, we remove the Guid_met variable from the model

# QDA on training data
qda.fit <- qda(Loan_status ~. -loan_status.rec -Guid_met, data = train) # we remove Guid_met because is less heterogeneous
qda.fit
# The error has been correctly removed

#predictions on test data
qda.pred.test <- predict(qda.fit, newdata = test)
qda.class.test <- qda.pred.test$class

#Confusion matrix (on the test set)
table(test$Loan_status,qda.class.test)
addmargins(table(test$Loan_status,qda.class.test))

#confusion matrix
#library(caret)
confusionMatrix(data=as.factor(qda.class.test),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT:
# The QDA model accuracy is worse than that of LDA model and still 
# not perform better than chance. In addition, the model is quite able to 
# predict positive values but unable at all to predict negative values. 
# It may be due to the unbalancing. 
# We will try to improve the model with a lower threshold.

#ROC curve
#install.packages("ROCR")
qda.pred.test <- predict(qda.fit, newdata = test)
qda.post <- qda.pred.test$posterior
pred.qda <- prediction(qda.post[,2],as.factor(test$Loan_status))
perf.qda <- performance(pred.qda,"tpr","fpr")
plot(perf.qda, main="ROC curve", colorize=TRUE)

#AUC
AUC.qda <- performance(pred.qda,"auc")@y.values[[1]] #0.6466165
AUC.qda

#changing the classification threshold
#predictions on test data
qda.post <- qda.pred.test$posterior
pred.class.01 <- ifelse(qda.post[,2]>0.1,"Yes", "No")

#Confusion matrix (on the test set)
table(test$Loan_status,pred.class.01)
addmargins(table(test$Loan_status,pred.class.01))

#Ensure the levels of SLproject$Loan_status and pred.class.01 are the same and in the same order
levels_ref <- levels(as.factor(SLproject$Loan_status))
pred.class.01 <- factor(pred.class.01, levels = levels_ref)

#confusion matrix
confusionMatrix(data=as.factor(pred.class.01),reference=as.factor(test$Loan_status),positive="Yes")
# With a lower threshold we improve Specificity to 0.5789, with the consequence 
# of a reduction in Sensitivity (0.6190). The P-Value [Acc > NIR] and accuracy
# are unchanged, but Mcnemar's Test P-Value became 0.00801, which is < 0.05.

#ROC curve
qda.pred.test <- predict(qda.fit, newdata = test)
qda.post <- qda.pred.test$posterior
pred.qda <- prediction(qda.post[,2],as.factor(test$Loan_status))
perf.qda <- performance(pred.qda,"tpr","fpr")
plot(perf.qda, main="ROC curve", colorize=TRUE)

#AUC
AUC.qda <- performance(pred.qda,"auc")@y.values[[1]] #0.6466165
AUC.qda

#Comparing Logistic Regression with LDA and QDA
plot(perf.glm, main="ROC curve", col = "blue")
plot(perf.lda, add =TRUE, col = "red")
plot(perf.qda, add =TRUE, col = "green")

AUC1 <- paste("Logistic:",round(AUC.glm,4), sep=" ")
AUC2 <- paste("LDA:",round(AUC.lda,4), sep=" ")
AUC3 <- paste("QDA:",round(AUC.qda,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2, AUC3), lty=1, col = c("blue","red","green"), title="AUC",cex=0.6)
# According to the AUC, QDA seems to be the best one, while from the confusion 
# matrix emerges that QDA model has the highest error rate (20%). 
# However, none of the models performs better than change. 


##############
# NAIVE BAYES 
##############

#Naive Bayes on training data
nb.fit <- naiveBayes(Loan_status ~ . -loan_status.rec, data = train)
nb.fit

#a priori probabilities
table(Loan_status)/nrow(train)

#Conditional probabilities for variable Male given Loan_status (conditioned on Loan_status):
table(Loan_status,Male)
prop.table(table(Loan_status,Male),margin=1)

#Conditional probabilities for variable Married given Loan_status (conditioned on Loan_status):
table(Loan_status,Married)
prop.table(table(Loan_status,Married),margin=1)

#Conditional probabilities for variable Dependents given Loan_status (conditioned on Loan_status):
table(Loan_status,Dependents)
prop.table(table(Loan_status,Dependents),margin=1)

#Conditional probabilities for variable Graduate given Loan_status (conditioned on Loan_status):
table(Loan_status,Graduate)
prop.table(table(Loan_status,Graduate),margin=1)

#Conditional probabilities for variable Self_empl given Loan_status (conditioned on Loan_status):
table(Loan_status,Self_empl)
prop.table(table(Loan_status,Self_empl),margin=1)

#Conditional probabilities for variable Guid_met given Loan_status (conditioned on Loan_status):
table(Loan_status,Guid_met)
prop.table(table(Loan_status,Guid_met),margin=1)

#Conditional probabilities for variable Prop_area given Loan_status (conditioned on Loan_status):
table(Loan_status,Prop_area)
prop.table(table(Loan_status,Prop_area),margin=1)

# predictions on test data
# predict posterior probabilities
nb.post <- predict(nb.fit, newdata=test, type="raw")

# predict classes
nb.class.test <- predict(nb.fit, newdata=test, type="class")
# It gives a vector of Yes or No

#Confusion matrix (on the test set)
table(test$Loan_status,nb.class.test)
addmargins(table(test$Loan_status,nb.class.test))

#confusion matrix
confusionMatrix(data=as.factor(nb.class.test),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT:
# The Naive Bayes model has a quite high accuracy, that indicates a 22% of 
# error rate. Its sensitivity metrics doubles the specificity one. 
# However, the model does not predict values better than chance.
# We didn't change the threshold because by trying to improvement 
# would not be sufficient to compensate the reduction in sensitivity. 

#ROC curve
nb.post <- predict(nb.fit, newdata=test, type="raw")
pred.nb <- prediction(nb.post[,2],as.factor(test$Loan_status))
perf.nb <- performance(pred.nb,"tpr","fpr")
plot(perf.nb, main="ROC curve", colorize=TRUE)

#AUC
AUC.nb <- performance(pred.nb,"auc")@y.values[[1]]  # 0.3634085
AUC.nb

# Comparing Logistic Regression with LDA, QDA and NB
plot(perf.glm, main="ROC curve", col = "blue")
plot(perf.lda, add =TRUE, col = "red")
plot(perf.qda, add =TRUE, col = "green")
plot(perf.nb, add =TRUE, col = "purple")

AUC1 <- paste("Logistic:",round(AUC.glm,4), sep=" ")
AUC2 <- paste("LDA:",round(AUC.lda,4), sep=" ")
AUC3 <- paste("QDA:",round(AUC.qda,4), sep=" ")
AUC4 <- paste("NB:",round(AUC.nb,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2, AUC3, AUC4), lty=1, col = c("blue","red","green", "purple"), title="AUC",cex=0.6)
# COMMENT: 
# According to the AUC, the better models still are QDA and Logistic. 
# As before, none of the models are not significantly better than chance. 


###################################
# POSSIBLE PROBLEMS OF OUR DATASET
###################################
# PROBLEM 1 - BALANCE of Y
# Models work better when classes of variable are balanced. 
# check balance in outcome variable
table(SLproject$Loan_status)    # No: 93, Yes: 235
# If the classes in the dataset are unbalanced, with one class having 
# significantly more samples than the other, models may struggle to accurately 
# classify the data. The model may be biased towards the majority class and 
# may struggle to accurately detect and classify the minority class. 

# To address imbalanced classes in this scenario, techniques such as 
# oversampling the minority class, undersampling the majority class, 
# or using different evaluation metrics (such as precision, recall, 
# and F1 score) can be applied to improve the performance of the LDA model.

#
##
###
####
#####
###### BALANCING METHODS
#####
####
###
##
#

n <- nrow(SLproject) #numer of row of the data frame
set.seed(1234) #function that fix a seed for random procedure, 
# we all get the same result and with this i can compare with others

# actual situation
table(SLproject$Loan_status)

loan_yes <- subset(SLproject, Loan_status == "Yes")
loan_no <- subset(SLproject, Loan_status == "No")

n_yes <- nrow(loan_yes) #235
n_no <- nrow(loan_no)   #93

# We will keep all the No and then we will add the same number of yes (93)
# Randomly select rows, from the Yes, to balance the dataset.
# If there are more "Yes" records, it randomly samples from the "Yes" records 
# to match the number of "No" records.
if (n_no > n_yes) {
  loan_no <- loan_no[sample(nrow(loan_no), n_yes, replace = FALSE), ]
} else {
  loan_yes <- loan_yes[sample(nrow(loan_yes), n_no, replace = FALSE), ]
}

# Merge the two balanced data frames
SLproject_balanced <- bind_rows(loan_no, loan_yes)

# new dataset
str(SLproject_balanced)

# final situation - dataset balanced (Loan_status)
table(SLproject_balanced$Loan_status) 


# Pie chart Loan Status - balanced 
names(SLproject_balanced)
table(SLproject_balanced$Loan_status)/length(SLproject_balanced$Loan_status)

df_loan <- data.frame(Loan_status=c("Yes", "No"))

Number_Loan_status <- c(sum(SLproject_balanced$Loan_status == "Yes"), 
                        sum(SLproject_balanced$Loan_status == "No"))
perc = round(Number_Loan_status/sum(Number_Loan_status)*100,2)
dd = data.frame(df_loan, perc) %>% arrange(perc)
dd

df = data.frame(status = dd$Loan_status,
                value = dd$perc)
df2 <- df %>% 
  mutate(cs = rev(cumsum(rev(value))), 
         pos = value/2 + lead(cs, 1),
         pos = if_else(is.na(pos), value/2, pos))

ggplot(df, aes(x = "" , y = value, fill = fct_inorder(status))) +
  geom_col(width=1, color="black") +
  coord_polar(theta = "y", start = 0 ) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(aes(y = pos, label = paste0(value, "%")),
                   data = df2, size=4, show.legend = FALSE, nudge_x = 1) +
  guides(fill = guide_legend(title = "Loan Status")) +
  labs(title = "Loan Status")+
  theme_void()

# ApplicantIncome by Loan_Status - balanced
ggplot(SLproject_balanced, aes(x=Loan_status, y = Appl_income, fill = Loan_status)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(legend.position="none") +
  scale_y_continuous(n.breaks = 10, name = "Applicant Income") + 
  xlab("Loan Status")    # no great difference

loan_yes = subset(SLproject_balanced, Loan_status == "Yes") 
median(loan_yes$Appl_income)

loan_no = subset(SLproject_balanced, Loan_status == "No")
median(loan_no$Appl_income)

# Loan_amount by Loan_Status - balanced
ggplot(SLproject_balanced, aes(x=Loan_status, y = Loan_amount, fill = Loan_status)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 200), n.breaks = 10, name = "Loan amount") + 
  xlab("Loan Status")    # no great difference

loan_yes = subset(SLproject, Loan_status == "Yes") 
median(loan_yes$Loan_amount)

loan_no = subset(SLproject, Loan_status == "No")
median(loan_no$Loan_amount)


##############################
# ANALYSIS WITH BALANCED DATA
##############################

#
##
###
####
##### LOGISTIC REGRESSION
####
###
##
#

#recode
SLproject_balanced$loan_status.rec <- ifelse(SLproject_balanced$Loan_status=="Yes",1,0)

glm.fit <- glm(loan_status.rec ~ . - Loan_status, 
               data = SLproject_balanced, family = binomial)
summary(glm.fit)

# classification
glm.probs <- predict(glm.fit, type="response")
glm.pred <- rep("No", length(Loan_status))

# vector of legth(dataset) values 
head(SLproject_balanced)   
head(glm.probs) 

attach(SLproject_balanced)
glm.pred[glm.probs > 0.5] = "Yes"   # rule of classification method
head(glm.pred)

M1 <- glm(loan_status.rec ~ 1, data = SLproject_balanced, family = binomial) 
M2 <- glm(loan_status.rec ~ . - Loan_status, 
          data = SLproject_balanced, family = binomial)
M3 <- glm(loan_status.rec ~ Prop_area + Guid_met + Self_empl, 
          data = SLproject_balanced, family = binomial)

anova(M1,M2,M3, test="Chisq") 
# The M3 model is created by only considering the significant variables, at 
# 0.1 significance level, found performing the logistic regression model. 
# By comparing the M1 and M2 model and then M2 and M3, it emerges that
# the best model includes all the variables of the dataset even if they 
# are not significant. It help improving the estimate of the 
# binary response variable.

# confusion matrix
# training and test sets
n <- nrow(SLproject_balanced) #numer of row of the data frame
set.seed(1234) #function that fix a seed for random procedure
# sample from integral 1 to n (total of bunch for the sample), a sample with a size 0.75
train.ind <- sample(1:n, size = 0.75*n) #75% is the training and the 25% is the test
train <- SLproject_balanced[train.ind,]
str(train)
test <- SLproject_balanced[-train.ind,] #the remaining part that is used to test the model, infatti c'è il meno davanti a train
str(test)

#multiple logistic regression model on training data
#before we use the entire dataframe, now only train
glm.fit <- glm(loan_status.rec ~ . - Loan_status,
               data = train, family = binomial)
summary(glm.fit)

#predictions on test data
#im predictin pgrego hat on test
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- rep("No", nrow(test))
glm.pred[glm.probs > 0.5] = "Yes" #glm.pred are the predicted classes
glm.pred

#confusion matrix
table(test$Loan_status,glm.pred)
addmargins(table(test$Loan_status,glm.pred))

#Ensure the levels of test$Loan_status and glm.pred are the same and in the same order
levels_ref <- levels(as.factor(test$Loan_status))
glm.pred <- factor(glm.pred, levels = levels_ref)

cm.glm <- confusionMatrix(data=as.factor(glm.pred),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT: 
# The accuracy is not very good as it reaches the 60%. The 
# model does not predict well neither positive and negative values 
# since the specificity and sensitivity slightly exceed 0.50. 
# However, the model does not perform better than chance. 

#ROC curve
pred.glm <- prediction(glm.probs,as.factor(test$Loan_status)) #we are considering all the possible threshold (glm.probs), than we considerate the predicted y
perf.glm <- performance(pred.glm,"tpr","fpr")
plot(perf.glm, main="ROC curve", colorize=TRUE)

#AUC
AUC.glm <- performance(pred.glm,"auc")@y.values[[1]] #0.7145455
AUC.glm

# COMMENT COMPARING BALANCED AND NO BALANCED
# Accuracy and sensitivity are higher in the unbalanced dataset.
# Specificity improves in the balanced dataset, as the model better
# predicts the minority class ('No'). However, the P-Value [Acc > NIR]
# indicates that neither model's accuracy is significantly 
# better than what would be expected by chance (NIR), but the balanced 
# dataset's p-value is lower, suggesting a trend towards significance.
# The Mcnemar's Test P-Value indicates no significant difference between false 
# positives and false negatives in both datasets, with the balanced dataset 
# showing an even less significant difference.

#
##
###
####
##### LINEAR DISCRIMINANT ANALYSIS - LDA
####
###
##
#

options(scipen=999)
lda.fit <- lda(Loan_status ~ . - loan_status.rec, data = SLproject_balanced)
lda.fit

# predictions
lda.pred <- predict(lda.fit)
names(lda.pred)

# predictions - posterior probabilities
lda.post <- lda.pred$posterior

# Probabilities used to predict the class:
# If it is close to 0 -> class Y=No
# If it is close to 1 -> class Y=Yes

# predictions - predicted classes
pred.class.05 <- ifelse(lda.post[,2]>0.5,"Yes", "No")
# Looking at the largest pi and it classify the observations by comparing it,
# using a classification threshold of 0.5. "Assign the observation for the 
# class for which the posterior probability is larger". 

# predictions - scores of observations on linear discriminants 
LD <- lda.pred$x

# plot LD
par(mar = c(4, 4, 2, 2))
ldahist(LD,g=SLproject_balanced$Loan_status)
# COMMENT:
# As with unbalanced data, there is no separation between the values of 
# x for the two groups. It leads to difficulty in predicting the values
# of the loan status variable. The values of group No are localized 
# across the entire range of x, while the values of group Yes 
# are mainly in the left side of the graph. 

#Confusion matrix
table(SLproject_balanced$Loan_status,pred.class.05)
addmargins(table(SLproject_balanced$Loan_status,pred.class.05))

#Ensure the levels of SLproject$Loan_status and pred.class.05 are the same and in the same order
levels_ref <- levels(as.factor(SLproject_balanced$Loan_status))
pred.class.05 <- factor(pred.class.05, levels = levels_ref)

confusionMatrix(data=as.factor(pred.class.05),
                reference=as.factor(SLproject_balanced$Loan_status),positive="Yes")

# COMMENT:
# Since there is no separation between values of x, we expect that 
# the model is not able to predict both positive and negative values, but it 
# would may be better in predicting negative values. The confusion matrix
# confirms our ideas, indicating a very low Sensitivity (0.13978) and 
# a quite large specificity (0.35484). Because of this difficulty in predicting 
# values, even the accuracy is low (0.2473).

# Furthermore, the P-Value [Acc > NIR]=1. It is lower than the 5% significance 
# level, indicating that model's predictions are not significantly better than
# chance. 

# To improve the model we can divide the dataset into training and a test set. 
# training and test sets

#LDA on training data
lda.fit <- lda(Loan_status ~ . - loan_status.rec, data = train)
lda.fit

#predictions on test data
lda.pred.test <- predict(lda.fit, newdata = test)
lda.class.test <- lda.pred.test$class

#Confusion matrix (on the test set)
table(test$Loan_status,lda.class.test)
addmargins(table(test$Loan_status,lda.class.test))

#confusion matrix
cm.lda <- confusionMatrix(data=as.factor(lda.class.test),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT:
# By using the training set and the test set, the model's sensitivity and 
# specificity increase, but the sensitivity improvement is greater 
# than the one of specificity. Accuracy increases and the 
# P-Value [Acc > NIR] becomes lower than 0.05, meaning that the 
# model performs better than chance. 

#ROC curve 
lda.pred.test <- predict(lda.fit, newdata = test)
lda.post <- lda.pred.test$posterior
pred.lda <- prediction(lda.post[,2],as.factor(test$Loan_status))
perf.lda <- performance(pred.lda,"tpr","fpr")
plot(perf.lda, main="ROC curve", colorize=TRUE)

#AUC of LDA
AUC.lda <- performance(pred.lda,"auc")@y.values[[1]] #0.2327273
AUC.lda

# Comparing Logistic Regression with LDA
plot(perf.lda, main="ROC curve", col = "red")
plot(perf.glm, add =TRUE, col = "blue")

AUC1 <- paste("Logistic:",round(AUC.glm,4), sep=" ")
AUC2 <- paste("LDA:",round(AUC.lda,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2), lty=1, col = c("blue","red"), title="AUC",cex=0.6)

#COMMENT COMPARING BALANCED AND NO BALANCED LDA models
# Accuracy and sensitivity are higher in the unbalanced dataset.
# Specificity improves in the balanced dataset, as the model 
# becomes better at predicting the minority class ('No').
# The P-Value [Acc > NIR] for LDA model performed on balanced
# data is lower than 0.05. 
# The first dataset has an AUC of 0.35, while the second dataset has 
# an AUC of 0.23. An higher AUC in the first dataset indicates better 
# discrimination ability between classes compared to the second dataset.
# However, the LDA on unbalanced data performs worse than chance. 


########################################
# QUADRATIC DISCRIMINANT ANALYSIS - QDA
########################################

#QDA on training data
qda.fit <- qda(Loan_status ~ . - loan_status.rec, data = train)
qda.fit

#predictions on test data
qda.pred.test <- predict(qda.fit, newdata = test)
qda.class.test <- qda.pred.test$class

#Confusion matrix (on the test set)
table(test$Loan_status,qda.class.test)
addmargins(table(test$Loan_status,qda.class.test))

#confusion matrix
cm.qda <- confusionMatrix(data=as.factor(qda.class.test),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT: 
# Accuracy, sensitivity and specificity are quite good since they exceed 0.50. 
# In addition, the P-Value [Acc > NIR] is lower than 0.05. 

#ROC curve
qda.pred.test <- predict(qda.fit, newdata = test)
qda.post <- qda.pred.test$posterior
pred.qda <- prediction(qda.post[,2],as.factor(test$Loan_status))
perf.qda <- performance(pred.qda,"tpr","fpr")
plot(perf.qda, main="ROC curve", colorize=TRUE)

#AUC
AUC.qda <- performance(pred.qda,"auc")@y.values[[1]] #0.2690909
AUC.qda

# Comparing Logistic Regression with LDA and QDA
plot(perf.glm, main="ROC curve", col = "blue")
plot(perf.lda, add =TRUE, col = "red")
plot(perf.qda, add =TRUE, col = "green")

AUC1 <- paste("Logistic:",round(AUC.glm,4), sep=" ")
AUC2 <- paste("LDA:",round(AUC.lda,4), sep=" ")
AUC3 <- paste("QDA:",round(AUC.qda,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2, AUC3), lty=1, col = c("blue","red","green"), title="AUC",cex=0.6)

# COMMENT COMPARING BALANCED AND NO BALANCED
# The QDA on balanced dataset shows an higher accuracy, sensitivity 
# and specificity compared to the QDA on unbalanced dataset. 
# This indicates that the balanced dataset's model is better at correctly 
# identifying both positive and negative cases. The P-Value for accuracy 
# greater than No Information Rate (NIR) gets lower than 0.05 for the QDA 
# on balanced data. This suggests that the accuracy of the balanced dataset 
# is significantly better than what would be expected by chance.

# Mcnemar's Test P-Value: Both datasets have relatively high, 
# indicating no significant difference between false positives and false negatives 
# within each dataset. However, the balanced dataset shows a lower P-Value, 
# suggesting a trend towards less significant error distribution compared to the 
# unbalanced dataset.

# The model on unbalanced data has an higher AUC compared to the balanced one. 

###############
# NAIVE BAYES 
###############

#Naive Bayes on training data
nb.fit <- naiveBayes(Loan_status ~ . - loan_status.rec, data = train)
nb.fit

#a priori probabilities
table(Loan_status)/nrow(train)

#Conditional probabilities for variable Male given Loan_status (conditioned on Loan_status):
table(Loan_status,Male)
prop.table(table(Loan_status,Male),margin=1)

#Conditional probabilities for variable Married given Loan_status (conditioned on Loan_status):
table(Loan_status,Married)
prop.table(table(Loan_status,Married),margin=1)

#Conditional probabilities for variable Dependents given Loan_status (conditioned on Loan_status):
table(Loan_status,Dependents)
prop.table(table(Loan_status,Dependents),margin=1)

#Conditional probabilities for variable Graduate given Loan_status (conditioned on Loan_status):
table(Loan_status,Graduate)
prop.table(table(Loan_status,Graduate),margin=1)

#Conditional probabilities for variable Self_empl given Loan_status (conditioned on Loan_status):
table(Loan_status,Self_empl)
prop.table(table(Loan_status,Self_empl),margin=1)

#Conditional probabilities for variable Guid_met given Loan_status (conditioned on Loan_status):
table(Loan_status,Guid_met)
prop.table(table(Loan_status,Guid_met),margin=1)

#Conditional probabilities for variable Prop_area given Loan_status (conditioned on Loan_status):
table(Loan_status,Prop_area)
prop.table(table(Loan_status,Prop_area),margin=1)

# predictions on test data
# predict posterior probabilities
nb.post <- predict(nb.fit, newdata=test, type="raw")
# predict classes
nb.class.test <- predict(nb.fit, newdata=test, type="class")
# It gives a vector of Yes or No

#Confusion matrix (on the test set)
table(test$Loan_status,nb.class.test)
addmargins(table(test$Loan_status,nb.class.test))

#confusion matrix
cm.nb <- confusionMatrix(data=as.factor(nb.class.test),reference=as.factor(test$Loan_status),positive="Yes")

# COMMENT:
# The accuracy is quite good as it is about 0.76, implying an error rate of 24%. 
# The sensitivity is very high and the specificity exceeds 0.50. 
# Both the p-values are lower than 0.05. 

#ROC curve
nb.post <- predict(nb.fit, newdata=test, type="raw")
pred.nb <- prediction(nb.post[,2],as.factor(test$Loan_status))
perf.nb <- performance(pred.nb,"tpr","fpr")
plot(perf.nb, main="ROC curve", colorize=TRUE)

#AUC
AUC.nb <- performance(pred.nb,"auc")@y.values[[1]]  # 0.22
AUC.nb

# COMMENT COMPARING BALANCED AND NO BALANCED - NAIVE BAYES 
# The model on unbalanced dataset has a slightly higher accuracy, 
# sensitivity and specificity compared to the model on the balanced dataset.
# The P-Value for both models is lower than threshold of 0.05. 
# Mcnemar's Test P-Value is 0.4795 for the first and 0.0158613 for the second.
# The model on unbalanced has a better AUC than the one on balanced data. 

# Comparing Logistic Regression with LDA, QDA and NB
plot(perf.glm, main="ROC curve", col = "blue")
plot(perf.lda, add =TRUE, col = "red")
plot(perf.qda, add =TRUE, col = "green")
plot(perf.nb, add =TRUE, col = "purple")

AUC1 <- paste("Logistic:",round(AUC.glm,4), sep=" ")
AUC2 <- paste("LDA:",round(AUC.lda,4), sep=" ")
AUC3 <- paste("QDA:",round(AUC.qda,4), sep=" ")
AUC4 <- paste("NB:",round(AUC.nb,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2, AUC3, AUC4), lty=1, col = c("blue","red","green", "purple"), title="AUC",cex=0.5)

# RESULTS FOR BALANCED AND NO BALANCED - ALL MODELS
# The balanced dataset is useful to better predict the binary response variable. 
# By using the balanced dataset, the AUC gets worsen, but the confusion matrix 
# shows that the models performance improve. We notice that the best model 
# is the Naive bayes because both p-values are lower 0.05. 
# Anyway, this model does not perform very well, since the data available to 
# us are not so good. The adoption of balancing methods does not solve 
# all the problems of the original dataset. 


# summarize the confusion matrix of all the models in a table
# model on balanced data with all the variables

# on rows the models and columns represent the metrics
Model <- c("Logistic", "LDA", "QDA", "Naive bayes")
Accuracy <- c(round(cm.glm$overall["Accuracy"],4),
              round(cm.lda$overall["Accuracy"],4),
              round(cm.qda$overall["Accuracy"],4),
              round(cm.nb$overall["Accuracy"],4))
Accuracy.pvalue <- c(round(cm.glm$overall["AccuracyPValue"],4),
                         round(cm.lda$overall["AccuracyPValue"],4),
                         round(cm.qda$overall["AccuracyPValue"],4),
                         round(cm.nb$overall["AccuracyPValue"],4))
Kappa <- c(round(cm.glm$overall["Kappa"],4),
           round(cm.lda$overall["Kappa"],4),
           round(cm.qda$overall["Kappa"],4),
           round(cm.nb$overall["Kappa"],4))
Sensitivity <- c(round(cm.glm$byClass["Sensitivity"],4),
                 round(cm.lda$byClass["Sensitivity"],4),
                 round(cm.qda$byClass["Sensitivity"],4),
                 round(cm.nb$byClass["Sensitivity"],4))
Specificity <- c(round(cm.glm$byClass["Specificity"],4),
                 round(cm.lda$byClass["Specificity"],4),
                 round(cm.qda$byClass["Specificity"],4),
                 round(cm.nb$byClass["Specificity"],4))

Cm_comparison <- data.frame(Model, Accuracy, Accuracy.pvalue, 
                            Kappa, Sensitivity, Specificity)
colnames(Cm_comparison) <- c("Model", "Accuracy", "P-Value [Acc > NIR]",
                             "Kappa", "Sensibility", "Specificity")
print (Cm_comparison)

# store results in a table
kbl(Cm_comparison, centering = TRUE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                position = "left", full_width = FALSE)

# switching rows with columns
# on rows the metrics and columns the models
Metrics <- c("Model", "Accuracy", "P-Value [Acc > NIR]",
             "Kappa", "Sensibility", "Specificity")
Logistic <- c(round(cm.glm$overall["Accuracy"],4),
              round(cm.glm$overall["AccuracyPValue"],4),
              round(cm.glm$overall["Kappa"],4),
              round(cm.glm$byClass["Sensitivity"],4),
              round(cm.glm$byClass["Specificity"],4))
LDA <- c(round(cm.lda$overall["Accuracy"],4),
         round(cm.lda$overall["AccuracyPValue"],4),
         round(cm.lda$overall["Kappa"],4),
         round(cm.lda$byClass["Sensitivity"],4),
         round(cm.lda$byClass["Specificity"],4))
QDA <- c(round(cm.qda$overall["Accuracy"],4),
           round(cm.qda$overall["AccuracyPValue"],4),
           round(cm.qda$overall["Kappa"],4),
           round(cm.qda$byClass["Sensitivity"],4),
           round(cm.qda$byClass["Specificity"],4))
NB <- c(round(cm.nb$overall["Accuracy"],4),
        round(cm.nb$overall["AccuracyPValue"],4),
        round(cm.nb$overall["Kappa"],4),
        round(cm.nb$byClass["Sensitivity"],4),
        round(cm.nb$byClass["Specificity"],4))

Cm_comparison <- data.frame(Logistic, LDA, QDA, NB)
colnames(Cm_comparison) <- c("Logistic", "LDA", "QDA", "Naive Bayes")
print (Cm_comparison)

# store results in a table
kbl(Cm_comparison, centering = TRUE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                position = "left", full_width = FALSE)


# PROBLEM 2 - VARIABILITY/HETEROGENEITY
# The limited variability of predictor classes. If the predictors classes 
# are too similar to each other, it could be challenging for the linear 
# discriminant model to distinguish between them and therefore it might 
# struggle to classify observations accurately. This could lead to inaccurate 
# and unreliable results.
# We have that the y is balanced, but all the other variables remain "unbalanced".

# Variables pre-balance
table(SLproject$Male)        # No: 70, Yes: 258
table(SLproject$Married)     # No: 133, Yes: 195
table(SLproject$Dependents)  # 0: 205, 1: 46, 2: 52, 3+: 25
table(SLproject$Self_empl)   # No: 291, Yes: 37
table(SLproject$Guid_met)    # No: 46, Yes: 282
table(SLproject$Prop_area)   # Rural: 94, Semiurban: 131, Urban: 103 

# Variables post-balance
table(SLproject_balanced$Male)        # No: 43, Yes: 143
table(SLproject_balanced$Married)     # No: 85, Yes: 101  °
table(SLproject_balanced$Dependents)  # 0: 125, 1: 25, 2: 26, 3+: 10
table(SLproject_balanced$Self_empl)   # No: 167, Yes: 19
table(SLproject_balanced$Guid_met)    # No: 46, Yes: 140
table(SLproject_balanced$Prop_area)   # Rural: 57, Semiurban: 72, Urban: 57 °

# tables showing qualitative variables heterogeneity
# on rows the number of Yes and No and columns the qualitative variables
Male <- c(table(SLproject_balanced$Male)["Yes"], 
          table(SLproject_balanced$Male)["No"])
Married <- c(table(SLproject_balanced$Married)["Yes"],
             table(SLproject_balanced$Married)["No"])
Graduate <- c(table(SLproject_balanced$Graduate)["Yes"],
              table(SLproject_balanced$Graduate)["No"])
Self_empl <- c(table(SLproject_balanced$Self_empl)["Yes"],
               table(SLproject_balanced$Self_empl)["No"])
Guid_met <- c(table(SLproject_balanced$Guid_met)["Yes"],
              table(SLproject_balanced$Guid_met)["No"])
# Prop_area <- c(table(SLproject_balanced$Prop_area)["Yes"],
#               table(SLproject_balanced$Prop_area)["No"])

Cm_comparison <- data.frame(Male, Married, Graduate, 
                            Self_empl, Guid_met)
colnames(Cm_comparison) <- c("Male", "Married", "Graduate",
                             "Self employed", "Guidelines met")
print (Cm_comparison)

# store results in a table
kbl(Cm_comparison, centering = TRUE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                position = "left", full_width = FALSE)

##
# dependents table
##
Dependents <- c(table(SLproject_balanced$Dependents)["0"],
                table(SLproject_balanced$Dependents)["1"],
                table(SLproject_balanced$Dependents)["2"],
                table(SLproject_balanced$Dependents)["3+"])
Cm_comparison <- data.frame(Dependents)
print (Cm_comparison)

# store results in a table
kbl(Cm_comparison, centering = TRUE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                position = "left", full_width = FALSE)
##
# prop-area table
##
Urban <- table(SLproject_balanced$Prop_area)["Urban"]
Semiurban <- table(SLproject_balanced$Prop_area)["Semiurban"]
Rural <- table(SLproject_balanced$Prop_area)["Rural"]

Cm_comparison <- data.frame(Urban, Semiurban, Rural)
print (Cm_comparison)

# store results in a table
kbl(Cm_comparison, centering = TRUE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                position = "left", full_width = FALSE)


#######################################
#WHY THE MODELS DOES NOT PERFORM WELL?
####################################### 
# Model performance is evaluated by checking the satisfaction of assumptions. 

# Check of Multicollinearity 
# Each model requires the absence of multicollinearity.
numeric_df <- SLproject_balanced %>%
  select_if(is.numeric)
numeric_df$loan_status.rec <- NULL

cor_matrix <- cor(numeric_df)

# Melt the correlation matrix for plotting with ggplot2
melted_cor <- melt(cor_matrix)

# Create a heatmap using ggplot2
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Correlation", limits = c(-1, 1)) +
  theme_minimal() +
  labs(x = "Predictors", y = "Predictors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RESULT: 
# Since correlation is very low we do not worry about Multicollinearity.

#
## LOGISTC MODEL - ipotesi di linearità è verificata (grafico 1)
#

# model
glm <- glm(loan_status.rec ~ . - Loan_status, data = SLproject_balanced, family = "binomial")

# Graph to check the linearity assumption
plot(glm)

# OR: 
plot(glm$fitted.values, residuals(glm, type = "pearson"), 
     xlab = "Estimated values", ylab = "Residuals")
abline(h = 0, col = "red")
# COMMENT: 
# The dashed red line 
# La linea rossa invece è una linea di tendenza, che ti aiuta a verificare
# la prima ipotesi. Se la linea rossa è abbastanza sovrapponibile alla linea
# tratteggiata, come in questo caso, allora l’ipotesi di linearità è verificata.
# Secondo l’ipotesi di linearità, i dati devono infatti distribuirsi 
# in modo casuale intorno allo 0.
# We have a pattern in the graphs, so the linear assumption does not hold.

# The logistic regression assumes a linear relationship between
# the predictors and the binary response variable. In this case, this
# assumption is not met, then logistic regression may produce not accurate 
# results.

#
## LDA 
#

# LDA works better if assumptions are satisfied 
# 1  - Normality assumption for quantitative variables x

shapiro.test(SLproject_balanced$Appl_income) # do not follow a normal distribution
shapiro.test(SLproject_balanced$Coappl_income) # do not follow a normal distribution
shapiro.test(SLproject_balanced$Loan_amount) # do not follow a normal distribution
shapiro.test(SLproject_balanced$Loan_term) # do not follow a normal distribution

# Since the normality assumption is not met by all the quantitative variables, 
# the LDA can not perform well.

# 2 - assumption of same covariance matrix for all the classes of variables

# Box's M test
# install.packages("biotools")
results_boxM <- boxM(SLproject_balanced[, c("Appl_income", "Coappl_income", "Loan_amount", "Loan_term")], SLproject_balanced$Loan_status)
print(results_boxM) # p-value ≤ 0.05
# Given that the assumption of homogeneity of covariance matrices is violated, 
# Linear Discriminant Analysis (LDA) might not perform optimally.

# The satisfaction of this assumption is also checked with the bartlett test

# Bartlett test
bartlett_results <- bartlett.test(list(
  SLproject_balanced$Appl_income,
  SLproject_balanced$Coappl_income,
  SLproject_balanced$Loan_amount,
  SLproject_balanced$Loan_term
), g = SLproject_balanced$Loan_status)
print(bartlett_results) # p-value ≤ 0.05
# Given that the assumption of homogeneity of covariance matrices is violated, 
# Linear Discriminant Analysis (LDA) might not perform optimally.

# 3 - Assumption of class separation 
LD <- lda.pred$x
par(mar = c(4, 4, 2, 2))
ldahist(LD,g=SLproject_balanced$Loan_status) 

# RESULTS
# As seen before, the plot shows classes overlapping across x values, 
# this may lead to inaccurate results. 

#
## QDA
#

# QDA works better if assumptions are satisfied 
# 1  - Normality assumption for quantitative variables x

shapiro.test(SLproject_balanced$Appl_income) # do not follow a normal distribution
shapiro.test(SLproject_balanced$Coappl_income) # do not follow a normal distribution
shapiro.test(SLproject_balanced$Loan_amount) # do not follow a normal distribution
shapiro.test(SLproject_balanced$Loan_term) # do not follow a normal distribution

# 2 - Assumption of different covariance matrix for all the classes of variables

# Box's M test
# install.packages("biotools")
results_boxM <- boxM(SLproject_balanced[, c("Appl_income", "Coappl_income", "Loan_amount", "Loan_term")], SLproject_balanced$Loan_status)
print(results_boxM) # p-value ≤ 0.05
# Given that the assumption of class-specific covariance matrices is not violated, 
# Quadratic Discriminant Analysis might perform well.

# This assumption can be checked even through the Bartlett test

# Bartlett test
bartlett_results <- bartlett.test(list(
  SLproject_balanced$Appl_income,
  SLproject_balanced$Coappl_income,
  SLproject_balanced$Loan_amount,
  SLproject_balanced$Loan_term
), g = SLproject_balanced$Loan_status)
print(bartlett_results) # p-value ≤ 0.05
# Given that the assumption of class-specific covariance matrices is not violated, 
# Quadratic Discriminant Analysis might perform well

# 3 - Assumption of classes separation 
LD <- lda.pred$x
par(mar = c(4, 4, 2, 2))
ldahist(LD,g=SLproject_balanced$Loan_status, col="lightskyblue2") 


# RESULTS
# As seen before, the plot shows classes overlapping across x values, 
# this may lead to inaccurate results.

# CONCLUSION
# We notice that at least one assumption (the second) holds with QDA model, while
# none of the assumption is met with the LDA. It means that QDA may performs 
# better than LDA. 

#
#NAIVE BAYES
#

# The Naive Bayes model relies on the assumption of independent 
# predictors within the 𝑘-th class. As it performs better than 
# the previous models, we can conclude that model's predictors
# are relatively independent within each class k. 

# CONCLUSION
# Even if the key assumption of Naive Bayes appear to be met, 
# the AUC is too low (0.22), meaning that the classification 
# model is not able to accuratly distinguish 
# the classes of interest. It implies that it may be not reliable 
# in correctly predicting the classes of interest.


##########################################################
# SUBSET SELECTION METHODS WITH BINARY RESPONSE VARIABLE
#########################################################
# Review of variable subselection methods...

# ... WITH QUANTITATIVE RESPONSE VARIABLE
# When the response variable is quantitative, the relevant variables
# for the analysis can be detected through subset selection methods and
# Shrinkage or regularization methods. 
# The subset selection methods are:
# - best subset selection, which can be realized using forward, backward or 
# mixed approach; - package leaps, function regsubsets
# - the stepwise. - package MASS, function stepwise
# The Shrinkage or regularization methods are:
# - the Ridge regression; - package glmnet, function glmnet con param. alpha = 0
# - the Lasso regression. - package glmnet, function glmnet con param. alpha = 1

# ... WITH QUALITATIVE RESPONSE VARIABLE
# When the response variable is qualitative, the relevant variables
# for the analysis can be detected through the same family of models,
# but using other functions that support this type of response variable. 
# The subset selection methods are: 
# - the best subset selection, which is implemented with the BeSS package
# and the bess function using parameter family = "binomial", useful to 
# specify the type of response variable and the parameter method = "sequential",
# which asks to use a mixed strategy to select the variables. 
# - the stepwise is implemented using the same package MASS, but the  
# function stepAIC that is bases on AIC evaluation. 
# The Shrinkage or regularization methods are:
# - the Ridge regression; - glmnet package, function glmnet 
# con param. alpha = 0 and parameter family="binomial"
# - the Lasso regression. - glmnet package, function glmnet 
# con param. alpha = 1 and parameter family="binomial"


##
####
#####
# BEST SUBSET SELECTION METHODS WITH BINARY RESPONSE VARIABLE
#####
####
##

# BESS
# install.packages("BeSS")
X <- as.matrix(SLproject_balanced[, c("Male", "Married", "Dependents", "Graduate", "Self_empl", "Appl_income",
                        "Coappl_income", "Loan_amount", "Loan_term", "Guid_met", "Prop_area")])

# Creating a matrix containing the predictor variables
predictive_matrix <- model.matrix(~. -Loan_status -loan_status.rec, SLproject_balanced)[, -1]
head(predictive_matrix)
y <- as.matrix(SLproject_balanced$Loan_status)

# SEQUENTIAL = it is similar to the mixed strategy 
# Perform best subset selection with bess
fit.seq <- bess(x = predictive_matrix, y = y,
            family = "binomial", method = "sequential",
            weights = rep(1,nrow(predictive_matrix)))

# To identify the relevant variables three criteria can be used:
# - AIC that used a penalty of 2 × logL + 2||β||0;
# - BIC, whose penalty is 2 x logL + log(n)||β||0;
# - EBIC, whose penalty is 2 × logL + (log(n) + 2 × log(p))||β||0.
# where n = n*variables, p= n*tot. of candidates variables 
# => EBIC is the more penalizing criterion


# We found k variables according AIC
K.opt.aic <- which.min(fit.seq$AIC) 
coef(fit.seq)[, K.opt.aic][which(coef(fit.seq)[, K.opt.aic] != 0)] #Self_emplYes, Loan_amount, Guid_metYes, Prop_areaSemiurban
plot(fit.seq, type = "both", breaks = TRUE, K = K.opt.aic)

# We found k variables according BIC
K.opt.bic <- which.min(fit.seq$BIC) 
coef(fit.seq)[, K.opt.bic][which(coef(fit.seq)[, K.opt.bic] != 0)] #Guid_metYes, Prop_areaSemiurban
plot(fit.seq, type = "both", breaks = TRUE, K = K.opt.bic)

# We found k variables according 
K.opt.ebic <- which.min(fit.seq$EBIC) 
coef(fit.seq)[, K.opt.ebic][which(coef(fit.seq)[, K.opt.ebic] != 0)] #Guid_metYes, Prop_areaSemiurban
plot(fit.seq, type = "both", breaks = TRUE, K = K.opt.ebic)

# RESULTS: 
summary(fit.seq)
# With the AIC, the k = 4, while with BIC it is equal to 2. It is due to the
# stricter penalty assigned by the BIC to the addition of predictors. 
# Even with the EBIC, the k = 2, because of its strict penalty that is even 
# higher of the one assigned with the BIC criteria. 

##
####
#####
# #### REGULARIZATION - GLM
#####
####
##

# The coefficient estimates are identified by minimizing 
# RSS + λ[(1−α)(∥β∥_2)^2/2+α∥β∥_1. The last addend contains the
# shrinking penalty aimed at shrinking the coefficient estimates 
# towards 0. It depends on alpha, lambda and the norm of beta. 
# The alpha controls the type of regularization method: 
# - when α = 0, the Ridge regression is performed and the calculations
# are based on squared L2 norm of the coefficients vector;
# - when α = 1, the Lasso regression is performed and the shrinking penalty
# is defined has λ∥β∥_1, where∥β∥_1is the L1 norm of the coefficient vector.
# The value of lambda has to be chosen with cross-validation method.

# When applying the regulation methods with binary response variables, 
# it is no more used the MSE - Minimum Standard Error. The new measure
# adopted is the Binomial Deviance. In addition, the model is no more based
# on linear regression but on a logistic regression. 


###################
# RIDGE REGRESSION 
###################

grid <- 10^seq(-2, 5, 0.01)
ridge.mod <- glmnet(predictive_matrix, y, family="binomial", alpha = 0, lambda = grid)
plot(ridge.mod, xvar = "lambda", label = TRUE)

coef(ridge.mod) #set of coefficients from models, we can rapersent those numbers with the line in the slides

# According to lambda, the coefficient estimates change
# coefficients when lambda is 10, 10000, 0.01
coef(ridge.mod)[,ridge.mod$lambda==10]     # low
coef(ridge.mod)[,ridge.mod$lambda==10000]  # high
coef(ridge.mod)[,ridge.mod$lambda==0.01]   # very low

#plot
plot(ridge.mod, xvar = "lambda", label = TRUE)

# Selecting the optimal value of the tuning parameter lambda
# the OPTIMAL LAMBDA is the one that minimizes the Binomial Deviance. 
# It is found by using cv.glmnet 
set.seed(123)
cv.out <- cv.glmnet(predictive_matrix, y,family="binomial", alpha = 0, lambda = grid)
plot(cv.out)

best.lambda <- cv.out$lambda.min
best.lambda #0.06025596 = e^-0.005 

# Re-fit the model on the full data set and use best.lambda to obtain coefficients
predict(ridge.mod, type="coefficients",s=best.lambda)

########
# LASSO
########

lasso.mod <- glmnet(predictive_matrix, y, family="binomial", alpha = 1, lambda = grid) #like in the ridge regression, but with alpha=1
coef(lasso.mod)
plot(lasso.mod, xvar = "lambda", label = TRUE)

set.seed(123)
cv.out <- cv.glmnet(predictive_matrix, y, family="binomial", alpha = 1, lambda = grid)
plot(cv.out)

best.lambda <- cv.out$lambda.min #value of lambda that minimize Binomial Deviance
best.lambda #0.05754399 = e^-0.005

predict(lasso.mod, type="coefficients",s=best.lambda) #Guid_metYes, Prop_areaSemiurban 

##
####
#####
# STEPWISE con stepAIC 
#####
####
##

#install.packages(MASS)
# Logistic regression model with all variables
model <- glm(Loan_status ~ .-Loan_status - loan_status.rec,
             data = SLproject_balanced, family = "binomial")

# Variables selection with stepAIC
selected_model <- stepAIC(model, direction = "both")

coef(selected_model) #Self_empl, Loan_amount, Guid_met, Prop_area


############################
#####################################################
# ANALYSIS WITH BALANCED DATA AND SELECTED VARIABLES
#####################################################
############################

# In order to include at least a quantitative variable 
# in the model, we decided to perform the model with 
# the four relevant variables identified with stepwise
# and best subset selection based on AIC.

##
### LOGISTIC - SELECTED VARIABLES
##

# training and test sets
n <- nrow(SLproject_balanced) 
set.seed(1234) #function that fix a seed for random procedure

#multiple logistic regression model on training data
glm.fit <- glm(loan_status.rec ~ Guid_met + Prop_area + Self_empl + Loan_amount,
               data = train, family = binomial)
summary(glm.fit)

#predictions on test data
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- rep("No", nrow(test))
glm.pred[glm.probs > 0.5] = "Yes" # predicted calsses
glm.pred

# confusion matrix to evaluate our logistc regression model on our data
table(test$Loan_status,glm.pred)
addmargins(table(test$Loan_status,glm.pred))

#Ensure the levels of test$Loan_status and glm.pred are the same and in the same order
levels_ref <- levels(as.factor(test$Loan_status))
glm.pred <- factor(glm.pred, levels = levels_ref)

#install.packages("caret")
cm.glm <- confusionMatrix(data=as.factor(glm.pred),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT: 
# The logistic model performs better than chance since the 
# P-Value [Acc > NIR] = 0.005762 and the lower than 0.05. However, the
# Mcnemar's Test P-Value = 0.096092, which is still higher than 0.05.
# Accuracy quite good as sensitivity and specificity. 

#ROC curve
#install.packages("ROCR")
pred.glm <- prediction(glm.probs,as.factor(test$Loan_status)) #we are considering all the possible threshold (glm.probs), than we considerate the predicted y
perf.glm <- performance(pred.glm,"tpr","fpr")
plot(perf.glm, main="ROC curve", colorize=TRUE)

#AUC
AUC.glm <- performance(pred.glm,"auc")@y.values[[1]] #0.78
AUC.glm

# COMMENT COMPARING MODEL WITH ALL VARIABLES AND MODEL WITH SELECTED VARIABLES
# The model with selected variables has a slightly higher accuracy, sensitivity
# and specificity compared to the model with all variables. Moving to the 
# model with only relevant variables, the values respectively goes from 0.7234,
# 0.8636 and 0.6000, to 0.6809, 0.8182 and 0.5600. P-Value [Acc > NIR] is lower 
# in the model with selected variables (0.005762) compared to the model 
# with all variables (0.02757). This indicates that the accuracy of the model
# with selected variables is significantly better than what would be 
# expected by chance. Mcnemar's Test P-Value is 0.12134 in the model with all variables and
# 0.096092 in the model with the selected variables. This p-value still is above
# the threshold of 0.05. The model with selected variables (AUC: 0.78) has 
# a slightly higher AUC compared to the model with all variables (AUC: 0.76). 

##
###LDA - SELECTED VARIABLES
##

#LDA on training data
lda.fit <- lda(Loan_status ~ Guid_met + Prop_area + Self_empl+Loan_amount, data = train)
lda.fit

#predictions on test data
lda.pred.test <- predict(lda.fit, newdata = test)
lda.class.test <- lda.pred.test$class

#Confusion matrix (on the test set)
table(test$Loan_status,lda.class.test)
addmargins(table(test$Loan_status,lda.class.test))

#confusion matrix
cm.lda <- confusionMatrix(data=as.factor(lda.class.test),reference=as.factor(test$Loan_status),positive="Yes")
# COMMENT:
# The LDA model is not very accurate and able to predict negative values, but
# it manages to predict positive values quite well. The difference between 
# the classes is not significant (Mcnemar's Test P-Value > 0.05), but the 
# model performs better than chance (P-Value [Acc > NIR] < 0.05). 

#ROC curve 
#install.packages("ROCR")
lda.pred.test <- predict(lda.fit, newdata = test)
lda.post <- lda.pred.test$posterior
pred.lda <- prediction(lda.post[,2],as.factor(test$Loan_status))
perf.lda <- performance(pred.lda,"tpr","fpr")
plot(perf.lda, main="ROC curve", colorize=TRUE)

#AUC of LDA
AUC.lda <- performance(pred.lda,"auc")@y.values[[1]] #0.22
AUC.lda

# COMMENT COMPARING MODEL WITH ALL VARIABLES AND MODEL WITH SELECTED VARIABLES
# The model including all the variables is slightly less accurate than 
# the model with only the relevant variables, since the accuracy moves 
# reaches 0.70. Even the sensitivity gets better removing the non relevant 
# variables from the model: its metrics moves from 0.8182 to 0.8636.
# Specificity remains the same for both models (0.5600). The model including
# only the relevant variables has a P-Value [Acc > NIR] = 0.01319, indicating 
# that its accuracy is significantly better than what would be expected 
# by chance. In the previous model it was higher (0.02757), but still under 0.05.
# For what concerns the Mcnemar's Test P-Value, the model comprehending 
# only the relevant variables, shows a lower P-Value (0.06137), that is 
# above 0.05, but greatly improved with respect to the previous model. 
# However, both models still have a very low AUC that does not reach even 0.25. 

#comparing Logistic Regression with LDA
plot(perf, main="ROC curve", col = "blue")
plot(perf.glm, add =TRUE, col = "red")

AUC1 <- paste("Logistic:",round(AUC.glm,4), sep=" ")
AUC2 <- paste("LDA:",round(AUC.lda,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2), lty=1, col = c("blue","red"), title="AUC",cex=0.6)
# According to AUC, the Logistic model performs better than the LDA. 

##
### QDA - SELECTED VARIABLES
##

#QDA on training data
qda.fit <- qda(Loan_status ~ Guid_met + Prop_area + Self_empl+Loan_amount, data = train)
qda.fit

#predictions on test data
qda.pred.test <- predict(qda.fit, newdata = test)
qda.class.test <- qda.pred.test$class

#Confusion matrix (on the test set)
table(test$Loan_status,qda.class.test)
addmargins(table(test$Loan_status,qda.class.test))

#confusion matrix
cm.qda <- confusionMatrix(data=as.factor(qda.class.test),reference=as.factor(test$Loan_status),positive="Yes")
# Both the p-value are <0.05, good. Better than before.

#ROC curve
#install.packages("ROCR")
qda.pred.test <- predict(qda.fit, newdata = test)
qda.post <- qda.pred.test$posterior
pred.qda <- prediction(qda.post[,2],as.factor(test$Loan_status))
perf.qda <- performance(pred.qda,"tpr","fpr")
plot(perf.qda, main="ROC curve", colorize=TRUE)

#AUC
AUC.qda <- performance(pred.qda,"auc")@y.values[[1]] #0.2127273
AUC.qda

# Comparing Logistic Regression with LDA and QDA
plot(perf.glm, main="ROC curve", col = "blue")
plot(perf.lda, add =TRUE, col = "red")
plot(perf.qda, add =TRUE, col = "green")

AUC1 <- paste("Logistic:",round(AUC.glm,4), sep=" ")
AUC2 <- paste("LDA:",round(AUC.lda,4), sep=" ")
AUC3 <- paste("QDA:",round(AUC.qda,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2, AUC3), lty=1, col = c("blue","red","green"), title="AUC",cex=0.6)
# According to AUC, logistic seems to still be the best one.

# COMMENT COMPARING MODEL WITH ALL VARIABLES AND MODEL WITH SELECTED VARIABLES
# The model including only the relevant variables has slightly higher accuracy 
# (0.7021 than 0.6809) and specificity (0.9091 than 0.8182) than the model 
# with all the variables. The exclusion of irrelevant variables leads to 
# the opposite result in terms of specificity as it slightly decreases
# going from 0.5600 to 0.5200. The big difference between the two model
# relies in the p-values. The model including only the relevant variables 
# performs better than change and shows a significant different among 
# the classes that cannot be attributed to casuality. Those p-values where 
# above 0.05 in the model including all the variable. By removing irrelevant 
# variables, also the AUC improves. However, it is still under 0.30. 

##
###NAIVE BAYES - SELECTED VARIABLES
##

#Naive Bayes on training data
nb.fit <- naiveBayes(Loan_status ~ Guid_met + Prop_area + Self_empl+Loan_amount, data = train)
nb.fit

# prior probabilities
table(Loan_status)/nrow(train)

#Conditional probabilities for variable Self_empl given Loan_status (conditioned on Loan_status):
table(SLproject_balanced$Loan_status,
      SLproject_balanced$Self_empl)
prop.table(table(SLproject_balanced$Loan_status,
                 SLproject_balanced$Self_empl),margin=1)

#Conditional probabilities for variable Guid_met given Loan_status (conditioned on Loan_status):
table(SLproject_balanced$Loan_status,
      SLproject_balanced$Guid_met)
prop.table(table(SLproject_balanced$Loan_status,
                 SLproject_balanced$Guid_met),margin=1)

#Conditional probabilities for variable Prop_area given Loan_status (conditioned on Loan_status):
table(SLproject_balanced$Loan_status,
      SLproject_balanced$Prop_area)
prop.table(table(SLproject_balanced$Loan_status,
                 SLproject_balanced$Prop_area),margin=1)

# predictions on test data
# predict posterior probabilities
nb.post <- predict(nb.fit, newdata=test, type="raw")
# predict classes
nb.class.test <- predict(nb.fit, newdata=test, type="class")
# It gives a vector of Yes or No

#Confusion matrix (on the test set)
table(test$Loan_status,nb.class.test)
addmargins(table(test$Loan_status,nb.class.test))

#confusion matrix
cm.nb <- confusionMatrix(data=as.factor(nb.class.test),reference=as.factor(test$Loan_status),positive="Yes")

# COMMENT:
# The accuracy is not so good as the error rate still reaches the 32%.  
# The specificity  is quite good, while the specificity is slightly over the 
# 0.5. Both p-values are lower than 0.05, meaning that the model performs
# better than chance and that there is a significant difference between 
# classes that cannot be attributed to casuality. 

#ROC curve
nb.post <- predict(nb.fit, newdata=test, type="raw")
pred.nb <- prediction(nb.post[,2],as.factor(test$Loan_status))
perf.nb <- performance(pred.nb,"tpr","fpr")
plot(perf.nb, main="ROC curve", colorize=TRUE)

#AUC
AUC.nb <- performance(pred.nb,"auc")@y.values[[1]]  # 0.22
AUC.nb

# Comparing Logistic Regression with LDA, QDA and NB
plot(perf.glm, main="ROC curve", col = "blue")
plot(perf.lda, add =TRUE, col = "red")
plot(perf.qda, add =TRUE, col = "green")
plot(perf.nb, add =TRUE, col = "purple")

AUC1 <- paste("Logistic:",round(AUC.glm,4), sep=" ")
AUC2 <- paste("LDA:",round(AUC.lda,4), sep=" ")
AUC3 <- paste("QDA:",round(AUC.qda,4), sep=" ")
AUC4 <- paste("NB:",round(AUC.nb,4), sep=" ")

legend(0,1, legend = c(AUC1, AUC2, AUC3, AUC4), lty=1, col = c("blue","red","green", "purple"), title="AUC",cex=0.55)

# COMMENT COMPARING MODEL WITH ALL VARIABLES AND MODEL WITH SELECTED VARIABLES
# By including only the relevant variables, the gets worse. 
# The accuracy moves from 0.766 to 0.6809, the sensitivity decreases of 0.2736 
# and the specificity reduces of 0.0.08. The p-values are both lower than 0.05. 
# By excluding the irrelevant variables from the model, the P-Value [Acc > NIR] 
# improves becoming even better than before. It reaches almost zero. 
# In contrast, the Mcnemar's Test P-Value increases, but it is still 
# below 0.05. The AUC remains unchanged at 0.22. 


# SOME PROBLEMS STILL REMAIN
# The implementation of undersampling balancing methods and the application
# of subset selection methods help in improving the model's performance. 
# However, the AUC is still too low, considering that the best model 
# as an AUC = 1. The best model is the ......
# The poor performances of the models are due to the dataset
# that comprehends not highly variable quantitative variables and not 
# highly heterogeneous qualitative variables. 

# TO DO LIST
# Conclusioni "The best model is the ......" + poster + discorso


# summarizing the confusion matrix of all the models in a table
# on rows the metrics and columns the models
Metrics <- c("Model", "Accuracy", "P-Value [Acc > NIR]",
             "Kappa", "Sensibility", "Specificity")
Logistic <- c(round(cm.glm$overall["Accuracy"],4),
              round(cm.glm$overall["AccuracyPValue"],4),
              round(cm.glm$overall["Kappa"],4),
              round(cm.glm$byClass["Sensitivity"],4),
              round(cm.glm$byClass["Specificity"],4))
LDA <- c(round(cm.lda$overall["Accuracy"],4),
         round(cm.lda$overall["AccuracyPValue"],4),
         round(cm.lda$overall["Kappa"],4),
         round(cm.lda$byClass["Sensitivity"],4),
         round(cm.lda$byClass["Specificity"],4))
QDA <- c(round(cm.qda$overall["Accuracy"],4),
         round(cm.qda$overall["AccuracyPValue"],4),
         round(cm.qda$overall["Kappa"],4),
         round(cm.qda$byClass["Sensitivity"],4),
         round(cm.qda$byClass["Specificity"],4))
NB <- c(round(cm.nb$overall["Accuracy"],4),
        round(cm.nb$overall["AccuracyPValue"],4),
        round(cm.nb$overall["Kappa"],4),
        round(cm.nb$byClass["Sensitivity"],4),
        round(cm.nb$byClass["Specificity"],4))

Cm_comparison <- data.frame(Logistic, LDA, QDA, NB)
colnames(Cm_comparison) <- c("Logistic", "LDA", "QDA", "Naive Bayes")
print (Cm_comparison)

# store results in a table
kbl(Cm_comparison, centering = TRUE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                position = "left", full_width = FALSE)

detach(SLproject_balanced)

#############
# END OF THE CODE USED
#############






#------------------------------------------------------------------
#####################---------------------------------------------
# CORRECT CODES FOR other balancing methods
####################


# BALANCING METHODS
count_yes <- table(SLproject$Loan_status)["Yes"]
print(count_yes)

count_no <- table(SLproject$Loan_status)["No"]
print(count_no)

SLproject = sample(c(Loan_status, "Yes"), 93, replace = FALSE)

##########################
# NEED TO BALANCE - SMOTE
# We notice that the dependent variable is unbalanced as most of the loans have 
# been granted. The proportion of loans denied is smaller. Therefore, we should balance y. 
# There are two approaches: 
# - oversampling;
# - undersampling. 

# OVERSAMPLING
# There are different methods: 
# - SMOTE and ADASYN works only with quantitative variables,
# - SMOTENC, that can be applied with a dataset of quantitative variables and a unique factor, 
# - SMOTEN, that can be implemented only with categorical variables;
# - ROSE, ????? Can be applied to our dataset that has both qualitative and quantitative vars?

# UNDERSAMPLING 
# There are other techinques that we think are useless in our case as our dataset 
# has already few observations. 

# As our data have both types of data, we used the R functions "upSample"
# and "downSample" that keep intact original data and adds/removes rows 
# from it sampling data with replacement. 


##################
## SMOTE METHOD
##################
# names(SLproject)

# actual situation
# table(SLproject$Loan_status)

# BALANCING Y (=Loan_status) 
# library(smotefamily)
# newData <- SMOTE(SLproject[,-12], SLproject[,12], K=5)
# View(newData$data)

# after having balanced y 
# table(newData$data$class)


##################
## SMOTENC METHOD  - why does it work even if those variables are all factors?????
##################
# library(themis)
# class(SLproject$Loan_status) # unique factor
# class(SLproject$Married)
# class(SLproject$Dependents)

# BALANCING Y (=Loan_status)
# newData1 <- smotenc(SLproject, var = "Loan_status", k = 5, over_ratio = 1)

# after having balanced y
# table(newData1$Loan_status)


##################  
## ROSE METHOD  - we haven't understood well how it works: Coappl becames negative and loan_term changes
##################
# library(ROSE)

# BALANCING Y (=Loan_status)
# newData2 <- ROSE(Loan_status~., data=SLproject, seed=3)$data
# table(newData2$Loan_status)

# after having balanced y
# table(newData2$Loan_status)

##############
# DOWNSAMPLE
##############
# library(lattice)
# library(caret)
# newData3 <- downSample(SLproject, SLproject$Loan_status)
# str(newData3)  # class is a copy of Loan_status
# newData3 <- downSample(SLproject[,-12], SLproject$Loan_status, yname = "Loan_status")

# after having balanced y
# table(newData3$Loan_status)
# (newData3$Male)
#table(newData3$Married)
#table(newData3$Graduate)
#table(newData3$Self_empl)
#table(newData3$Guid_met)
#table(newData3$Dependents)
#table(newData3$Prop_area)

##############
# UPSAMPLE
##############
# newData4 <- upSample(SLproject, SLproject$Loan_status)
# str(newData4)  # class is a copy of Loan_status
#newData4 <- upSample(SLproject[,-12], SLproject$Loan_status, yname = "Loan_status")

# after having balanced y
#table(newData4$Loan_status)
#table(newData4$Male)
#table(newData4$Married)
#table(newData4$Graduate)
#table(newData4$Self_empl)
#table(newData4$Guid_met)
#table(newData4$Dependents)
#table(newData4$Prop_area)

# => We decided to implement an oversampling method as it balances the y 
# by increasing the number of observations in our dataset. It allows us to 
# avoid the loss of information. 
# newData4 and ROSE Or SMOTENC if can be used.




