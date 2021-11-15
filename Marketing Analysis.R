####################################################################
############# Created by Team 5
############# MBAN2 HULT 2021
############# 11/13/21
############# Version 0.2
####################################################################

#reading dataset
library(readxl)
marketing <- read_excel("D:/Hult - MBAN/2 - Data Analytics/Team Project/datasets_marketing_campaign_SF.xlsx")
View(marketing)

#Analyzing Numerical variables

#validating NA
sapply(marketing, function(x) sum(is.na(x))) #Income 24
summary(marketing) #1 error in Income=666666

mkt <- marketing[-which(marketing$Income==666666),]

#obtaining the median of Income without the outlier
median_Income <- round(median(mkt$Income, na.rm=TRUE),0)

#for loop to change NA and outlier for the median
for (i in 1:nrow(marketing)){
  if (is.na(marketing$Income[i])) {
    marketing$Income[i] <- median_Income
  } else if (marketing$Income[i]==666666){
    marketing$Income[i] <- median_Income
  } else {}
} #closing the i loop

remove(mkt)

sapply(marketing, function(x) sum(is.na(x))) #No more NA

#Analyzing Categorical variables
table(marketing$Education) #Basic has the lowest frequency, we can remove it after create the dummy variables
table(marketing$Country) #ME has the lowest frequency, we can remove it after create the dummy variables
table(marketing$Marital_Status) #Unifying Absurd, Alone, Yolo w/ Single

marketing$MS_Final <- marketing$Marital_Status

for (i in 1:nrow(marketing)){
  if (marketing$Marital_Status[i] %in% c("Absurd", "Alone", "YOLO")) {
    marketing$MS_Final[i] <- "Single"
  } else {}
}

#Analyzing date variables
marketing$Dt_Customer <- as.Date(marketing$Dt_Customer, "%Y-%m-%d")

#Creating new relevant variables
marketing$campaign <- c()

for (i in 1:nrow(marketing)){
  if (marketing$AcceptedCmp1[i] + marketing$AcceptedCmp2[i] 
      + marketing$AcceptedCmp3[i] + marketing$AcceptedCmp4[i]
      + marketing$AcceptedCmp5[i]>0 ) {
    marketing$campaign[i]<-1
  } else {marketing$campaign[i]<-0}
}

marketing$Days_customer <- as.numeric(difftime(max(marketing$Dt_Customer)+1, marketing$Dt_Customer))
marketing$total_purchases <- marketing$NumWebPurchases + marketing$NumCatalogPurchases + marketing$NumStorePurchases
marketing$age <- as.numeric(format(max(marketing$Dt_Customer), format="%Y")) - marketing$Year_Birth

#Creating a new data frame with the variables that we want to convert into dummies
mkt_df <- marketing

#eliminating the variables that we are not going to use
mkt_df[,c("ID","Year_Birth","Dt_Customer","Marital_Status", "Z_CostContact","Z_Revenue")] <- list(NULL)

#creating dummies variables for our characters variables
#install.packages("fastDummies")
library(fastDummies)

mkt_final <- as.data.frame(fastDummies::dummy_cols(mkt_df))

#removing one dummy variable for each categorical variable
mkt_final[,c("Education_Basic","Country_ME", "MS_Final_Widow","Education","Country","MS_Final")] <- list(NULL) #these variables are the lowest values 
#mkt_final is our final data frame 

######QUESTIONS

#Q1: What features are significantly related to the number of web purchases? 
#What would you suggest to CMO to improve web purchases?

#Dependent variable: NumWebPurchases
#creating histograms per each variable to see their distributions

for (i in 1: ncol(mkt_final)){
  hist(mkt_final[,i], main= names(mkt_final[i]), xlab="")
}

#Insights:
#NumWebPurchases: Exponential distribution and presents outliers (>20)
#Income presents outliers (higher than $150,000)

#creating plots Y vs each independent variable to see if they have linear patterns
library(ggplot2)
#Plot Income vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20 | mkt_final$Income>150000),], aes(x=Income, y=NumWebPurchases)) + geom_point()
##linear pattern

#Plot Recency vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=Recency, y=NumWebPurchases)) + geom_point() 
##No relationship

#Plot MntWines vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=MntWines, y=NumWebPurchases)) + geom_point() 
##curve pattern: Transformation

ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=log(MntWines+1), y=log(NumWebPurchases+1))) + geom_point() 
##linear pattern

#Plot MntFruits vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=MntFruits, y=NumWebPurchases)) + geom_point() 
##No relationship

#Plot MntMeatProducts vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=MntMeatProducts, y=NumWebPurchases)) + geom_point() 
##curve pattern: Transformation

ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=log(MntMeatProducts+1), y=log(NumWebPurchases+1))) + geom_point() 
##linear pattern

#Plot MntFishProducts vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=MntFishProducts, y=NumWebPurchases)) + geom_point() 
##No relationship

#Plot MntSweetProducts vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=MntSweetProducts, y=NumWebPurchases)) + geom_point() 
##No relationship

#Plot MntGoldProds vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=MntGoldProds, y=NumWebPurchases)) + geom_point() 
##No relationship

#Plot NumDealsPurchases vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=NumDealsPurchases, y=NumWebPurchases)) + geom_point() 
##No relationship

#Plot NumCatalogPurchases vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=NumCatalogPurchases, y=NumWebPurchases)) + geom_point() 
##No relationship

#Plot NumStorePurchases vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=NumStorePurchases, y=NumWebPurchases)) + geom_point() 
##curve pattern: Transformation

ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=log(NumStorePurchases+1), y=log(NumWebPurchases+1))) + geom_point() 
##linear pattern

#Plot NumWebVisitsMonth vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=NumWebVisitsMonth, y=NumWebPurchases)) + geom_point() 
##curve pattern: Transformation

ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=log(NumWebVisitsMonth+1), y=log(NumWebPurchases+1))) + geom_point() 
##linear pattern

#Plot Days_customer vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=Days_customer, y=NumWebPurchases)) + geom_point() 
##No relationship

#Plot total_purchases vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=total_purchases, y=NumWebPurchases)) + geom_point() 
##linear pattern

ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20),], aes(x=total_purchases, y=log(NumWebPurchases+1))) + geom_point() 
##linear pattern (converting log Number just to have the same Y variable for every variable transformed)

#Plot age vs NumWebPurchases (without outliers)
ggplot(data=mkt_final[-which(mkt_final$NumWebPurchases>20 | mkt_final$age>100),], aes(x=age, y=NumWebPurchases)) + geom_point() 
##No relationship

#####Finish of plotting numerical independent variables vs Y (dependent)

#Transforming variables

mkt_final$LogNumWeb <- log(mkt_final$NumWebPurchases+1)
mkt_final$LogMntWines <- log(mkt_final$MntWines+1)
mkt_final$LogMntMeat <- log(mkt_final$MntMeatProducts+1)
mkt_final$LogStorePurch <- log(mkt_final$NumStorePurchases+1)
mkt_final$LogNumWebVisits <- log(mkt_final$NumWebVisitsMonth+1)

web.lm <- lm(LogNumWeb ~ Income+ LogMntWines+
              LogMntMeat+ LogStorePurch+
               LogNumWebVisits+
               total_purchases, data = mkt_final)
summary(web.lm)
plot(summary(web.lm)$residuals)

#Including dummy variables
#Attempt 1
web.lm.dum1 <- lm(LogNumWeb ~ Income+ LogMntWines+
                  LogMntMeat+ LogStorePurch+
                  LogNumWebVisits+
                  total_purchases+
                  Kidhome+ Teenhome+
                  AcceptedCmp1+ AcceptedCmp2+
                  AcceptedCmp3+ AcceptedCmp4+ 
                  AcceptedCmp5+ Complain+ Response+ campaign+
                 `Education_2n Cycle`+ Education_Graduation+
                   Education_Master+ Education_PhD+ 
                   Country_AUS+ Country_CA+ Country_GER+
                   Country_IND+ Country_SA+ Country_SP+
                   Country_US+ MS_Final_Divorced+ MS_Final_Married+
                   MS_Final_Single+ MS_Final_Together
               , data = mkt_final)

summary(web.lm.dum1)

#Attempt 2: eliminating variables not significant
web.lm.dum2 <- lm(LogNumWeb ~ Income+ LogMntWines+
                    LogMntMeat+ LogStorePurch+
                    LogNumWebVisits+
                    total_purchases+
                    Kidhome+ 
                    AcceptedCmp2+
                    AcceptedCmp3+  
                    `Education_2n Cycle`+ Education_Graduation+
                    Education_Master+ Education_PhD
                  , data = mkt_final)

summary(web.lm.dum2) #all the variables significant and R2 adjusted= 0.7627

plot(summary(web.lm.dum2)$residuals) #residuals ok

#b.Does US fare significantly better than RoW (Rest of the World) in terms of total purchases. 

#Creating a linear regression to see if the variable Country_US is significant for total purchases
purch.lm <- lm(total_purchases ~ Country_US, data=mkt_final)

summary(purch.lm)

plot(summary(purch.lm)$residuals)

#c. Your supervisor insists that people who buy gold are more conservative and 
#as such people who spent an above average amount on gold in last 2 years would have more 
#in store purchases. Statistical tests

#c.1. Testing independence of counts: People who buy gold > Avg vs People who buy in stores

avg_gold <- mean(mkt_final$MntGoldProds, na.rm= TRUE)

mkt_final$More_Avg_Gold <- c()

for (i in 1:nrow(mkt_final)){
  if(mkt_final$MntGoldProds[i]>avg_gold){
    mkt_final$More_Avg_Gold[i] <- 1
  } else {mkt_final$More_Avg_Gold[i] <- 0}
}

mkt_final$Store <- c()

for (i in 1:nrow(mkt_final)){
  if(mkt_final$NumStorePurchases[i]<3){
    mkt_final$Store[i] <- "1. [0-2]"
  } else if (mkt_final$NumStorePurchases[i]<6){
    mkt_final$Store[i] <- "2. [3-5]"
  } else if (mkt_final$NumStorePurchases[i]<9){
    mkt_final$Store[i] <- "3. [6-8]"
  } else if (mkt_final$NumStorePurchases[i]<12){
    mkt_final$Store[i] <- "4. [9-11]"
  } else {mkt_final$Store[i] <- "5. [12-13]"}
}

contingency_table <- table(mkt_final$Store, mkt_final$More_Avg_Gold)

#install.packages("summarytools")
library(summarytools)

ctable(x=mkt_final$Store,
       y=mkt_final$More_Avg_Gold,
       chisq=TRUE,
       headings=FALSE)

library(graphics)

mosaicplot(contingency_table, shade=TRUE, main="Store Purchases - Avg. Amount Gold")

#chi-squared=375.86 (Reject H0: The variables Store and Amount of Gold are independent)

#c.2. Testing if the average of number of visits for people who buy gold more than the average
#is higher than the average of number of visits for people who buy gold less than the average

#UDF: T-testing for a comparison 
t_testing <- function(table1, table2, var1, var2, D){
  X1 <- mean(table1[,var1], na.rm=TRUE) 
  X2 <- mean(table2[,var2], na.rm=TRUE) 
  n1 <- nrow(table1)
  n2 <- nrow(table2)
  S1 <- sd(table1[,var1], na.rm=TRUE)
  S2 <- sd(table2[,var2], na.rm=TRUE)
  t <- (X1-X2-D)/(((S1^2)/n1 + (S2^2)/n2)^0.5)
  return(t)
}

t_testing(mkt_final[which(mkt_final$MntGoldProds>=avg_gold),],
          mkt_final[which(mkt_final$MntGoldProds<avg_gold),],
          14,#number of column: number store purchases
          14,#number of column: number store purchases
          0)

#t=20.89134 Reject H0: There is not enough evidence that the average of visits at the store is going to 
#increase if people buy an amount of gold higher than the average

#c.3. Testing if the average of total purchases for people who buy gold more than the average
#is higher than the average of total purchases for people who buy gold less than the average

mkt_final$total_amount_spent <- mkt_final$MntWines+ mkt_final$MntFruits+
                                mkt_final$MntMeatProducts+ mkt_final$MntFishProducts+
                                mkt_final$MntFishProducts+ mkt_final$MntSweetProducts+
                                mkt_final$MntGoldProds

t_testing(mkt_final[which(mkt_final$MntGoldProds>=avg_gold),],
          mkt_final[which(mkt_final$MntGoldProds<avg_gold),],
          49,#number of column: total amount spent
          49,#number of column: total amount spent
          0)

#t=27.0355 Reject H0: There is not enough evidence that the average of amount spent is going to 
#increase if people buy an amount of gold higher than the average

#d.Fish has Omega 3 fatty acids, good for brain, accordingly, do "Married PhD candidates" 
#have a significant relation with amount spent on fish? 
#What other factors are significantly related to amount spent on fish?

#Creating a Regression with the variables : Education PHD, MS Married and both (interaction)

mkt_final$Married_PhD <- mkt_final$MS_Final_Married * mkt_final$Education_PhD

#defining the Y response variable

# Y = Amount spend on Fish
fish.lm <- lm(MntFishProducts ~ MS_Final_Married+ Education_PhD+ Married_PhD
               , data = mkt_final)

summary(fish.lm)

# Y = Percentage of the total amount spent on Fish

mkt_final$perc_fish <- mkt_final$MntFishProducts/mkt_final$total_amount_spent

perc.fish.lm <- lm(perc_fish ~ MS_Final_Married+ Education_PhD+ Married_PhD
              , data = mkt_final)

summary(perc.fish.lm)

# Y = Percentage of Income spent on Fish

mkt_final$inc_fish <- mkt_final$MntFishProducts/mkt_final$Income

inc.fish.lm <- lm(inc_fish ~ MS_Final_Married+ Education_PhD+ Married_PhD
                   , data = mkt_final)

summary(inc.fish.lm)

#e. Do any other analysis you deem relevant to show to your CMO. 
#For the purpose, propose a hypothesis and perform the appropriate tests. 

#Which variables impact on accepting a campaign (Business success=1, failure=0)
#creating training and testing data using Stratified Sampling

# Normalizing these numerical variables
#creating a UDF 
normal_data <- function(vector){
  vector_normal <- (vector-min(vector, na.rm=TRUE))/(max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))
  return(vector_normal)
}#closing UDF

#creating the new normalized variables
mkt_final$Income_norm <- normal_data(mkt_final[,1])
mkt_final$Kidhome_norm <- normal_data(mkt_final[,2])
mkt_final$Teenhome_norm <- normal_data(mkt_final[,3])
mkt_final$CatalogPurch_norm <- normal_data(mkt_final[,13])
mkt_final$WebVisits_norm <- normal_data(mkt_final[,15])
mkt_final$Days_customer_norm <- normal_data(mkt_final[,24])

library(splitstackshape)
training_testing <- stratified(as.data.frame(mkt_final), group=23, size=0.8, bothSets = T)

train_ss <- training_testing$SAMP1 #this is my training data
test_ss <- training_testing$SAMP2 #this is my testing data

#Attempt 1
my_logit <- glm(campaign ~ Income+ Kidhome+ Teenhome+
                     Recency+ NumDealsPurchases+ NumWebPurchases+
                     NumCatalogPurchases+ NumStorePurchases+ NumWebVisitsMonth+
                     Complain+ Days_customer+ total_purchases+
                     age+ `Education_2n Cycle`+ Education_Graduation+
                     Education_Master+ Education_PhD+ 
                     Country_AUS+ Country_CA+ Country_GER+
                     Country_IND+ Country_SA+ Country_SP+
                     Country_US+ MS_Final_Divorced+ MS_Final_Married+
                     MS_Final_Single+ MS_Final_Together
                  , data = train_ss, family= "binomial")

summary(my_logit)

#Attempt 2: eliminating variables not significant
my_logit <- glm(campaign ~ Income+ Kidhome+ Teenhome+ 
                    NumCatalogPurchases+
                    NumWebVisitsMonth+ Days_customer
                  , data = train_ss, family= "binomial")

summary(my_logit)

my_summary <- summary(my_logit)$coef

100*(exp(my_summary[2,1])-1) #coef: Income
###insight: one more dollar in the income increases the odds of success in 0.004%
100*(exp(my_summary[3,1])-1) #coef: Kidhome 
####insight: one more kid at home decreases the odds of success in 38%
100*(exp(my_summary[4,1])-1)#coef: Teenhome
####insight: one more teen at home decreases the odds of success in 49%
100*(exp(my_summary[5,1])-1) #coef: NumCatalogPurchases
####insight: one more purchase in the calatog increases the odds of success in 17%
100*(exp(my_summary[6,1])-1) #coef: NumWebVisitsMonth
###insight: one more vistis on the web increases the odds of success in 37%
100*(exp(my_summary[7,1])-1) #coef: Days_customer
###insight: one more day as customer decreases the odds of success in 0.14%

#Logistic regression normalized
my_logit_norm <- glm(campaign ~ Income_norm+ Kidhome_norm+ Teenhome_norm+ 
                  CatalogPurch_norm+ 
                  WebVisits_norm+ Days_customer_norm
                , data = train_ss, family= "binomial")

summary(my_logit_norm)$coef

###insight: the variables that have a higher impact for our Business Success (Accept a campaign)
###are Income, web visits, and number of purchase in catalog (all in a positive way)

library(caret)
library(ROCR)

my_prediction_testing <- predict(my_logit, test_ss, type="response")

my_prediction_testing

confusionMatrix(data= as.factor(as.numeric(my_prediction_testing > 0.5)), 
                reference= as.factor(as.numeric(test_ss$campaign)))
#Accuracy: 0.817 95%CI = (0.778,0.8517)

##validating the model with the training data
my_prediction_train <- predict(my_logit, train_ss, type="response")

my_prediction_train

confusionMatrix(data= as.factor(as.numeric(my_prediction_train > 0.5)), 
                reference= as.factor(as.numeric(train_ss$campaign)))
#Accuracy: 0.803 95%CI = (0.7838,0.8212)
###insight: stable model

####Hypothesis: Testing the variable with high impact
#People who accept a campaign depends on the income
#Inference for counts: Range of Income vs People who accept a campaign

percentiles <- quantile(mkt_final$Income, c(.25, .50, .75))

percentiles #35K 50K 70K round numbers

mkt_final$Range_Income <- c() #4 ranges: percentiles

for (i in 1:nrow(mkt_final)){
  if(mkt_final$Income[i]<35000){
    mkt_final$Range_Income[i] <- "1. [0-35K>"
  } else if (mkt_final$Income[i]<50000){
    mkt_final$Range_Income[i] <- "2. [35K-50K>"
  } else if (mkt_final$Income[i]<70000){
    mkt_final$Range_Income[i] <- "3. [50K-70K>"
  } else {mkt_final$Range_Income[i] <- "4. [70K-+>"}
}

ct_income <- table(mkt_final$Range_Income, mkt_final$campaign)

#install.packages("summarytools")
library(summarytools)

ctable(x=mkt_final$Range_Income,
       y=mkt_final$campaign,
       chisq=TRUE,
       headings=FALSE)

library(graphics)

mosaicplot(ct_income, shade=TRUE, main="Income - Campaign")

#chi-squared=264.1104 (Reject H0: The variables Income and Campaign are dependent)
