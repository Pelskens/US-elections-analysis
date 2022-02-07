### 2020 Project: Analysis of Continuous Data & Statistical Modelling
##Contributors: Hamza Rarou & Katrien Meert

library(dplyr)
library(tidyr)
library(effects)
library(ggplot2)
library(corrplot)

### Data import & tidying ---------------------------------------------------
training_data_import <- read.csv(file.choose(), header = T) #the imported data is the final training dataset from SAS
validation_data_import <- read.csv(file.choose(), header = T) #the imported data is the final training dataset from SAS

training_data_import <- training_data_import %>% 
  mutate(focus=0) %>% 
  mutate(focus=case_when(GOP>=0.48 & GOP<= 0.52 ~1, 
                         GOP<0.48 | GOP>0.52 ~ 0)) 
validation_data_import <- validation_data_import %>% 
  mutate(focus=0) %>% 
  mutate(focus=case_when(GOP>=0.48 & GOP<= 0.52 ~1, 
                         GOP<0.48 | GOP>0.52 ~ 0)) 

#Final models 
final_model_noint <- read.csv(file.choose(), header = T) 
final_model_withint <- read.csv(file.choose(), header = T) 


final_model_noint <- final_model_noint %>% 
  mutate(focus=0) %>% 
  mutate(focus=case_when(GOP>=0.48 & GOP<= 0.52 ~1, 
                         GOP<0.48 | GOP>0.52 ~ 0)) %>%
 select(focus,GOP,Plus65, White, Bachelor, Travel, Poverty, PopDensity, LogPop, Income, LogIncome) 

View(final_model_noint)


final_model_withint <- final_model_withint %>% 
  mutate(focus=0) %>% 
  mutate(focus=case_when(GOP>=0.48 & GOP<= 0.52 ~1, 
                         GOP<0.48 | GOP>0.52 ~ 0)) %>% 
  select(-FIPS, -County, -Difference)

View(final_model_withint)



## Data exploration  -------------------------------------------------------
#no interactions
lapply(final_model_noint, function(x) {is.factor(x)} )
final_model_noint$focus <- factor(final_model_noint$focus)
summary(final_model_noint$focus)     #67 counties that are 'swing counties (with not filtering: 70)


#with  interactions
lapply(final_model_withint, function(x) {is.factor(x)} )
final_model_withint$focus <- factor(final_model_withint$focus)
summary(final_model_withint$focus) #65 counties that are 'swing counties (with not filtering: 70)


#Correlations
#no interactions
cor_noint<-lapply(final_model_noint, function(x) as.numeric((x)))
cor_noint <- as.data.frame(cor_noint)
cor_noint<-cor_noint%>%
  select(focus,Plus65,White, Bachelor, Travel, Poverty, PopDensity,Income)
View(cor(x=cor_noint$focus, y=cor_noint))
corrplot(cor(cor_noint))

#with interactions
cor_withint<-lapply(final_model_withint, function(x) as.numeric((x)))
cor_withint <- as.data.frame(cor_withint)
View(cor(x=cor_withint$focus, y=cor_withint))
corrplot(cor(cor_withint))


# Categorical data analysis -----------------------------------------------

## Model with only main predictor variable Income
glm_Inc<-glm(focus~Income, family = binomial, data = final_model_noint)
summary(glm_Inc)   #AIC 545
exp(glm_Inc$coefficients)

## Add Income to final model (no int)
glm_main <- glm(focus~White+Bachelor+LogPop+Poverty+Travel+Income, family = binomial, data = final_model_noint)
summary(glm_main) #AIC 535
exp(glm_main$coefficients)

plot(y=glm_main$fitted.values, x=final_model_noint$focus)
plot(x=final_model_noint$White, y=predict(glm_main, type="response"))
plot(x=final_model_noint$Bachelor, y=predict(glm_main, type="response"))

glm_0<-glm(focus~1,family=binomial, data = final_model_noint)
anova(glm_0,glm_main,test="Chisq")

##Back to final model from continuous (no int)
glm_final <- glm(focus~White+Bachelor+LogPop+Poverty+Travel, family = binomial, data = final_model_noint)
summary(glm_final) #AIC 533
exp(glm_final$coefficients)

#Siginificant deviations from zero. LogPop, Poverty and Travel not significant

glm_final_WB <- glm(focus~White+Bachelor, family = binomial, data = final_model_noint)
summary(glm_final_WB) #AIC 528
exp(glm_final_WB$coefficients)

anova(glm_0,glm_final_WB,test="Chisq")
anova(glm_final_WB,test="Chisq")

LL<-(glm_final_WB$coefficients[2]-1.96*0.007147)
UL<-(glm_final_WB$coefficients[2]+1.96*0.007147) 
exp(LL)
exp(UL)

diff<-(glm_final_WB$null.deviance-glm_final_WB$deviance)
df.diff<-(glm_final_WB$df.null-glm_final_WB$df.residual)
df.diff
1-pchisq(diff,df.diff)
anova(glm_final_WB,test="LRT")

## White and Bachelor are incorporated in the  final model. 

## 1. Choose a cut-off probability to predict swing county's. 
## 2. Are the predicted swing county's closer to the range 48% - 52% and thus closer to undecided as compared to the total data set? 

pred_prob<-predict(glm_final_WB,type="response")
pred_high<-pred_prob>0.06

sum(pred_high) #Cut-off of 0.06 results in a selection of 264 county's

GOP_high<-final_model_noint$GOP[pred_high==TRUE]
focus_high<-final_model_noint$focus[pred_high==TRUE]

summary(focus_high)             #26 of 264 county's are real swing county's, which is 10% (<--> 4% for random selection)
summary(GOP_high)               #37% - 54% 
summary(final_model_noint$GOP)  #56% - 64% 

## Interaction between White and bachelor?

glm_final_WBint <- glm(focus~White*Bachelor, family = binomial, weight=count, data = final_model_noint)
summary(glm_final_WBint) ##AIC of 519
exp(glm_final_WBint$coefficients)

anova(glm_final_WB,glm_final_WBint,test="Chisq")

plot(effect("White*Bachelor",glm_final_WBint))

##Do we see this effect if we look at the data?

plot(final_model_noint$White[final_model_noint$Bachelor>(20)],final_model_noint$focus[final_model_noint$Bachelor>(20)])
plot(final_model_noint$White[final_model_noint$Bachelor<(-8)],final_model_noint$focus[final_model_noint$Bachelor<(-8)])


pred_probint<-predict(glm_final_WBint,type="response")
pred_highint<-(pred_probint>0.06)
sum(pred_highint)

GOP_highint<-final_model_noint$GOP[pred_highint==TRUE]
focus_highint<-final_model_noint$focus[pred_highint==TRUE]

summary(GOP_highint)
summary(focus_highint)


##Final model but with complete data set (with "outliers")
glm_training <- glm(focus~White+Bachelor+LogPop+Poverty+Travel, family = binomial, data = training_data_import)
summary(glm_training)
exp(glm_training$coefficients)

#white and Bachelore in the model
glm_training_WB <- glm(focus~White + Bachelor, family = binomial, data = training_data_import)
summary(glm_training_WB)
exp(glm_training_WB$coefficients)


##What with  standardization?
final_model_noint$Whitec<-scale(final_model_noint$White)
final_model_noint$Bachelorc<-scale(final_model_noint$Bachelor)
mean(final_model_noint$Whitec)
hist(final_model_noint$Bachelor)
glm_WB_c<-glm(focus~Whitec + Bachelorc, family = binomial, data = final_model_noint)
summary(glm_WB_c)
exp(glm_WB_c$coefficients)

glm_WBint_c<-glm(focus~Whitec*Bachelorc, family = binomial, data = final_model_noint)
summary(glm_WBint_c)
exp(glm_WBint_c$coefficients)


# Validation of the data --------------------------------------------------

glm_val<-glm(focus~White+Bachelor,data=validation_data_import)
summary(glm_val)
exp(glm_val$coefficients)

#Predict the probabilities for a new data set (pi_val_estimate = own calculation; pp = with predict function; gives the same result)
pi_val_estimate<-(1+exp(-glm_final_WB$coefficients[1]-glm_final_WB$coefficients[2]*validation_data_import$White-
         glm_final_WB$coefficients[3]*validation_data_import$Bachelor))^-1
pp<-predict(glm_final_WB,newdata=validation_data_import,type="response")

plot(pi_val_estimate)

##Test several cutoff probabilities
focus_pred<-ifelse(pi_val_estimate>0.015,1,0)

total_focus_pred<-sum(focus_pred)
correct_focus_pred<-sum(validation_data_import$focus[focus_pred==1])
successrate<-correct_focus_pred/total_focus_pred
successrate

summary(validation_data_import$GOP[focus_pred==1])
summary(validation_data_import$GOP)


#Validation with interactions

glm_val_int<-glm(focus~White+Bachelor+ White*Bachelor,data=validation_data_import)
summary(glm_val_int)
exp(glm_val_int$coefficients)
plot(effect("White*Bachelor",glm_val_int))




