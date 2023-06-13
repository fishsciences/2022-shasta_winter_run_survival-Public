####  Winter Run JPI Modeling ####
# 
# Initial linear analysis of JPI for winter run. Potential factors include temperature, flow, and females.
# 1.) Exploratory data review
# 2.) Lasso models for variable selection
# 3.) Poisson models for selected models
# 4.) Output
#
# Author: Kai Ross, Cramer Fish Sciences.
# Date Modified: 4/12/2023

# Required packages --------------
library(openxlsx) # Read xlsx files
library(ggplot2) # Figures
library(MASS) #BIC, glm.nb
library(glmnet) # For lasso models
library(sjPlot) #HTML Model outputs
library(MuMIn) #AICc

#Read in data -----------
mdat=readWorkbook('Winter_run_data_and_covariates_4_10_23.xlsx',sheet='Data')[,-11]
mdat=mdat[,-11]# remove last column

# 1.) Exploratory plots --------------------
ggplot()+geom_point(aes(x=mdat$Females,y=mdat$JPI,color=mdat$Year))

mdat2=reshape2::melt(mdat,id='JPI')
ggplot(data=mdat2)+geom_point(aes(x=value,y=JPI))+facet_wrap(.~variable,scales='free')

# Look at correlation
cor(mdat$Flow_IE,mdat$Total_discarge_IE)
plot(mdat$Flow_IE,mdat$Total_discarge_IE)

cor(mdat$Temp_RB_M,mdat$Temp_SAC_I)
plot(mdat$Temp_RB_M,mdat$Temp_SAC_I)

# 2.) LASSO Model for variable importance ----------------
#Build Lasso model
mdat3=mdat[,-1] # remove year as predictor variable
x_train = model.matrix(JPI~1+., mdat3) # Set model structure
lasso_model = cv.glmnet(x=x_train,y = mdat3$JPI, family = "gaussian", alpha=1, nfolds=5,intercept=TRUE) # find lambda
lambda_b = lasso_model$lambda.min # set lambda

#Explore alternate lambda values to see what other parameters would be included next
# lambda_b = 902637.1
# lambda_b = 312637.1 
# lambda_b = 102637.1
# lambda_b = 52637.1

# Fit to data
model_b = glmnet(x_train, mdat3$JPI, alpha = 1, lambda = lambda_b, standardize = TRUE,family="gaussian",intercept=TRUE)
# Review results
print(model_b)
print(coef(model_b))

# 3.) Create Poisson Models ---------------
m1=glm.nb(JPI~ Females + Flow_IE + Flow_CV_M + Temp_SAC_I,data=mdat,link='sqrt',init.theta=5)
m2=glm.nb(JPI~ Females + Flow_IE,data=mdat,link='sqrt',init.theta=5)
m3=glm.nb(JPI~ Females + Flow_CV_M,data=mdat,link='sqrt',init.theta=5)
m4=glm.nb(JPI~ Females + Flow_IE + Flow_CV_M,data=mdat,link='sqrt',init.theta=5)
m5=glm.nb(JPI~ Females + Temp_SAC_I,data=mdat,link='sqrt',init.theta=5)
m6=glm.nb(JPI~ Females + Flow_CV_M + Temp_SAC_I,data=mdat,link='sqrt',init.theta=5)
m7=glm.nb(JPI~ Females + Flow_IE + Temp_SAC_I,data=mdat,link='sqrt',init.theta=5)

# Look at summaries 
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)

# Compare BIC scores
AIC(m1,m2,m3,m4,m5,m6,m7)
AICc(m1,m2,m3,m4,m5,m6,m7)
BIC(m1,m2,m3,m4,m5,m6,m7)

#look at LOOCV
kloocv = function(model) {
  n = nrow(model$model)
  ypred = numeric(n)
  R2_vals = numeric(n)
  for (i in 1:n) {
    # Leave out observation i
    ndat=model$model[-i,]
    
    # Fit the model without observation i
    fit = glm.nb(JPI ~ .,data=ndat,link='sqrt',init.theta=5)
    
    # Predict the left-out observation
    ypred[i]= predict(fit, newdata = model$model[i, -1])^2
  }
  r2=cor(model$model$JPI,ypred)^2
  rmse=sqrt(mean( (model$model$JPI-ypred)^2 ) )
  return(list(R2=r2,RMSE=rmse))
}

#review results
kloocv(m1)
kloocv(m2)
kloocv(m3)
kloocv(m4)
kloocv(m5)
kloocv(m6)
kloocv(m7)

# 4.) Output ------------
# Summaries for selected models


sink("model_results_wM1.txt")
summary(m2)
summary(m4)
summary(m7)
summary(m1)
sink()

write.csv(confint(m1),'m1_confint.csv')
write.csv(confint(m2),'m2_confint.csv')
write.csv(confint(m4),'m4_confint.csv')
write.csv(confint(m7),'m7_confint.csv')



#tab_model(m2,file="M2-Summary-Sqrt_NB.html",p.style = "numeric_stars",digits = 2,show.stat=T)
#tab_model(m4,file="M4-Summary-Sqrt_NB.html",p.style = "numeric_stars",digits = 2,show.stat=T)
#tab_model(m7,file="M7-Summary-Sqrt_NB.html",p.style = "numeric_stars",digits = 2,show.stat=T)

# Create prediction comparison plots
# By Year
ggplot()+labs(col=element_blank(),y='JPI',x='Year')+
  geom_point(aes(x=mdat$Year,y=(mdat$JPI),color='Observed'),size=12,shape='-')+
  geom_point(aes(x=mdat$Year+.25,y=(predict(m2)^2),color='M2'))+
  geom_segment(aes(x=mdat$Year+.25,y=mdat$JPI,xend=mdat$Year+.25,yend=(predict(m2)^2),color='M2'))+
  geom_point(aes(x=mdat$Year-.25,y=(predict(m4)^2),color='M4'))+
  geom_segment(aes(x=mdat$Year-.25,y=mdat$JPI,xend=mdat$Year-.25,yend=(predict(m4)^2),color='M4'))+
  geom_point(aes(x=mdat$Year,y=(predict(m7)^2),color='M7'))+
  geom_segment(aes(x=mdat$Year,y=mdat$JPI,xend=mdat$Year,yend=(predict(m7)^2),color='M7'))
ggsave('Prediction_by_year_Sqrt_NB.png',height=3,width=6.5,units='in',dpi=600)

# By JPI
ggplot()+labs(col=element_blank(),y='JPI',x='JPI')+
  geom_point(aes(x=mdat$JPI,y=(mdat$JPI),color='Observed'),size=12,shape='-')+
  geom_point(aes(x=mdat$JPI+200000,y=(predict(m2)^2),color='M2'))+
  geom_segment(aes(x=mdat$JPI+200000,y=mdat$JPI,xend=mdat$JPI+200000,yend=(predict(m2)^2),color='M2'))+
  geom_point(aes(x=mdat$JPI-200000,y=(predict(m4)^2),color='M4'))+
  geom_segment(aes(x=mdat$JPI-200000,y=mdat$JPI,xend=mdat$JPI-200000,yend=(predict(m4)^2),color='M4'))+
  geom_point(aes(x=mdat$JPI,y=(predict(m7)^2),color='M7'))+
  geom_segment(aes(x=mdat$JPI,y=mdat$JPI,xend=mdat$JPI,yend=(predict(m7)^2),color='M7'))
ggsave('Prediction_plot_Sqrt_NB.png',height=3,width=6.5,units='in',dpi=600)






