library(readxl)
library(dplyr)
library(AER)
library(plm)
library(stargazer)
#loading the dataset
pd<-read_excel("C:/Users/uppad/Desktop/pan_dat.xlsx")
pd$realIK=(pd$realIK)*100
pd$real_fk=(pd$real_fk)*100
View(pd)


## Since Cobb-Douglas Production is of the form Q=A*(L^a)*(K^b)*(M^c)
# taking log on both sides we get the linear form of the above equation as log Q=log A+a log L+b log K+c log M
#Thus our panel regression comes in the form Yit=intercept+ (beta 1)*X1+(beta 2)*X2+(beta 3)*X3+uit
#Where Yit=Real GVA, X1=real capital,X2=workers,X3=real materials,uit=white noise
pd$X1=log(pd$realIK)
pd$X2=log(pd$workers)
pd$X3=log(pd$realmaterial)
pd$Yit=log(pd$realgva)

# After taking logarithms of the variables required we create our panel dataset
# by selecting the necessary variables from the original dataset.
pd_reg<-pd%>%select(yearcode,stateid,X1,X2,X3,Yit)
View(pd_reg)
library(gplots)
# we check heterogenity in the GVA values across the states and across the years
plotmeans(Yit~stateid,main="Heterogeneity Across States",ylab="GVA",xlab="States",data=pd_reg)
plotmeans(Yit~yearcode,main="Heterogeneity Across Years",ylab="GVA",xlab="Year",data=pd_reg)



##  PANEL DATA MODELLING
# Fixed Effect Dummy variable model coefficients are estimated using lm() function.
FE.LSDV<-lm(Yit~X1+X2+X3+factor(stateid)-1,data=pd_reg)
t<-summary(FE.LSDV)
t
#Fixed Effect Within Group model coefficients are estimated using plm() function
FE.WG<-plm(Yit~X1+X2+X3,data=pd_reg,index=c("stateid","yearcode"),model="within")
summary(FE.WG)
#Random Effect Model coefficients are estimated
RE<-plm(Yit~X1+X2+X3,data=pd_reg,index=c("stateid","yearcode"),model="random")
summary(RE)
# Pooled OLS model coeff are estimated
pooled<-plm(Yit~X1+X2+X3,data=pd_reg,index=c("stateid","yearcode"),model="pooling")
summary(pooled)



# Hausman test is performed to check whether there is a significant difference in coefficients coming from
# the fixed and random effect model.
# H0:No significant difference is detected=> Random Effect Model can be selected provisionally
# H1: significant difference is found=> Fixed Effect model is to be selected
haus.test<-phtest(FE.WG,RE)
haus.test


#Breusch Pagan test is performed to check whether panel data model is required or not.
# H0: Pooled OLS model is to be selected
# H1: Random effects models is to be selected
plmtest(pooled,type=c("bp"))



fixed.time<-plm(Yit~X1+X2+X3+factor(yearcode),data=pd_reg,index=c("stateid","yearcode"),model="within")
summary(fixed.time)
## Testing for time fixed effects
pFtest(fixed.time,pooled)
## Testing for individual fixed effects
pFtest(FE.WG,pooled)

##not a necessary part so skip it and dont ask questions!!
df<-coef(summary(FE.LSDV))
as.data.frame(df)
write.csv(df,"C:/Users/uppad/Desktop/fix_coef.csv")


# Now considering the FE coefficients as a measure of productivity
# We run an OLS regression considering productivity as the dependant variable and Labour Share,skill and PCNSDP as the regressors.
# This is done to analyze which statewise factors contribute to the productivity
reg_data<-read_excel("C:/Users/uppad/Desktop/ols.reg.data.xlsx")
reg_data$Cint<-(reg_data$Cint)*100
productivity_reg<-lm(Productivity~Cint+Skill+pcnsdp,data=reg_data)
summary(productivity_reg)




stargazer(FE.LSDV,type="html",title="TABLE NO 2: LSDV MODEL",style="all",model.names=FALSE,summary=TRUE,covariate.labels=c("Capital Input","Workers Input","Material Input",
          "Andhra Pradesh","Goa","Gujarat","Haryana","Himachal Pradesh","Jharkhand","Karnataka","Kerala"
          ,"Madhya Pradesh","Maharashtra","Odisha","Punjab","Rajasthan"
          ,"Tamil Nadu","Uttar Pradesh","Uttarakhand","West Bengal"),out="LSDV_new.html",intercept.bottom=FALSE,flip=TRUE)


stargazer(FE.WG,type="html",title="TABLE NO 3: FIXED EFFECT MODEL",style="all",model.names=FALSE,summary=TRUE,covariate.labels=c("Capital Input","Workers Input","Material Input"
                                                                                                                           ),out="fixed_new.html",intercept.bottom=FALSE,flip=TRUE)
stargazer(RE,type="html",title="TABLE NO 4: RANDOM EFFECT MODEL",style="all",model.names=FALSE,summary=TRUE,covariate.labels=c("Intercept","Capital Input","Workers Input","Material Input"),
          out="random_new.html",intercept.bottom = FALSE)

stargazer(pooled,type="html",title="TABLE NO 5: POOLED OLS MODEL",style="all",model.names=FALSE,summary=TRUE,covariate.labels=c("Intercept","Capital Input","Workers Input","Material Input"),
          out="POLS_new.html",intercept.bottom = FALSE)




yhat=predict(FE.LSDV)
scatterplot(yhat~pd_reg$X1|pd_reg$stateid,boxplots=FALSE,smooth=FALSE,main="Output Versus Capital Input",xlab="capital input",ylab="GVA")
abline(lm(yhat~pd_reg$X1,data=pd_reg),lwd=3,col="red")




                                                                                                                                 


