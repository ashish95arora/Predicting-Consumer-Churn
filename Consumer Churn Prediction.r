setwd("C:\\Users\\Ashish Arora\\Desktop\\logistic reg case study")


mydata<-read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv",header=T)

#View(mydata)
#str(mydata)


mydata$CHURN<- factor(mydata$CHURN)
mydata$CSA<- factor(mydata$CSA)
mydata$CHILDREN<- factor(mydata$CHILDREN)
mydata$CREDITA<- factor(mydata$CREDITA)
mydata$CREDITAA<- factor(mydata$CREDITAA)
mydata$CREDITB<- factor(mydata$CREDITB)
mydata$CREDITC<- factor(mydata$CREDITC)
mydata$CREDITDE<- factor(mydata$CREDITDE)
mydata$CREDITGY<- factor(mydata$CREDITGY)
mydata$CREDITZ<- factor(mydata$CREDITZ)
mydata$PRIZMRUR<- factor(mydata$PRIZMRUR)
mydata$PRIZMUB<- factor(mydata$PRIZMUB)
mydata$PRIZMTWN<- factor(mydata$PRIZMTWN)
mydata$REFURB<- factor(mydata$REFURB)
mydata$WEBCAP<- factor(mydata$WEBCAP)
mydata$TRUCK<- factor(mydata$TRUCK)
mydata$RV<- factor(mydata$RV)
mydata$OCCPROF<- factor(mydata$OCCPROF)
mydata$OCCCLER<- factor(mydata$OCCCLER)
mydata$OCCCRFT<- factor(mydata$OCCCRFT)
mydata$OCCSTUD<- factor(mydata$OCCSTUD)
mydata$OCCHMKR<- factor(mydata$OCCHMKR)
mydata$OCCRET<- factor(mydata$OCCRET)
mydata$OCCSELF<- factor(mydata$OCCSELF)
mydata$OWNRENT<- factor(mydata$OWNRENT)
mydata$MARRYUN<- factor(mydata$MARRYUN)
mydata$MARRYYES<- factor(mydata$MARRYYES)
mydata$MARRYNO<- factor(mydata$MARRYNO)
mydata$MAILORD<- factor(mydata$MAILORD)
mydata$MAILRES<- factor(mydata$MAILRES)
mydata$MAILFLAG<- factor(mydata$MAILFLAG)
mydata$TRAVEL<- factor(mydata$TRAVEL)
mydata$PCOWN<- factor(mydata$PCOWN)
mydata$CREDITCD<- factor(mydata$CREDITCD)
mydata$NEWCELLY<- factor(mydata$NEWCELLY)
mydata$NEWCELLN<- factor(mydata$NEWCELLN)
mydata$INCMISS<- factor(mydata$INCMISS)
mydata$MCYCLE<- factor(mydata$MCYCLE)
mydata$SETPRCM<- factor(mydata$SETPRCM)
mydata$RETCALL<- factor(mydata$RETCALL)
mydata$CALIBRAT<- factor(mydata$CALIBRAT)
mydata$CHURNDEP<- factor(mydata$CHURNDEP)

var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

#Vector of numaerical variables
num_var= sapply(mydata,is.numeric)
Other_var= !sapply(mydata,is.numeric)

#Applying above defined function on numerical variables
my_num_data<-t(data.frame(apply(mydata[num_var], 2, var_Summ)))
my_cat_data<-data.frame(t(apply(mydata[Other_var], 2, var_Summ)))



# Number of missing values
apply(is.na(mydata[,]),2,sum)

#Missing Value Treatment
mydata$REVENUE[is.na(mydata$REVENUE)]<-0
mydata$MOU[is.na(mydata$MOU)]<-0
mydata$RECCHRGE[is.na(mydata$RECCHRGE)]<-0
mydata$DIRECTAS[is.na(mydata$DIRECTAS)]<-0
mydata$OVERAGE[is.na(mydata$OVERAGE)]<-0
mydata$ROAM[is.na(mydata$ROAM)]<-0
mydata$CHANGEM[is.na(mydata$CHANGEM)]<-0
mydata$CHANGER[is.na(mydata$CHANGER)]<-0
mydata$AGE1[is.na(mydata$AGE1)]<-0
mydata$AGE2[is.na(mydata$AGE2)]<-0
mydata$PHONES[is.na(mydata$PHONES)]<-0
mydata$MODELS[is.na(mydata$MODELS)]<-0
mydata$EQPDAYS[is.na(mydata$EQPDAYS)]<-0


mydata$REVENUE[is.na(mydata$REVENUE)]<-mean(mydata$REVENUE)
mydata$MOU[is.na(mydata$MOU)]<-mean(mydata$MOU)
mydata$RECCHRGE[is.na(mydata$RECCHRGE)]<-mean(mydata$RECCHRGE)
mydata$DIRECTAS[is.na(mydata$DIRECTAS)]<-mean(mydata$DIRECTAS)
mydata$OVERAGE[is.na(mydata$OVERAGE)]<-mean(mydata$OVERAGE)
mydata$ROAM[is.na(mydata$ROAM)]<-mean(mydata$ROAM)
mydata$CHANGEM[is.na(mydata$CHANGEM)]<-mean(mydata$CHANGEM)
mydata$CHANGER[is.na(mydata$CHANGER)]<-mean(mydata$CHANGER)
mydata$AGE1[is.na(mydata$AGE1)]<-mean(mydata$AGE1)
mydata$AGE2[is.na(mydata$AGE2)]<-mean(mydata$AGE2)
mydata$PHONES[is.na(mydata$PHONES)]<-mean(mydata$PHONES)
mydata$MODELS[is.na(mydata$MODELS)]<-mean(mydata$MODELS)
mydata$EQPDAYS[is.na(mydata$EQPDAYS)]<-mean(mydata$EQPDAYS)




M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
mydata[,"OPEAKVCE"] <- M1_fun( mydata[,"OPEAKVCE"] )
mydata[,"MOUREC"] <- M1_fun( mydata[,"MOUREC"] )
mydata[,"OUTCALLS"] <- M1_fun( mydata[,"OUTCALLS"] )
mydata[,"MOU"] <- M1_fun( mydata[,"MOU"] )
mydata[,"PEAKVCE"] <- M1_fun( mydata[,"PEAKVCE"] )
mydata[,"INCALLS"] <- M1_fun( mydata[,"INCALLS"] )
mydata[,"CALLWAIT"] <- M1_fun( mydata[,"CALLWAIT"] )
mydata[,"UNANSVCE"] <- M1_fun( mydata[,"UNANSVCE"] )
mydata[,"DROPVCE"] <- M1_fun( mydata[,"DROPVCE"] )
mydata[,"OVERAGE"] <- M1_fun( mydata[,"OVERAGE"] )
mydata[,"CUSTCARE"] <- M1_fun( mydata[,"CUSTCARE"] )
mydata[,"PHONES"] <- M1_fun( mydata[,"PHONES"] )
mydata[,"MODELS"] <- M1_fun( mydata[,"MODELS"] )
mydata[,"SETPRC"] <- M1_fun( mydata[,"SETPRC"] )
mydata[,"EQPDAYS"] <- M1_fun( mydata[,"EQPDAYS"] )
mydata[,"AGE1"] <- M1_fun( mydata[,"AGE1"] )
mydata[,"AGE2"] <- M1_fun( mydata[,"AGE2"] )
mydata[,"INCOME"] <- M1_fun( mydata[,"INCOME"] )
mydata[,"REVENUE"] <- M1_fun( mydata[,"REVENUE"] )
mydata[,"ROAM"] <- M1_fun( mydata[,"ROAM"] )
mydata[,"DIRECTAS"] <- M1_fun( mydata[,"DIRECTAS"] )
mydata[,"RECCHRGE"] <- M1_fun( mydata[,"RECCHRGE"] )
mydata[,"BLCKVCE"] <- M1_fun( mydata[,"BLCKVCE"] )
mydata[,"DROPBLK"] <- M1_fun( mydata[,"DROPBLK"] )
mydata[,"THREEWAY"] <- M1_fun( mydata[,"THREEWAY"] )
mydata[,"UNIQSUBS"] <- M1_fun( mydata[,"UNIQSUBS"] )
mydata[,"ACTVSUBS"] <- M1_fun( mydata[,"ACTVSUBS"] )
mydata[,"RETACCPT"] <- M1_fun( mydata[,"RETACCPT"] )
mydata[,"RETCALLS"] <- M1_fun( mydata[,"RETCALLS"] )
mydata[,"MONTHS"] <- M1_fun( mydata[,"MONTHS"] )
mydata[,"CREDITAD"] <- M1_fun( mydata[,"CREDITAD"] )
mydata[,"CHANGEM"] <- M1_fun( mydata[,"CHANGEM"] )
mydata[,"CHANGER"] <- M1_fun( mydata[,"CHANGER"] )
mydata[,"REFER"] <- M1_fun( mydata[,"REFER"] )
mydata[,"CALLFWDV"] <- M1_fun( mydata[,"CALLFWDV"] )


#vars<- c("OPEAKVCE","MOUREC","OUTCALLS","MOU","PEAKVCE","INCALLS","CALLWAIT","UNANSVCE",
#         "DROPVCE","OVERAGE","CUSTCARE","PHONES", "MODELS","SETPRC","EQPDAYS","AGE1",
#         "AGE2","INCOME","REVENUE","ROAM","DIRECTAS","RECCHRGE","BLCKVCE","DROPBLK",
#        "THREEWAY","UNIQSUBS","ACTVSUBS","RETACCPT","RETCALLS","MONTHS","CREDITAD","CHANGEM",
#       "CHANGER","REFER","CALLFWDV")

#Splitting data into development and Validaton Dataset
dev <- subset(mydata, CALIBRAT == 1)

val <- subset(mydata, CALIBRAT == 0)



#Building Models for dev dataset
#Building First model with all variables or we can go for factor analysis first and remove some collinearity

fit<-glm(CHURN~CALLFWDV+  REVENUE+  MOU+  RECCHRGE+  DIRECTAS+  OVERAGE+  ROAM+  CHANGEM+
           CHANGER+  DROPVCE+  BLCKVCE+  UNANSVCE+  CUSTCARE+  THREEWAY+  MOUREC+  OUTCALLS+
           INCALLS+  PEAKVCE+  OPEAKVCE+  DROPBLK+  CALLWAIT+ MONTHS+  UNIQSUBS+  ACTVSUBS+
           PHONES+  MODELS+  EQPDAYS +  AGE1+  AGE2+  INCOME+  SETPRC+  REFER+  CREDITAD+  CHILDREN+
           CREDITA+  CREDITAA+  CREDITB+  CREDITC+  CREDITDE+  CREDITGY+  CREDITZ+  PRIZMRUR+  PRIZMUB+
           PRIZMTWN+  REFURB+  WEBCAP+  TRUCK+  RV+  OCCPROF+  OCCCLER+  OCCCRFT+  OCCSTUD+  OCCHMKR+
           OCCRET+  OCCSELF+  OWNRENT+  MARRYUN+  MARRYYES+  MARRYNO+  MAILORD+  MAILRES+  MAILFLAG+  TRAVEL+
           PCOWN+  CREDITCD+  NEWCELLY+  NEWCELLN+  INCMISS+  MCYCLE+  SETPRCM,data = dev,
         family = binomial(logit))



#Output of Logistic Regression
summary(fit)
ls(fit)
fit$model

coeff<-fit$coef #Coefficients of model

#Checking for concordance 
source("Concordance.R")
Concordance(fit)  

#Stepwise regression
step1=step(fit,direction="both")


#Final Model
final_fit<-glm(CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
                 CHANGER + BLCKVCE  + THREEWAY + INCALLS + 
                 PEAKVCE + DROPBLK + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + 
                 EQPDAYS + AGE1 + INCOME + SETPRC + CREDITAD + CHILDREN + 
                 CREDITAA + CREDITB + CREDITC + CREDITDE + PRIZMUB + REFURB + 
                 WEBCAP +  MARRYUN + MAILRES + NEWCELLY + INCMISS + 
                   SETPRCM,data = dev,
               family = binomial(logit))


#Output of Logistic Regression
summary(final_fit)
ls(final_fit)

coeff<-final_fit$coef #Coefficients of model
write.csv(coeff, "coeff.csv")

#Checking for concordance 
source("Concordance.R")
Concordance(final_fit)

#Concordance acheived with this model is  0.62125 and all the variables are significant to atleast 90% confidence level.
#Though a model with 85% confidence level will yield a better concordance. But to make the variables significant we consider 90% level.

################################VALIDATION ##############################
#Decile Scoring for Development dataset
dev1<- cbind(dev, Prob=predict(final_fit, type="response")) #response calculates the probability value.
#View(dev1)

decLocations <- quantile(dev1$Prob, probs = seq(0.1,0.9,by=0.1))
dev1$decile <- findInterval(dev1$Prob,c(-Inf,decLocations, Inf))
#View(dev1)

#Decile Analysis Reports
require(sqldf)
devdata_deciles <- sqldf("select decile, min(Prob) as Min_prob
                         , max(Prob) as max_prob
                         , sum(CHURN) as churn_Count
                         , (count(decile)-sum(CHURN)) as Non_churn_Count 
                         from dev1
                         group by decile
                         order by decile desc")

write.csv(devdata_deciles,"devdata_deciles.csv",row.names = F)

##Validation dataset
val1<- cbind(val, Prob=predict(final_fit,val, type="response")) 
#View(val1)

decLocations <- quantile(val1$Prob, probs = seq(0.1,0.9,by=0.1))
val1$decile <- findInterval(val1$Prob,c(-Inf,decLocations, Inf))

#Decile Analysis Reports
require(sqldf)

val_deciles <- sqldf("select decile, min(Prob) as Min_prob
                     , max(Prob) as max_prob
                     , sum(CHURN) as churn_Count
                     , (count(decile)-sum(CHURN)) as Non_churn_Count 
                     from val1
                     group by decile
                     order by decile desc")

write.csv(val_deciles,"val_deciles.csv",row.names = F)



#val1<-cbind(val, Prob=predict(final_fit, val, type="response"))
#val1$CHURNDEP <- ifelse(val1$Prob>0.501, 1,0)
#sum(val1$CHURNDEP)
