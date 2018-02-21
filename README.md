# CreditCardDeliquency
#Read the CSV file

setwd("C:/Users/ravis/Desktop/Uconn/Spring 2017/Kaggle/UCI_Credit_Card")
credit=read.csv("UCI_Credit.csv")
str(credit)
View(credit)


#Libraries
callLib=function(){
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('randomForest') # Classification algorithm
library(plyr) # splitting and combining data
library(reshape2)
library(psych) #performs statistical operations
library(ROCR) # generates ROC curve 
library(party) # generates Lift curve
library(corrplot) # generates Correation plots
library(tree) # to implement tree alogirtm.
}


callLib()
str(credit)
sapply(credit, function(x) sum(is.na(x)))
#Create a directory to store output:

pwd=getwd()
dir.create(paste0(pwd,"/output"))

#checking Missing values

sum(is.na(credit))

#Data Cleaning:

#Since the code 0,4,5 & 6 denote "Other" status for education we will try to do some feature engineering to reassign these codes
#all 6 with 5

#credit$EDUCATION[credit$EDUCATION==6]<-5
#credit[credit$EDUCATION==6,]
attach(credit)


# When I analyze factors for education I realize that code 0,4,5 & 6 denote "Other" status for education, which doesnt look fair
# we will try to implement some feature engineering concepts and will try to replce "unknown" with some other values.
# After consulting with my friend who has worked for couple of years with one of the major credit card company of the world
#I came to know that education level of applicant plays a major role in deciding applicants balance limit.
#Now we will use this information to decide education level of applicants having educationa as "Unknown"

jpeg(filename = paste0(pwd, "/output/BalLimit_Education", ".jpg"))
ggplot(credit, aes(x=factor(EDUCATION), y=LIMIT_BAL)) + stat_summary(fun.y="mean", geom="bar",width=0.40,fill = "#FF6650",col="red")
dev.off()


#We observe that mean balance limit for applicants having education level "0","4" and "1" are quite same.
#So we will replace education level "0" and "4" to "1". Similarly 5 & 6 are almost equivalent to 2.

credit$MARRIAGE[credit$MARRIAGE==0 & credit$AGE>27]<-1
credit$MARRIAGE[credit$MARRIAGE==0 & credit$AGE<=27]<-2

credit$EDUCATION[credit$EDUCATION==0]<-1
credit$EDUCATION[credit$EDUCATION==4]<-1
credit$EDUCATION[credit$EDUCATION==6]<-2
credit$EDUCATION[credit$EDUCATION==5]<-4
ggplot(credit, aes(x=factor(EDUCATION), y=LIMIT_BAL)) + stat_summary(fun.y="mean", geom="bar",width=0.40,fill = "#FF6650",col="red")


jpeg(filename = paste0(pwd, "/output/BalLimit_Education_upd", ".jpg"))
ggplot(credit, aes(x=factor(EDUCATION), y=LIMIT_BAL)) + stat_summary(fun.y="mean", geom="bar")+theme_few()
dev.off()

#For marriage some of the records have code 0 which is not valid. We will replace them with code 3 i.e others.
ggplot(credit, aes(x=factor(MARRIAGE), fill=MARRIAGE)) + geom_bar(width = 0.50)+theme_few()
nrow(credit[credit$MARRIAGE==0,])
credit$AGE[credit$MARRIAGE==0]
credit$MARRIAGE[credit$MARRIAGE==0 && credit$AGE>27]<-1
credit$MARRIAGE[credit$MARRIAGE==0 && credit$AGE<=27]<-2
View
#We need to convert some of the variables to factor.

rename<-function(credit){
  rm(creditcrd)
  creditcrd=as.data.frame(credit)

  creditcrd$SEX=as.factor(SEX)
  creditcrd$EDUCATION=as.factor(EDUCATION)
  creditcrd$MARRIAGE=as.factor(MARRIAGE)
  creditcrd$EDUCATIONTYP
  creditcrd$SEXTYP
  creditcrd$MARRIAGETYP
  
  creditcrd$EDUCATIONTYP[creditcrd$EDUCATION==0]="Others"
  creditcrd$EDUCATIONTYP[creditcrd$EDUCATION==1]="Graduate school"
  creditcrd$EDUCATIONTYP[creditcrd$EDUCATION==2]="University"
  creditcrd$EDUCATIONTYP[creditcrd$EDUCATION==3]="High School"
  creditcrd$MARRIAGETYP[creditcrd$MARRIAGE==1]="Married"
  creditcrd$MARRIAGETYP[creditcrd$MARRIAGE==2]="Single"
  creditcrd$MARRIAGETYP[creditcrd$MARRIAGE==3]="Others"
  creditcrd$SEXTYP[creditcrd$SEX==1]="Male"
  creditcrd$SEXTYP[creditcrd$SEX==2]="Female"

  
}


rename(credit)
attach(credit)
#We will convert all the categorical variable to factors.

credit$SEX=as.factor(SEX)
credit$EDUCATION=as.factor(EDUCATION)
credit$MARRIAGE=as.factor(MARRIAGE)
credit$PAY_0=as.factor(PAY_0)
credit$PAY_2=as.factor(PAY_2)
credit$PAY_3=as.factor(PAY_3)
credit$PAY_4=as.factor(PAY_4)
credit$PAY_5=as.factor(PAY_5)
credit$PAY_6=as.factor(PAY_6)


# Check the datatype for each class



cat_var<-sapply(credit,class)
const_var<-names(cat_var[cat_var=="integer" | cat_var=="numeric"])
const_var

catg_var<-names(cat_var[cat_var=="factor"])
catg_var







attach(creditcrd)
str(creditcrd)

# We will try to seggregate the defaulters on the basis of Their

#1) Sex

jpeg(filename = paste0(pwd, "/output/Barplot_", "SexBasedDefaulter", ".jpg"))
ggplot(creditcrd, aes(x = SEXTYP, fill = factor(default.payment.next.month))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'SEX') +
  theme_few()
dev.off()

#2) Education

jpeg(filename = paste0(pwd, "/output/Barplot_", "EducationBasedDefaulter", ".jpg"))
ggplot(credit, aes(x = EDUCATIONTYP, fill = factor(default.payment.next.month))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'EDUCATION LEVEL') +
  theme_few()
dev.off()

#3) Marriage

jpeg(filename = paste0(pwd, "/output/Barplot_", "MarriageBasedDefaulter", ".jpg"))
ggplot(credit, aes(x = MARRIAGE, fill = factor(default.payment.next.month))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'MARRIAGE STATUS') +
  theme_few()
dev.off()


# On the basis of their repayment status in september month

jpeg(filename = paste0(pwd, "/output/Barplot_", "Repayment status in September", ".jpg"))
ggplot(credit, aes(x = PAY_0, fill = factor(default.payment.next.month))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'REPAYMENT STATUS') +
  theme_few()
dev.off()

# We will try to plot the defaulter as per the distribution of balance limit

ggplot(credit,aes(x = credit$LIMIT_BAL/1000)) +
  geom_histogram(aes(fill = credit$default.payment.next.month)) +
  xlab("Balance Limit x 1000") +
  ylab("Count") +
  scale_fill_discrete(name="Default Payment Next Month",
                      breaks=c(0,1),
                      labels=c(0,1)) +
  xlim(c(0,750))




# Lets try to visualise the ratio of defaulters and non defaulters in our dataset.

jpeg(filename = paste0(pwd, "/output/", "Default Count", ".jpg"))
ggplot(credit, aes(x = credit$default.payment.next.month, fill = factor(default.payment.next.month))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'DEFAULT COUNT') +
  theme_few()
dev.off()


M <- cor(subset(ccdata, select = c(LIMIT_BAL,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)))
corrplot(M, method="number")
x.res=cor(credit[const_var[-c(1,16)]])
corrplot(x.res,method="number")
class(x.res)

#From the correlation plot we observe that except BILL_AMNT1 to BILL_AMNT6 no other variables are correlated to each other.
#Correlation of these variables makes sense as unpaid bill amount of previous gets added to the next months bill amount.

#since these variable are highly correlated instead of using BILL_AMNT1 to BILL_AMNT6 we will only use BILL_AMNT1 & BILL_AMNT2
#in our model.

#We will try to divide our data into tarainig and validation set
drops <- c("BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")
credit <- credit[, !(names(credit) %in% drops)]


write.csv(credit,"CleanedCredit.csv")

smp_size<-floor(0.70*nrow(credit))
set.seed(231)
train_ind=sample(seq_len(nrow(credit)),size=smp_size)
train<-credit[train_ind,]
test<-credit[-train_ind,]


train$PAY_2[train$ID==982]<-8
train$PAY_5[train$ID==15191]<-8

#We will tru to fit a Logistic model to understand VIF factor.

glm.fit=glm(default.payment.next.month~.-ID,data = train,family = binomial)
glm.prob=predict(glm.fit,type="response")
train_res<-train
train_res$Prob<-glm.prob
train_res$Default_Predict<-ifelse(glm.prob>0.4,"1","0")
table(train_res$default.payment.next.month,train_res$Default_Predict)
x=15317
y=1931

classficationrate=function(x,y,total){
  clsrt=100*(x+y)/total
  print(clsrt)
}

classficationrate(15332,1945,21000)

ROCRpred <- prediction(glm.prob, train$default.payment.next.month)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


# Now we will test the model on test/Validation data set


glm.prob_test=predict(glm.fit,test,type = "response")
test_res<-test
test_res$Prob<-glm.prob_test
test_res$Default_predict<-ifelse(glm.prob_test>0.4,"1","0")
table(test_res$default.payment.next.month,test_res$Default_predict)

#ROC & Lift Curve for validation data

ROCRpred <- prediction(glm.prob_test, test$default.payment.next.month)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
perf=performance(ROCRpred,"lift","rpp")
plot(perf,main="lift Curve",colorize=T)

# We will save the probabilities of the logistic model in file.

names(test_res)[23]=paste("Default_Predict")
names(test_res)
View(test_res)
View(train_res)
rbind(train_res,test_res)
write.csv(rbind(train_res,test_res),paste0(pwd,"/output/Logistic.csv"))

#Classification using Binary tree algorithm

smp_size<-floor(0.70*nrow(credit))
set.seed(231)
train_ind=sample(seq_len(nrow(credit)),size=smp_size)
train_tree<-credit[train_ind,]
test_tree<-credit[-train_ind,]
train_tree$PAY_2[train_tree$ID==982]<-8
train_tree$PAY_5[train_tree$ID==15191]<-8

# We will convert default.payment.next.month as factorial


train_tree$default.payment.next.month=as.factor(train_tree$default.payment.next.month)
test_tree$default.payment.next.month=as.factor(test_tree$default.payment.next.month)

#We will generate a model using our training dataset first.

tree.default<-tree(train_tree$default.payment.next.month~.,data=train_tree)
tree.predtrain=predict(tree.default,train_tree,type="class")
table(tree.predtrain,train_tree$default.payment.next.month)
summary(tree.default)



plot(tree.default)
text(tree.default,pretty = 0)
tree.pred=predict(tree.default,test_tree,type="class")
summary(tree.pred)
plot(tree.pred)
text(tree.default,pretty = 0)
table(tree.pred,test_tree$default.payment.next.month)
tree.pred


classficationrate(6761,611,9000)


#Now we will try random forest.

smp_size<-floor(0.70*nrow(credit))
set.seed(231)
train_ind=sample(seq_len(nrow(credit)),size=smp_size)
train_forest<-credit[train_ind,]
test_forest<-credit[-train_ind,]
train_forest$PAY_2[train_forest$ID==982]<-8
train_forest$PAY_5[train_forest$ID==15191]<-8

train_forest$default.payment.next.month=as.factor(train_forest$default.payment.next.month)
test_forest$default.payment.next.month=as.factor(test_forest$default.payment.next.month)

rf.default=randomForest(train_forest$default.payment.next.month~.,data=train_forest,mtry=5,importance=TRUE)
rf.pred=predict(rf.default,test_forest)
plot(rf.pred,test_forest$default.payment.next.month)
abline(0,1)
table(rf.pred,test_forest$default.payment.next.month)
importance(rf.default)
varImpPlot(rf.default)

classficationrate(15410,1791,21000)
classficationrate(6665,705,9000)


#Taking losgistic model as our best model we will try to do analysis of the significant predictors


#PAY0 differnce between people who defaulted and who did not default.

modeldata=read.csv(paste0(pwd,"/output/Logistic.csv"))
str(modeldata)
x<-modeldata[1:4000,]
x<-modeldata[26000:30000,]
x
d1 <- ggplot(x, aes(x=x$PAY_0)) + 
  geom_bar(stat="count",color='red',fill='orange') +
  xlab("Payment Status") + ylab("Customer Count") 


#BILL_AMT2

ggplot(data=x, aes(x$BILL_AMT2/1000)) + geom_histogram(color='red',fill='orange')+
  xlab("Bill Amount") + ylab("count")

#Balance Limit

ggplot(data=x, aes(x$LIMIT_BAL/1000)) + geom_histogram(color='red',fill='orange')+
  xlab("Balance Limit") + ylab("count")

#Age

ggplot(data=x, aes(x$AGE)) + geom_histogram(color='red',fill='orange')+
  xlab("Balance Limit") + ylab("count")

modeldata<-modeldata[order(modeldata$Prob,decreasing = TRUE),]
View(modeldata)
?order



#K-Mean Clustering algorithm.

smp_size<-floor(0.70*nrow(credit))
set.seed(231)
train_ind=sample(seq_len(nrow(credit)),size=smp_size)
train<-credit[train_ind,]
test<-credit[-train_ind,]


train$PAY_2[train$ID==982]<-8
train$PAY_5[train$ID==15191]<-8
km <- kmeans(train[,c("LIMIT_BAL","SEX","BILL_AMT2","PAY_0")],centers=2)
km
km$centers
km$size
km$cluster
plot(train,col=(km.out$cluster+1),main="K-means Clustering", xlab="",ylab="",pch=20,cex=2)
km
table(km$cluster,train$default.payment.next.month)
attach(train)
km$cluster <- as.factor(km$cluster)
library(ggplot2)
ggplot(train, aes(BILL_AMT2/1000 , PAY_0 , color = km$cluster)) + geom_point()
km

colnames(train)

#Anothe package for implementing K-means
install.packages("flexclust")
library("flexclust")
cl1 = kcca(train[,c("LIMIT_BAL","SEX","BILL_AMT2","PAY_0")], k=2, kccaFamily("kmeans"))
cl1 
pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 1:2])

clust=apply(test,2,km)

image(cl1)
points(dat[dat[["train"]]==TRUE, 1:2], col=pred_train, pch=19, cex=0.3)
points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch=22, bg="orange")
