#File takes in subset of RIIID dataset, applies covariance clustering to make KC model, then fits it
#Generates some important features, and trial duration.

library(reshape2)
library(car)
library(zoo)
library(gplots)
#library(LKT)
library(rsvd)
library(e1071)
library(Rgraphviz)
library(pROC)
library(dplyr)
library(stringr)
library(SparseM)
library(LiblineaR)
library(Matrix)
library(data.table)

`%ni%` = Negate(`%in%`)


questions = fread("questions.csv")

val <- fread(file="train.csv")

colnames(val)[3]<-"Anon.Student.Id"
val<-smallSet(val,1000)
#val <-val[1:1000000,]
#val$content_id<-paste0("v",val$content_id)
names(val)[names(val) == "answered_correctly"] <- "CF..ansbin."
val<-val[val$CF..ansbin.>-1,]
val<-merge(val,questions,by.x="content_id",by.y="question_id")
val<-val[order(val$row_id),]



unq=unique(val$Anon.Student.Id)
val$lecture = rep(0,length(val[,1]))
val$num_prior_lecture = rep(0,length(val[,1]))
val$lecture[which(val$user_answer==-1)] = 1 #when lectures, mark down
val$same_prev_bundle = rep(0,length(val[,1]))
for( i in 1:length(unq)){
  tmp_idx = which(val$Anon.Student.Id==unq[i])
  val$num_prior_lecture[tmp_idx]=cumsum(val$lecture[tmp_idx])
  val$same_prev_bundle[tmp_idx] = c(0,ifelse(diff(val$bundle_id[tmp_idx],lag=1)==0,1,0))#add padding for lag
}


names(val)[names(val) == "timestamp"] <- "CF..Time."

val$priorExp = ifelse(val$prior_question_had_explanation=="True",1,0)

val$KC..Default. = val$tags




#Phil covar clustering

#==========================Data Preparation==============================


#-1 are lectures, so we do want to keep track of these and use them eventually

val<-val[val$CF..ansbin.!=-1,]

aggdata<-aggregate(val$CF..ansbin.,by=list(val$KC..Default.,val$Anon.Student.Id),FUN=mean)
colnames(aggdata)<-c('KC..Default.','Anon.Student.Id','CF..ansbin.')

aggdata<-aggdata[with(aggdata,order(KC..Default.)),]

mydata<-dcast(aggdata, KC..Default. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format

rownames(mydata)<-mydata[,1]
mydata<-mydata[,-1]
mydata<-na.aggregate(mydata)

mydata<-log(mydata/(1-mydata))
mydata[mydata>2] <- 2
mydata[mydata<(-2)] <- -2


#==========================Feature matrix================================

df<-data.frame()
for (i in 1:ncol(mydata)){
  #print(i)
  disVector<-mydata[,i]-mean(mydata[,i])  #means for each subject
  diagvectors<-disVector %*% t(disVector) #matrix for each subject
  if(i>1){
    df=df+diagvectors # sum of matrices for all students _-> feature matrix
  }else{
    df=diagvectors
  }
}
df<-df/nrow(df)

rownames(df)<-1:nrow(mydata)
colnames(df)<-rownames(mydata)


#parameters###############################################################################

posKC<-12
usethresh<-TRUE
KCthresh<-.07
usethreshm<-TRUE
KCthreshm<-.07
RSVDcomp<-4

#==========================Reduce matrix================================

reducedmat2<-rsvd(df,RSVDcomp)
rownames(reducedmat2$v)<-rownames(mydata)

#==========================cluster matrix==============================
cm <- (cmeans(reducedmat2$v,centers=posKC))



#=================extrapolate KC model==============

if(usethresh) {
  KCmodel <-
    as.data.frame(sapply(apply(cm$membership, 1, function(x)
      which(x > KCthresh)), paste, collapse = "_"))
} else{
  KCmodel <-
    as.data.frame(sapply(apply(cm$membership, 1, function(x)
      which(x == max(x))), paste, collapse = "_"))
}
#View(KCmodel)
colnames(KCmodel)[1] <- "AC"
KCmodel$rows<-rownames(KCmodel)
val<-merge(val,
            KCmodel,
            by.y = 'rows',
            by.x = 'KC..Default.',
            sort = FALSE)


if (usethreshm) {
  KCmodelm <- as.data.frame(ifelse(cm$membership > KCthreshm, 1, 0))
} else {
  KCmodelm <- as.data.frame(cm$membership)
}
#View(KCmodelm)
colnames(KCmodelm)<-paste0("c", colnames(KCmodelm), sep = "")

KCmodelm$rows<- rownames(KCmodelm)

val<-merge(val,
            KCmodelm,
            by.y = 'rows',
            by.x = 'KC..Default.',
            sort = FALSE
)



#=================Sort============
val<-val[order(val$row_id),]
#=================Test===============================
compKC<-paste(paste("c",1:posKC,sep=""),collapse="__")



val$part2<-as.character(val$part)
val$KC..Content. = val$content_id

system.time(modelob<-LKT(data=val,
             components=c("KC..Content.","Anon.Student.Id","Anon.Student.Id","Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.",
                          "num_prior_lecture","priorExp","part",compKC,compKC),
             features=c("intercept","intercept","expdecsuc","expdecfail","logsuc$","logfail$","lineafm$",
                        "lineafm","intercept","intercept","clinesuc","clinefail"),
             #covariates = c(NA,NA,NA,NA,NA,"part2",NA,NA,NA,NA,NA),
             fixedpars=c(.6,.6),seedpars=c(NA,NA),interc = TRUE,epsilon=1e-6,cost=512))
auc(modelob$newdata$CF..ansbin.,modelob$prediction)
plot.roc(modelob$newdata$CF..ansbin.,modelob$prediction)



