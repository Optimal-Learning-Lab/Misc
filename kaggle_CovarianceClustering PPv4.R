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
library(fastmatch)
`%ni%` = Negate(`%in%`)


questions = fread("questions.csv")

val <- fread(file="train.csv")

colnames(val)[3]<-"Anon.Student.Id"
val<-smallSet(val,25000)
#val <-val[1:1000000,]
#val$content_id<-paste0("v",val$content_id)
names(val)[names(val) == "answered_correctly"] <- "CF..ansbin."
val<-val[val$CF..ansbin.>-1,]
val<-merge(val,questions,by.x="content_id",by.y="question_id")
val<-val[order(val$row_id),]

keytab<-merge(questions,val,by.y="content_id",by.x="question_id")
#
#
# unq=unique(val$Anon.Student.Id)
# val$lecture = rep(0,length(val[,1]))
# val$num_prior_lecture = rep(0,length(val[,1]))
# val$lecture[which(val$user_answer==-1)] = 1 #when lectures, mark down
# val$same_prev_bundle = rep(0,length(val[,1]))
# for( i in 1:length(unq)){
#   tmp_idx = which(val$Anon.Student.Id==unq[i])
#   val$num_prior_lecture[tmp_idx]=cumsum(val$lecture[tmp_idx])
#   val$same_prev_bundle[tmp_idx] = c(0,ifelse(diff(val$bundle_id[tmp_idx],lag=1)==0,1,0))#add padding for lag
# }


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
usethresh<-FALSE
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

if(usethresh) {
  KCmodel <-
    as.data.frame(sapply(apply(cm$membership, 1, function(x)
      which(x > KCthresh)), paste, collapse = "_"))
} else{
  KCmodel <-
    as.data.frame(sapply(apply(cm$membership, 1, function(x)
      which(x == x[order(x)[2]])), paste, collapse = "_"))

}
#View(KCmodel)
colnames(KCmodel)[1] <- "AC2"
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

val$part<-as.character(val$part)
val$Anon.Student.Id<-as.character(val$Anon.Student.Id)
val$KC..Content. = val$content_id

compTags<-paste(paste("V",1:189,sep=""),collapse="__")


cumroll <- function(x) {
  x[is.na(x)]<-25667
  c(cumsum(x) / seq_along(x))
}
val<-transform(val, avg = ave(prior_question_elapsed_time, Anon.Student.Id, FUN = cumroll))

val$prior_question_had_explanation[is.na(val$prior_question_had_explanation)]<-FALSE



movelog <- function(x) {if(length(x)>1){
  c(x[seq_along(x)][2:length(x)],FALSE)} else {FALSE}}

val$had_explanation = ave(val$prior_question_had_explanation, val$Anon.Student.Id, FUN = movelog)
val$studentassess <- paste(val$Anon.Student.Id,val$had_explanation)
val$conbun<-paste(val$content_id,val$bundle_id)

library(splitstackshape)
val <- cSplit(val, "KC..Default.", " ",drop=FALSE)


newmat<-matrix(0,nrow=nrow(val),ncol=189)
val<-cbind(val,newmat)
for(i in 1:nrow(val)){
  val[i, (names(val)[42:230]):=
        as.list(as.numeric(1:189 %in% eval(parse(text=paste(strsplit(val$tags[i]," "))))))]
}
rm(newmat)

system.time(modelob<-LKT(data=val,
                         components=c("Anon.Student.Id","KC..Content.","Anon.Student.Id","Anon.Student.Id"),
                         features=c("intercept","intercept","logsuc$","logfail$"),
                         # covariates = c(NA,"","",NA,NA),
                         fixedpars=c(.25),seedpars=c(NA),interc = TRUE,epsilon=1e-5,cost=256))
auc(modelob$newdata$CF..ansbin.,modelob$prediction)
plot.roc(modelob$newdata$CF..ansbin.,modelob$prediction)

val$lec<-val$content_type_id
system.time(modelob<-LKT(data=val,
                         components=c("Anon.Student.Id","KC..Content.","Anon.Student.Id","Anon.Student.Id","Anon.Student.Id"),
                         features=c("intercept","intercept","logsuc","logfail","lineafm$"),
                         # covariates = c(NA,"","",NA,NA),
                         fixedpars=c(.25),seedpars=c(NA),interc = TRUE,epsilon=1e-5,cost=256))
auc(modelob$newdata$CF..ansbin.,modelob$prediction)
plot.roc(modelob$newdata$CF..ansbin.,modelob$prediction)




system.time(modelob<-LKT(data=val,
                         components=c("KC..Default.","KC..Default.","KC..Content.","Anon.Student.Id","Anon.Student.Id","Anon.Student.Id"),
                         features=c("logsuc$","logfail$","intercept","logsuc$","logfail$","intercept"),
                         # covariates = c(NA,"","",NA,NA),
                         fixedpars=c(.25),seedpars=c(NA),interc = TRUE,epsilon=1e-5,cost=256))
auc(modelob$newdata$CF..ansbin.,modelob$prediction)
plot.roc(modelob$newdata$CF..ansbin.,modelob$prediction)



system.time(modelob<-LKT(data=val,
                         components=c("KC..Content.","Anon.Student.Id","Anon.Student.Id"),
                         features=c("intercept","logsuc","logfail"),
                         # covariates = c(NA,"","",NA,NA),
                         fixedpars=c(.25),seedpars=c(NA),interc = FALSE,epsilon=1e-6,cost=1024))
auc(modelob$newdata$CF..ansbin.,modelob$prediction)
plot.roc(modelob$newdata$CF..ansbin.,modelob$prediction)



valsuc<- aggregate(val$answered_correctly[val$answered_correctly==1],
                   by=list(val$user_id[val$answered_correctly==1]),FUN=length)
valfail <- aggregate(val$answered_correctly[val$answered_correctly==0],
                   by=list(val$user_id[val$answered_correctly==0]),FUN=length)
vallec <- aggregate(val$answered_correctly[val$answered_correctly==-1],
                     by=list(val$user_id[val$answered_correctly==-1]),FUN=length)

valtab<-merge(valsuc,valfail,by="Group.1",all=TRUE)
valtab<-merge(valtab,vallec,by="Group.1",all=TRUE)
rownames(valtab)<-valtab$Group.1
valtab<-valtab[,-1]
colnames(valtab) <- c("suc","fail","lec")
valtab[is.na(valtab)]<-0

system.time(modelob<-LKT(data=val,
                         components=c("meanv","KC..Content.","Anon.Student.Id","Anon.Student.Id"),
                         features=c("numer","intercept","logsuc","logfail"),
                         # covariates = c(NA,"","",NA,NA),
                         fixedpars=c(.25),seedpars=c(NA),interc = TRUE,epsilon=1e-5,cost=256))
auc(modelob$newdata$CF..ansbin.,modelob$prediction)
plot.roc(modelob$newdata$CF..ansbin.,modelob$prediction)


system.time(modelob<-LKT(data=val,
                         components=c("KC..Content.","Anon.Student.Id","Anon.Student.Id","KC..Default.","KC..Default."),
                         features=c("intercept","logsuc$","logfail$","logsuc$","logfail$"),
                         # covariates = c(NA,"","",NA,NA),
                         fixedpars=c(.25),seedpars=c(NA),interc = FALSE,epsilon=1e-6,cost=1024))
auc(modelob$newdata$CF..ansbin.,modelob$prediction)
plot.roc(modelob$newdata$CF..ansbin.,modelob$prediction)




x<-modelob[["coefs"]]
rx<-rownames(modelob[["coefs"]])[grep("interceptKC..Content.",rownames(modelob[["coefs"]]))]
x<- as.data.frame(modelob[["coefs"]][grep("interceptKC..Content.",rownames(modelob[["coefs"]]))])
rownames(x)<-gsub("interceptKC..Content.","",rx)
colnames(x)<-"intval"

rows<-rownames(x)
missing<-(1:max(questions$question_id))[1:max(questions$question_id) %ni% as.numeric(rows)]
x[nrow(x)+1:length(missing),] <- 0
rownames(x)[(1+nrow(x)-length(missing)):nrow(x)]<-paste0(rownames(x)[(1+nrow(x)-length(missing)):nrow(x)],"C")
rownames(x)[(1+nrow(x)-length(missing)):nrow(x)]<-missing
x<-x[order(as.numeric(rownames(x))), ,drop = FALSE]

write.csv(x,file=("content_idintercepts.csv"))

x<-modelob[["coefs"]]
rx<-rownames(modelob[["coefs"]])[grep("logsucKC..Default.",rownames(modelob[["coefs"]]))]
x<- as.data.frame(modelob[["coefs"]][grep("logsucKC..Default.",rownames(modelob[["coefs"]]))])
rx<-gsub(":logsucKC..Default.","",rx)
rownames(x)<-gsub("e$data$KC..Default.","",rx,fixed=TRUE)
colnames(x)<-"slope"

rows<-rownames(x)
missing<-unique(questions$tags)[unique(questions$tags) %ni% rows]
x[nrow(x)+1:length(missing),] <- 0
rownames(x)[(1+nrow(x)-length(missing)):nrow(x)]<-paste0(rownames(x)[(1+nrow(x)-length(missing)):nrow(x)],"C")
rownames(x)[(1+nrow(x)-length(missing)):nrow(x)]<-missing

write.csv(x,file=("logsucKC.csv"))

x<-modelob[["coefs"]]
rx<-rownames(modelob[["coefs"]])[grep("logfailKC..Default.",rownames(modelob[["coefs"]]))]
x<- as.data.frame(modelob[["coefs"]][grep("logfailKC..Default.",rownames(modelob[["coefs"]]))])
rownames(x)<-gsub("logfailKC..Default.:e$data$KC..Default.","",rx,fixed=TRUE)
colnames(x)<-"slope"


rows<-rownames(x)
missing<-unique(questions$tags)[unique(questions$tags) %ni% rows]
x[nrow(x)+1:length(missing),] <- 0
rownames(x)[(1+nrow(x)-length(missing)):nrow(x)]<-paste0(rownames(x)[(1+nrow(x)-length(missing)):nrow(x)],"C")
rownames(x)[(1+nrow(x)-length(missing)):nrow(x)]<-missing

write.csv(x,file=("logfailKC.csv"))





x<-modelob[["coefs"]]
rx<-rownames(modelob[["coefs"]])[grep("interceptAnon.Student.Id",rownames(modelob[["coefs"]]))]
x<- as.data.frame(modelob[["coefs"]][grep("interceptAnon.Student.Id",rownames(modelob[["coefs"]]))])
rownames(x)<-gsub("interceptAnon.Student.Id","",rx)
colnames(x)<-"intval"

prbs<-aggregate(val$CF..ansbin.,by=list(val$Anon.Student.Id),FUN=mean)$x[2:5000]

x$len<-aggregate(val$CF..ansbin.,by=list(val$Anon.Student.Id),FUN=length)$x[2:5000]
x$lec<-aggregate(val$content_type_id,by=list(val$Anon.Student.Id),FUN=sum)$x[2:5000]
x$inter<-sqrt(x$len)*x$prob
prbs<-pmin(pmax(prbs,.05),.95)
cor(x)

summary(lm(intval~prob+I(prob^2),data=x))$r.squared

prbs<- aggregate(answered_correctly ~ user_id, data=val2, FUN=mean)
rownames(prbs) <- prbs[,1]
prbs[,1] <- NULL



valtab<-merge(valsuc,valfail,by="Group.1",all=TRUE)
valtab<-merge(valtab,vallec,by="Group.1",all=TRUE)
rownames(valtab)<-valtab$Group.1
valtab<-valtab[,-1]
colnames(valtab) <- c("suc","fail","lec")
valtab[is.na(valtab)]<-0

write.csv(valtab,file=("history.csv"))
