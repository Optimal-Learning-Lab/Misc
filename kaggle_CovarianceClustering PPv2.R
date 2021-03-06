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
`%ni%` = Negate(`%in%`)

riiid = read.csv("riiid_n1000.csv")
questions = fread("questions.csv")



library(data.table)
bigdat <- fread(file="train.csv")

colnames(bigdat)[3]<-"Anon.Student.Id"
bigdat<-smallSet(bigdat,4000)
#bigdat <-bigdat[1:1000000,]
#bigdat$content_id<-paste0("v",bigdat$content_id)
names(bigdat)[names(bigdat) == "answered_correctly"] <- "CF..ansbin."
bigdat<-bigdat[bigdat$CF..ansbin.>-1,]
bigdat<-merge(bigdat,questions,by.x="content_id",by.y="question_id")
bigdat<-bigdat[order(bigdat$row_id),]

modelob<-LKT(data=bigdat,
    components=c("content_id","Anon.Student.Id","Anon.Student.Id"),
    features=c("intercept","logsuc","logfail"),
    interc = TRUE,epsilon=1e-6,cost=512)
auc(modelob$newdata$CF..ansbin.,modelob$prediction)

modelob<-LKT(data=bigdat,
             components=c("part","content_id","Anon.Student.Id","Anon.Student.Id"),
             features=c("intercept","intercept","logsuc","logfail"),
                     interc = TRUE,epsilon=1e-6,cost=512)
auc(modelob$newdata$CF..ansbin.,modelob$prediction)

modelob<-LKT(data=bigdat,
             components=c("content_id","Anon.Student.Id","Anon.Student.Id","tags","tags"),
             features=c("intercept","logsuc","logfail","logsuc$","logfail$"),
                    interc = TRUE,epsilon=1e-6,cost=512)
auc(modelob$newdata$CF..ansbin.,modelob$prediction)




#split tags into separate columns
#Questions.csv allegedly has sufficient information to generate KC model: https://www.kaggle.com/andradaolteanu/answer-correctness-rapids-crazy-fast
tags=strsplit(questions$tags," ")
questions$tag1=rep(NA,length(questions$question_id))
questions$tag2=rep(NA,length(questions$question_id))
questions$tag3=reion_id))
questions$tag4=rep(NAp(NA,length(questions$quest,length(questions$question_id))
questions$tag5=rep(NA,length(questions$question_id))
questions$tag6=rep(NA,length(questions$question_id))
questions$tag7=rep(NA,length(questions$question_id))
for(i in 1:length(questions$tags)){
  #  ord = order(as.numeric(tags[[i]][1:7]))
  tmp = tags[[i]][1:7]
  questions$tag1[i] = tmp[1];questions$tag2[i] = tmp[2]
  questions$tag3[i] = tmp[3];questions$tag4[i] = tmp[4]
  questions$tag5[i] = tmp[5];questions$tag6[i] = tmp[6]
  questions$tag7[i] = tmp[7]
}
#Submission rules might mean we have to return to original names later
names(riiid)[names(riiid) == "user_id"] <- "Anon.Student.Id"
set.seed(5)
val = smallSet(riiid,1000)

#add columns for tags
which(val$content_id[1] == questions$question_id)
unq.tags = unique(c(questions$tag1,questions$tag2,questions$tag3,questions$tag4,questions$tag5,questions$tag6,questions$tag7))
for(i in 1:length(unq.tags)){
  eval(parse(text=paste("val$tag",unq.tags[i],"=rep(0,length(val[,1]))",sep="")))
}


#Get part and bundle_id from questions.csv
val$part=rep(0,length(val[,1]))
val$bundle_id = rep(0,length(val[,1]))
for(i in 1:length(val[,1])){
  #If NOT lecture
  if(val$content_type_id[i]==0){
    question_idx = which(questions$question_id==val$content_id[i])
    val$part[i] = questions$part[question_idx]
    val$bundle_id[i] = questions$bundle_id[question_idx]
    tmp_tags = unlist(strsplit(questions$tags[which(questions$question_id==val$content_id[i])]," "))
    for(j in 1:length(tmp_tags)){
      eval(parse(text=paste("val$tag",tmp_tags[j],"[i]=1",sep="")))
    }
  }
}
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
val<-val[order(val$Anon.Student.Id, val$CF..Time.),]
val$priorExp = ifelse(val$prior_question_had_explanation=="True",1,0)
#Make timestamp into Duration
val$Duration..sec. = rep(NA,length(val[,1]))
unq = unique(val$Anon.Student.Id)
for(i in 1:length(unq)){
  idx = which(val$Anon.Student.Id %in% unq[i])
  for(j in idx){
    #on each trial, go to next trial up with different bundle, get time
    tmp.win = (j+1):(j+10)
    win.idx = tmp.win[which(val$bundle_id[(j+1):(j+10)]!=val$bundle_id[j])[1]]
    val$Duration..sec.[j] = val$prior_question_elapsed_time[win.idx]
  }
}
val$Duration..sec. = val$Duration..sec./1000

#I think these NAs could also be used to infer N sessions per student...
if(any(is.na(val$Duration..sec.))){
  val$Duration..sec.[which(is.na(val$Duration..sec.))] = median(val$Duration..sec.,na.rm=TRUE)
}
val$CF..reltime. = practiceTime(val)

#MAKE KC column!
val$KC_tag_ID = rep("",length(val[,1]))
tag.col = which(substr(names(val),1,3)=="tag")
for(i in 1:length(val[,1])){
  val$KC_tag_ID[i] = paste(names(val)[tag.col[which(val[i,tag.col]==1)]],collapse="",sep="")
}

names(val)[names(val) == "answered_correctly"] <- "CF..ansbin."

val$KC..Default. = val$KC_tag_ID
#val$KC..Default. = val$KC..Default.2
# for(i in "KC..Default."){
#   print(i)
#   val$index<-paste(eval(parse(text=paste("val$",i,sep=""))),val$Anon.Student.Id,sep="")
#   eval(parse(text=paste("val$",i,"spacing <- componentspacing(val,val$index,val$CF..Time.)",sep="")))
#   eval(parse(text=paste("val$",i,"relspacing <- componentspacing(val,val$index,val$CF..reltime.)",sep="")))
# }



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

mydata<-apply(mydata,1:2,logit)
mydata[which(mydata>2)] <- 2
mydata[which(mydata<(-2))] <- -2

sample_test = FALSE
#plot(tapply(aggdata$CF..ansbin.,aggdata$Anon.Student.Id,function(x){mean(x)}),tapply(aggdata$CF..ansbin.,aggdata$Anon.Student.Id,function(x){sd(x)})^2)
#Students with no variance due to ceiling/floor will not help improving KC model
if(sample_test){
  stu_var = tapply(aggdata$CF..ansbin.,aggdata$Anon.Student.Id,function(x){sd(x)})^2
  stu_length = tapply(aggdata$CF..ansbin.,aggdata$Anon.Student.Id,function(x){length(x)})
  stu_out = stu_length[which(stu_length>400)]#boxplot(stu_var)$out
  orig_mydata = mydata
  #mydata = mydata[,(sample(1:dim(mydata)[2],replace=TRUE))]
  mydata = mydata[,which(colnames(mydata) %ni% names(stu_out))]
  print(length(unique(colnames(mydata))))
}

#==========================Feature matrix================================

df<-data.frame()
for (i in 1:ncol(mydata)){
  print(i)
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

#===========================visualizations====================



val3<-val
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
val3<-merge(val3,
            KCmodel,
            by.y = 0,
            by.x = 'KC..Default.',
            sort = FALSE)


if (usethreshm) {
  KCmodelm <- ifelse(cm$membership > KCthreshm, 1, 0)
} else {
  KCmodelm <- cm$membership
}
#View(KCmodelm)
colnames(KCmodelm)<-paste0("c", colnames(KCmodelm), sep = "")

val3<-merge(val3,
            KCmodelm,
            by.y = 0,
            by.x = 'KC..Default.',
            sort = FALSE
)




#=================Sort============
val3<-val3[order(val3$row_id),]
#=================Test===============================
compKC<-paste(paste("c",1:posKC,sep=""),collapse="_")


# for(i in "AC"){
#   print(i)
#   val3$index<-paste(eval(parse(text=paste("val$",i,sep=""))),val3$Anon.Student.Id,sep="")
#   eval(parse(text=paste("val3$",i,"spacing <- componentspacing(val3,val$index,val3$CF..Time.)",sep="")))
#   eval(parse(text=paste("val3$",i,"relspacing <- componentspacing(val3,val$index,val3$CF..reltime.)",sep="")))
# }
# for(i in rep(paste("c",c(1:posKC),sep=""))){
#   print(i)
#   val3$index<-paste(eval(parse(text=paste("val3$",i,sep=""))),val3$Anon.Student.Id,sep="")
#   eval(parse(text=paste("val3$",i,"spacing <- componentspacing(val3,val3$index,val3$CF..Time.)",sep="")))
#   eval(parse(text=paste("val3$",i,"relspacing <- componentspacing(val3,val3$index,val3$CF..reltime.)",sep="")))
# }

val3$part2<-factor(val3$part)
val3$KC..Content. = val3$content_id
val3$KC..Content.[ val3$KC..Content. %in% names(which(table(val3$KC..Content.) < 2)) ] = "foo"

val3$KC..Default.2 = val3$KC..Default.
val3$KC..Default.2[ val3$KC..Default.2 %in% names(which(table(val3$KC..Default.) < 10)) ] = "foo"

system.time(modelob<-LKT(data=val3,
             components=c("KC..Content.","Anon.Student.Id","Anon.Student.Id","Anon.Student.Id","KC..Default.2","KC..Default.2","KC..Default.2",
                          "num_prior_lecture","priorExp","part",compKC,compKC),
             features=c("intercept","intercept","expdecsuc","expdecfail","logsuc$","logfail$","lineafm$",
                        "lineafm","intercept","intercept","clinesuc","clinefail"),
             #covariates = c(NA,NA,NA,NA,NA,"part2",NA,NA,NA,NA,NA),
             fixedpars=c(.6,.6),seedpars=c(NA,NA),interc = TRUE,epsilon=1e-6,cost=512))
auc(modelob$newdata$CF..ansbin.,modelob$prediction[,1])
plot.roc(modelob$newdata$CF..ansbin.,modelob$prediction[,1])

View(modelob$model$data)
summary(modelob$model)

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","Anon.Student.Id","KC..Content."),
             features=c("logsuc","logfail","intercept"),interc = TRUE
             )
auc(modelob$newdata$CF..ansbin.,modelob$prediction[,1])
modelob$r2
options(max.print=50000)
val3$content_id<-as.factor(val3$content_id)
modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","Anon.Student.Id","content_id"),
             features=c("logsuc","logfail","intercept"),interc = TRUE
)
auc(modelob$newdata$CF..ansbin.,modelob$prediction[,1])
modelob$r2


#auc(modelob$model$model$CF..ansbin.,ifelse(predict(modelob$model,type="response")<0,0,
#                                           ifelse(predict(modelob$model,type="response")>1,1,predict(modelob$model,type="response"))))




library(SparseM)
library(LiblineaR)
library(Matrix)

val3$contf<-as.factor(val3$KC..Content.)
X<-sparse.model.matrix(~-1+contf,data=val3)



X.csc <- new("matrix.csc", ra = X@x,
             ja = X@i + 1L,
             ia = X@p + 1L,
             dimension = X@Dim)
X.csr <- as.matrix.csr(X.csc)


predictset2<-X.csr

m<-LiblineaR(predictset2,val3$CF..ansbin.,bias=0,cost=1000,epsilon=.00001)
m$W[1:10]
modelvs<-data.frame(m$W)
colnames(modelvs)<-colnames(X)
modelvs<-t(modelvs)
colnames(modelvs)<-"coefficient"

pred<-predict(m,predictset2,proba=TRUE)$probabilities
