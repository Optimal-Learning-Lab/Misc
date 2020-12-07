#modelob (line 359) AUC = .8162 no student intercepts with random N=1000 from larger 5k dataset

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
`%ni%` = Negate(`%in%`)
#get kaggle data
setwd("C:/Users/lukee/Desktop/Kaggle/RIIID")
#example small file below, but we need to fit with larger N probably:
riiid = read.csv("riiid_n5000.csv")
questions = read.csv("questions.csv")
#split tags into separate columns
#Questions.csv allegedly has sufficient information to generate KC model: https://www.kaggle.com/andradaolteanu/answer-correctness-rapids-crazy-fast
tags=strsplit(questions$tags," ")
questions$tag1=rep(NA,length(questions$question_id))
questions$tag2=rep(NA,length(questions$question_id))
questions$tag3=rep(NA,length(questions$question_id))
questions$tag4=rep(NA,length(questions$question_id))
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
val = smallSet(riiid,2000)

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
  print(i)
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
  print(i)
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


 for(i in "AC"){
   print(i)
   val3$index<-paste(eval(parse(text=paste("val3$",i,sep=""))),val3$Anon.Student.Id,sep="")
   eval(parse(text=paste("val3$",i,"spacing <- componentspacing(val3,val3$index,val3$CF..Time.)",sep="")))
   eval(parse(text=paste("val3$",i,"relspacing <- componentspacing(val3,val3$index,val3$CF..reltime.)",sep="")))
 }
 for(i in rep(paste("c",c(1:posKC),sep=""))){
   print(i)
   val3$index<-paste(eval(parse(text=paste("val3$",i,sep=""))),val3$Anon.Student.Id,sep="")
   eval(parse(text=paste("val3$",i,"spacing <- componentspacing(val3,val3$index,val3$CF..Time.)",sep="")))
   eval(parse(text=paste("val3$",i,"relspacing <- componentspacing(val3,val3$index,val3$CF..reltime.)",sep="")))
 }

KC_quants = quantile(table(val3$KC..Content.),seq(0,1,.05))
KCdef_quants = quantile(table(val3$KC..Default.),seq(0,1,.05))
 val3$part2<-factor(val3$part)
val3$KC..Content. = val3$content_id
val3$KC..Content.[ val3$KC..Content. %in% names(which(table(val3$KC..Content.) < 10)) ] = "foo"

val3$KC..Default.2 = val3$KC..Default.
val3$KC..Default.2[ val3$KC..Default.2 %in% names(which(table(val3$KC..Default.) < 10)) ] = "foo"

val3$KC2_part = paste(val3$KC..Default.2,val3$part,sep="")
val3$stu_part = paste(val3$Anon.Student.Id,val3$part,sep="")


for(i in "KC2_part"){
  print(i)
  val3$index<-paste(eval(parse(text=paste("val3$",i,sep=""))),val3$Anon.Student.Id,sep="")
  eval(parse(text=paste("val3$",i,"spacing <- componentspacing(val3,val3$index,val3$CF..Time.)",sep="")))
  eval(parse(text=paste("val3$",i,"relspacing <- componentspacing(val3,val3$index,val3$CF..reltime.)",sep="")))
}

#First part
val3$firstpart = rep(NA,length(val3[,1]))
val3$prevpart = rep(NA,length(val3[,1]))
val3$prevdiff = rep(NA,length(val3[,1]))
val3$bigcount = rep(NA,length(val3[,1]))
for(i in 1:length(unq)){
  print(i)
  idx = which(val3$Anon.Student.Id %in% unq[i])
  val3$firstpart[idx] = val3$part[which(val3$Anon.Student.Id %in% unq[i])][1]
  
  #mark when N previous trials is above 200
  tmp_bc = rep(0,length(idx))
  if(length(idx)>=50){tmp_bc[100:length(idx)]=1}
  if(length(idx)>=100){tmp_bc[100:length(idx)]=2}
  if(length(idx)>=200){tmp_bc[200:length(idx)]=3}
  if(length(idx)>=300){tmp_bc[300:length(idx)]=4}
  if(length(idx)>=400){tmp_bc[400:length(idx)]=5}
  if(length(idx)>=500){tmp_bc[500:length(idx)]=6}
  if(length(idx)>=600){tmp_bc[600:length(idx)]=7}
  if(length(idx)>=700){tmp_bc[700:length(idx)]=8}
  if(length(idx)>=800){tmp_bc[800:length(idx)]=9}
  if(length(idx)>=900){tmp_bc[900:length(idx)]=10}
  if(length(idx)>=1000){tmp_bc[1000:length(idx)]=11}
  if(length(idx)>=2000){tmp_bc[2000:length(idx)]=12}
  val3$bigcount[idx] = tmp_bc
  #most recent previous different part (did they go from 4 to 5, or 5 to 4?)
  tmp_prevpart=rep(0,length(idx))
  for(j in 2:length(idx)){
    tmp_part = val3$part[idx[1:(j)]]
    #most recent part that is different
    tmp_prevpart[j] =   ifelse(is.na(tmp_part[max(which(diff(tmp_part)!=0))]),0,tmp_part[max(which(diff(tmp_part)!=0))])
  }
  val3$prevpart[idx] = tmp_prevpart
  val3$prevdiff[idx] = tmp_part-tmp_prevpart#ifelse(tmp_part-tmp_prevpart>0,1,-1)
}

#make different transitions have own intercepts
val3$part_transition = paste(val3$part,val3$prevpart,sep="")
#if started on part 5, meaningful that now on part x?
val3$part_firstpart = paste(val3$part,val3$firstpart,sep="")

val3$firstpart = as.factor(val3$firstpart)
modelob_p<-LKT(data=val3,
             components=c("KC..Content.",
                          "num_prior_lecture","priorExp",
                          "part",compKC,
                          "KC2_part",
                          "KC2_part",
                          "KC2_part"
             ),
             features=c("intercept",
                        "lineafm","intercept",
                        "intercept","clineafm",
                        "intercept",
                        "lineafm$",
                        "recency$"
             ),
             fixedpars=c(.22),seedpars=c(NA),bias=1,interc = TRUE)
val3$pred = modelob_p$prediction[,1]
auc(modelob$newdata$CF..ansbin.,modelob$prediction[,1])

keep_names = (names(which(table(val3$Anon.Student.Id)<1000)))

#As N increases model AUC drops off, I think tail distribution students appear and mess it up
#failures are poorly predicted
#What about recent rolling average interacting with other features?
#Three way interactions (ask phil)
#regularization

#Need to account for their outlier type behavior, identify them somehow via trial duration or something
#Negative infinity loglik with small N
#https://stats.stackexchange.com/questions/405701/what-does-it-mean-when-the-negative-log-likelihood-returns-infinity

#try log time for spacing
smallVal = smallSet(val3,1000)
val3$KC..Content.=as.factor(val3$KC..Content.)
shortVal = val3
shortVal = shortVal[which(shortVal$Anon.Student.Id %in% names(which(table(shortVal$Anon.Student.Id)<200))),]
system.time(modelob<-LKT(data=smallVal,
                         components=c("KC..Content.","Anon.Student.Id","Anon.Student.Id",
                                      "KC..Default.2","KC..Default.2","KC..Default.2",
                                      "num_prior_lecture","priorExp",
                                      "part",compKC,compKC,
                                      "KC2_part","KC2_part",
                                      "KC2_part","KC2_part", "KC2_part",
                                      "KC2_part","KC2_part","KC2_part",
                                      "KC2_part",
                                      "Anon.Student.Id",
                                      "prevdiff",
                                      "prevpart",
                                      "part_transition",
                                      "bigcount"
                                    ),
                         features=c("intercept","propdec","logafm",
                                   "logsuc$","logfail$","logafm$",
                                    "lineafm","intercept","intercept",
                                    "clogsuc","clogfail","logitdec$",
                                    "intercept",
                                    "logafm$","logsuc$","logfail$",
                                    "expdecafm$", "expdecsuc$", "expdecfail$",
                                    "recency$",
                                    "pderr",
                                    "intercept",
                                    "intercept",
                                    "intercept",
                                    "intercept"
                                   ),
                         covariates = c(NA,NA,NA,
                                        NA,NA,NA,
                                        NA,NA,NA,
                                        NA,NA,NA,
                                        NA,NA,NA,
                                        NA,NA,NA,
                                        NA,NA,NA),
                         fixedpars=c(.82,
                                     .65,
                                     .3,.8,.8,
                                     .25,
                                     .999),seedpars=c(NA,NA,NA,NA,NA,NA,NA),bias=1,type=0,cost=512,interc = TRUE))
auc(modelob$newdata$CF..ansbin.,modelob$prediction[,1])

modelob$coefs[1:13,]

glm_seg=(glm(modelob$newdata$CF..ansbin.~modelob$prediction[,1]+modelob$newdata$seg_logafmKC2_part,family=binomial))

auc(modelob$newdata$CF..ansbin.,predict(glm_seg,type="response"))

#dropping High N subj: .8468 w/ int, vs. .8309 w/o int
#with high N subj  .8152 w/ int, w/o int = .8036
#type=0;cost=512;auc=.8413
#type=6;cost=512;auc=.8453

#cross-validate? Currently predict(model,...) doesn't work, liblinear objects want different test data format
LKT_cv(components,features,offsetl=NA,fixedl=fixedpars,elastictest=FALSE,val=val3,interc=TRUE)

#CALIBRATION PLOT - without student intercepts we have poor calibration
val3$cuts = cut(modelob$prediction[,1], breaks = 100)
actual_bin=tapply(val3$CF..ansbin.,val3$cuts,function(x){mean(x)})
pred_bin=tapply(modelob$prediction[,1],val3$cuts,function(x){mean(x)})
plot(actual_bin,pred_bin,xlim=c(0,1),ylim=c(0,1))
plot(actual_bin,pred_bin)
hist(modelob$prediction[,1])

#We've got a false positive problem
hist(modelob$prediction[which(val3$CF..ansbin.==0),1])
val3$pred2 = modelob$prediction[,1]

median(modelob$newdata$logafmKC2_part[which(val3$pred2>.8 & val3$CF..ansbin.==0)])
median(modelob$newdata$logafmKC2_part[which(val3$pred2<.4 & val3$CF..ansbin.==0)])

#Not good at predicting failures
median(modelob$prediction[which(modelob$newdata$CF..ansbin.==1),1])
median(modelob$prediction[which(modelob$newdata$CF..ansbin.==0),1])

#Proportion of really egregious errors normalized by amount of data being in that part
#Becomes an obvious issue in absence of student intercepts
#Most data in part 5, rest get underrepresented!
table((modelob$newdata$part[which(val3$pred2>.8 & val3$CF..ansbin.==0)]))/table(modelob$newdata$part)

hist(modelob$prediction[which(val$CF..ansbin.==1),1])

#What is relationship between suc/fail trial duration and model error?
subj_desc = matrix(nrow=length(unq),ncol=6)
for( i in 1:length(unq)){
  
  if(i%%100 == 0){print(i)}
  tmp_idx = which(val3$Anon.Student.Id %in% unq[i])
  if(mean(val3$CF..ansbin.[tmp_idx]) %ni% c(0,1)){
  subj_desc[i,1] = auc(val3$CF..ansbin.[tmp_idx],val3$pred2[tmp_idx],verbose=FALSE)
  }
  subj_desc[i,2] = sqrt(mean((val3$pred2[tmp_idx]-val3$CF..ansbin.[tmp_idx])^2)) #rmse 
  subj_desc[i,3] = mean(val3$Duration..sec.[which(val3$Anon.Student.Id %in% unq[i] & val$CF..ansbin.==0)])#fail Duration
  subj_desc[i,4] = mean(val3$Duration..sec.[which(val3$Anon.Student.Id %in% unq[i] & val$CF..ansbin.==1)])#succ Duration
  subj_desc[i,5] = mean(val3$CF..ansbin.[which(val3$Anon.Student.Id %in% unq[i])])
  subj_desc[i,6] = length(val3$CF..ansbin.[which(val3$Anon.Student.Id %in% unq[i])])
  
  }
colMedians(subj_desc,na.rm=TRUE)
boxplot(subj_desc[,6],ylim=c(0,3000))
hist(subj_desc[,6],breaks=100)
quantile(subj_desc[,6],probs = seq(0, 1, 0.05))
plot((subj_desc[,6]),(subj_desc[,5]))
plot((subj_desc[,5]),subj_desc[,1])
plot((subj_desc[,6]),subj_desc[,1])
plot((subj_desc[,1]),subj_desc[,5])
plot((subj_desc[,1]),subj_desc[,6])
plot((subj_desc[,2]),subj_desc[,6])
plot((subj_desc[,1]),log(subj_desc[,3]))
plot((subj_desc[,1]),log(subj_desc[,4]))
cor.test((subj_desc[,1]),subj_desc[,5])

#auc(modelob$model$model$CF..ansbin.,ifelse(predict(modelob$model,type="response")<0,0,
#                                           ifelse(predict(modelob$model,type="response")>1,1,predict(modelob$model,type="response"))))



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



###SIMPLER, with only afm type stuff

system.time(modelob_s<-LKT(data=val3[which(val3$Anon.Student.Id %in% keep_names),],
                         components=c("KC..Content.","KC..Default.2","num_prior_lecture",
                                      "priorExp","part",compKC,
                                      "KC2_part","KC2_part","KC2_part",
                                      "KC2_part","Anon.Student.Id",compKC
                         ),
                         features=c("intercept","logafm$","logafm",
                                    "intercept","intercept","clogafm",
                                    "intercept","logafm$","expdecafm$",
                                    "recency$","pderr","crecency"
                         ),
                         covariates = c(NA,NA,NA,
                                        NA,NA,NA,
                                        NA,NA,NA,
                                         NA,NA,"priorExp"),
                         fixedpars=c(.3,
                                     .7,.99,.5),seedpars=c(NA,NA,NA,NA),cross=0,bias=0,type=0,cost=512,interc = TRUE))
auc(modelob_s$newdata$CF..ansbin.,modelob_s$prediction[,1])

#cost=512, auc = .7747
#cost=1024, auc = .7749
#cost=2048, auc = .7752
