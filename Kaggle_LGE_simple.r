#12-9-2020 Bottom has fairly simple model with no sequential features (aka propdec) required
#File takes in subset of RIIID dataset, applies covariance clustering to make KC model, then fits it
#Generates some important features, and trial duration.

library(reshape2)
library(car)
library(zoo)
library(gplots)
library(LKT)
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

#setwd("C:/Users/lukee/Desktop/Kaggle/RIIID")

questions = fread("questions.csv")

val <- fread(file="train.csv")

colnames(val)[3]<-"Anon.Student.Id"
val<-smallSet(val,5000)
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
  print(i)
  tmp_idx = which(val$Anon.Student.Id==unq[i])
  val$num_prior_lecture[tmp_idx]=cumsum(val$lecture[tmp_idx])
  val$same_prev_bundle[tmp_idx] = c(0,ifelse(diff(val$bundle_id[tmp_idx],lag=1)==0,1,0))#add padding for lag
}

#val$Duration..sec. = rep(NA,length(val[,1]))
#unq = unique(val$Anon.Student.Id)
#for(i in 1:length(unq)){
#  print(i)
#  idx = which(val$Anon.Student.Id %in% unq[i])
#  for(j in idx){
#    if((j %% 100) == 0){print(paste("subj:",i," trial:",j,sep=""))}
#    #on each trial, go to next trial up with different bundle, get time
#    tmp.win = (j+1):(j+10)
#    win.idx = tmp.win[which(val$bundle_id[(j+1):(j+10)]!=val$bundle_id[j])[1]]
#    val$Duration..sec.[j] = val$prior_question_elapsed_time[win.idx]
#  }
#}
#val$Duration..sec. = val$Duration..sec./1000

#I think these NAs could also be used to infer N sessions per student...
#if(any(is.na(val$Duration..sec.))){
#  val$Duration..sec.[which(is.na(val$Duration..sec.))] = median(val$Duration..sec.,na.rm=TRUE)
#}
#val$CF..reltime. = practiceTime(val)


names(val)[names(val) == "timestamp"] <- "CF..Time."

val$priorExp = ifelse(val$prior_question_had_explanation=="True",1,0)

val$KC..Default. = val$tags


#Count recency somehow, but quickly



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

val$KC..Content. = val$content_id
val$KC..Content.[ val$KC..Content. %in% names(which(table(val$KC..Content.) < 10)) ] = "foo"

val$KC..Default.2 = val$KC..Default.
val$KC..Default.2[ val$KC..Default.2 %in% names(which(table(val$KC..Default.) < 10)) ] = "foo"
val$KC2_part = paste(val$KC..Default.2,val$part,sep="")

val$firstpart = rep(NA,length(val[,1]))
val$prevpart = rep(NA,length(val[,1]))
val$prevdiff = rep(NA,length(val[,1]))
val$bigcount = rep(NA,length(val[,1]))
for(i in 1:length(unq)){
  print(i)
  idx = which(val$Anon.Student.Id %in% unq[i])
  val$firstpart[idx] = val$part[which(val$Anon.Student.Id %in% unq[i])][1]
  
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
  val$bigcount[idx] = tmp_bc
  #most recent previous different part (did they go from 4 to 5, or 5 to 4?)
  tmp_prevpart=rep(0,length(idx))
  for(j in 2:length(idx)){
    tmp_part = val$part[idx[1:(j)]]
    #most recent part that is different
    tmp_prevpart[j] =   ifelse(is.na(tmp_part[max(which(diff(tmp_part)!=0))]),0,tmp_part[max(which(diff(tmp_part)!=0))])
  }
  val$prevpart[idx] = tmp_prevpart
  val$prevdiff[idx] = tmp_part-tmp_prevpart#ifelse(tmp_part-tmp_prevpart>0,1,-1)
}

#make different transitions have own intercepts
val$part_transition = paste(val$part,val$prevpart,sep="")
#if started on part 5, meaningful that now on part x?
val$part_firstpart = paste(val$part,val$firstpart,sep="")


#.784  clinesuc+clinefail w/o errordec
#.7845 clogsuc+clogfail w/o errordec
#.7847 same as above + bigcount
#.7851 Same as above + prevdiff+prevpart+part_firstpart
#.7904 Same as above + errordec                               
smallval$CF..ansbin.[1]
smallval$CF..ansbin.[1]=1
system.time(modelob2<-LKT(data=smallval,
                          components=c("KC..Content.","KC..Content.","KC..Content.","Anon.Student.Id",
                                       "KC..Default.","KC..Default.","KC..Default.",
                                       "num_prior_lecture","priorExp","part",
                                       compKC,compKC,
                                       "bigcount",
                                       "prevpart",
                                       "prevdiff",
                                       "part_firstpart",
                                       "Anon.Student.Id"
                          ),
                          features=c("intercept","logsuc$","logfail$","logit",
                                     "logit$","logsuc$","logfail$",
                                     "lineafm","intercept","intercept",
                                     "clogsuc","clogfail",
                                     "intercept",
                                     "intercept",
                                     "intercept",
                                     "intercept",
                                     "errordec"
                          ),
                          covariates = c(NA,NA,NA,
                                         NA),
                          fixedpars=c(.15,.2,.999),seedpars=c(NA,NA),
                          interc = TRUE,epsilon=1e-6,cost=512,verbose=TRUE))
#smallval$pred = modelob2$prediction
auc(modelob2$newdata$CF..ansbin.,modelob2$prediction)
length(modelob2$coefs)

                               
# .7639 one step up in complexity
system.time(modelob_s2<-LKT(data=val,
                            components=c("KC..Content.","Anon.Student.Id","Anon.Student.Id",
                                         "KC..Content.","KC..Content."
                                         ),
                            features=c("intercept","logsuc","logfail",
                                       "logsuc$","logfail$"),
                            fixedpars=c(NA),seedpars=c(NA),interc = TRUE,epsilon=1e-6,cost=512))
auc(modelob_s2$newdata$CF..ansbin.,modelob_s2$prediction)
length(modelob_s2$coefs)               
                               
#.766 step up in complexity
system.time(modelob_s3<-LKT(data=val,
                            components=c("KC..Content.","Anon.Student.Id","Anon.Student.Id",
                                         "KC..Content.","KC..Content.","KC..Content.","KC..Content."
                            ),
                            features=c("intercept","logsuc","logfail",
                                       "intercept","logsuc$","logfail$","logit$"),
                            #covariates = c(NA,NA,NA,NA,NA,"part2",NA,NA,NA,NA,NA),
                            fixedpars=c(.11),seedpars=c(NA),interc = TRUE,epsilon=1e-6,cost=512))
auc(modelob_s3$newdata$CF..ansbin.,modelob_s3$prediction)
length(modelob_s3$coefs)

# .765 logit for student instead of KC..Content
system.time(modelob_s4<-LKT(data=val,
                            components=c("KC..Content.","Anon.Student.Id","Anon.Student.Id","Anon.Student.Id",
                                         "KC..Content.","KC..Content.","KC..Content."
                            ),
                            features=c("intercept","logsuc","logfail","logit",
                                       "intercept","logsuc$","logfail$"),
                            #covariates = c(NA,NA,NA,NA,NA,"part2",NA,NA,NA,NA,NA),
                            fixedpars=c(.11),seedpars=c(NA),interc = TRUE,epsilon=1e-6,cost=512))
auc(modelob_s4$newdata$CF..ansbin.,modelob_s4$prediction)
length(modelob_s4$coefs)
                               
