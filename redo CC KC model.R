#redo KC model
system.time({
#set test to val
  #start point 1 may not need to ever run again
val<-valhist

aggdata<- val[,mean(CF..ansbin.),by=list(KC..Default.,Anon.Student.Id)]
valhist<-val

colnames(aggdata)<-c('KC..Default.','Anon.Student.Id','CF..ansbin.')

aggdata<-aggdata[with(aggdata,order(KC..Default.)),]

mydata<-dcast(aggdata, KC..Default. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format
#rm(aggdata)

rownamesmydata<-mydata$KC..Default.
mydata<-mydata[,-1]

# determine the column names that contain NA values
nm <- names(mydata)[colSums(is.na(mydata)) != 0]
## replace with the mean - by 'id'
mydata[, (nm) := lapply(nm, function(x) {
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})]
mydata<-log(mydata/(1-mydata))
mydata[mydata>2] <- 2
mydata[mydata<(-2)] <- -2
rownames(mydata)<-rownamesmydata

#==========================Feature matrix================================

mydata[, names(mydata) :=lapply(.SD, function(x) x - mean(x)), .SDcols = names(mydata)]
df <- mydata[,as.matrix(.SD) %*% t(as.matrix(.SD)),.SDcols=names(mydata)]
df<-df/nrow(df)
rownames(df)<-1:nrow(mydata)
colnames(df)<-rownames(mydata)



#Start point 1
#parameters###############################################################################

posKC<-15
usethresh<-FALSE
KCthresh<-.3
usethreshm<-TRUE
KCthreshm<-.2
RSVDcomp<-3

#==========================Reduce matrix================================

reducedmat2<-rsvd(df,RSVDcomp)
rownames(reducedmat2$v)<-rownames(mydata)

#==========================cluster matrix==============================
cm <- (cmeans(reducedmat2$v,centers=posKC))




val<-valsamp
val<-val[,-1]
val<-val[,1:19] # restore to original
val$KC..Default. = val$tags

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



#=================Test===============================
compKC<-paste(paste("c",1:posKC,sep=""),collapse="__")

system.time(val[,compKC:= do.call(paste,val[,23:(22+posKC)])])
colnames(val)[23+posKC]<-compKC
val$part<-as.character(val$part)
val$Anon.Student.Id<-as.character(val$Anon.Student.Id)
#val$KC..Content. = as.character(val$content_id)


valsamp<-val
})
