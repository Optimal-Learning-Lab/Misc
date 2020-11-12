#setwd("C:/Users/ppavl/OneDrive/IES Data")
valrb<-read.csv("C:/Users/ppavl/OneDrive/IES Data/Decimal Point data for GKT v3.csv",colClasses = c("Skill"="character"))
colnames(valrb)[1]="Anon.Student.Id"
colnames(valrb)[3]="CF..ansbin."
#colnames(valrb)[6]="Duration..sec."

#valrb<-valrb[1:1000,]

options(max.print=999999)

#valrb<-computeSpacingPredictors(valrb,"Skill")

modelob<-LKT(data=valrb,
             components=c("Anon.Student.Id","Skill","Skill"),
             features=c("logitdec","logitdec$","lineafm$"),
            fixedpars=c(.9,.85))

print(summary(modelob$model))
print(round(sqrt(mean((valrb$CF..ansbin.-modelob$prediction)^2)),4))


write.csv(valrb,"GKT model.csv")



valrb$index<-paste(valrb$KC..Default.,valrb$Anon.Student.Id,sep="")
valrb$cor<-countOutcome(valrb,valrb$index,"correct")
valrb$incor<-countOutcome(valrb,valrb$index,"incorrect")
valrb$tot<-valrb$cor +valrb$incor
aggregate(valrb$CF..ansbin.[valrb$Skill=="1"],by=list(valrb$tot[valrb$Skill=="1"]),FUN=count)
aggregate(valrb$CF..ansbin.[valrb$Skill=="8"],by=list(valrb$tot[valrb$Skill=="8"]),FUN=mean)
aggregate(valrb$CF..ansbin.[valrb$Skill=="5"],by=list(valrb$tot[valrb$Skill=="5"]),FUN=mean)



#Compute spacing then mean spacing for all components that might be used
for(i in c("Skill")){
  valrb$index<-paste(eval(parse(text=paste("valrb$",i,sep=""))),valrb$Anon.Student.Id,sep="")
  eval(parse(text=paste("valrb$",i,"spacing <- componentspacing(valrb,valrb$index,valrb$CF..Time.)",sep="")))
  eval(parse(text=paste("valrb$",i,"relspacing <- componentspacing(valrb,valrb$index,valrb$CF..reltime.)",sep="")))
  eval(parse(text=paste("valrb$",i,"prev <- componentprev(valrb,valrb$index,valrb$CF..ansbin.)",sep="")))
  valrb$index<-paste(eval(parse(text=paste("valrb$",i,sep=""))),valrb$Anon.Student.Id,sep="")
  eval(parse(text=paste("valrb$",i,"meanspacing <- meanspacingf(valrb,valrb$index,valrb$",i,"spacing)",sep="")))
  eval(parse(text=paste("valrb$",i,"relmeanspacing <- meanspacingf(valrb,valrb$index,valrb$",i,"spacing)",sep="")))
  valrb$index<-paste(eval(parse(text=paste("valrb$",i,sep=""))),valrb$Anon.Student.Id,sep="")
  eval(parse(text=paste("valrb$",i,"spacinglagged <- laggedspacingf(valrb,valrb$index,valrb$",i,"spacing)",sep="")))}

