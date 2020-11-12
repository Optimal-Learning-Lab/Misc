

library(multcomp)

options(max.print=1000000)
workingDirectory<- "C:\\Users\\ppavl\\Dropbox\\Comp Testing"
setwd(workingDirectory)
dat <- read.csv("datashopChineseroot.xml-data (fin).txt", sep = '\t', header = TRUE)

#These users are selected from all participants according to their answers of technical issues.
users<-c("A100Y89FZO4J0B",
         "A14IPKOBOPID9H",
         "A17PAE30BGBO1T",
         "A1CSDIX05PK9V",
         "A1GXFMAC759VRM",
         "A1L3937MY09J3I",
         "A1NGABF082HR2P",
         "A1S6C6EN8MPKC3",
         "A1VIP6S8H2XXH7",
         "A1YNGQXMGLM9L7",
         "A23H5PTASRYHHB",
         "A289D98Z4GAZ28",
         "A2E1GWCDFUAZA3",
         "A2HHGI5CNSS5PD",
         "A2MKXI4KCRRI7Y",
         "A2OVWCQ4B9AHFY",
         "A2VAL2BRKVSUB5",
         "A2WVCXVSE0YGML",
         "A326O3VWP8TO8N",
         "A35JCNZJI2MQ20",
         "A39C210BKD7YNN",
         "A3D2U4QF7821ZW",
         "A3GPTSDDHJ2DBV",
         "A3L0DCUXI7X3A9",
         "A3MD34XEB4H6JF",
         "A3QSJY1FFN0N6V",
         "A3V2LUYXBLBAPS",
         "A94DL4GI8ZBUO",
         "AAASQIW3J32OL",
         "AG263U3LX899H",
         "ALCOP0S6PFR2O",
         "AOKTAHY4F4J2B",
         "AQM940W1VBAG6",
         "ATF7HBD1TFI2U2",
         "AZ69TBTDH7AZS",
         "A10XW6SNPNQX16",
         "A150GMV1YQWWB3",
         "A1967FI5GJEZN8",
         "A1F34ZP6YNI98F",
         "A1H72Y9Z5NJXDS",
         "A1LRJ2MQD4AMES",
         "A1NLJ1L4VCQYV2",
         "A1SOOCPS9CRCQE",
         "A1WJ9DBGFY21MQ",
         "A1YVMDRR9W1LZ1",
         "A248LF7KKHXZ14",
         "A29IYYTS4YDXR7",
         "A2E3TO92MCQ9XU",
         "A2IQ0QCTQ3KWLT",
         "A2NJ6AR78O07FD",
         "A2OXTS31LD48BC",
         "A2VF6506WDNXKS",
         "A2YCMT5BPA0AG9",
         "A32E28U1ZP67NE",
         "A35NM2285DUAFH",
         "A39Q4SNT7SRK94",
         "A3EJ44J2ZNRMDA",
         "A3HESOWHVBQOEI",
         "A3L2UB07EZJAMD",
         "A3MUINLM5FMKYF",
         "A3QUGJF2AN8TF2",
         "A3VP27UUQ34OXK",
         "A9692Y27LBXT9",
         "AADRVG2QVZH75",
         "AGVIHLR1DX7",
         "ALEE1QD4TW9G4",
         "AOXM7IK32TST5",
         "AR1IWBDA7MC86",
         "ATXY4CAGV87ZL",
         "AZNIEFUIVB2H0",
         "A11NPNK8885N99",
         "A1601UNFFAAQOI",
         "A19LVWX8ZLO6CS",
         "A1F4N58CAX8IMK",
         "A1HKCTDOTL6C9I",
         "A1MJFW27HXR76D",
         "A1P6OXEJ86HQRM",
         "A1TLQ6BTDQX7US",
         "A1WZBK5PRHBW3H",
         "A1Z0O5MC9DAE8C",
         "A25XXQPMBBDAV3",
         "A2AAY4VT9L71SY",
         "A2EI075XZT9Y2SA2",
         "A2KLJKDG90K1PP",
         "A2NZ7RMSBXESNI",
         "A2PXJTMWGUE5DC",
         "A2VNK2H6USLQTK",
         "A2YQBBAK7NBZHU",
         "A32QWM7BWCSPTS",
         "A35UOR07SAT5IY",
         "A3AFC26CB0AXMI",
         "A3F51C49T9A34D",
         "A3I40B0FATY8VH",
         "A3L8LSM7V7KX3T",
         "A3N3LHDJHLHLTQ",
         "A3RQN5RZD1N2KP",
         "A3W2YF5RJ6R01N",
         "A99XFZJQZBRU7",
         "ABI1EI8UQK6LK",
         "AJTKU0XJ7QTME",
         "ALZVG5501Y5IX",
         "APKZZ03P89DHY",
         "AR5LYXBO5D982",
         "AV22FQTJNBUZT",
         "A138BU1VWM2RKN",
         "A16335MOISDG1F",
         "A1F8ONXY28ROAA",
         "A1IFF4KV23FGHJ",
         "A1MNJ1VJEZE8NY",
         "A1PJLZSOUQ4MIL",
         "A1TMZLYXQAK8Q0",
         "A1Y9VA9FKJLS5K",
         "A20NITCOBY4775",
         "A26UIS59SY4NM6",
         "A2B5HZBQ6C3J3L",
         "A2EKR2ZFO10VMV",
         "A2LF84L3K71GR2",
         "A2O7H7VXLFN6BP",
         "A2R8IV2PWFTY00",
         "A2VRDE2FHCBMF8",
         "A2Z70GL7HTFFQR",
         "A32TGAK321JDFF2",
         "A36IYIE23IO4HE",
         "A3B7TNVOISSZ2O",
         "A3FCZNB9E8K3CX",
         "A3ITZNJQUTIZ4C",
         "A3LC6M2EMDBBXP",
         "A3N5RLYH05PY8L",
         "A3RVHUY67SVXQV",
         "A4UIXESOEZI05",
         "A9HQ3E0F2AGVO",
         "ACWYF9Z50PCA9",
         "AKLV0WIZZ356X",
         "AM2KK02JXXW48",
         "APR6H3HAOE9OU",
         "AROOCBM042SJD",
         "AW0K78T4I2T72",
         "A13BZCNJ0WR1T7",
         "A16G6PPH1INQL8",
         "A1AGPPFSUSJSZN",
         "A1F9KLZGHE9DTA",
         "A1IXH2QLUU5BKZ",
         "A1MYLQQL8BBOYT",
         "A1QVLNM0795FRM",
         "A1TP0SGMUSJ34B",
         "A1YC558J4E5KZ",
         "A2196WCNDZULFS",
         "A27O2IILV3S5YS",
         "A2BNOEYZ3VRW2R",
         "A2F2DDH12YU4AK",
         "A2LT6KC1X51FVW",
         "A2ORPKVJKI0EIO",
         "A2V3ECGQQYNO85",
         "A2WNVSJ1Y06SS1",
         "A2ZJS73XSSMRTD",
         "A33AZCOKCCTUXV",
         "A37BBKFZU9BINN",
         "A3BI0AX5T5GVO3",
         "A3G5IPGLH1IIZN",
         "A3J5KTBHZI60H",
         "A3LRSX7ECYPSF4",
         "A3NM3GAVMJEI3J",
         "A62H1UZADLF7Q2",
         "AA4D4WHT4HBYW",
         "ADXHWQLUQBK77",
         "AKZ8SFOAI4RZN",
         "AMDX0UNZS4A1G",
         "AQ5KOKZQSZU0B",
         "ARTSG66V2MOJ6",
         "AW0MG225VXWCN",
         "A13PXTFOXDCKBF",
         "A173LV77LF3SHB",
         "A1AKX1C8GCVCTP",
         "A1GRPIBHW72HDU",
         "A1KS8EZV7U2D3O",
         "A1N4AZ1QXPOPXH",
         "A1RATFICCKLCQ",
         "A1UH396UOK0X9Z",
         "A1YH2I4Y2SYAXJ",
         "A22ABLVEI5EGPL",
         "A27QBYDLN5V11F",
         "A2CHDWKAYZ3P3E",
         "A2FLOH4DM8A4ZS",
         "A2LU259QPV1I4V",
         "A2OS0VCSQLU3HE",
         "A2V3P1XE33NYC3",
         "A2WT6FV92737W6",
         "A2ZNOMZ35LKY8Q",
         "A353XKDTUX10NC",
         "A38DXFI1TZA295",
         "A3C6U2CJYDOSNC",
         "A3GIIL73GE2CBQ",
         "A3JP7DIFRS0ZBY",
         "A3LV60O7E3XAN4",
         "A3O7X46E3REM7I",
         "A3UTFL5JHRQCM1",
         "A89R5XGMHOTJE",
         "AA4KKLIU4C3NY",
         "AFZ7Z5FOWM9VN",
         "AL83KKXUORS9Y",
         "AOAZMLP27GD81",
         "AQLHX3G8KP4TS",
         "ASI2B6A3Y556Z",
         "A19WXS1CLVLEEX",
         "A3S1TLYMH9WWUG",
         "AY5QUURIR136D")
# subjects
dat<- dat[dat$Anon.Student.Id %in% users,]
dat$Anon.Student.Id<-factor(dat$Anon.Student.Id)

# Assign values to the Outcomes
dat$CF..ansbin.<-ifelse(tolower(dat$Outcome)=="correct",1,ifelse(tolower(dat$Outcome)=="incorrect",0,-1))

# Selecting data of learning session
dat<- dat[dat$Level..Unitname.=="Learning Session"& dat$CF..Display.Order.>0,]

# Speparate variables 
library(tidyr)
dat<-separate(dat, KC.Default.,sep=" ",into=c("code","filepath"))
dat<-separate(dat, filepath,sep="/",into=c("d1","d2","d3","filename"))
dat<-separate(dat, filename,sep="__",into=c("tonetime","vowelsex"))
dat<-separate(dat, tonetime,sep="_",into=c("tone","duration"))

dat$duration<-ifelse(dat$duration=="400ms",400,dat$duration)
dat$duration<-ifelse(dat$duration=="800ms",800,dat$duration) 
dat$duration<-ifelse(dat$duration=="1200ms",1200,dat$duration)

dat$vowelsex<-ifelse(dat$vowelsex=="vowel_changeGender","female",dat$vowelsex)
dat$vowelsex<-ifelse(dat$vowelsex=="vowel","male",dat$vowelsex)
colnames(dat)[30]<-"gender"

# Form expansion and syllable
dat <- cbind(dat,substr(dat$d3,1,3),substr(dat$d3,4,8))
colnames(dat)[49] <- "expansion"
colnames(dat)[50] <- "syllable"

dat$tone<-as.factor(dat$tone)
dat$duration<-as.factor(dat$duration)
dat$expansion<-as.factor(dat$expansion)
dat$syllable<-as.factor(dat$syllable)
dat$gender<-as.factor(dat$gender)

dat$CF..Start.Latency. <- as.numeric(dat$CF..Start.Latency.)
dat$CF..Review.Latency. <- as.numeric(dat$CF..Review.Latency.)
dat$duration.sec<-dat$CF..Start.Latency.+dat$CF..Review.Latency.

val<-dat
val$CF..Time. <- val$Time/1000
val<-val[order(val$Anon.Student.Id, val$CF..Time.),]
val$Duration..sec.<-(val$duration.sec)/1000


val$CF..reltime. <- practiceTime(val)

#Compute spacing (during practice for all periods) then mean spacing for all components that might be used
for(i in c("tone")){
  val$index<-paste(eval(parse(text=paste("val$",i,sep=""))),val$Anon.Student.Id,sep="")
  eval(parse(text=paste("val$",i,"spacing <- componentspacing(val,val$index,val$CF..Time.)",sep="")))
  eval(parse(text=paste("val$",i,"relspacing <- componentspacing(val,val$index,val$CF..reltime.)",sep="")))
  eval(parse(text=paste("val$",i,"prev <- componentprev(val,val$index,val$CF..ansbin.)",sep="")))}

for(i in c("tone")){
  val$index<-paste(eval(parse(text=paste("val$",i,sep=""))),val$Anon.Student.Id,sep="")
  eval(parse(text=paste("val$",i,"meanspacing <- meanspacingf(val,val$index,val$",i,"spacing)",sep="")))
  eval(parse(text=paste("val$",i,"relmeanspacing <- meanspacingf(val,val$index,val$",i,"spacing)",sep="")))  }

for(i in c("tone")){
  val$index<-paste(eval(parse(text=paste("val$",i,sep=""))),val$Anon.Student.Id,sep="")
  eval(parse(text=paste("val$",i,"spacinglagged <- laggedspacingf(val,val$index,val$",i,"spacing)",sep="")))
}


lookupt<-aggregate(val$CF..ansbin.,by=list( val$CF..Correct.Answer.,val$Input,val$Problem.Name),FUN=length)
colnames(lookupt)[3]<-"Problem.Name"
lookupt1<-lookupt[lookupt$Group.2=="tone 1",]
colnames(lookupt1)[4]<-"tone1"
lookupt2<-lookupt[lookupt$Group.2=="tone 2",]
colnames(lookupt2)[4]<-"tone2"
lookupt3<-lookupt[lookupt$Group.2=="tone 3",]
colnames(lookupt3)[4]<-"tone3"
lookupt4<-lookupt[lookupt$Group.2=="tone 4",]
colnames(lookupt4)[4]<-"tone4"


componentl <- list(c("Anon.Student.Id","Problem.Name","tone"),     
                   c("tone","tone","Problem.Name","Anon.Student.Id"))
featl <- list(c("logitdec","intercept","logitdec"), 
              c("diffcorComp$","diffincor1$","intercept","logitdec")            )
fixedl<- list(c(0.9866454,0.9326521),       
              c(.9860589)          )
seedl <- list( c(NA,NA),             
               c(NA)               )
#val<-val2
ms<-c(1,2)
offsetl <- list(c(NA,NA,NA,NA,NA),c(NA,NA,NA,NA,NA,NA,NA))
elastictest<- c("FALSE","FALSE")


for (i in ms) {
  filenamePath<- paste(workingDirectory, "Mengmodel_result_values",i,".xml", sep="")
  cat(paste(i,"\n"))
  
  modelob<-gkt(data=val,
               components=componentl[[i]],
               features=featl[[i]],
               offsetvals=offsetl[[i]],
               fixedpars=fixedl[[i]],
               seedpars=seedl[[i]],
               outputFilePath=filenamePath,
               dualfit=TRUE,
               interc=FALSE,
               elastic=elastictest[i])
  t<-summary(modelob$model)
  print(t)
  if(elastictest[i]=="FALSE"){
    val$pred <- modelob$prediction
    datvec[i]<-round(1-modelob$fitstat[1]/modelob$nullfit[1],4)}
  cat("\n")
}

temp<-modelob$model #result of last model


#code to compute optimality for each tone (do 4 times)
f1<- .34 ##coefficient diffcorcomp
f2<- -.18 ##diffincor1
pv<-0:100/100

gains<- pv*(f1*pv-f1*pv^2)+(1-pv)*(f2*pv)
fixedcost <- 9.16
scal <- 0.93
const <- 3.08

plot(pv,gains / (pv * (const + scal * exp(-qlogis(pv))) + (1 - pv) * (fixedcost)),type="n",ylim=c(0,.005),
     xlab="Difficulty of practice trial", ylab="Logit gain from trial per second",
     main="Tone 1 Efficiency")
lines(pv,gains / (pv * (const + scal * exp(-qlogis(pv))) + (1 - pv) * (fixedcost)),lwd=3,col="red")


plot(pv,gains ,type="n",ylim=c(0,.02),
     xlab="Difficulty of practice trial", ylab="Logit gain from trial",
     main="Tone 1 Difficulty Effect")
lines(pv,gains,lwd=3,col="red")


# this part I will write when you have the rest ready, it copies the item and tone parameters into the stim file
# the overall coefficients go in the tdf model
# fileval<-     readLines("C:\\Users\\ppavl\\Dropbox\\Active projects\\mofacts\\mofacts\\private\\stims\\Chinesestims0.xml")
# 
# for (i in names(temp$coefficients)){ 
#   if(substring(i,1,2)=="F3"){
#   searchstr<-paste("<display>",substring(i,3),"<",sep="")
#   print(searchstr)
#   if(grepl("T1",substring(i,3))){
#   replacestr<-paste("<parameter>",temp$coefficients[i],",.65,0.2301098,-0.1451144</parameter>",searchstr,sep="")
#   print(replacestr)}
#   if(grepl("T2",substring(i,3))){
#     replacestr<-paste("<parameter>",temp$coefficients[i],",.65,0.0531732,-0.0414137</parameter>",searchstr,sep="")
#     print(replacestr)}
#   if(grepl("T3",substring(i,3))){
#     replacestr<-paste("<parameter>",temp$coefficients[i],",.65,0.1866965,-0.0967954</parameter>",searchstr,sep="")
#     print(replacestr)}
#   if(grepl("T4",substring(i,3))){
#     replacestr<-paste("<parameter>",temp$coefficients[i],",.65,0.3945071,-0.1699775</parameter>",searchstr,sep="")
#     print(replacestr)}
#   fileval<-gsub(searchstr,replacestr,fileval)}
#   }
# writeLines(fileval,"C:\\Users\\ppavl\\Dropbox\\Active projects\\mofacts\\mofacts\\private\\stims\\Chinesestimsoptimvals.xml")

# Various plots and statistics to think about models
mean(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name"  ,temp$coefficients,NA),na.rm=TRUE)
sd(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" ,temp$coefficients,NA),na.rm=TRUE)

mean(ifelse(substring(names(temp$coefficients),1,24)=="interceptAnon.Student.Id",temp$coefficients,NA),na.rm=TRUE)
sd(ifelse(substring(names(temp$coefficients),1,24)=="interceptAnon.Student.Id",temp$coefficients,NA),na.rm=TRUE)

mean(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" & substring(names(temp$coefficients),48,49)=="T1" ,temp$coefficients,NA),na.rm=TRUE)
sd(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name"& substring(names(temp$coefficients),48,49)=="T1" ,temp$coefficients,NA),na.rm=TRUE)
hist(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" & substring(names(temp$coefficients),48,49)=="T1" ,temp$coefficients,NA))

mean(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" & substring(names(temp$coefficients),48,49)=="T2" ,temp$coefficients,NA),na.rm=TRUE)
sd(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name"& substring(names(temp$coefficients),48,49)=="T2" ,temp$coefficients,NA),na.rm=TRUE)
hist(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" & substring(names(temp$coefficients),48,49)=="T2" ,temp$coefficients,NA))

mean(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" & substring(names(temp$coefficients),48,49)=="T3" ,temp$coefficients,NA),na.rm=TRUE)
sd(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name"& substring(names(temp$coefficients),48,49)=="T3" ,temp$coefficients,NA),na.rm=TRUE)
hist(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" & substring(names(temp$coefficients),48,49)=="T3" ,temp$coefficients,NA))

mean(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" & substring(names(temp$coefficients),48,49)=="T4" ,temp$coefficients,NA),na.rm=TRUE)
sd(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name"& substring(names(temp$coefficients),48,49)=="T4" ,temp$coefficients,NA),na.rm=TRUE)
hist(ifelse(substring(names(temp$coefficients),1,21)=="interceptProblem.Name" & substring(names(temp$coefficients),48,49)=="T4" ,temp$coefficients,NA))