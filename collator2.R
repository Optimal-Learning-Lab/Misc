library(htmlTable)
library(plyr)
library(stargazer)

df <- data.frame(matrix(ncol = 11, nrow = 12))
colnames(df)<- c("AIC*","BIC*","McFadden R<sup>2<sup>","RMSE<sub>train</sub>", "RMSE<sub>test</sub>","RMSE<sub>test<sub>",
                 "SD<sub>test<sub>","Acc","Sensitivity","Specificity")

featl <- list(
  c("propdec","lineafm$"),
  c("propdec","logafm$"),
  c("propdec","linesuc$","linefail$"),
  c("propdec","logsuc$","logfail$"),
  c("propdec","logsuc$","logfail$","recency"),
  c("propdec","expdecsuc$","expdecfail$"),
  c("propdec","propdec$"),
  c("propdec","propdec$","logafm$"),
  c("propdec","propdec$","expdecfail$"),
  c("propdec","propdec$","ppe$"),
  c("propdec","propdec$","base2$"),
  c("propdec","propdec$","base4$"))  

#rownames(df) <- sapply(featl, paste, collapse = " ")
folders<-c("chinese_tones","assistments","SVO","KDD","MH")
folders<-c("Andes")
for (dats in folders){
# make table of stats

for (j in 1:12){

setwd(paste("C:\\Users\\ppavl\\Dropbox\\Documents - Academic\\HPC Output\\",dats,sep=""))

patv<-paste("results_for_model_",j,".txt",sep="")

dataset <- do.call("rbind.fill", lapply(list.files(pattern=patv), FUN = function(file) {
  read.table(file, header=FALSE, sep=" ")
}))

datalist<-dataset ##lapply(dataset, function(x) x[is.finite(x)])

for (i in 1:12){
df[j,i]<-signif(mean(datalist[[i]]),3)}}
  
  setwd("C:\\Users\\ppavl\\Dropbox\\Documents - Academic\\HPC Output")
#write(dats,file=paste("table",dats,".html",sep=""),append=TRUE)

write(htmlTable(df[1:12,c(3,4,5,6,7,9,10)], n.tspanner = c(1,12),
                css.table = "font-family: Calibri; font-size: 9pt; padding-left: 1em; padding-right: 1em;"
                , rname=1:12),
      file=paste("tablenew",dats,".html",sep=""),append=TRUE)



dataset<-list()
for (j in 1:12){
  
  setwd(paste("C:\\Users\\ppavl\\Dropbox\\Documents - Academic\\HPC Output\\",dats,sep=""))
    patv<-paste("subdiflist_for_model_",j,".txt",sep="")
  print(length(list.files(pattern=patv)))
  dataset[[j]] <- do.call("rbind.fill", lapply(list.files(pattern=patv), FUN = function(file) {
    read.table(file, header=FALSE, sep=" ")[length(read.table(file, header=FALSE, sep=" ")[,1]),]
  }))
  setwd("C:\\Users\\ppavl\\Dropbox\\Documents - Academic\\HPC Output")
  write(length(dataset[[j]][,1]),file=paste("tablenew",dats,".html",sep=""),append=TRUE)
}
options(scipen=3)
pvalues <- matrix(data=0,nrow=12,ncol=12)
for (i in 1:12){
  for(j in 1:12){
    difs<-dataset[[j]]-dataset[[i]]
    difs<-difs[1:50,]
    pvalues[i,j] <- as.numeric(
      pt(mean(apply(difs,1,FUN=mean,na.rm=TRUE)/
                (apply(difs,1,FUN=sd,na.rm=TRUE)/
                   sqrt( max(col(dataset[[1]]))))),max(col(dataset[[1]]))))
   
    
    }}


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# Melt the correlation matrix
library(reshape2)
library(ggplot2)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- order(1-apply(pvalues,MARGIN =2,FUN=mean,na.rm=T))
  cormat <-cormat[dd, dd]
}

colnames(pvalues)<-paste("#",as.character(1:12),sep="")
rownames(pvalues)<-paste("#",as.character(1:12),sep="")
  # Reorder the correlation matrix
pvalues2 <- reorder_cormat(pvalues)
upper_tri <- get_upper_tri(pvalues2)
melted_pvalues <- melt(upper_tri, na.rm = TRUE)
melted_pvalues$value2<-p.adjust(melted_pvalues$value, method = "fdr", n = length(melted_pvalues$value))
# Create a ggheatmap

ggheatmap<-ggplot(data = melted_pvalues, aes(Var2, Var1, fill =value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "pink", high = "blue", mid = "light blue", 
                       midpoint = .5, limit = c(0,1), space = "Lab", trans="sqrt",
                       name="p-value") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                    hjust = 1))#+  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = ifelse(value2<.025,"**",ifelse(value<.025,"*",""))), color = "black", size = 3) +
  labs(x = "Model")+ labs(y= "Model")

ggsave(paste("pvalues",dats,".png"),width=3.5,height=2)
}

#<([0-9]@)(-)([0-9]@)>
#  \1^=\3