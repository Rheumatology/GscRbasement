########################################################################################
###   Title: Heatmap based on GSI and high variable MHL
########################################################################################

library("gplots")
library("RColorBrewer")
library("grDevices")
library("impute")

args = commandArgs(trailingOnly=TRUE)


RawNARemove<-function(data,missratio=0.3){
  threshold<-(missratio)*dim(data)[2]
  NaRaw<-which(apply(data,1,function(x) sum(is.na(x))>threshold))
  zero<-which(apply(data,1,function(x) all(x==0))==T)
  NaRAW<-c(NaRaw,zero)
  if(length(NaRAW)>0){
    dat<-data[-NaRAW,]
  }else{
    dat<-data;
  }
  dat
} 

gsi<-function(data){
  group=names(table(colnames(data)))
  index=colnames(data)
  gsi<-c()
  gmaxgroup<-c()
  for(i in 1:nrow(data)){
    gsit<-0
    gmax<-names(which.max(tapply(as.numeric(data[i,]),index,mean)))
    for(j in 1:length(group)){
      tmp<-(1-10^(mean(data[i,][which(index==group[j])]))/10^(mean(data[i,][which(index==gmax)])))/(length(group)-1)
      gsit<-gsit+tmp
    }
    gmaxgroup<-c(gmaxgroup,gmax)
    gsi<-c(gsi,gsit)
    print(c(gmax,gsit))
  }
  rlt=data.frame(region=rownames(data),group=gmaxgroup,GSI=gsi)
  return(rlt)
}

heatmap<-function(mydata){
  hclustfunc <- function(x) hclust(x, method="complete")
  distfunc <- function(x) dist(x, method="euclidean")
  # perform clustering on rows and columns
  cl.row <- hclustfunc(distfunc(mydata))
  cl.col <- hclustfunc(distfunc(t(mydata)))
  # extract cluster assignments; i.e. k=8 (rows) k=5 (columns)
  gr.row <- cutree(cl.row, 6)
  # gr.col <- cutree(cl.col, 5)
  # require(RColorBrewer)
  col1 <- brewer.pal(6, "Set1")     # the maximum of brewer.pal is 12
  #col2 <- brewer.pal(5, "Pastel1")
  tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
  col2<-tol21rainbow[as.numeric(as.factor(cl.col$labels))]
  col=colorRampPalette(c("blue", "yellow"))(20) 
  heatmaprlt<-heatmap.2(as.matrix(mydata),hclustfun=hclustfunc, distfun=distfunc,
                        RowSideColors=col1[gr.row], 
                        ColSideColors=col2,
                        labRow=F,
                        trace="none",
                        col=col,
                        density.info="none")
  legend=unique(data.frame(col2,cl.col$labels))
  legend(x=0.85,y=0.8,legend=legend[,2],col=as.character(legend[,1]),pch=15,cex = 0.5)
  dev.off()
  return(heatmaprlt)
}


################################################################################################
#########################################Data Input ############################################
################################################################################################

infile="WGBS_methHap_load_matrix_20Oct2015.txt";
file1<-read.table(infile,head=T,sep="\t",row.names=1,as.is=T,check.names=F)

infile="Heatmap.MHL.txt"
file2<-read.table(infile,head=T,sep="\t",row.names=1,as.is=T,check.names=F)
dim(file1)
dim(file2)

file1<-file1[match(rownames(file2),rownames(file1)),]
file1<-cbind(file1,file2)

# miss value detection and imputation
library("impute")
library("preprocessCore")
library("sva")
f2<-RawNARemove(file1,missratio=0.20)
f2<-impute.knn(data.matrix(f2))$data
colnames(f2)<-gsub("_","-",colnames(f2))
colname2<-unlist(lapply(colnames(f2),function(x) unlist(strsplit(x,"[.]"))[1]))
colnames(f2)<-colname2
# be sure all the sample information has been stored in the following database
saminfo2<-read.table("newsaminfo.txt",head=T,sep="\t",as.is=T)
colnames(f2)<-saminfo2[match(colname2,saminfo2[,1]),2]
colnames(f2)

mydata=f2
# feature selection plan A
GSI<-gsi(mydata)
GSI<-GSI[order(GSI[,3],decreasing=T),]
mydata1<-mydata[match(names(sort(apply(mydata,1,sd),decreasing=T))[1:5000],rownames(mydata)),]
colnames(mydata1)<-colname2
#pdf("heatmap.wgbs.mhl.top5000gsi.pdf")
#rlt1<-heatmap(mydata1)
#dev.off()
hc1 <- hclust( dist(t(mydata1), method = "euclidean") , method="complete")

# feature selection plan B
x<-apply(mydata,1,function(x) sd(x))
y<-order(x,decreasing=T)[1:args[1]]
mydata2<-mydata[match(names(x[y]),rownames(mydata)),]
colnames(mydata2)<-colname2
#file=paste("heatmap.wgbs.mhl.top-sd-",args[1],".pdf",sep="")
#pdf(file)
#rlt2<-heatmap(mydata2)
hc2 <- hclust( dist(t(mydata2), method = "euclidean") , method="complete")

# compare the two cluster result(correlation)
cor(cophenetic(hc1), cophenetic(hc2)) # 0.8836503  (correlation of cophenetic dissimilarity matrix)
save.image("Figure3a.heatmap.RData")
