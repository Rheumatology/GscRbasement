source("https://raw.githubusercontent.com/Shicheng-Guo/GscRbasement/master/GscTools.R")
library("meta")
library("metafor")
library("survival")
library("survminer")

Symbol2ENSG<-function(Symbol){
  db<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/AnnotationDatabase/master/ENSG.ENST.ENSP.Symbol.hg19.bed",sep="\t")
  ENSG<-as.character(db[match(Symbol,db$V4),8])
  ENSG<-na.omit(data.frame(Symbol,ENSG))
  return(ENSG)
}
ENSG2Symbol<-function(ENSG){
  db<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/AnnotationDatabase/master/ENSG.ENST.ENSP.Symbol.hg19.bed",sep="\t")
  ENSG<-unlist(lapply(strsplit(ENSG,split="[.]"),function(x) x[1]))
  Symbol<-db[match(as.character(ENSG),db$V8),4]
  return(Symbol)
}

ensg2bed<-function(ENSG){
  db<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/AnnotationDatabase/master/hg19/ENSG.ENST.hg19.txt",as.is=T,head=F)
  ENSG<-unlist(lapply(strsplit(ENSG,split="[.]"),function(x) x[1]))
  bed<-unique(db[db$V5 %in% as.character(ENSG),c(1,2,3,5)])
  return(bed)
}

chr2num<-function(x){
  x<-output$V1
  x<-gsub("chr","",x)
  x[x=="X"]<-23
  x[x=="Y"]<-24
  return(x)
}


load("~/hpc/methylation/Pancancer/RNA-seq/rnaseqdata.pancancer.RData")

TCGAProjects=c("BLCA","BRCA","CESC","CHOL","COAD","ESCA","GBM","HNSC","KICH","KIRC","KIRP","LIHC","LUAD","LUSC","PAAD","PCPG","PRAD","READ","SARC","STAD","THCA","THYM","UCEC")
panc<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/PANC/master/extdata/panc.txt",head=T)
phen1=read.table("https://raw.githubusercontent.com/Shicheng-Guo/PANC/master/extdata/TCGA-clinical-11093.tsv",header = T,sep="\t")
phen2=read.table("https://raw.githubusercontent.com/Shicheng-Guo/PANC/master/extdata/File_metadata2.txt",header = T,sep="\t")
head(phen1)
head(phen2)
colnames(rnaseqdata)<-unlist(lapply(strsplit(colnames(rnaseqdata),"/"),function(x) x[2]))
phen<-data.frame(phen2,phen1[match(phen2$cases.0.case_id,phen1$case_id),])
phen$file_name=gsub(".gz","",phen$file_name)
# prepare phenotype information
phen<-phen[match(colnames(rnaseqdata),phen$file_name),]
phen$phen4<-id2phen4(phen$cases.0.samples.0.submitter_id)
phen$phen3<-id2phen3(phen$cases.0.samples.0.submitter_id)
phen$phen2<-id2bin(phen$cases.0.samples.0.submitter_id)
phen$pid<-phen$project_id
head(phen)

OS<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/HowtoBook/master/TCGA/OverallSurvivalTime.txt",head=T,sep="\t")
# match survival information
idx<-which(c(phen$phen2==1))
phen<-phen[idx,]
input<-rnaseqdata[,idx]
input[1:5,1:5]
idx<-na.omit(match(OS$submitter_id,phen$phen3))
input<-log(input[,idx]+1,2)

phen<-phen[idx,]
phen<-data.frame(phen,OS[match(phen$phen3,OS$submitter_id),])
phen$censored<-as.numeric(!phen$censored)
phen$week=phen$time/7

library("CMplot")
ensg<-read.table("~/hpc/db/hg19/ENSG.hg19.bed")

library(limma)
library(AnnotationDbi)
library(org.Hs.eg.db)
tab <- getGeneKEGGLinks(species="hsa")
tab$Symbol <- mapIds(org.Hs.eg.db, tab$GeneID,column="SYMBOL", keytype="ENTREZID")
head(tab)

for(kname in names(which(table(tab$PathwayID)>1))){
  print(kname)  
  xout<-subset(tab,PathwayID==kname)
  knamedir<-gsub("path:","",kname)
  dir.create(knamedir)
  knamedirname<-paste("/mnt/bigdata/Genetic/Projects/shg047/meta/",knamedir,sep="")
  setwd(knamedirname)
  write.csv(xout,file=paste(knamedir,".kegg.list.csv",sep=""),quote=F)
  
  xii<-as.character(xout$Symbol)
  ENSG<-Symbol2ENSG(as.character(xii))
  xgene<-c(as.character(ENSG[,2]))
  ii<-na.omit(unique(unlist(lapply(xgene,function(x) grep(x,rownames(input))))))
  
  out<-c()
  z<-1
  for(i in ii){
    z<-z+1
    HR<-c()
    for(TCGAProject in TCGAProjects){
      newdata<-input[,phen$project_id==paste("TCGA-",TCGAProject,sep="")]
      xphen<-phen[phen$project_id==paste("TCGA-",TCGAProject,sep=""),]
      dat<-data.frame(Rna=newdata[i,],xphen)
      thres<-mean(dat[,1],na.rm=T)
      hr.fit<-summary(coxph(Surv(week,censored)~Rna,dat))
      hr1=hr.fit$coefficients[1,]
      hr2=hr.fit$conf.int[1,]
      HR<-rbind(HR,c(hr1,hr2[3],hr2[4]))
      #fit <- survfit(Surv(week,censored)~Rna, data = dat)
    }
    
    print(c(z,i,as.character(rownames(input)[i])))
    rownames(HR)<-TCGAProjects
    m<-metagen(HR[,1],seTE=HR[,3],comb.fixed = TRUE,comb.random = TRUE,prediction=F,sm="HR")
    fixedEffect<-c(exp(m$TE.fixed),exp(m$lower.fixed),exp(m$upper.fixed),m$pval.fixed)
    randomEffect<-c(exp(m$TE.random),exp(m$lower.random),exp(m$upper.random),m$pval.random)
    out<-rbind(out,c(fixedEffect,randomEffect))
    
    pdf(paste(ENSG2Symbol(rownames(input)[i]),"-",rownames(input)[i],".OS.HR.PANC.pdf",sep=""))
    forest(m,leftlabs = rownames(HR),
           lab.e = "Intervention",
           pooled.totals = FALSE,
           smlab = "",studlab=rownames(HR),
           text.random = "Overall effect",
           print.tau2 = FALSE,
           col.diamond = "blue",
           col.diamond.lines = "black",
           col.predict = "red",
           print.I2.ci = TRUE,
           digits.sd = 2,fontsize=9,xlim=c(0.5,2))
    dev.off()
    write.table(HR,file=paste(ENSG2Symbol(rownames(input)[i]),"-",rownames(input)[i],".OS.HR.EACH.txt",sep=""),sep="\t",quote=F,col.names=NA,row.names=T)
  }
  
  colnames(out)<-c("TE.fixed","lower.fixed","upper.fixed","pval.fixed","TE.random","lower.random","upper.random","pval.random")
  rownames(out)<-rownames(input)[ii]
  out3<-data.frame(out)
  out3<-out3[order(out3$pval.random),]
  out3$symbol<-as.character(ENSG2Symbol(as.character(rownames(out3))))
  
  memo=knamedir
  
  write.table(out3,file=paste(memo,".tcga.pancancer.meta.pvalue.txt",sep=""),sep="\t",quote=F,col.names = NA,row.names = T)
  write.csv(out3,file=paste(memo,".tcga.pancancer.meta.pvalue.csv",sep=""),quote=F)
  
  output<-data.frame(ensg[match(unlist(lapply(strsplit(rownames(out3),"[.]"),function(x) x[1])),ensg[,5]),],out3)
  
  cminput<-na.omit(data.frame(SNP=output$V5,Chromosome=chr2num(output$V1),Position=output$V2,trait1=output$pval.fix))
  CMplot(cminput,plot.type="b",memo=paste(memo,".fix",sep=""),LOG10=TRUE,threshold=NULL,file="jpg",dpi=300,file.output=TRUE,verbose=TRUE,width=14,height=6)
  write.table(cminput,file=paste(memo,".pval.fix.manhattan.qqplot.meta.dge.txt",sep=""),sep="\t",quote=F,row.name=T,col.names=NA)
  
  cminput<-na.omit(data.frame(SNP=output$V5,Chromosome=chr2num(output$V1),Position=output$V2,trait1=output$pval.random))
  CMplot(cminput,plot.type="b",memo=paste(memo,".random",sep=""),LOG10=TRUE,threshold=NULL,file="jpg",dpi=300,file.output=TRUE,verbose=TRUE,width=14,height=6)
  write.table(cminput,file=paste(memo,".pval.random.manhattan.qqplot.meta.dge.txt",sep=""),sep="\t",quote=F,row.name=T,col.names=NA)
  
  setwd("/mnt/bigdata/Genetic/Projects/shg047/meta/")
  
}


