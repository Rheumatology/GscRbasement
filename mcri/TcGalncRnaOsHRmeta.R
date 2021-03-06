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

# i=grep("ENSG00000231246",rownames(input))          # PANC246
# i=grep("ENSG00000213754",rownames(input))          # PANC754
# i=grep("ENSG00000181896",rownames(input))          # ZNF101
# i=grep("ENSG00000131849",rownames(input))          # ZNF132
# i=grep(Symbol2ENSG("NCOA4")[1,2],rownames(input))  # NCOA4
# i=grep(Symbol2ENSG("SLC7A11")[1,2],rownames(input))# SLC7A11
# i=grep(Symbol2ENSG("SAT2")[1,2],rownames(input))   # SAT2
# i=grep(Symbol2ENSG("GSS")[1,2],rownames(input))    # GSS
# i=grep("ENSG00000120256",rownames(input))          # LRP11

xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/ferroptosis/master/codependency.txt",as.is=T)[,1]
xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/ferroptosis/master/ferroptosis.genelist.txt",sep="\t",as.is=T)[,2]
xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/ferroptosis/master/SLC7A11.codependency.txt",as.is=T)[,1]
xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/breast/master/extdata/brca.tcga.target.hg19.bed",sep="\t",as.is=T)[,4]
xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/cholangiocarcinoma/master/cholangiocarcinoma.hg19.bed",sep="\t",as.is=T)[,4]
xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/drugtarget/master/extdata/2993drugtarget.txt",sep="\t",as.is=T)[,1]
xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/encode/master/TFBS/685TFBS.txt",sep="\t",as.is=T)[,1]
xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/cholangiocarcinoma/master/cholangiocarcinoma.V2.hg19.bed",sep="\t",as.is=T)[,4]
xii<-read.table("https://raw.githubusercontent.com/Shicheng-Guo/lncRNA/master/extdata/tcgameta/1668.ncRNA.dge.txt",sep="\t",as.is=T)[,1]

# mkdir /mnt/bigdata/Genetic/Projects/shg047/meta/tfbs/
# mkdir /mnt/bigdata/Genetic/Projects/shg047/meta/tfbs/os
# mkdir /mnt/bigdata/Genetic/Projects/shg047/meta/tfbs/dge
# mkdir /home/guosa/hpc/methylation/chol/meta/os

setwd("/mnt/bigdata/Genetic/Projects/shg047/meta/drug")
setwd("/mnt/bigdata/Genetic/Projects/shg047/meta/tfbs/dge")
setwd("/mnt/bigdata/Genetic/Projects/shg047/meta/tfbs/os")
setwd("/home/guosa/hpc/methylation/chol/meta/os")
setwd("/home/guosa/hpc/project/lncrna/meta/os")

ENSG<-Symbol2ENSG(as.character(xii))
xgene<-c(as.character(ENSG[,2]))
ii<-unique(unlist(lapply(xgene,function(x) grep(x,rownames(input)))))
ii<-unique(unlist(lapply(xii,function(x) grep(x,rownames(input)))))
ii<-na.omit(match(xii,rownames(input)))

out2<-c()
z<-1

for(i in ii){
z<-z+1
HR<-c()
for(TCGAProject in TCGAProjects){
  newdata<-input[,phen$project_id==paste("TCGA-",TCGAProject,sep="")]
  xphen<-phen[phen$project_id==paste("TCGA-",TCGAProject,sep=""),]
  dat<-data.frame(Rna=newdata[i,],xphen)
  thres<-mean(dat[,1],na.rm=T)
  #dat$Rna[dat$Rna<=thres]<-0
  #dat$Rna[dat$Rna>thres]<-1
  hr.fit<-summary(coxph(Surv(week,censored)~Rna,dat))
  hr1=hr.fit$coefficients[1,]
  hr2=hr.fit$conf.int[1,]
  HR<-rbind(HR,c(hr1,hr2[3],hr2[4]))
  
  fit <- survfit(Surv(week,censored)~Rna, data = dat)
  # survp<-ggsurvplot(fit, data = dat,conf.int = F,pval = TRUE,
  #                 fun = "pct",risk.table = TRUE,size = 1,linetype = "strata",
  #                 palette = c("#2E9FDF","#E7B800"),
  #                 legend = "bottom",legend.title = rownames(input)[i],
  #                 legend.labs = c("Low-expression","High-expression"))
  # ggsave(file = paste(ENSG2Symbol(rownames(input)[i]),"-",rownames(input)[i],"_",TCGAProject,"_KM.pdf",sep=""), survp$plot)
}

print(c(z,i))
rownames(HR)<-TCGAProjects
m<-metagen(HR[,1],seTE=HR[,3],comb.fixed = TRUE,comb.random = TRUE,prediction=F,sm="HR")
fixedEffect<-c(exp(m$TE.fixed),exp(m$lower.fixed),exp(m$upper.fixed),m$pval.fixed)
randomEffect<-c(exp(m$TE.random),exp(m$lower.random),exp(m$upper.random),m$pval.random)
out2<-rbind(out2,c(fixedEffect,randomEffect))

if(! is.na(fixedEffect[4]) & fixedEffect[4]<1 & randomEffect[4]<1){
pdf(paste(ENSG2Symbol(rownames(input)[i]),"-",rownames(input)[i],".OS.HR.PANC.pdf",sep=""))
print(rownames(input)[i])
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
       digits.sd = 2,fontsize=9,xlim=c(0.2,3))
dev.off()
write.table(HR,file=paste(ENSG2Symbol(rownames(input)[i]),"-",rownames(input)[i],".OS.HR.EACH.txt",sep=""),sep="\t",quote=F,col.names=NA,row.names=T)
}
}
colnames(out2)<-c("TE.fixed","lower.fixed","upper.fixed","pval.fixed","TE.random","lower.random","upper.random","pval.random")
rownames(out2)<-rownames(input)[ii]
out3<-data.frame(out2)
out3<-out3[order(out3$pval.random),]
write.csv(out3,file=paste("pancancer.lncRNA.rnaseq.","OS.HR.csv",sep=""),quote=F)
write.table(out3,file=paste("pancancer.lncRNA.rnaseq.","OS.HR.txt",sep=""),quote=F,sep="\t",col.names=NA,row.names=T)

# install.packages("CMplot")
library("CMplot")
cminput<-data.frame(SNP=output$V5,Chromosome=output$V1,Position=output$V2,trait1=output[,7])
CMplot(cminput,plot.type="b",LOG10=TRUE,threshold=NULL,file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE,width=14,height=6)
write.table(cminput,file="Pancancer.drug.manhattan.qqplot.meta.dge.txt",sep="\t",quote=F,row.name=T,col.names=NA)

# merge with DGE result
out1<-read.csv("../dge/pancancer.chol.dmg.smd.meta.pvalue.csv")
out<-merge(out1,out3,by.x="X",by.y="symbol")
pick<-subset(out,pval<10^-3 & pval.fixed<0.05 & pval.random<0.05 & beta*(TE.fixed-1)>0)
write.csv(out,file=paste("pancancer.chol.overall.rnaseq.dmg.smd","os.hr.pick.csv",sep=""),quote=F)
write.table(out,file=paste("pancancer.chol.overall.rnaseq.dmg.smd","os.hr.pick.txt",sep=""),quote=F,col.names=NA,row.names=T,sep="\t")
write.csv(pick,file=paste("pancancer.chol.pick.rnaseq.dmg.smd","os.hr.pick.csv",sep=""),quote=F)
write.table(pick,file=paste("pancancer.chol.pick.rnaseq.dmg.smd","os.hr.pick.txt",sep=""),quote=F,col.names=NA,row.names=T,sep="\t")


out1<-read.csv("./dge/drugtarget.dmg.smd.meta.table.pvalue.csv")
out<-merge(out1,out3,by.x="X",by.y="symbol")
pick<-subset(out,pval<10^-5 & pval.fixed<0.05 & pval.random<0.05 & beta*(TE.fixed-1)>0)
write.csv(pick,file=paste("pancancer.drugtarget.pick.rnaseq.dmg.smd","os.hr.pick.csv",sep=""),quote=F)

pick1<-subset(out,pval<10^-5 & pval.fixed<0.05 & pval.random<0.05 & beta*(TE.fixed-1)>0 & beta >0)
pick2<-subset(out,pval<10^-5 & pval.fixed<0.05 & pval.random<0.05 & beta*(TE.fixed-1)>0 & beta <0)

write.csv(pick1,file=paste("pancancer.drugtarget.112upregulated.pick.rnaseq.dmg.smd","os.hr.pick.csv",sep=""),quote=F)
write.csv(pick2,file=paste("pancancer.drugtarget.53downregulated.pick.rnaseq.dmg.smd","os.hr.pick.csv",sep=""),quote=F)


pick<-subset(out,pval<10^-7 & pval.fixed<0.01 & pval.random<0.05 & beta*(TE.fixed-1)>0)
for(i in 1:nrow(pick)){
x<-paste("cp ./dge/",pick[i,1],"-* ./pick",sep="")
y<-paste("cp ./os/",pick[i,1],"-* ./pick",sep="")
system(x)
system(y)
}


colnames(out2)<-c("TE.fixed","lower.fixed","upper.fixed","pval.fixed","TE.random","lower.random","upper.random","pval.random")
rownames(out2)<-rownames(input)[ii]
out2$Symbol<-as.character(ENSG2Symbol(as.character(rownames(out2))))

out2<-out2[order(out2$pval.random),]
write.csv(out2,file=paste("pancancer.tfbs.rnaseq.","OS.HR.csv",sep=""),quote=F)
write.table(out2,file=paste("pancancer.tfbs.rnaseq.","OS.HR.txt",sep=""),quote=F,sep="\t",col.names=NA,row.names=T)

out1<-read.csv("../dge/pancancer.tfbs.dmg.smd.meta.pvalue.csv")
out<-merge(out1,out2,by.x="X",by.y="Symbol")
pick<-subset(out,pval<10^-5 & pval.fixed<0.05 & pval.random<0.05 & beta*(TE.fixed-1)>0)
write.csv(pick,file=paste("pancancer.tfbs.pick.rnaseq.dmg.smd","os.hr.pick.csv",sep=""),quote=F)









