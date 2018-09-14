rm(list=ls())
require(lumi)

load("") #Collapsed Eset

All_dataset <- read.table("CIBERSORT-Results.txt",sep="\t",header=T,row.names=1)

dataset.coll <- dataset.coll[,rownames(All_dataset)]
rownames(All_dataset) <- paste(rownames(All_dataset),dataset.coll$Class,sep="_") 


#Tengo solo i campioni statisticamente significativi
All_dataset <- All_dataset[All_dataset$P.value < 0.05,]


#6weeks
neut6 <- grep("\\NeuT_06",rownames(All_dataset))
balbc6 <- grep("\\BALBc_06",rownames(All_dataset))

lung <- length(colnames(All_dataset))-3

pdf("Weeks6_boxplot.pdf")
for ( i in 1:lung )
{
  m1 <- mean(All_dataset[neut6,i])
  m2 <- mean(All_dataset[balbc6,i])
  s <- sum(m1+m2)

  if(s != 0)
  {
    boxplot(All_dataset[balbc6,i],All_dataset[neut6,i],main=colnames(All_dataset)[i],col=c("lightblue","darkviolet"),las=2,names=c("BALBc_6","NeuT_6"),xlim=c(0.5,3))
    vett <- c(All_dataset[balbc6,i],All_dataset[neut6,i])
    text(2.75, max(vett), paste0("p < ", round(t.test(All_dataset[balbc6,i],All_dataset[neut6,i])$p.value, 3)))

  
  p.val <- round(t.test(All_dataset[balbc6,i],All_dataset[neut6,i])$p.value,3)
  
  if ( p.val < 0.06 )
  {
    setwd("Significativi/")
    tt <- paste("weeks6",i,sep="_")
    tt <- paste(tt,"jpeg",sep=".")
    jpeg(tt)
    boxplot(All_dataset[balbc6,i],All_dataset[neut6,i],main=colnames(All_dataset)[i],col=c("lightblue","darkviolet"),las=2,names=c("BALBc_6","NeuT_6"),xlim=c(0.5,3))
    vett <- c(All_dataset[balbc6,i],All_dataset[neut6,i])
    text(2.75, max(vett), paste0("p < ", round(t.test(All_dataset[balbc6,i],All_dataset[neut6,i])$p.value, 3)))
    dev.off()
    setwd("../")
  }
  
  }
                   
}
dev.off()
