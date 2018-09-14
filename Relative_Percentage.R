#--------------------------------------------------------------------------------------------------------------
# Relative Percentage
#--------------------------------------------------------------------------------------------------------------

load() #collapsed eset

#Add an id variable for the filled regions
library(reshape)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

#Tengo solo i campioni statisticamente significativi
All_dataset <- read.table("CIBERSORT-Results.txt",sep="\t",header=T,row.names=1)
All_dataset <- All_dataset[All_dataset$P.value < 0.05,]

datm <- melt(cbind(All_dataset, ind = rownames(All_dataset)), id.vars = c('ind'))
datm <- datm[!datm$variable%in%c("RMSE","Correlation","P.value"),]

temp <- datm$variable
datm$variable <- datm$ind
datm$ind <- temp

colnames(datm) <- c("legend","samples","relative.percentage")


a <- c(brewer.pal(9,"Set1"),brewer.pal(7,"PuOr"),brewer.pal(11,"Spectral"))

a <- c("#99CC00","#CCCCCC","#FFCC00","#FF9900","#FF6600",
       "#CC3300","#FFFF00","#CC9900","#99CC00","#CCFF33",
       "#00FF33","#33FFFF","#3399FF","#0066CC","#0000CC",
       "#993300","#996600","#660000","#FF0000","#330000",
       "#6600CC","#0099FF","#00CCFF","#003300","#FF00CC")

#Classi biologiche in cui divido i dataset

... wt <- datm[grep("...",datm$samples),]
... other <- datm[grep("...",datm$samples),]

... wt$samples <- dataset.coll$GF_ID[colnames(dataset.coll)%in%gsub("...","",...$samples)] 

b6 <- ggplot(... wt ,aes(x = samples, y = relative.percentage,fill = legend)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=a) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(legend.key.size = unit(0.7,"line")) +
  guides(fill=guide_legend(ncol=1))


... other$samples <- dataset.coll$GF_ID[colnames(dataset.coll)%in%gsub("... other","",... other$samples)] 

n6 <- ggplot(... other,aes(x = samples, y = relative.percentage,fill = legend)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=a) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(legend.key.size = unit(0.7,"line")) +
  guides(fill=guide_legend(ncol=1))

dir.create("Relative_percentage")
pdf("Relative_percentage/...pdf")
print(b6 + ggtitle(label = "...") + theme(plot.title = element_text(hjust = 0.5)))
dev.off()

pdf("Relative_percentage/...pdf")
print(n6 + ggtitle(label = "...") + theme(plot.title = element_text(hjust = 0.5)))
dev.off()
