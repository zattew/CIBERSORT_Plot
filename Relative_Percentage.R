#--------------------------------------------------------------------------------------------------------------
# Relative Percentage
#--------------------------------------------------------------------------------------------------------------

load("C:/Users/milanimatteo/progetti/Renzi/Gep1_coll/dataset_GEP1_RSN_f1_coll.RData")

#Add an id variable for the filled regions
library(reshape)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)



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

Balbc6 <- datm[grep("BALBc_06",datm$samples),]
Neut6 <- datm[grep("NeuT_06",datm$samples),]

Balbc6$samples <- dataset.coll$GF_ID[colnames(dataset.coll)%in%gsub("_BALBc_06","",Balbc6$samples)] 

b6 <- ggplot(Balbc6,aes(x = samples, y = relative.percentage,fill = legend)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=a) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(legend.key.size = unit(0.7,"line")) +
  guides(fill=guide_legend(ncol=1))


Neut6$samples <- dataset.coll$GF_ID[colnames(dataset.coll)%in%gsub("_NeuT_06","",Neut6$samples)] 

n6 <- ggplot(Neut6,aes(x = samples, y = relative.percentage,fill = legend)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=a) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(legend.key.size = unit(0.7,"line")) +
  guides(fill=guide_legend(ncol=1))

pdf("Relative_percentage/Balbc_weeks6.pdf")
print(b6 + ggtitle(label = "Balbc at 6 weeks") + theme(plot.title = element_text(hjust = 0.5)))
dev.off()

pdf("Relative_percentage/NeuT_weeks6.pdf")
print(n6 + ggtitle(label = "NeuT at 6 weeks") + theme(plot.title = element_text(hjust = 0.5)))
dev.off()
