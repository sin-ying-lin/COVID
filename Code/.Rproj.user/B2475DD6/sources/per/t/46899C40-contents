library(dplyr)
under = data[data$position==1, ] #N = 1666
grad = data[data$position %in% c(2,3), ] #N = 499

pdf('../Results/StudentMH.pdf', width=8,5, height=11)
par(mfrow=c(3,2))
under_PHQ = rowSums(under[ ,grep('PHQ_[1-9]', colnames(under))]-1)
under_PHQ_f =
  cut(under_PHQ, breaks = c(0, 5, 10, 15, 20, 25), 
      labels = c('Minimal', 'Mild', 'Moderate', 'Moderately \n severe', 'Severe'))
bpUnder_PHQ = barplot(table(under_PHQ_f)/sum(table(under_PHQ_f)), main='Depression in Undergrads',
                      xlab='Severity', font.lab=2, cex.names=0.9)
text(bpUnder_PHQ, 0, round(table(under_PHQ_f)/sum(table(under_PHQ_f)), 2), cex=1, pos=3)

grad_PHQ = rowSums(grad[ ,grep('PHQ_[1-9]', colnames(grad))]-1)
grad_PHQ_f =
  cut(grad_PHQ, breaks = c(0, 5, 10, 15, 20, 28), 
      labels = c('Minimal', 'Mild', 'Moderate', 'Moderately \n severe', 'Severe'))
bpGrad_PHQ = barplot(table(grad_PHQ_f)/sum(table(grad_PHQ_f)), main='Depression in Grad Students',
                     xlab='Severity', font.lab=2, cex.names=0.9)
text(bpGrad_PHQ, 0, round(table(grad_PHQ_f)/sum(table(grad_PHQ_f)), 2), cex=1, pos=3)

under_GAD = rowSums(under[ ,grep('GAD_[1-7]', colnames(under))]-1)
under_GAD_f =
  cut(under_GAD, breaks = c(0, 5, 10, 15, 22), 
      labels = c('Minimal', 'Mild', 'Moderate', 'Severe'))
bpUnder_GAD = barplot(table(under_GAD_f)/sum(table(under_GAD_f)), main='Anxiety in Undergrads',
                      xlab='Severity', font.lab=2)
text(bpUnder_GAD, 0, round(table(under_GAD_f)/sum(table(under_GAD_f)), 2), cex=1, pos=3)

grad_GAD = rowSums(grad[ ,grep('GAD_[1-7]', colnames(grad))]-1)
grad_GAD_f =
  cut(grad_GAD, breaks = c(0, 5, 10, 15, 22), 
      labels = c('Minimal', 'Mild', 'Moderate', 'Severe'))
bpGrad_GAD = barplot(table(grad_GAD_f)/sum(table(grad_GAD_f)), main='Anxiety in Grad Students',
                     xlab='Severity', font.lab=2)
text(bpGrad_GAD, 0, round(table(grad_GAD_f)/sum(table(grad_GAD_f)), 2), cex=1, pos=3)

under_PTSD = rowSums(under[ ,grep('PTSD_[1-4]', colnames(under))])
bpUnder_PTSD = barplot(table(under_PTSD)/sum(table(under_PTSD)), 
                      main='COVID-19 PTSD in Undergrads', xlab='Symptom Counts', font.lab=2)
text(bpUnder_PTSD, 0, round(table(under_PTSD)/sum(table(under_PTSD)), 2), cex=1, pos=3)
abline(v=3.7, col="Red",lty=5, lwd=3)
text(5, 0.25, 'Further \n Clinical Assessment \n Suggested',cex=1,col='Red')


grad_PTSD = rowSums(grad[ ,grep('PTSD_[1-4]', colnames(grad))])
bpGrad_PTSD = barplot(table(grad_PTSD)/sum(table(grad_PTSD)), 
                       main='COVID-19 PTSD in Grad Students', xlab='Symptom Counts', font.lab=2)
text(bpGrad_PTSD, 0, round(table(grad_PTSD)/sum(table(grad_PTSD)), 2), cex=1, pos=3)
abline(v=3.7, col="Red",lty=5, lwd=3)
text(5, 0.25, 'Further \n Clinical Assessment \n Suggested',cex=1,col='Red')

title("SBU Student Mental Heatlh ", line = -1, outer = TRUE, cex=3)
dev.off()

under_txHist = under[ ,grep('^txHist.', colnames(under))]
under_txHist$txHist.past = ifelse(under_txHist$txHist.now == 1, 0, under_txHist$txHist.past)
under_txHist$txHist.wanted = ifelse(under_txHist$txHist.now == 1|
                                      under_txHist$txHist.past == 1, 0, 
                                    under_txHist$txHist.wanted)
under_txHist$txHist.neverWanted = ifelse(under_txHist$txHist.now == 1|
                                      under_txHist$txHist.past == 1|
                                      under_txHist$txHist.wanted == 1, 0, 
                                      under_txHist$txHist.neverWanted)
library(tidyr)
library(scales)
under_txHist_f = na.omit(gather(under_txHist)[gather(under_txHist)[,2] == 1,])
under_txHist_table = as.data.frame(table(under_txHist_f))[c(2:4,1),]
under_txHist_table$label = c('Current', 'Past', 'Wanted', 'Never Wanted') 

grad_txHist = grad[ ,grep('^txHist.', colnames(grad))]
grad_txHist$txHist.past = ifelse(grad_txHist$txHist.now == 1, 0, grad_txHist$txHist.past)
grad_txHist$txHist.wanted = ifelse(grad_txHist$txHist.now == 1|
                                      grad_txHist$txHist.past == 1, 0, 
                                    grad_txHist$txHist.wanted)
grad_txHist$txHist.neverWanted = ifelse(grad_txHist$txHist.now == 1|
                                           grad_txHist$txHist.past == 1|
                                           grad_txHist$txHist.wanted == 1, 0, 
                                         grad_txHist$txHist.neverWanted)
grad_txHist_f = na.omit(gather(grad_txHist)[gather(grad_txHist)[,2] == 1,])
grad_txHist_table = as.data.frame(table(grad_txHist_f))[c(2:4,1),]
grad_txHist_table$label = c('Current', 'Past', 'Wanted', 'Never Wanted') 
txHist_table = rbind(under_txHist_table, grad_txHist_table)
txHist_table$group = c(rep('Under',4), rep('Grad', 4))

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
under_txHist_table$label = factor(under_txHist_table$label,
                                  levels = c('Never Wanted', 'Wanted', 'Past', 'Current'))
grad_txHist_table$label = factor(grad_txHist_table$label,
                                  levels = c('Never Wanted', 'Wanted', 'Past', 'Current'))


underTx = 
ggplot(under_txHist_table, aes(x="", y=Freq, fill = label))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0) + 
  ggtitle('Undergrad \n Mental Health Care Access') +
  labs(fill = "Tx History") + 
  geom_text(aes(y = Freq*0.5 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = percent(Freq/sum(Freq), accuracy=0.01)), size=5) + 
  scale_fill_manual(values=cbp1,limits=c('Current', 'Past', 'Wanted', 'Never Wanted')) +
  theme_minimal() +
  theme(
        plot.title = element_text(face='bold',  size=15, hjust=0.5, vjust=-1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size=8)) 
  
gradTx = 
ggplot(grad_txHist_table, aes(x="", y=Freq, fill = label))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0) + 
  labs(fill = "Tx History") + 
  ggtitle('Grad Student \n Mental Health Care Access') +
  geom_text(aes(y = Freq*0.5 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = percent(Freq/sum(Freq), accuracy=0.01)), size=5) + 
  scale_fill_manual(values=cbp1,limits=c('Current', 'Past', 'Wanted', 'Never Wanted')) +
  theme_minimal() +
  theme(
    plot.title = element_text(face='bold',  size=15, hjust=0.5, vjust=-1),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid  = element_blank(),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=8)) 

library(gridExtra)
library(ggpubr)
txPlot = ggarrange(underTx, gradTx,  
          ncol = 2, nrow = 1, heights = c(1,5))
ggsave('../Results/StudentTxHistory.pdf', txPlot, width = 11, height = 8.5)


tx