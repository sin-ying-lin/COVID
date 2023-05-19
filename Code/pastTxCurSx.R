subData.naOmit = read.csv('../Results/subData.naOmit.csv')

subData.naOmit.std = 
  subData.naOmit %>%
  filter(race != 5) %>% #too few Pacific Islander
  mutate(across(c(3:5, 12:20, 22:25),  as.factor)) %>% 
  mutate(across(c(6:11), scale))
str(subData.naOmit.std)  

#Individuals who has ever wanted treatment or rated thier mental health condition in before COVID as poor or fair 

PHQ_pastTx = summary(lm(PHQ.mean ~ txHist.past + (gender + race + eth), 
                        subData.naOmit.std[subData.naOmit.std$txHist.neverWanted == 0|subData.naOmit.std$baseline <= -1,]))

GAD_pastTx = 
  summary(lm(GAD.mean ~ txHist.past + (gender + race + eth), 
             subData.naOmit.std[subData.naOmit.std$txHist.neverWanted == 0|subData.naOmit.std$baseline <= -1,]))
IUS_pastTx = 
  summary(lm(IUS.mean ~ txHist.past + (gender + race + eth), 
             subData.naOmit.std[subData.naOmit.std$txHist.neverWanted == 0|subData.naOmit.std$baseline <= -1,]))
PTSD_pastTx = 
  summary(lm(PTSD.mean ~ txHist.past + (gender + race + eth), 
             subData.naOmit.std[subData.naOmit.std$txHist.neverWanted == 0|subData.naOmit.std$baseline <= -1,]))
overallSev_pastTx =
  summary(lm(overallSev ~ txHist.past + (gender + race + eth), 
             subData.naOmit.std[subData.naOmit.std$txHist.neverWanted == 0|subData.naOmit.std$baseline <= -1,]))

for(x in ls(pattern = 'pastTx')){
  print(x)
  print(round(get(x)$coefficients,3))
}
