library(dplyr)
subData = read.csv('../Results/subData.csv')

barr = subData %>% 
  select(contains(c('barr', 'txHist', 'race', 'eth', 'age', 'gender')))
colnames(barr)

barrData = barr[-which(rowSums(is.na(barr[1:16]))  == 16), ]
nrow(barrData) #1245

group = c(rep(NA, nrow(barrData)))
group[(barrData$txHist.now == 1 & barrData$txHist.past == 1)] = 'exp' #N = 115
group[(barrData$txHist.now == 1 & barrData$txHist.past == 0)] = 'cur' #N = 117
group[(barrData$txHist.now == 0 & barrData$txHist.past == 1)] = 'past' #N = 545
group[(barrData$txHist.now == 0 & barrData$txHist.past == 0)] = 'per' #N = 408
barrData$group = group

#Demographics of each group
barrData %>% 
  group_by(group) %>% 
  group_map(~table(.x$race))


barrData %>% 
  group_by(group) %>% 
  group_map(~table(.x$eth)) %>% 
  as.data.frame()


barrData %>% 
  group_by(group) %>% 
  group_map(~table(.x$gender))

barrData %>% 
  group_by(group) %>% 
  summarize(across(contains('barr'),
            mean, na.rm = T)) 


barr_gender = 
  subData.naOmit.std %>% 
  group_by(gender) %>%
  select(contains('barr')) %>%
  summarise_all(mean, na.rm = T)

barr_race = 
  subData.naOmit.std %>% 
  group_by(race) %>%
  select(contains('barr')) %>%
  summarise_all(mean, na.rm = T)

barr_eth = 
  subData.naOmit.std %>% 
  group_by(eth) %>%
  select(contains('barr')) %>%
  summarise_all(mean, na.rm = T)


barr_gender_chi = as.data.frame(t(barr_gender[,-c(1:2)]))
chisq.test(barr_gender_chi) 

barr_race_chi = as.data.frame(t(barr_race[,-c(1:2)]))
chisq.test(barr_race_chi) 

barr_eth_chi = as.data.frame(t(barr_eth[,-c(1:2)]))
chisq.test(barr_eth_chi) 


for(x in substring(colnames(barr)[1:8], 12)){
  assign(paste0('tAll.',x),
         t.test(barr[ , paste0('barrBefore.',x)],
                barr[ , paste0('barrAfter.',x)]))
  
  d1All = na.omit(barr[ , paste0('barrBefore.',x)])
  d2All = na.omit(barr[ , paste0('barrAfter.',x)])
  n1All = length(d1All)
  n2All = length(d2All)
  
  
  assign(paste0('tExp.',x),
         t.test(barrData[barrData$txHist.now == 1 & barrData$txHist.past == 1, paste0('barrBefore.',x)],
                barrData[barrData$txHist.now == 1 & barrData$txHist.past == 1, paste0('barrAfter.',x)]))
  
  d1Exp = na.omit(barrData[barrData$txHist.now == 1 & barrData$txHist.past == 1, paste0('barrBefore.',x)])
  d2Exp = na.omit(barrData[barrData$txHist.now == 1 & barrData$txHist.past == 1, paste0('barrAfter.',x)])
  n1Exp = length(d1Exp)
  n2Exp = length(d2Exp)
  
  assign(paste0('dExp.',x),
         cohen.d(c(d1Exp,d2Exp), c(rep('0', n1Exp), rep('1',n2Exp))))
             
  assign(paste0('tPer.',x),
         t.test(barrData[barrData$txHist.now == 0 & barrData$txHist.past == 0, paste0('barrBefore.',x)],
                barrData[barrData$txHist.now == 0 & barrData$txHist.past == 0, paste0('barrAfter.',x)]))
  
  d1Per = na.omit(barrData[barrData$txHist.now == 0 & barrData$txHist.past == 0, paste0('barrBefore.',x)])
  d2Per = na.omit(barrData[barrData$txHist.now == 0 & barrData$txHist.past == 0, paste0('barrAfter.',x)])
  n1Per = length(d1Per)
  n2Per = length(d2Per)
  
  assign(paste0('dPer.',x),
         cohen.d(c(d1Per,d2Per), c(rep('0', n1Per), rep('1',n2Per))))
  
}



tAll.cost #insig.
tAll.avail #singnificant
tAll.logistics #sig increased
tAll.doItMyself #sig reduced
tAll.overwhelmed  #non
tAll.pastExperiences #sig reduced
tAll.stigma #nonsig
tAll.unsureHow #sig increased

tExp.cost #nonsig
tExp.avail #nosig
tExp.logistics #nonsig
tExp.doItMyself #watned to take care of myself gets lower
tExp.overwhelmed  #nonsig
tExp.pastExperiences #influence of negative past experinces gets lower
tExp.stigma #nonsig
tExp.unsureHow #nonsig


tPer.cost #nonsig
tPer.avail #therapist less available more difficult
tPer.logistics #transportation or scheudling issues gets higher
tPer.doItMyself #nonsig
tPer.overwhelmed  #nonsig
tPer.pastExperiences #nonsig
tPer.stigma #nonsig
tPer.unsureHow #gets higher



library(effsize)

i = 0
barrCompExp = matrix(NA, nrow = 8, ncol = 6)
for(x in ls(pattern = c('^tExp.[a-z]'))){
  i = i+1
  mean.before = get(x)$estimate[1]
  mean.after = get(x)$estimate[2]
  
  df = get(x)$parameter
  t = get(x)$statistic
  p_value = get(x)$p.value
  d = get(paste0('d',substring(x,2)))$estimate
  
  barrCompExp[i,1:6] = c(round(mean.before, 2),
                         round(mean.after, 2),
                         round(df, 2),
                         round(t, 2),
                         round(p_value, 3),
                         round(d, 2))  
}
barrCompExp

i = 0
barrCompPer = matrix(NA, nrow = 8, ncol = 6)
for(x in ls(pattern = c('^tPer.[a-z]'))){
  i = i+1
  mean.before = get(x)$estimate[1]
  mean.after = get(x)$estimate[2]
  
  df = get(x)$parameter
  t = get(x)$statistic
  p_value = get(x)$p.value
  d = get(paste0('d',substring(x,2)))$estimate
  
  barrCompPer[i,1:6] = c(round(mean.before, 2),
                         round(mean.after, 2),
                         round(df, 2),
                         round(t, 2),
                         round(p_value, 3),
                         round(d, 2))  
  
}


chisq.test(data.frame(expBeofre = barrCompExp[,1],
                      expAfter = barrCompExp[,2],
                      perBefore = barrCompPer[,1],
                      perAfter = barrCompPer[,2]))

chisq.test(data.frame(expBeofre = barrCompExp[,1],
                      expAfter = barrCompExp[,2]))

chisq.test(data.frame(perBefore = barrCompPer[,1],
                      perAfter = barrCompPer[,2]))
                      
chisq.test(data.frame(expBeofre = barrCompExp[,1],
                      perBefore = barrCompPer[,1])) # p = 0.015

chisq.test(data.frame(expAfter = barrCompExp[,2],
                      perAfter = barrCompPer[,2])) # p = 0.1049

barrComp = cbind(barrCompExp, barrCompPer)
barrComp[,c('exp.t', 'exp.d', 'per.t', 'per.d')] = 
  barrComp[,c('exp.t', 'exp.d', 'per.t', 'per.d')]*(-1) 

rownames(barrComp) = substring(ls(pattern = c('^tPer.[a-z]')), 6)
colnames(barrComp) = c('exp.mean.before', 'exp.mean.after', 'exp.df', 'exp.t', 'exp.p', 'exp.d',
                       'per.mean.before', 'per.mean.after', 'per.df', 'per.t', 'per.p', 'per.d')

write.csv(barrComp, '../Results/barrComp.csv', row.names = T)


upSample = upSample(x = subData.naOmit.std,
                    y = subData.naOmit.std$txHist.now)
train.control = trainControl(method = 'repeatedcv', number = 5, repeats = 10)
