###################
#Current Treatment#
###################
library(dplyr)
library(tidyr)
library(purrr)

subData = read.csv('../Results/subData.csv', header = T)
txData = subData[ ,c(1:11, 22:25)]
str(txData) #1903

#Age 
txData_clean = txData[-which(txData$age < 18), ] 
nrow(txData_clean) #N = 2410 #1900

#Contradictory Tx History
txData_clean = txData_clean[-which(txData_clean$txHist.neverWanted == 1 & 
                                     txData_clean$txHist.wanted == 1), ]
nrow(txData_clean) #1889
str(txData_clean)
txData_naOmit = na.omit(txData_clean)
nrow(txData_naOmit) #1,423

txData_naOmit = txData_naOmit[(txData_naOmit$race != 5 & 
                                txData_naOmit$race != 3),] #too few Pacific Islander and Native American


1415/1903 #74.36
str(txData_naOmit)
txData_naOmit$overallSev = rowMeans(apply(txData_naOmit[,7:10], 2, scale))
write.csv(txData_naOmit, '../Results/txData_naOmit.csv', row.names = F)
range(txData_naOmit$age)
sd(txData_naOmit$age)


###############################################################
#Demographic Comparisons Between Included and Excluded Samples#
###############################################################

drop = c(1:nrow(subData))[-which(subData$id %in% txData_naOmit$id)]
nrow(txData[-drop,])

#age
age.diff = t.test(subData$age[drop], subData$age[-drop], paired = F, na.action = na.omit) 
age.mean.inc = round(mean(subData$age[-drop]), 2)
age.mean.exc = round(mean(subData$age[drop], na.rm = T), 2)
age.sd.inc = round(sd(subData$age[-drop]), 2)
age.sd.exc = round(sd(subData$age[drop], na.rm = T), 2)
age.meanSd.inc = paste0(age.mean.inc, ' (', age.sd.inc,')')
age.meanSd.exc = paste0(age.mean.exc, ' (', age.sd.exc,')')
age.stats = paste0('t(', round(age.diff$parameter, 2), ') = ', round(age.diff$statistic, 2), '; p = ', round(age.diff$p.value, 3))

#gender
gender.diff = chisq.test(cbind(table(subData$gender[-drop]),
                               table(subData$gender[drop]))) 
gender.table.inc = paste0(round(table(subData$gender[-drop])/nrow(subData[-drop,]), 4)*100, '%')
gender.table.exc = paste0(round(table(subData$gender[drop])/
                                  sum(table(subData$gender[drop])), 4)*100, '%')
gender.stats = paste0('X2(',gender.diff$parameter,') = ', round(gender.diff$statistic, 2), 
                      '; p = ', round(gender.diff$p.value, 3) )

#race
race.diff = chisq.test(cbind(table(subData$race[-drop]),
                             table(subData$race[drop])[c(1:2,4,6)])) #insig
race.table.inc = paste0(round(table(subData$race[-drop])/nrow(subData[-drop,]), 4)*100, '%')
race.table.exc = paste0(round(table(subData$race[drop])/
                                sum(table(subData$race[drop])), 4)*100, '%')
race.table.inc = c(race.table.inc[1:2], '0.00%',  race.table.inc[3], '0.00%',  race.table.inc[4]) 
race.stats = paste0('X2(',race.diff$parameter,') = ', round(race.diff$statistic, 2), 
                    '; p = ', round(race.diff$p.value, 3) )

#ethnicity
eth.diff = chisq.test(cbind(table(subData$eth[-drop]),
                            table(subData$eth[drop]))) #insig
eth.table.inc = paste0(round(table(subData$eth[-drop])/nrow(subData[-drop,]), 4)*100, '%')
eth.table.exc = paste0(round(table(subData$eth[drop])/
                               sum(table(subData$eth[drop])), 4)*100, '%')
eth.stats = paste0('X2(',eth.diff$parameter,') = ', round(eth.diff$statistic, 2), 
                   '; p = ', round(eth.diff$p.value, 3) )


#Comparisons between included and excluded samples
incExcComp = data.frame(
  INC = c(age.meanSd.inc, gender.table.inc, race.table.inc, eth.table.inc),
  EXC = c(age.meanSd.exc, gender.table.exc, race.table.exc, eth.table.exc),
  stats = c(age.stats, rep(gender.stats, length(gender.table.inc)), 
            rep(race.stats, length(race.table.inc)), 
            rep(eth.stats, length(eth.table.inc))) 
)
rownames(incExcComp) = c('Age',
                         'Men', 'Women', 'Gender Minority',
                         'White', 'Black', 'Native', 'Asian', 'Hawaiian/Pacific Islander', 'Multiple',
                         'Non-Hispanic', 'Hispanic')
write.csv(incExcComp, '../Results/incExcComp.csv', row.names = T)


#############
#Descriptive#
#############

descriptive.stats = function(x){
  mean = colMeans(x, na.rm = T)
  sd = apply(x, 2, sd, na.rm = T)
  sd.adj = ifelse(sd == 0, NA, sd)
  desc.table = data.frame(
    mean_sd = paste0(round(mean, 2), ' (',
                     round(sd.adj, 2),')'),
    min = round(apply(x, 2, min, na.rm = T), 2),
    max = round(apply(x, 2, max, na.rm = T), 2)
    
  )
  return(desc.table)
}

txDescriptive =  
  as.data.frame(txData_naOmit %>% select(colnames(txData_naOmit)[6:11]) %>% 
                  group_map(~descriptive.stats(.x)))

table(txData_naOmit$txHist.now)/1415
table(txData_naOmit$txHist.past)/1415
table(txData_naOmit$txHist.wanted)/1415
table(txData_naOmit$txHist.neverWanted)/1415

cor(txData_naOmit[,7:11])

###################################
#Paired T-test with Control Groups#
###################################
txData_naOmit$eth2 = ifelse(txData_naOmit$eth == 0 & txData_naOmit$race == 1, 0, NA)
txData_naOmit$eth2 = ifelse(txData_naOmit$eth == 1, 1, txData_naOmit$eth2)
table(txData_naOmit$eth2)

library(effsize)
#Gender (control: men)
descriptive.mentalHealth.gender =  
  as.data.frame(txData_naOmit %>% group_by(gender) %>% select(colnames(txData_naOmit)[6:11]) %>% 
                  group_map(~descriptive.stats(.x)))

gender.t.test = 
  txData_naOmit[,c(2:11, 16)] %>% 
  gather(key = variable, value = value, -c(age, gender, race, eth, eth2)) %>%
  group_by(gender, variable) %>%
  summarize(value = list(value)) %>%
  spread(gender, value) %>%
  group_by(variable) %>%
  summarize(
    t_value_1vs2 = t.test(unlist(`1`), unlist(`2`))$statistic,
    p_value_1vs2 = t.test(unlist(`1`), unlist(`2`))$p.value,
    t_value_1vs3 = t.test(unlist(`1`), unlist(`3`))$statistic,
    p_value_1vs3 = t.test(unlist(`1`), unlist(`3`))$p.value,
    d_value_1vs2 = cohen.d(unlist(`1`), unlist(`2`))$estimate,
    d_value_1vs2low = cohen.d(unlist(`1`), unlist(`2`))$conf.int[1],
    d_value_1vs2high = cohen.d(unlist(`1`), unlist(`2`))$conf.int[2],
    d_value_1vs3 = cohen.d(unlist(`1`), unlist(`3`))$estimate, 
    d_value_1vs3low = cohen.d(unlist(`1`), unlist(`3`))$conf.int[1],
    d_value_1vs3high = cohen.d(unlist(`1`), unlist(`3`))$conf.int[2]
    )
gender.t.test[,c(1,9:11)]

#Race (Control: Caucasian)
descriptive.mentalHealth.race =  
  as.data.frame(txData_naOmit%>% group_by(race) %>% 
                  select(colnames(txData_naOmit)[6:11]) %>% 
                  group_map(~descriptive.stats(.x)))

race.t.test = 
txData_naOmit[ ,c(2:11,16)] %>% 
  gather(key = variable, value = value, -c(age, gender, race, eth, eth2)) %>%
  group_by(race, variable) %>%
  summarize(value = list(value)) %>%
  spread(race, value) %>%
  group_by(variable) %>%
  summarize(
    t_value_1vs2 = t.test(unlist(`1`), unlist(`2`))$statistic,
    p_value_1vs2 = t.test(unlist(`1`), unlist(`2`))$p.value,
    t_value_1vs4 = t.test(unlist(`1`), unlist(`4`))$statistic,
    p_value_1vs4 = t.test(unlist(`1`), unlist(`4`))$p.value,
    t_value_1vs6 = t.test(unlist(`1`), unlist(`6`))$statistic,
    p_value_1vs6 = t.test(unlist(`1`), unlist(`6`))$p.value,
    d_value_1vs2 = cohen.d(unlist(`1`), unlist(`2`))$estimate,
    d_value_1vs2low = cohen.d(unlist(`1`), unlist(`2`))$conf.int[1],
    d_value_1vs2high = cohen.d(unlist(`1`), unlist(`2`))$conf.int[2],
    
    d_value_1vs4 = cohen.d(unlist(`1`), unlist(`4`))$estimate,
    d_value_1vs4low = cohen.d(unlist(`1`), unlist(`4`))$conf.int[1],
    d_value_1vs4high = cohen.d(unlist(`1`), unlist(`4`))$conf.int[2],
    
    d_value_1vs6 = cohen.d(unlist(`1`), unlist(`6`))$estimate,
    d_value_1vs6low = cohen.d(unlist(`1`), unlist(`6`))$conf.int[1],
    d_value_1vs6high = cohen.d(unlist(`1`), unlist(`6`))$conf.int[2])

race.t.test[,c(1,11:13)]
race.t.test[,c(1,14:16)]
#Ethnicity (Control: Non-Hispanic)
descriptive.mentalHealth.eth =  
  as.data.frame(txData_naOmit %>%
                  filter(!(is.na(eth2))) %>% 
                  group_by(eth2) %>% 
                  select(colnames(txData_naOmit)[6:11]) %>% 
                  group_map(~descriptive.stats(.x)))

eth.t.test = 
  txData_naOmit %>% 
  select(colnames(txData_naOmit)[c(2:11,16)]) %>%
  gather(key = variable, value = value, -c( age, gender, race, eth, eth2)) %>%
  group_by(eth2, variable) %>%
  summarize(value = list(value)) %>%
  spread(eth2, value) %>%
  group_by(variable) %>%
  summarize(
    t_value_0vs1 = t.test(unlist(`0`), unlist(`1`))$statistic,
    p_value_0vs1 = t.test(unlist(`0`), unlist(`1`))$p.value,
    d_value_0vs1 = cohen.d(unlist(`0`), unlist(`1`))$estimate,
    d_value_0vs1low = cohen.d(unlist(`0`), unlist(`1`))$conf.int[1],
    d_value_0vs1high = cohen.d(unlist(`0`), unlist(`1`))$conf.int[2],)

#Table Formation
addStar = function(x, y){
  if(is.na(x)) NA else
    if(x < 0.001/y) '***' else 
      if(x <0.01/y) '**' else
        if(x<.05/y) '*' else
          ''
  
}

for(x in c('gender', 'race', 'eth')){
  p_value  = 
    get(paste0( x,'.t.test')) %>%  select(contains('p_value')) 
  table.star = 
    as.data.frame(apply(p_value, c(1,2), addStar, 6))
  rownames(table.star) = get(paste0( x,'.t.test'))$variable
  table.meanSd = get(paste0('descriptive.mentalHealth.', x)) %>% select(contains('mean'))
  
  assign(paste0('table.comparisons.', x),
         if(ncol(p_value) >1) {
           data.frame(table.meanSd[,1],
                      as.data.frame(map2(table.meanSd[,-1], 
                                         table.star[rownames(table.meanSd),], paste0)))
           
         }else
           data.frame(table.meanSd[,1],
                      paste0(table.meanSd[,-1], table.star[rownames(table.meanSd),]))
  )
  
}

table.comparisons = cbind(table.comparisons.gender, 
                          table.comparisons.race, 
                          table.comparisons.eth)
rownames(table.comparisons) = c('Baseline', 'PHQ', 'GAD', 'IUS', 'PTSD', 'Severity')
colnames(table.comparisons) = c('Men', 'Women', 'Minority', 
                                'Caucasian', 'Black', 'Asian', 'Multiple',
                                'Non-Hispanic White', 'Hispanic')
table.comparisons
write.csv(table.comparisons, '../Results/mentalHealthDemoComparisons.csv', row.names = T)

summary(lm(PHQ.mean ~ as.factor(race) + baseline, txData_naOmit)) #6 multiracial
summary(lm(GAD.mean ~ as.factor(race) + baseline, txData_naOmit)) #4
summary(lm(IUS.mean ~ as.factor(race) + baseline, txData_naOmit)) 
summary(lm(PTSD.mean ~ as.factor(race) + baseline, txData_naOmit)) #4 
summary(lm(overallSev ~ as.factor(race) + baseline, txData_naOmit)) #4 

summary(lm(PHQ.mean ~ as.factor(eth) + baseline, txData_naOmit)) #2
summary(lm(GAD.mean ~ as.factor(eth) + baseline, txData_naOmit)) #2
summary(lm(IUS.mean ~ as.factor(eth) + baseline, txData_naOmit)) 
summary(lm(PTSD.mean ~ as.factor(eth) + baseline, txData_naOmit)) #2
summary(lm(overallSev ~ as.factor(eth) + baseline, txData_naOmit)) #2

summary(lm(PHQ.mean ~ as.factor(gender) + baseline, txData_naOmit)) #2, 3
summary(lm(GAD.mean ~ as.factor(gender) + baseline, txData_naOmit)) #2
summary(lm(IUS.mean ~ as.factor(gender) + baseline, txData_naOmit)) #2,3
summary(lm(PTSD.mean ~ as.factor(gender) + baseline, txData_naOmit)) #2
summary(lm(overallSev ~ as.factor(gender) + baseline, txData_naOmit)) #2, 3
