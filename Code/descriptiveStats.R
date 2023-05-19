#################
#Data Extraction#
#################

library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)

data = read.csv('../Data/COVID_19_Survey_Baseline.Csv', header = T)
nrow(data) #2132
data = data[data$position %in% c(1:3), ] #1903

for(x in c('PHQ', 'GAD', 'IUS', 'PTSD')){
  assign(x, data[ ,grep(x, colnames(data))])
  assign(paste0(x,'.mean'), rowMeans(get(x), na.rm = T))
  assign(paste0(x,'.mean.z'), scale(get(paste0(x,'.mean'))))
}

overallSev = rowMeans(data.frame(PHQ.mean.z, GAD.mean.z, IUS.mean.z, PTSD.mean.z), na.rm = T)

probType = data[ ,grep('^probType', colnames(data))]
comorbidity = rowSums(probType, na.rm = T)
comorbidity[which(is.na(probType$probType.anxiety))] = NA
txHist = data[ ,grep('^txHist', colnames(data))]
barr = data[ ,grep("^barr", colnames(data))]
barr.mean = rowMeans(barr, na.rm = T)

gender
table(data$sex[data$genderDiff==0])
table(data$gender[data$genderDiff!=0])

gender = data$sex
data$gender[data$genderDiff == 1]
data$gender[data$genderDiff == 2]
gender[which(data$genderDiff != 0)] = data$gender[which(data$genderDiff != 0)]

data$gender[which(data$genderDiff!=0 & data$sex!= data$gender)]
data$genderDetailed[which(data$genderDiff!=0 & data$sex!= data$gender)]
data$genderDetailed[which(data$genderDiff!=0 & data$sex== data$gender)]

data$gender[which(data$sex[which(data$genderDiff != 0)] == data$gender[which(data$genderDiff != 0)])]
data$gender[which(data$sex[data$genderDiff != 0] != data$gender[data$genderDiff != 0])]

data$genderDetailed[which(data$gender == 3)]

which(data$sex[data$genderDiff != 0] != data$gender[data$genderDiff != 0])


subData = data.frame(id = data$responseID, age = data$age, gender = gender, race = data$race, eth = data$hispanic, 
                     baseline = data$before.mentalHealth,
                     PHQ.mean, GAD.mean, IUS.mean, PTSD.mean, overallSev, 
                     probType, comorbidity,  txHist, barr)
nrow(subData)

write.csv(subData, '../Results/subdata.csv', row.names = F)

######################
#Clean Data for Tx#
######################

naCount = function(x){length(which(is.na(x)))}
subData.c = subData[-which(apply(subData, 1, naCount) == ncol(subData)-1), ] #incomplete survey
nrow(subData.c) #2132 #1883

#Age 
subData.c = subData.c[-which(subData.c$age <18), ] 
nrow(subData.c) #N = 2129 #1880

#Contradictory Tx History
subData.c = subData.c[-which(subData.c$txHist.neverWanted == 1 & subData.c$txHist.wanted == 1), ]
subData.c[which(subData.c$txHist.neverWanted == 1) ,grep('probType|comorbidity|barr', colnames(subData.c))] = 0
subData.naOmit = na.omit(subData.c[,c(1:11)])
subData.naOmit = merge(subData.naOmit, subData.c[,grep('^id|^barr[A-B]', colnames(subData.c))], by = 'id', all.x = T)

nrow(na.omit(subData.c[,1:42]))

nrow(subData.naOmit) #1,631
write.csv(subData.naOmit, '../Results/subData.naOmit.csv', row.names = F)

nrow(subData.c)/nrow(subData) #87.77% completion rate


###############################################################
#Demographic Comparisons Between Included and Excluded Samples#
###############################################################

drop = c(1:nrow(subData))[-which(subData$id %in% subData.naOmit$id)]
subData[-drop, ]

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
race.diff = chisq.test(cbind(table(subData$race[-drop])[-5],
                 table(subData$race[drop]))) #insig
race.table.inc = paste0(round(table(subData$race[-drop])/nrow(subData[-drop,]), 4)*100, '%')
race.table.exc = paste0(round(table(subData$race[drop])/
                                  sum(table(subData$race[drop])), 4)*100, '%')
race.table.exc = c(race.table.exc[1:4], '0.00%',  race.table.exc[5]) 
race.stats = paste0('X2(',race.diff$parameter,') = ', round(race.diff$statistic, 2), 
                    '; p = ', round(race.diff$p.value, 3) )

#ethnicity
eth.diff = chisq.test(cbind(table(subData$his[-drop]),
                 table(subData$his[drop]))) #insig
eth.table.inc = paste0(round(table(subData$his[-drop])/nrow(subData[-drop,]), 4)*100, '%')
eth.table.exc = paste0(round(table(subData$his[drop])/
                                sum(table(subData$his[drop])), 4)*100, '%')
eth.stats = paste0('X2(',eth.diff$parameter,') = ', round(eth.diff$statistic, 2), 
                   '; p = ', round(eth.diff$p.value, 3) )


#Comparisons between included and excluded samples
incExcComp = data.frame(
  INC = c(age.meanSd.inc, gender.table.inc, race.table.inc, eth.table.inc),
  ENC = c(age.meanSd.exc, gender.table.exc, race.table.exc, eth.table.exc),
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

descriptive.mentalHealth =  
  as.data.frame(subData.naOmit %>% select(colnames(subData.naOmit)[6:21]) %>% 
  group_map(~descriptive.stats(.x)))

#######
#ANOVA#
#######

anovaTable = matrix(NA, nrow = 16, ncol = 6)
i = 0
for(x in colnames(subData.naOmit)[6:21]){
  i = i+1
  j = 0
  
  for(y in c('gender', 'race', 'eth')){
    j = j+1
    assign(paste0(x,'.',y,'Aov'),
           summary(aov(get(x) ~ as.factor(get(y)), subData.naOmit)))
    assign(paste0('F.',y), get(paste0(x,'.', y,'Aov')))
    
    anovaTable[i, (2*j-1):(2*j)] = c(
      round(get(paste0(x,'.',y,'Aov'))[[1]][1,4], 2),
      round(get(paste0(x,'.',y,'Aov'))[[1]][1,5], 3))
      
  }
}

mentalHealthDescAov = cbind(descriptive.mentalHealth, anovaTable)
colnames(mentalHealthDescAov) = c('M(SD)', 'Min', 'Max', 
                                  paste0('Gender.F(',  F.gender[[1]][1,1],' ,', F.gender[[1]][2,1], ')'),  
                                  'Gender.p',
                                  paste0('Race.F(',  F.race[[1]][1,1],' ,', F.race[[1]][2,1], ')'), 
                                  'Race.P',
                                  paste0('Ethnicity.F(',  F.eth[[1]][1,1],' ,', F.eth[[1]][2,1], ')'), 
                                  'Ethnicity.P')

write.csv(mentalHealthDescAov, '../Results/descriptive_aov.csv', row.names = T)

###################################
#Paired T-test with Control Groups#
###################################

#Gender (control: men)
descriptive.mentalHealth.gender =  
  as.data.frame(subData.naOmit %>% group_by(gender) %>% select(colnames(subData.naOmit)[6:21]) %>% 
                  group_map(~descriptive.stats(.x)))

gender.t.test = 
subData.naOmit %>% 
  select(colnames(subData.naOmit)[2:21]) %>%
  gather(key = variable, value = value, -c( age, gender, race, eth)) %>%
  group_by(gender, variable) %>%
  summarize(value = list(value)) %>%
  spread(gender, value) %>%
  group_by(variable) %>%
  summarize(
    t_value_1vs2 = t.test(unlist(`1`), unlist(`2`))$statistic,
    p_value_1vs2 = t.test(unlist(`1`), unlist(`2`))$p.value,
    t_value_1vs3 = t.test(unlist(`1`), unlist(`3`))$statistic,
    p_value_1vs3 = t.test(unlist(`1`), unlist(`3`))$p.value)

cbind(as.data.frame(gender.t.test)[,1],
     round(as.data.frame(gender.t.test)[,2:5],3))

round(gender.t.test)

#Race (Control: Caucasian)
descriptive.mentalHealth.race =  
  as.data.frame(subData.naOmit %>% group_by(race) %>% select(colnames(subData.naOmit)[6:21]) %>% 
                  group_map(~descriptive.stats(.x)))

race.t.test = 
subData.naOmit %>% 
  select(colnames(subData.naOmit)[2:21]) %>%
  gather(key = variable, value = value, -c(age, gender, race, eth)) %>%
  group_by(race, variable) %>%
  summarize(value = list(value)) %>%
  spread(race, value) %>%
  group_by(variable) %>%
  summarize(
    t_value_1vs2 = t.test(unlist(`1`), unlist(`2`))$statistic,
    p_value_1vs2 = t.test(unlist(`1`), unlist(`2`))$p.value,
    t_value_1vs3 = t.test(unlist(`1`), unlist(`3`))$statistic,
    p_value_1vs3 = t.test(unlist(`1`), unlist(`3`))$p.value,
    t_value_1vs4 = t.test(unlist(`1`), unlist(`4`))$statistic,
    p_value_1vs4 = t.test(unlist(`1`), unlist(`4`))$p.value,
    t_value_1vs5 = t.test(unlist(`1`), unlist(`5`))$statistic,
    p_value_1vs5 = t.test(unlist(`1`), unlist(`5`))$p.value,
    t_value_1vs6 = t.test(unlist(`1`), unlist(`6`))$statistic,
    p_value_1vs6 = t.test(unlist(`1`), unlist(`6`))$p.value)

race.t.test[,c(1,11)]
cbind(as.data.frame(race.t.test)[,1],
      round(as.data.frame(race.t.test)[,10:11],3))

#Ethnicity (Control: Non-Hispanic)

descriptive.mentalHealth.eth =  
  as.data.frame(subData.naOmit %>% group_by(eth) %>% select(colnames(subData.naOmit)[6:21]) %>% 
                  group_map(~descriptive.stats(.x)))

eth.t.test = 
  subData.naOmit %>% 
  select(colnames(subData.naOmit)[2:21]) %>%
  gather(key = variable, value = value, -c( age, gender, race, eth)) %>%
  group_by(eth, variable) %>%
  summarize(value = list(value)) %>%
  spread(eth, value) %>%
  group_by(variable) %>%
  summarize(
    t_value_0vs1 = t.test(unlist(`0`), unlist(`1`))$statistic,
    p_value_0vs1 = t.test(unlist(`0`), unlist(`1`))$p.value)

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
    as.data.frame(apply(p_value, c(1,2), addStar, ncol(p_value)))
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
rownames(table.comparisons) = c('Baseline', 'PHQ', 'GAD', 'IUS', 'PTSD', 'Severity',
                                'Anxiety', 'Depression', 'Aggression', 'Attention',
                                'Substance', 'Autism', 'Relationship', 'Trauma', '
                                Other', 'Comorbidity')
colnames(table.comparisons) = c('Men', 'Women', 'Minority', 
                                'Caucasian', 'Black', 'Native', 'Asian', 'Pacific Islander', 'Multiple',
                                'Non-Hispanic', 'Hispanic')
table.comparisons
write.csv(table.comparisons, '../Results/mentalHealthDemoComparisons.csv', row.names = T)
########
#Others#
########

#Treatment History
round(table(subData$txHist.now)/nrow(subData.c), 4)
round(table(subData$txHist.past)/nrow(subData.c), 4)
round(table(subData$txHist.wanted)/nrow(subData.c), 4)
round(table(subData$txHist.neverWanted)/nrow(subData.c), 4)

#Correlations between overall severity and other sympotomatology measures 
cor(subData.c[,c('PHQ.mean', 'GAD.mean', 'IUS.mean', 'PTSD.mean', 'overallSev')], use = 'pair')


t.test(subData.naOmit$overallSev[subData.naOmit.std$eth==1], 
       subData.naOmit$overallSev[subData.naOmit.std$eth==0&subData.naOmit.std$race ==1])
