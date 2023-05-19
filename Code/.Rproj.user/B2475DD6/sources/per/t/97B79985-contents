txData_naOmit = read.csv('../Results/txData_naOmit.csv')

library(dplyr)
library(caret)

txData_naOmit_std = 
  txData_naOmit %>%
  filter(race != 5 & race != 3 ) %>% #too few Pacific Islander and Native American
  mutate(across(c(3:5, 12:15),  as.factor)) %>% 
  mutate(across(c(6:10), scale))

str(txData_naOmit_std) 
table(txData_naOmit_std$race)

txData_naOmit_std2 = txData_naOmit_std[txData_naOmit_std$txHist.neverWanted == 0, ]
colnames(txData_naOmit_std2) = gsub('\\.', '_', colnames(txData_naOmit_std2))
write.csv(txData_naOmit_std2, '../Results/txData_naOmit2.csv', row.names = F)
str(txData_naOmit_std2) #N = 909

range(txData_naOmit_std2$overallSev)

train.control = trainControl(method = 'repeatedcv', number = 10, repeats = 50,
                             sampling = 'up', returnResamp = 'all', returnData = T,
                             seeds = .Random.seed[1:501])

#Demographics, pre-COVID mental health condition, past tx history
M0_2= 
  train(txHist_now ~ gender + race + eth + baseline + txHist_past, 
        txData_naOmit_std2, 
        method = 'glm',
        trControl = train.control)

#Add separate internalizing symptomatology
M1_2 = 
  train(txHist_now ~ PHQ.mean + GAD.mean + IUS.mean + PTSD.mean + 
          gender + race+ eth + baseline + txHist_past, 
        txData_naOmit_std2, method = 'glm', family= 'binomial',
        trControl = train.control)


#Replace separate internalizing symptomatology with one summary index
M2_2 = 
  train(txHist_now ~ overallSev+ 
          gender + race + eth + baseline + txHist_past,  
        txData_naOmit_std2, method = 'glm', family= 'binomial',
        trControl = train.control)

m = glm(txHist_now ~ overallSev*( 
      gender + race + eth) + baseline + txHist_past,  
    txData_naOmit_std2, family = 'binomial')

#M1 + interaction terms
M3_2 = 
  train(txHist_now ~ (PHQ.mean + GAD.mean + IUS.mean + PTSD.mean)* 
          (gender + race + eth) + baseline + txHist_past, 
        txData_naOmit_std2, method = 'glm', family= 'binomial',
        trControl = train.control)

#M2 + interaction terms
M4_2 = train(txHist_now ~ (overallSev)*(gender + race + eth) + baseline + txHist_past, 
           txData_naOmit_std2, method = 'glm', family= 'binomial',
           trControl = train.control)
summary(M4_2$finalModel)


#Model Performance Comparison
model_names2 = c()
for(i in 0:4){ 
  model_names2 = c(model_names2, paste0('M', i,'_2'))
  }

model.comparisons2 = matrix(NA, ncol = 5, nrow = 2)
a = 0
for(x in model_names2){
  a = a+1
  perf = get(x)$results
  accu = perf[1,2]
  kappa = perf[1,3]
  accu.SD = perf [1,4]
  kappa.SD = perf [1,5]
  model.comparisons2[1:2,a] = 
    c(paste0(round(accu, 4),'(', round(accu.SD,2),')'),
      paste0(round(kappa, 4),'(', round(kappa.SD,2), ')'))
}

colnames(model.comparisons2) = model_names2
rownames(model.comparisons2) = c('Accuracy+-SE ', 'Kappa+-SE')
model.comparisons2

write.csv(model.comparisons2, '../Results/model.comparisons2.csv', row.names = T)

#############
#Final Model#
#############

set.seed(1)
final_model2 = matrix(0, ncol = 2, nrow = 16)
est2 = matrix(0, ncol = 500, nrow = 16)
final.control = trainControl(method = 'none', sampling = 'smote') 

for(i in 1:500){
  final.control$seeds = .Random.seed[i]
  model2 =  train(txHist.now ~ (overallSev) * (gender + race + eth) + baseline + txHist.past, 
                 txData_naOmit_std2, method ='glm', family = 'binomial',
                 trControl = final.control)
  summary2 = summary(model2)
  final_model2 = final_model2 + summary2$coefficients[,1:2]
  est2[,i] = summary2$coefficients[,1]
}

final_model_table2 = as.data.frame(final_model2/500)
final_model_table2$z = final_model_table2$Estimate/final_model_table2$`Std. Error`
final_model_table2$p = pnorm(abs(final_model_table2$z), lower.tail = F)*2
final_model_table2$sd = apply(est2, 1, sd)
final_model_table2$z2 = final_model_table2$Estimate/final_model_table2$sd
final_model_table2$p2 = pnorm(abs(final_model_table2$z2), lower.tail = F)*2
round(final_model_table2, 3)


write.csv(final_model_table2, '../Results/final_model2.csv', row.names = T)




library(lsmeans)

leastsquare = lsmeans(model, "Treatment")
Contrasts = list(Red_vs_white    = c( 1,  1,  1, -1, -1, -1),
                 Merlot_vs_Cab   = c( 1, -1,  0,  0,  0,  0),
                 Cab_vs_Syrah    = c( 0,  1, -1,  0,  0,  0),
                 Syrah_vs_Merlot = c(-1,  0,  1,  0,  0,  0))
contrast(leastsquare, Contrasts, adjust="sidak")



library(ggplot2)
est = as.data.frame(t(est))
colnames(est) = rownames(final_model_table)
est_g = 
  est %>%
  gather(key = 'key', value = 'value') %>%
  mutate(key_f= factor(key, levels = colnames(est),
                       labels = c('Intercept', 'Severity', 'Women', 'Gender Minority',
                                  'Black', 'Asian', 'Multi-race', 'Hispanic', 'Pre-COVID MH',
                                  'Past Tx', 'Severity*Women', 'Severity*Minority',
                                  'Severity*Balck', 'Severity*Asian', 'Severity*Multi-Race', 
                                  'Severity*Hispanic')))
plot =  
  ggplot(est_g, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~key_f ) + 
  labs(x ="Lostistic Regression Coefficient", 
       y = "Count")+
  theme_minimal()

ggsave('../Results/coefficients.pdf',plot, height = 11, width = 11)

?ggsave

labeller = label_both

ggplot(df, aes(x=weight)) + geom_histogram()
