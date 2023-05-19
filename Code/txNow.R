txData_naOmit = read.csv('../Results/txData_naOmit.csv')

library(dplyr)
library(caret)
library(tidyr)
library(ggplot2)

txData_naOmit_std = 
  txData_naOmit %>%
  filter(race != 5 & race != 3 ) %>% #too few Pacific Islander and Native American
  mutate(across(c(3:5, 12:15),  as.factor)) %>% 
  mutate(across(c(6:10), scale))

str(txData_naOmit_std) 
table(txData_naOmit_std$race)

set.seed(123)
train.control = trainControl(method = 'repeatedcv', number = 5, repeats = 100,
                            sampling = 'smote', returnResamp = 'all', returnData = T,
                            seeds = .Random.seed[1:501])


#Demographics, pre-COVID mental health condition, past tx history
M0= 
  train(txHist.now ~ gender + race + eth + baseline + txHist.past, 
        txData_naOmit_std, 
        method = 'glm',
        trControl = train.control)


#Add separate internalizing symptomatology
M1 = 
  train(txHist.now ~ PHQ.mean + GAD.mean + IUS.mean + PTSD.mean + 
        gender + race+ eth + baseline + txHist.past, 
        txData_naOmit_std, method = 'glm', family= 'binomial',
      trControl = train.control)

#Replace separate internalizing symptomatology with one summary index
M2 = 
  train(txHist.now ~ overallSev+ 
          gender + race + eth + baseline + txHist.past,  
        txData_naOmit_std, method = 'glm', family= 'binomial',
        trControl = train.control)

#M1 + interaction terms
M3 = 
  train(txHist.now ~ (PHQ.mean + GAD.mean + IUS.mean + PTSD.mean)* 
          (gender + race + eth) + baseline + txHist.past, 
        txData_naOmit_std, method = 'glm', family= 'binomial',
        trControl = train.control)

#M2 + interaction terms
M4 = train(txHist.now ~ (overallSev)*(gender + race + eth) + baseline + txHist.past, 
           txData_naOmit_std, method = 'glm', family= 'binomial',
            trControl = train.control)
summary(M4)

M5 = train(txHist.now ~ overallSev + baseline + txHist.past, 
           txData_naOmit_std, method = 'glm', family= 'binomial',
           trControl = train.control)
M5

model_names = c()
for(i in 0:4){ 
  model_names = c(model_names, paste0('M', i))}

model.comparisons = matrix(NA, ncol = 5, nrow = 2)
a = 0
for(x in model_names){
  a = a+1
  perf = get(x)$results
  accu = perf[1,2]
  kappa = perf[1,3]
  accu.SD = perf [1,4]
  kappa.SD = perf [1,5]
  model.comparisons[1:2,a] = 
    c(paste0(round(accu, 4),'(', round(accu.SD,2),')'),
      paste0(round(kappa, 4),'(', round(kappa.SD,2), ')'))
}

colnames(model.comparisons) = model_names
rownames(model.comparisons) = c('Accuracy+-SE ', 'Kappa+-SE')
model.comparisons

write.csv(model.comparisons, '../Results/model.comparisons.csv', row.names = T)

#############
#Final Model#
#############
set.seed(1)
final_model = matrix(0, ncol = 2, nrow = 16)
est = matrix(0, ncol = 500, nrow = 16)
final.control = trainControl(method = 'none', sampling = 'smote') 
      
for(i in 1:500){
  final.control$seeds = .Random.seed[i]
  model =  train(txHist.now ~ (overallSev)*(gender + race + eth) + baseline + txHist.past, 
               txData_naOmit_std, method ='glm', family = 'binomial',
               trControl = final.control)
  summary = summary(model)
  final_model = final_model + summary$coefficients[,1:2]
  est[,i] = summary$coefficients[,1]
}

final_model_table = as.data.frame(final_model/500)
final_model_table$z = final_model_table$Estimate/final_model_table$`Std. Error`
final_model_table$p = pnorm(abs(final_model_table$z), lower.tail = F)*2
final_model_table$sd = apply(est, 1, sd)
final_model_table$z2 = final_model_table$Estimate/final_model_table$sd
final_model_table$p2 = pnorm(abs(final_model_table$z2), lower.tail = F)*2
round(final_model_table, 3)

write.csv(final_model_table, '../Results/final_model.csv', row.names = T)


est = as.data.frame(t(est))
colnames(est) = rownames(final_model_table)
est_g = 
  est %>%
  gather(key = 'key', value = 'value') %>%
  mutate(key_f= factor(key, levels = colnames(est),
                       labels = c('Intercept', 'Internalizing problem severity', 'Woman', 
                                  'Non-Binary Gender identities',
                                  'Black', 'Asian', 'Multiracial', 'Hispanic/Latinx', 
                                  'Pre-pandemic mental health',
                                    'Past treatment use', 'INT*Woman', 'INT*Non-binary',
                                    'INT**Balck', 'INT*Asian', 'INT*Multiracial', 
                                    'INT*Hispanic/Latinx')))
plot =  
ggplot(est_g, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~key_f ) + 
  labs(x ="Logistic Regression Coefficient", 
      y = "Count")+
  theme_minimal()

colnames(est)


apply(est, 1, kurtosis, method='moment')
mean(apply(est, 1, kurtosis, method='moment'))
range(apply(est, 1, kurtosis, method='moment'))

ggsave('../Results/coefficients.pdf',plot, height = 11, width = 11)


