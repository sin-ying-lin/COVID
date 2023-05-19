library(dplyr)
library(foreign)
library(haven)

data$probType$PHdepression
support = data[,grep('^support', colnames(data))]
support = support[,-1]
colnames(support) = gsub(pattern = "support.",replacement = "", x  = colnames(support))

academicEngagement = data[,grep('academicEngagement_', colnames(data))]
colnames(academicEngagement) = c('masterSkills', 'doDifficultTasks', 'completeClassWork',
                                 'learnHardWork', 'doHardestWork',
                                 'doWellInClass', 'meetGoals',
                                 'findJob', 'getIntoGradSchool')

COVID_demo_acEng_sup = 
  data.frame(
    ID = c(1:nrow(data)),
    age = data$age,
    gender = factor(ifelse(is.na(data$gender), data$sex, data$gender),
                       labels = c('M', 'F', 'Unsure')),
    race = factor(data$race, 
                     labels = c('White', 'Black', 'IAI/AN','Asian', 'NH/OPI', 'Multiple')),
    hispanic = as.factor(data$hispanic),
    edu = as.ordered(data$education),
    liveAlone = as.factor(data$liveWith.alone),
    support, 
    academicEngagement,
    dep = data$PHQ_FI
    )

str(COVID_demo_acEng_sup)

write_sav(COVID_demo_acEng_sup, '../Data/COVID_demo_supp_acEng.sav')
