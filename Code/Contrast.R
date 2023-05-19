library(multcomp)
library(lsmeans)
library(psych)
data4Glm = read.csv('../Data/data4Glm.csv')
str(data4Glm)
data4Glm[,c('gender', 'race', 'eth', 'txHist_past')] = 
  lapply(data4Glm[,c('gender', 'race', 'eth')], as.factor)

describe(data4Glm)

summary(glm(txHist_now ~ (gender + race + eth), 
                 data4Glm, family= 'binomial'))
M5 = glm(txHist_now ~ (overallSev)+(gender + race + eth), 
            data4Glm, family= 'binomial')
sumM5 = summary(M5)
write.csv(sumM5$coefficients, '../Results/M5_Coef.csv')

overallSevByGR = lsmeans(finalModel,  pairwise~ overallSev|race+gender, at = list(overallSev = c(1,0)))
overallSevByGE = lsmeans(finalModel,  pairwise~ overallSev|gender+eth, at = list(overallSev = c(1,0)))
overallSevByGRE = lsmeans(finalModel,  pairwise~ overallSev|race+gender+eth, at = list(overallSev = c(1,0)))

write.csv(overallSevByGE$contrasts, '../Results/M7_ContrastsGE.csv')
write.csv(overallSevByGR$contrasts, '../Results/M7_ContrastsGR.csv')
write.csv(overallSevByGRE$contrasts, '../Results/M7_ContrastsGRE.csv')

 lsmeans(finalModel,  pairwise~ overallSev|race+gender+eth, at = list(overallSev = c(1,0)))



lsmeans(finalModel,  pairwise~ overallSev|race, at = list(overallSev = c(1,0), gender = '1', eth= '0'))
lsmeans(finalModel,  pairwise~ overallSev|race, at = list(overallSev = c(1,0), gender = '2', eth ='1' ))
lsmeans(finalModel,  pairwise~ overallSev|race, at = list(overallSev = c(1,0), gender = '3'))



?lsmeans


lsmeans(finalModel , pairwise ~ (gender_2+gender_3):(race_2  + race_4 + race_6): eth_1)
lsmeans(finalModel , pairwise ~ (gender_2+gender_3))
lsmeans(finalModel , pairwise ~ (gender_2+gender_3):race_2)

lsmeans(finalModel , pairwise ~ (gender_2+gender_3):(race_2  + race_4 + race_6): eth_1)
?lsmeans
finalModel = glm(txHist_now ~ (overallSev)*(gender+race+eth),
                 txData_naOmit_std2, family= 'binomial')

summary(finalModel)

test = data.frame(
  gender = c(rep('F', 100), rep('M', 100), rep('O', 100), rep('Q', 104)),
  sexO = rep(c('Homo', 'Hetero'), 202),
  outcome = c(rnorm(100, 10, 2), rnorm(100, 5, 3), rnorm(100, 3, 5), rnorm(104, 2, 3))
)

test2 = data.frame(
  gender = c(rep('F', 100), rep('M', 100)),
  genderM = c(rep(0,100), rep(1,100)),
  outcome = as.factor(c(rep(c(1,0,1,0),25) , rep(c(1,0,1,1),25)))
)
str(test2)
summary(glm(outcome~genderM, test2, family = 'binomial'))

m = 
m = lm(outcome ~ gender, test)
lmeans(m)

ref.grid(a)

summary(m)
1 0 0 
-1/2 + 1/2 + 0 
-1/2 
0.3264/sqrt(1/100)*sqrt(1/4/100+1/4/100)
0.3264*sqrt(1/100+1/100)/1*10
0.07 
0.1
0.3264
within-group mean square 


summary(glht(m,linfct = c("genderM = 1", "sexOHomo = 1")))
summary(glht(m,linfct = mcp(gender = 'Tukey', sexO = 'Tukey')))
lsmeans(a , pairwise ~ gender:(race))
str(a)
class(a$call)
class(m$call)
a$call = as.call(glm(formula = txHist.now ~ (overallSev) * (gender + race + eth) + 
               baseline + txHist.past, family = "binomial", data = txData_naOmit_std2))

a$data$gender = ifelse(a$data$gender2 == 1, 2, 1)
a$data$gender = ifelse(a$data$gender3 == 1, 3, a$data$gender)
a$data$gender2 = NULL
a$data$gender3 = NULL

str(m)
m$R
a$xlevels = m$xlevels
a$model = m$model
m$R

ref.grid(a)

lsmeans(a, pairwise ~ gender)

oh = a$data
sample = oh[,c('overallSev', )]

sub = gsub('\\..*','',rownames(a$data, baseline txHist.past1))


sub = gsub('X','',sub)
which(duplicated(sub))
which(sub == '2')
a$data[1056:1400,]
a$data[1:10,]

which(rownames(a$data) == 'X2')
which(txData_naOmit_std2$overallSev == 1.234593459)


sample = data.frame()
for (i in sub){
  sample = rbind(sample, 
                 txData_naOmit_std2[which(rownames(txData_naOmit_std2) == i), ],
                 make.row.names = F)
  if(length(which(rownames(txData_naOmit_std2) == i)) < 1) {print(i)}
}

str(sample)
which(duplicated(nrow(sample)))

data[which(rownames(txData_naOmit_std2) == i),]
i

?gsub
substr(rownames(a$data), 2, 1000L)


which(rownames(m$data) == '176')
m$data[107,]
a$data[1,]
a$data$gender = toupper(names(data)[w[order(w[,1]),2]])
a$model
toupper(names(a$data[])[max.col(a$data)])
m$
class(a)
ref.grid(m)
ref.grid(a, at = list(gender = c(1,2,3)))


a = M4_2$finalModel
class(a)
str(a)
a$contrasts = m$contrasts
a$xlevels = m$xlevels
m$model$txHist.now
a$model 
sample = a$model
newmodel = sample[,c('overallSev','baseline')]
newmodel$txHist.now = sample$.outcome
newmodel$txHist.past = sample$txHist.past1
newmodel$gender = ifelse(sample$gender2 == 1, 2, 1)
newmodel$gender = ifelse(sample$gender3 == 1, 3, newmodel$gender)
newmodel$race = ifelse(sample$race2== 1, 2, 1)
newmodel$race = ifelse(sample$race4== 1, 4, newmodel$race )
newmodel$race = ifelse(sample$race6== 1, 6, newmodel$race )
newmodel$eth = sample$eth1
str(newmodel)

a$model = newmodel
a$call = m$call
a$data = a$model
ref.grid(a, data = recover_data(a))

str(a$model) 
a$model$gender = as.factor(a$model$gender)
a$model$eth = as.factor(a$model$eth)
a$model$race = as.factor(a$model$race)
a$model$txHist.past = as.factor(a$model$txHist.past)


glm(txHist.now ~ (overallSev)*(gender + race + eth) + baseline + txHist.past, 
    a$model, family= 'binomial')
a$coefficients

str(m)

class(M4_2$finalModel)
a
M4_2$finalModel
class(M4_2)
library(ModelMetrics)
mse(m)*404/402
sqrt(sum(resid(m)^2)/(nrow(test) -2)*(1/100))

(10.3483*(1/4/100+1/4/100))^(1/2)

(10.3483*(1/404))^(1/2)
#sqrt [ Σ(yi – ŷi)2 / (n – 2) ] / sqrt [ Σ(xi – x)2 ].
library(lsmeans)

?lsmeans
