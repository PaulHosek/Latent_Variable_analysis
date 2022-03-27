library('psych')
library('GPArotation')

my_data = read.csv('LVMQ_v3_nosub.csv', sep = ';')

fit = fa.parallel(my_data, fa='fa')
eigenvalues = fa.parallel(my_data, fa='fa')$fa.values 

fit1=fa(my_data,nfactors=1, rotate='promax',fm='ml')
fit2=fa(my_data,nfactors=2, rotate='promax',fm='ml')
fit3=fa(my_data,nfactors=3, rotate='promax',fm='ml')
fit4=fa(my_data,nfactors=4, rotate='promax',fm='ml')


fit1_measures = c(fit1$STATISTIC,fit1$dof,fit1$PVAL,fit1$RMSEA[1], fit1$BIC)
fit2_measures = c(fit2$STATISTIC,fit2$dof,fit2$PVAL,fit2$RMSEA[1], fit2$BIC)
fit3_measures = c(fit3$STATISTIC,fit3$dof,fit3$PVAL,fit3$RMSEA[1], fit3$BIC)
fit4_measures = c(fit4$STATISTIC,fit4$dof,fit4$PVAL,fit4$RMSEA[1], fit4$BIC)



