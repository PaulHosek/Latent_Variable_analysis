library('lavaan')
library(RColorBrewer)
library(reshape2)


conf_data = read.csv('LVMQ_v2_nosubject.csv', sep=';', header = TRUE)


model3='Competence=~CO01+CO02+CO03+CO04+CO05+CO06+CO07+CO08+CO09+CO09+CO10
        Autonomy=~AU01+AU02+AU03+AU04+AU05+AU06+AU07+AU08+AU09+AU09+AU10
        Relatedness=~RE01+RE02+RE03+RE04+RE05+RE06+RE07+RE08+RE09+RE09+RE10'

fit3_conf=cfa(model=model3, data=conf_data, std.lv= TRUE, ) 
summary(fit3_conf, standardized=TRUE)


# global fit
fitmeasures(fit3_conf)[['rmsea']]
fitmeasures(fit3_conf)[['cfi']]
fitmeasures(fit3_conf)[['tli']]
fitmeasures(fit3_conf)[['tli']]

# local fit
max(modindices(fit3_conf)[,4])
hist(modindices(fit3_conf)[,4])

# plot
heatmap(cor(conf_data), scale='column', Colv = NA, Rowv = NA, col= colorRampPalette(brewer.pal(8, "Blues"))(25))
legend(,x="bottomleft", legend=c("<0.125", "0.25", "0.375", "0.5", "0.625", "0.75", '0.875','>0.875'),
title= 'Correlation', 
       fill=colorRampPalette(brewer.pal(8, "Blues"))(8))



