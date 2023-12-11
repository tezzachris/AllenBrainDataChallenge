library(ggplot2)
library(dplyr)

load("cells.Rdata")
load("sessionA.Rdata")
load("sessionB.Rdata")
load("sessionC.Rdata")
load("stimuliA.Rdata")
load("stimuliB.Rdata")
load("stimuliC.Rdata")

sessionA_stim = cbind(sessionA, stimuliA)
sessionB_stim = cbind(sessionB, stimuliB)
sessionC_stim = cbind(sessionC, stimuliC)

sessionA_stim$stimuliA <- as.factor(sessionA_stim$stimuliA)
sessionB_stim$stimuliB <- as.factor(sessionB_stim$stimuliB)
sessionC_stim$stimuliC <- as.factor(sessionC_stim$stimuliC)

sessionA_stim$stimuliA <- relevel(sessionA_stim$stimuliA, ref = 'none')
sessionB_stim$stimuliB <- relevel(sessionB_stim$stimuliB, ref = 'none')
sessionC_stim$stimuliC <- relevel(sessionC_stim$stimuliC, ref = 'none')



sessionA_stim[,-1]%>%
  group_by(stimuliA) %>% as.matrix() %>%
  mean(na.rm=T)

sum(is.na(sessionA_stim))

summ = data.frame(Session =character(),
                  Stimuli =character(),
                  Min = double(),
                  First_Qu=double(),
                  Median=double(),
                  Mean=double(),
                  Third_Qu=double(),
                  Max = double(),
                  Var = double(),
                  n = double(),
                  stringsAsFactors=FALSE)


for(i in unique(stimuliA)){
  t = table(stimuliA)
  sub = sessionA[sessionA_stim$stimuliA==i,]
  partial = as.vector(apply(sub,1, as.vector))
  summary = summary(partial)
  partial_df= data.frame(Session = "SessionA",
                         Stimuli=i,
                         Min = as.double(summary[1]),
                         First_Qu = as.double(summary[2]),
                         Median = as.double(summary[3]),
                         Mean = as.double(summary[4]),
                         Third_Qu = as.double(summary[5]),
                         Max = as.double(summary[6]),
                         Var = var(partial),
                         n = as.double(t[i]))
  summ = rbind(summ, partial_df)
  
}


for(i in unique(stimuliB)){
  t = table(stimuliB)
  sub = sessionB[sessionB_stim$stimuliB==i,]
  partial = as.vector(apply(sub,1, as.vector))
  summary = summary(partial)
  partial_df= data.frame(Session = "SessionB",
                         Stimuli=i,
                         Min = as.double(summary[1]),
                         First_Qu = as.double(summary[2]),
                         Median = as.double(summary[3]),
                         Mean = as.double(summary[4]),
                         Third_Qu = as.double(summary[5]),
                         Max = as.double(summary[6]),
                         Var = var(partial),
                         n = as.double(t[i]))
  summ = rbind(summ, partial_df)
  
}

for(i in unique(stimuliC)){
  t = table(stimuliC)
  sub = sessionC[sessionC_stim$stimuliC==i,]
  partial = as.vector(apply(sub,1, as.vector))
  summary = summary(partial)
  partial_df= data.frame(Session = "SessionC",
                         Stimuli=i,
                         Min = as.double(summary[1]),
                         First_Qu = as.double(summary[2]),
                         Median = as.double(summary[3]),
                         Mean = as.double(summary[4]),
                         Third_Qu = as.double(summary[5]),
                         Max = as.double(summary[6]),
                         Var = var(partial, na.rm = T),
                         n = as.double(t[i]))
  summ = rbind(summ, partial_df)
  
}



regA<-as.data.frame(apply(as.matrix(sessionA_stim[,1:(ncol(sessionA_stim)-1)]), 2, function(x){
  
  
  sum <-lm(x ~ sessionA_stim$stimuliA)
  sum <-sum['coefficients'] 
  return(sum)
}))

regB<-as.data.frame(apply(as.matrix(sessionB_stim[,1:(ncol(sessionB_stim)-1)]), 2, function(x){
  
  
  sum <-lm(x ~ sessionB_stim$stimuliB)
  sum <-sum['coefficients'] 
  return(sum)
}))


regC<-as.data.frame(apply(as.matrix(sessionC_stim[,1:(ncol(sessionC_stim)-1)]), 2, function(x){
  
  
  sum <-lm(x ~ sessionC_stim$stimuliC)
  sum <-sum['coefficients'] 
  return(sum)
}))


row.names(regA) <- unique(stimuliA)
row.names(regB) <- unique(stimuliB)
row.names(regC) <- unique(stimuliC)




boxplot(t(regA[2:4,]))
boxplot(t(regB[2:4,]))
boxplot(t(regC[2:5,]))


sessionA_stim<- as.data.table(sessionA_stim) %>% group_by(stimuliA)

sessionA_stim<-as.data.table(sessionA_stim)
sessionA_stim[mean,by=c('stimuliA')]

sessionA_stim[,-1]%>%
  group_by(stimuliA) %>% as.matrix() %>%
  mean(na.rm=T)




#sessionA_stim$stimuliAB<-as.numeric(sessionA_stim$stimuliA)
sessionA_stim2<-aggregate(x = sessionA_stim[,1:(ncol(sessionA_stim)-1)],    # Specify data column
          by = list(sessionA_stim$stimuliA),              # Specify group indicator
          FUN = mean) 

merge(names(sessionA_stim2),cells$depth,by.y='cell_id')

for ( i in 1:ncol(sessionA_stim2)){
  if(names(sessionA_stim2)[i] %in% cells$cell_id) 
    cells$cell_id==
    names(sessionA_stim2)<-paste0(names(sessionA_stim2),'_',cells$depth)
}
meanA<-cbind(as.data.frame(apply(sessionA_stim2[2:ncol(sessionA_stim2)],1,mean)),sessionA_stim2$Group.1)


paste0(names(sessionA_stim2),'_',cells$depth)


interaction.plot(npk$block, npk$P, npk$yield,  #x,group,y
                 xlab = "terreno", ylab = "rendimento")



means_a<-apply(sessionA,2,mean)
sd_a<-apply(sessionA,2,sd)

means_b<-apply(sessionB,2,mean)
sd_b<-apply(sessionB,2,sd)

means_c<-apply(sessionC,2,mean)
sd_c<-apply(sessionC,2,sd)




par(mfrow = c(2, 3))
hist(sd_c,main='Session C',xlab='SD value')



sd_a<-apply(sessionA,2,sd)

boxplot(sd_a,main='Session C',xlab='Mean value')
