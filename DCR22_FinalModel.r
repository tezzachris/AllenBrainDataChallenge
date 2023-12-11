library(ggplot2)
library(factoextra)
library(gridExtra)
library(depmixS4)
library(tscount)
library(latex2exp)
library(dplyr)

set.seed(1)
setwd('/Users/chris/Desktop/unibo/fokianos/data')


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

sessionA_stim$stimuliA = as.factor(sessionA_stim$stimuliA)
sessionB_stim$stimuliB = as.factor(sessionB_stim$stimuliB)
sessionC_stim$stimuliC = as.factor(sessionC_stim$stimuliC)

sessionA_stim$stimuliA = relevel(sessionA_stim$stimuliA, ref = "none")
sessionB_stim$stimuliB = relevel(sessionB_stim$stimuliB, ref = "none")
sessionC_stim$stimuliC = relevel(sessionC_stim$stimuliC, ref = "none")

table(cells$depth)


# Summaries

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

summ

# Anova Significance

signif_A = data.frame(id =double(),
                      depth = double(),
                      p_value = double(),
                      stringsAsFactors=FALSE)

for(i in 1:ncol(sessionA)){
  id = names(sessionA)[i]
  a = anova(lm(sessionA_stim[, i] ~ sessionA_stim$stimuliA))
  if(a$`Pr(>F)`[1]< 0.05){
    partial_df = data.frame(id = id,
                            depth = cells$depth[cells$cell_id==id],
                            p_value = a$`Pr(>F)`[1])
    signif_A = rbind(signif_A, partial_df)
    
  }
}

# Coefficient boxplots - ANOVA MODEL

# Session A
coeff_A = data.frame(id =double(),
                     interc = double(),
                     drifting_gratings = double(),
                     natural_movie_one = double(),
                     natural_movie_three = double(),
                     stringsAsFactors=FALSE)

for(i in 1:ncol(sessionA)){
  id = names(sessionA)[i]
  lm = lm(sessionA_stim[,i] ~ sessionA_stim$stimuliA)
  partial_df = data.frame(id = id,
                          interc = lm$coefficients[1],
                          drifting_gratings = lm$coefficients[2],
                          natural_movie_one = lm$coefficients[3],
                          natural_movie_three = lm$coefficients[4])
  coeff_A  = rbind(coeff_A , partial_df)
  
}


coeff_A %>% tidyr::gather("id", "value",3:5) %>% 
  ggplot(., aes(x = id, y = value))+geom_boxplot()+ ggtitle("Coefficient Boxplots - Session A") +
  xlab("Stimuli") + ylab("Value")



# Session B
coeff_B = data.frame(id =double(),
                     interc = double(),
                     static_gratings = double(),
                     natural_scenes = double(),
                     natural_movie_one = double(),
                     stringsAsFactors=FALSE)

for(i in 1:ncol(sessionB)){
  id = names(sessionB)[i]
  lm = lm(sessionB_stim[,i] ~ sessionB_stim$stimuliB)
  partial_df = data.frame(id = id,
                          interc = lm$coefficients[1],
                          static_gratings = lm$coefficients[2],
                          natural_scenes = lm$coefficients[3],
                          natural_movie_one = lm$coefficients[4])
  coeff_B  = rbind(coeff_B , partial_df)
  
}


coeff_B %>% tidyr::gather("id", "value",3:5) %>% 
  ggplot(., aes(x = id, y = value))+geom_boxplot()+ ggtitle("Coefficient Boxplots - Session B") +
  xlab("Stimuli") + ylab("Value")


# Session C
coeff_C = data.frame(id =double(),
                     interc = double(),
                     locally_sparse_noise_4deg = double(),
                     locally_sparse_noise_8deg = double(),
                     natural_movie_one = double(),
                     natural_movie_two = double(),
                     stringsAsFactors=FALSE)

for(i in 1:ncol(sessionC)){
  id = names(sessionC)[i]
  lm = lm(sessionC_stim[,i] ~ sessionC_stim$stimuliC)
  partial_df = data.frame(id = id,
                          interc = lm$coefficients[1],
                          locally_sparse_noise_4deg = lm$coefficients[2],
                          locally_sparse_noise_8deg = lm$coefficients[3],
                          natural_movie_one = lm$coefficients[4],
                          natural_movie_one = lm$coefficients[5])
  coeff_C  = rbind(coeff_C , partial_df)
  
}

coeff_C %>% tidyr::gather("id", "value",3:6) %>% 
  ggplot(., aes(x = id, y = value))+geom_boxplot()+ ggtitle("Coefficient Boxplots - Session C") +
  xlab("Stimuli") + ylab("Value")

# Coefficient clusters - ANOVA MODEL

# Session A

fviz_nbclust(coeff_A[,3:5], kmeans, method = "wss")

kc = kmeans(coeff_A[,3:5], 2, nstart = 50)
table(kc$cluster)

kc = kmeans(coeff_A[-5,3:5], 2, nstart = 30) #remove outlier
table(kc$cluster)

cells_A = cells[cells$A==T, 1:2] # id and depth of session A
cells_A = cells_A[-5,] #remove outlier
cluster_A = cbind(cells_A, kc$cluster)
table(cluster_A[,2:3])


dist_mat <- dist(coeff_A[-5,3:5], method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)

cluster_A_hier = cbind(cells_A, cut_avg)
table(cluster_A_hier[,2:3])


# Session B
fviz_nbclust(coeff_B[,3:5], kmeans, method = "wss")

kc = kmeans(coeff_B[,3:5], 2, nstart = 50)
table(kc$cluster)

cells_B = cells[cells$B==T, 1:2] # id and depth of session A
cluster_B_kmeans = cbind(cells_B, kc$cluster)
table(cluster_B_kmeans[,2:3])


dist_mat <- dist(coeff_B[,3:5], method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)

cluster_B_hier = cbind(cells_B, cut_avg)
table(cluster_B_hier[,2:3])


# Session C
fviz_nbclust(coeff_C[,3:6], kmeans, method = "wss")

kc = kmeans(coeff_C[,3:6], 2, nstart = 50)
table(kc$cluster)


fviz_nbclust(coeff_C[-c(5,12),3:6], kmeans, method = "wss") # remove outlier

kc = kmeans(coeff_C[-c(5,12),3:6], 2, nstart = 50) 
table(kc$cluster)



cells_C = cells[cells$C==T, 1:2] # id and depth of session C
cells_C = cells_C[-c(5,12),] # remove outlier
cluster_C_kmeans = cbind(cells_C, kc$cluster)
table(cluster_C_kmeans[,2:3])


dist_mat <- dist(coeff_C[-c(5,12),3:5], method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)

cluster_C_hier = cbind(cells_C, cut_avg)
table(cluster_C_hier[,2:3])



# HMM Model

# Session A

n_A = nrow(sessionA)
post_A = data.frame(matrix(NA, nrow = n_A, ncol = ncol(sessionA)))
names(post_A) = names(sessionA)
par_A = data.frame(id =double(),
                 drifting_gratings_s1 = double(),
                 natural_movie_one_s1 = double(),
                 natural_movie_three_s1 = double(),
                 drifting_gratings_s0 = double(),
                 natural_movie_one_s0 = double(),
                 natural_movie_three_s0 = double(),
                 sd_s1 = double(),
                 sd_s2 = double(),
                 BIC = double(),
                 stringsAsFactors=FALSE)

for (i in 1:ncol(sessionA) ){
  id = names(sessionA)[i]
  mod = depmix(sessionA_stim[,i] ~ 1+sessionA_stim$stimuliA, nstates = 2, ntimes = n_A)
  HMM = fit(mod, solnpcntrl=list(rho = 1, outer.iter = 400, inner.iter = 800, 
                                 delta = 1e-9, tol = 1e-15))
  t = table(posterior(HMM)$state)
  if(t[1]<t[2]){
    partial_df = data.frame(id = id,
                            drifting_gratings_s1 = summary(HMM)[3],
                            natural_movie_one_s1 = summary(HMM)[5],
                            natural_movie_three_s1 = summary(HMM)[7],
                            drifting_gratings_s0 = summary(HMM)[4],
                            natural_movie_one_s0 = summary(HMM)[6],
                            natural_movie_three_s0 = summary(HMM)[8],
                            sd_s1 = summary(HMM)[9],
                            sd_s0 = summary(HMM)[10],
                            BIC = BIC(HMM))
    par_A  = rbind(par_A, partial_df)
    
    post_A[id] = posterior(HMM)$S1
  }else{
    partial_df = data.frame(id = id,
                            drifting_gratings_s0 = summary(HMM)[3],
                            natural_movie_one_s0 = summary(HMM)[5],
                            natural_movie_three_s0 = summary(HMM)[7],
                            drifting_gratings_s1 = summary(HMM)[4],
                            natural_movie_one_s1 = summary(HMM)[6],
                            natural_movie_three_s1 = summary(HMM)[8],
                            sd_s0 = summary(HMM)[9],
                            sd_s1 = summary(HMM)[10],
                            BIC = BIC(HMM))
    par_A  = rbind(par_A, partial_df)
    
    post_A[id] = posterior(HMM)$S2 
  }
  
}


par_A %>% tidyr::gather("id", "value",2:9) %>% 
  ggplot(., aes(x = id, y = value))+
  geom_boxplot(color = c("red","green","red","green","red","green", "red","green"))+ 
  ggtitle("Coefficient Boxplots - Session A") +
  xlab("Stimuli") + ylab("Value")+  
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = -40, vjust = 0.5, hjust = 0.5))
ggsave("boxplot_sessionA.png", width = 6, height = 4)

# Session B

n_B = nrow(sessionB)
post_B = data.frame(matrix(NA, nrow = n_B, ncol = ncol(sessionB)))
names(post_B) = names(sessionB)
par_B = data.frame(id =double(),
                   natural_movie_one_s1 = double(),
                   natural_scenes_s1 = double(),
                   static_gratings_s1 = double(),
                   natural_movie_one_s0 = double(),
                   natural_scenes_s0 = double(),
                   static_gratings_s0 = double(),
                   sd_s1 = double(),
                   sd_s2 = double(),
                   BIC = double(),
                   stringsAsFactors=FALSE)

for (i in 1:ncol(sessionB)){
  id = names(sessionB)[i]
  mod = depmix(sessionB_stim[,i] ~ 1+sessionB_stim$stimuliB, nstates = 2, ntimes = n_B)
  HMM = fit(mod, solnpcntrl=list(rho = 1, outer.iter = 400, inner.iter = 800, 
                                 delta = 1e-9, tol = 1e-15))
  t = table(posterior(HMM)$state)
  if(t[1]<t[2]){
    partial_df = data.frame(id = id,
                            natural_movie_one_s1 = summary(HMM)[3],
                            natural_scenes_s1 = summary(HMM)[5],
                            static_gratings_s1 = summary(HMM)[7],
                            natural_movie_one_s0 = summary(HMM)[4],
                            natural_scenes_s0 = summary(HMM)[6],
                            static_gratings_s0 = summary(HMM)[8],
                            sd_s1 = summary(HMM)[9],
                            sd_s0 = summary(HMM)[10],
                            BIC = BIC(HMM))
    par_B  = rbind(par_B, partial_df)
    
    post_B[id] = posterior(HMM)$S1
  }else{
    partial_df = data.frame(id = id,
                            natural_movie_one_s0 = summary(HMM)[3],
                            natural_scenes_s0 = summary(HMM)[5],
                            static_gratings_s0 = summary(HMM)[7],
                            natural_movie_one_s1 = summary(HMM)[4],
                            natural_scenes_s1 = summary(HMM)[6],
                            static_gratings_s1 = summary(HMM)[8],
                            sd_s0 = summary(HMM)[9],
                            sd_s1 = summary(HMM)[10],
                            BIC = BIC(HMM))
    par_B  = rbind(par_B, partial_df)
    
    post_B[id] = posterior(HMM)$S2 
  }
}

par_B %>% tidyr::gather("id", "value",2:9) %>% 
  ggplot(., aes(x = id, y = value))+
  geom_boxplot(color = c("red","green","red","green","red","green", "red","green"))+ 
  ggtitle("Coefficient Boxplots - Session B") +
  xlab("Stimuli") + ylab("Value")+  
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = -40, vjust = 0.5, hjust = 0.5))
ggsave("boxplot_sessionB.png", width = 6, height = 4)

# Session C

n_C = nrow(sessionC)
post_C = data.frame(matrix(NA, nrow = n_C, ncol = ncol(sessionC)))
names(post_C) = names(sessionC)
par_C = data.frame(id =double(),
                   locally_sparse_noise_4deg_s1 = double(),
                   locally_sparse_noise_8deg_s1 = double(),
                   natural_movie_one_s1 = double(),
                   natural_movie_two_s1 = double(),
                   locally_sparse_noise_4deg_s0 = double(),
                   locally_sparse_noise_8deg_s0 = double(),
                   natural_movie_one_s0 = double(),
                   natural_movie_two_s0 = double(),
                   sd_s1 = double(),
                   sd_s2 = double(),
                   BIC = double(),
                   stringsAsFactors=FALSE)

for (i in 37:ncol(sessionC)){
  id = names(sessionC)[i]
  mod = depmix(sessionC_stim[,i] ~ 1+sessionC_stim$stimuliC, nstates = 2, ntimes = n_C)
  HMM = fit(mod, solnpcntrl=list(rho = 1, outer.iter = 400, inner.iter = 800, 
                                delta = 1e-9, tol = 1e-15))
  t = table(posterior(HMM)$state)
  if(t[1]<t[2]){
    partial_df = data.frame(id = id,
                            locally_sparse_noise_4deg_s1 = summary(HMM)[3],
                            locally_sparse_noise_8deg_s1 = summary(HMM)[5],
                            natural_movie_one_s1 = summary(HMM)[7],
                            natural_movie_two_s1 = summary(HMM)[9],
                            locally_sparse_noise_4deg_s0 = summary(HMM)[4],
                            locally_sparse_noise_8deg_s0 = summary(HMM)[6],
                            natural_movie_one_s0 = summary(HMM)[8],
                            natural_movie_two_s0 = summary(HMM)[9],
                            sd_s1 = summary(HMM)[11],
                            sd_s0 = summary(HMM)[12],
                            BIC = BIC(HMM))
    par_C  = rbind(par_C, partial_df)
    
    post_C[id] = posterior(HMM)$S1
  }else{
    partial_df = data.frame(id = id,
                            locally_sparse_noise_4deg_s0 = summary(HMM)[3],
                            locally_sparse_noise_8deg_s0 = summary(HMM)[5],
                            natural_movie_one_s0 = summary(HMM)[7],
                            natural_movie_two_s0 = summary(HMM)[9],
                            locally_sparse_noise_4deg_s1 = summary(HMM)[4],
                            locally_sparse_noise_8deg_s1 = summary(HMM)[6],
                            natural_movie_one_s1 = summary(HMM)[8],
                            natural_movie_two_s1 = summary(HMM)[9],
                            sd_s1 = summary(HMM)[11],
                            sd_s0 = summary(HMM)[12],
                            BIC = BIC(HMM))
    par_C  = rbind(par_C, partial_df)
    
    post_C[id] = posterior(HMM)$S2 
  }
}

par_C %>% tidyr::gather("id", "value",2:11) %>% 
  ggplot(., aes(x = id, y = value))+geom_boxplot(color = c("red","green","red","green","red","green", "red","green", "red","green"))+
  ggtitle("Coefficient Boxplots - Session C") +
  xlab("Stimuli") + ylab("Value") +  
  theme(axis.text.x = element_text(angle = -40, vjust = 0.5, hjust = 0.5))
ggsave("boxplot_sessionC.png", width = 6, height = 4)


# save(post_A,file="post_A.Rdata")
# save(par_A,file="par_A.Rdata")

# save(post_B,file="post_B.Rdata")
# save(par_B,file="par_B.Rdata")

# save(post_C,file="post_C.Rdata")
# save(par_C,file="par_C.Rdata")


# Model Diagnostic - Comparison with L0 penalization method

#neurone di laura:: 696326163


laura_spikes = c(259,  376,  389,  469,  477,  483,  509,  519,  524,  535,  539,  547,  
                 560,  566,  573,  578,  581,  585,  588,  594,  599,  603,  607,  614,  
                 618,  622,  630,  638,  645,  654,  660,  666,   671,  683,  686,  692,
                 701,  705,  715,  723,  727,  732,  744,  752,  766,  778, 1290, 1463, 
                 1468, 1477, 1490, 1494, 1501, 1512, 1534, 1561, 1588, 1676, 1690, 1802, 
                 2195, 2303, 2332, 2351, 2378, 2401, 2421, 2436, 2474, 2499, 2518, 2522, 
                 2797, 2933, 3079, 3227, 3247, 3253, 3268, 3278,   3291, 3302, 3334, 3342, 
                 3367, 3376, 3385, 3397, 3409, 3419, 3431, 3441, 3459, 3689, 3977, 4112, 
                 4151, 4158, 4171, 4185, 4194, 4200, 4219, 4232, 4247, 4280, 4298, 4311, 
                 4450, 4904, 4996, 5058,  5076, 5083, 5100, 5116, 5144, 5215, 5497, 5749) 

l_spikes = laura_spikes + 39200

df_spikes = data.frame ( x = l_spikes, y = rep(-0.6,length(l_spikes)))



mod_comp = depmix(sessionA_stim[,4] ~ 1+sessionA_stim$stimuliA, nstates = 2, ntimes = length(sessionA_stim[,4]))
HMM_comp = fit(mod_comp)
BIC(HMM_comp)

probs_comp = posterior(HMM_comp)
table(probs_comp$state)

post_res_comp = data.frame(cbind(calcium = sessionA[,4], state = probs_comp$state))

sub = post_res_comp[39200:45000,]

sub = post_res_comp

our_sp = data.frame( x = which(sub$state==which.min(table(sub$state))) + 39200, y = rep(-0.4,length(which(sub$state==which.min(table(sub$state))))))

ggplot() +
  geom_line(data = sub, aes(x= 39200:45000, y = calcium), color = as.factor(sub$state)) +
  geom_point(data = df_spikes, aes(x= x, y = y), color = "blue")+
  geom_point(data = our_sp, aes(x= x, y = y), color = "red")+
  ggtitle("Spike Detection")+
  xlab("Timestamps") + ylab("Value") 

ggsave("Spike_comparison_Laura.png", width = 7, height = 5)

#mia versione
sub = post_res_comp
our_sp = data.frame( x = which(sub$state==which.min(table(sub$state))) , y = rep(-0.4,length(which(sub$state==which.min(table(sub$state))))))

ggplot() +
  geom_line(data = sub, aes(x= 1:nrow(sub), y = calcium), color = as.factor(sub$state)) +
  ggtitle("Spike Detection")+
  xlab("Timestamps") + ylab("Value") 

#21k activation su 122k obs

activations = data.frame(x=which(sub$state==which.min(table(sub$state))), y = -0.5)
neuronfour = data.frame(x=sessionA[,4])
ggplot() + 
  geom_line(data = neuronfour, aes( x= 1:nrow(neuronfour), y=x))+
  geom_point(data =activations,aes(x=x,y=y), color = 'red')


# Clustering - HMM
load("par_A.Rdata")
load("par_B.Rdata")
load("par_C.Rdata")


# Session A
fviz_nbclust(par_A[,2:7], kmeans, method = "wss")

kc = kmeans(par_A[,2:7], 2, nstart = 50)
table(kc$cluster)

fviz_nbclust(par_A[-5,2:7], kmeans, method = "wss") #remove outlier
kc = kmeans(par_A[-5,2:7], 2, nstart = 50)
table(kc$cluster)



cells_A = cells[cells$A==T, 1:2] # id and depth of session A
cells_A = cells_A[-5,] #remove outlier
cluster_A = cbind(cells_A, kc$cluster)
table(cluster_A[,2:3])


dist_mat <- dist(par_A[-5,2:7], method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
# plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)

cluster_A_hier = cbind(cells_A, cut_avg)
table(cluster_A_hier[,2:3])

load("cluster_sess_a_k_mean.Rdata")
clusters= cbind(fede, f=cluster_A$`kc$cluster`)
table(clusters[,c(3,5)])



# Session B
fviz_nbclust(par_B[,2:7], kmeans, method = "wss")

kc = kmeans(par_B[,2:7], 2, nstart = 50)
table(kc$cluster)

cells_B = cells[cells$B==T, 1:2] # id and depth of session A
cluster_B_kmeans = cbind(cells_B, kc$cluster)
table(cluster_B_kmeans[,2:3])


dist_mat <- dist(par_B[,2:7], method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
# plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)

cluster_B_hier = cbind(cells_B, cut_avg)
table(cluster_B_hier[,2:3])


# Session C
fviz_nbclust(par_C[,2:9], kmeans, method = "wss")

kc = kmeans(par_C[,2:9], 2, nstart = 50)
table(kc$cluster)

cells_C = cells[cells$C==T, 1:2] # id and depth of session A
cluster_C_kmeans = cbind(cells_C, kc$cluster)
table(cluster_C_kmeans[,2:3])


dist_mat <- dist(par_C[,2:9], method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
# plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)

cluster_C_hier = cbind(cells_C, cut_avg)
table(cluster_C_hier[,2:3])


# Convert posterior probabilities to binary data

load("post_A.Rdata")
load("post_B.Rdata")
load("post_C.Rdata")

#threshold set to 1/2

post_A_01 = round(post_A)
post_B_01 = round(post_B)
post_C_01 = round(post_C)

#Bin the data (to reduce time series length)

post_01 <- post_A_01 #change A,B,C 

bin_length<-100 #tuning parameter
floor<-floor(dim(post_01)[1]/bin_length)  
post_01_cut<- post_01[1:(bin_length*floor),] #we removed last obs to make the bins equal
post_01_cut_sum<-matrix(nrow = floor,ncol = ncol(post_01))
for(i in 1:floor){
  post_01_cut_sum[i,]<-colSums(post_01_cut[(i*bin_length-bin_length+1):(i*bin_length),])
}

#Estimation count TS model

estimates<-matrix(nrow=3,ncol=ncol(post_01)) #three parameters of interest
for(i in 1:ncol(post_01)){
  estimates[,i]<-summary(tscount::tsglm(post_01_cut_sum[,i]  ,model=list(past_obs=1,past_mean=1),distr='nbinom',link='log'))[['coefficients']]['Estimate'][c('beta_1','alpha_1','sigmasq'),]
}

estimates[3,] = 1/estimates[3,] #since sigmasq = 1/phi

row.names(estimates) <- c('beta_1','alpha_1','phi')


estimatesC = estimates

#Boxplots of coefficients

par(mfrow=c(1,3))

boxplot(estimatesA[1,], at=1, xlim=c(0, 4),col = 'red',main=latex2exp::TeX(r'(Coefficient Boxplots: $\beta$)'))
boxplot(estimatesB[1,], at=2, add=TRUE, col = 'green')
boxplot(estimatesC[1,], at=3, add=TRUE, col = 'blue')

boxplot(estimatesA[2,], at=1, xlim=c(0, 4),col = 'red',main=latex2exp::TeX(r'(Coefficient Boxplots: $\alpha$)'))
boxplot(estimatesB[2,], at=2, add=TRUE, col = 'green')
boxplot(estimatesC[2,], at=3, add=TRUE, col = 'blue')

boxplot(estimatesA[3,], at=1, xlim=c(0, 4),col = 'red',main=latex2exp::TeX(r'(Coefficient Boxplots: $\phi$)'))
boxplot(estimatesB[3,], at=2, add=TRUE, col = 'green')
boxplot(estimatesC[3,], at=3, add=TRUE, col = 'blue')

#Diagnostics (first neuron)

val=matrix(data=NA,nrow=31,ncol=ncol(post_01))
for(i in 1:ncol(post_01)){
  fit<-tscount::tsglm( post_01_cut_sum[,i]  , model=list(past_obs=1,past_mean=1),distr='nbinom',link='log')
  val[,i]=data.frame(acf(residuals(fit),plot=F)[[1]])
}

fit<-tscount::tsglm( post_01_cut_sum[,1]  , model=list(past_obs=1,past_mean=1),distr='nbinom',link='log')
acf(residuals(fit), main = "ACF of response residuals") 
marcal(fit, main = "Marginal calibration")
lines(marcal(fit, plot = FALSE), lty = "dashed")
pit(fit, ylim = c(0, 1.5), main = "PIT Negative Binomial")

#Clustering 

session <- 'A'  #choose A,B,C

#1- K-means

kc = kmeans(t(estimates[c('beta_1','alpha_1'),]), centers=3, nstart = 20) #only beta and alpha
table(kc$cluster)

factoextra::fviz_nbclust(t(estimates[c('beta_1','alpha_1',''),]), kmeans, method = "wss")

kc = kmeans(t(estimates[c('beta_1','alpha_1','phi')]), centers=2, nstart = 50)
table(kc$cluster)

df<-cbind(neuron=cells[cells[session]==T,'cell_id'], 
          cluster=kc$cluster,
          depth=cells[cells[session]==T,'depth'])
k_clust<-table(as.data.frame(df[,c('cluster','depth')]))

#2- Hierarchical

dist_mat <- dist(t(estimates[c('beta_1','alpha_1'),]), method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
cut_avg <- cutree(hclust_avg, k = 2)
table(cut_avg)

df2<-cbind(neuron=cells[cells[session]==T,'cell_id'], 
          cluster=cut_avg,
          depth=cells[cells[session]==T,'depth'])
h_clust<-table(as.data.frame(df2[,c('cluster','depth')]))


#Bin length sensitivity

#Run 581-627 for different lengths and compare


# Clustering Analysis - Plots 

data_01 = data.frame(post_01_cut_sum)

#200 depth

d200_c1 = ggplot(data_01, aes(x=1:nrow(data_01), y=data_01[,38])) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Value")

d200_c2 = ggplot(data_01, aes(x=1:nrow(data_01), y=data_01[,45])) +
  geom_line( color="orange") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Value")

d275_c1 = ggplot(data_01, aes(x=1:nrow(data_01), y=data_01[,14])) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Value")

d275_c2 = ggplot(data_01, aes(x=1:nrow(data_01), y=data_01[,16])) +
  geom_line( color="orange") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Value")

d375_c1 = ggplot(data_01, aes(x=1:nrow(data_01), y=data_01[,2])) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Value")

d375_c2 = ggplot(data_01, aes(x=1:nrow(data_01), y=data_01[,1])) +
  geom_line( color="orange") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Value")



grid.arrange(d200_c1, d275_c1, d375_c1, d200_c2, d275_c2, d375_c2, ncol = 3)


ggplot(data_01, aes(x=1:nrow(data_01), y=data_01[,7])) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Value")






#Calcio e attivazioni al minuto

par(mfrow=c(3,2))

i = 39 #neuron cambia da 1 ... 4
calcio = sessionC[,i]
obs = post_01_cut_sum[,i] 
ts.plot(calcio,xlab='',ylab = 'calcium')
fit<-tscount::tsglm( obs ,model=list(past_obs=1,past_mean=1),distr='nbinom',link='identity')
pt = fitted(fit) -fit$coefficients[[1]]
ts.plot(data.frame(obs,pt),col=c('black','red'),xlab='',ylab = '#activations')






#Outofsample

n = nrow(post_01_cut_sum )
nin = floor (n * 0.6)
insample = post_01_cut_sum[1:nin,i] 
fit<-tscount::tsglm( insample  ,model=list(past_obs=1,past_mean=1),start.control = list(intercept = 2),distr='nbinom',link='identity')
outsample = ts(as.matrix(post_01_cut_sum[(nin+1):n,i]))
predict(fit, n.ahead = 1, level = 0.9, global = TRUE, newxreg = outsample )$pred


p1 = which(kc$cluster==1)
obs = post_01_cut_sum[,p1[7]] #1,5
fit<-tscount::tsglm( obs ,model=list(past_obs=1,past_mean=1),distr='nbinom',link='identity')
pt = fitted(fit) -fit$coefficients[[1]]
ts.plot(data.frame(obs,pt),col=c('black','red'), xlab='bins',ylab='activations')


p2 = which(kc$cluster==2)
obs = post_01_cut_sum[,p2[7]] #1,6,7
fit<-tscount::tsglm( obs ,model=list(past_obs=1,past_mean=1),distr='nbinom',link='identity')
pt = fitted(fit) #-fit$coefficients[[1]]
ts.plot(data.frame(obs,pt),col=c('black','red'), xlab='bins',ylab='activations')


medio = median(estimates[1,]/estimates[2,])
pp = c()
for (i in 1:ncol(post_01_cut_sum)){
  
  if (estimates[1,i]/estimates[2,i] >= medio ){
    pp[i] = 2
  }else{
    pp[i] = 1
  }

}







mod_comp1 = depmix(sessionA_stim[,4] ~ 1+sessionA_stim$stimuliA, nstates = 2, ntimes = length(sessionA_stim[,4]))
HMM_comp1 = fit(mod_comp1)
probs_comp1 = posterior(HMM_comp1)
table(probs_comp1$state)

mod_comp2 = depmix(sessionA_stim[,1] ~ 1+sessionA_stim$stimuliA, nstates = 2, ntimes = length(sessionA_stim[,1]))
HMM_comp2 = fit(mod_comp2)
probs_comp2 = posterior(HMM_comp2)
table(probs_comp2$state)


write.xlsx(data.frame(salva), file = 'posterior.xlsx')





