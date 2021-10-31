library(truncdist)
library(BBmisc)
library(e1071) 
library(SuppDists)
library(plyr)
location <- 0
scale    <- 2

#weibul
rand<-runif(1000, min=1, max=100)

set.seed(13579)                                       # Set seed for reproducibility
N <- 1000  #sample size


b <- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  return(quantile(y_rweibull, 0.95))
}

std <- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  return(sd(y_rweibull))
}

cv <- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  return(cv(y_rweibull))
}

pv<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  std=sd(y_rweibull)
  return(std*100/mean(y_rweibull))
}

lamb_sk<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  
  return(quantile(y_rweibull, 0.90)-quantile(y_rweibull, 0.50))/(quantile(y_rweibull, 0.50)-quantile(y_rweibull, 0.10))
}

lamb_var<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  return((quantile(y_rweibull, 0.90)-quantile(y_rweibull, 0.10))/(quantile(y_rweibull, 0.50)))
}

buffI<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  return((quantile(y_rweibull, 0.95)-mean(y_rweibull))/mean(y_rweibull))
}

sk<-function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  return(skewness( y_rweibull))
}


misery<-function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_rweibull <- rweibull(N,shape = x)
  q8<-quantile(y_rweibull, 0.8)
  wors<-y_rweibull[y_rweibull>q8]
  return((mean(wors)-mean(y_rweibull))/mean(y_rweibull))
}


df<-data.frame(matrix(ncol=0,nrow=1000))

#df$quant<-sapply(rand, b)
#df$std<-sapply(rand,  std)
#df$cv<-sapply(rand, cv)
df$pv<-sapply(rand,pv)
df$lamb_sk<-sapply(rand,  lamb_sk)
df$lamb_var<-sapply(rand,  lamb_var)
df$buffI<-sapply(rand, buffI)
df$misery<-sapply(rand,misery)
df$sk<-sapply(rand, sk)
df$shape<-rand





min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to first four columns in iris dataset
df3 <- as.data.frame(lapply(df[1:6], min_max_norm))



###################################
###################################
#########lnorm#####################





pv_lnorm<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_lnorm <- rlnorm(N,meanlog = 0.3, sdlog = x)
  std=sd(y_lnorm)
  return(std*100/mean(y_lnorm))
}

lamb_sklnorm<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_lnorm <- rlnorm(N,meanlog = 0.3, sdlog = x)
  
  return(quantile(y_lnorm, 0.90)-quantile(y_lnorm, 0.50))/(quantile(y_lnorm, 0.50)-quantile(y_lnorm, 0.10))
}

lamb_varlnorm<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_lnorm<- rlnorm(N,meanlog = 0.3, sdlog = x)
  return((quantile(y_lnorm, 0.90)-quantile(y_lnorm, 0.10))/(quantile(y_lnorm, 0.50)))
}

buffI_lnorm<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_lnorm <- rlnorm(N,meanlog = 0.3, sdlog = x)
  return((quantile(y_lnorm, 0.95)-mean(y_lnorm))/mean(y_lnorm))
}

sk_lnorm<-function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_lnorm <- rlnorm(N,meanlog = 0.3, sdlog = x)
  return(skewness( y_lnorm))
}


misery_lnorm<-function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_lnorm <-rlnorm(N,meanlog = 0.3, sdlog = x)
  q8<-quantile(y_lnorm, 0.8)
  wors<-y_lnorm[y_lnorm>q8]
  return((mean(wors)-mean(y_lnorm))/mean(y_lnorm))
}





df_lnorm<-data.frame(matrix(ncol=0,nrow=1000))


df_lnorm$pv<-sapply(rand_lnorm,pv_lnorm)
df_lnorm$lamb_sk<-sapply(rand_lnorm,  lamb_sklnorm)
df_lnorm$lamb_var<-sapply(rand_lnorm,  lamb_varlnorm)
df_lnorm$buffI<-sapply(rand_lnorm, buffI_lnorm)
df_lnorm$misery<-sapply(rand_lnorm, misery_lnorm)

#df_lnorm<- as.data.frame(lapply(df_lnorm[1:5], min_max_norm))

df_lnorm$sk<-sapply(rand_lnorm, sk_lnorm)


df_lnorm$shape<-rand_lnorm






df_lnorm1<-df_lnorm



df_lnorm1[round(df_lnorm1$pv, 5)<0.5, "pv"] <- "ur"
df_lnorm1[which(df_lnorm1$pv!="ur"),"pv"]<-"r"


df_lnorm1[round(df_lnorm1$lamb_sk, 5)<0.5, "lamb_sk"] <- "ur"
df_lnorm1[which(df_lnorm1$lamb_sk!="ur"),"lamb_sk"]<-"r"

df_lnorm1[round(df_lnorm1$lamb_var, 5)<0.5, "lamb_var"] <- "ur"
df_lnorm1[which(df_lnorm1$lamb_var!="ur"),"lamb_var"]<-"r"

df_lnorm1[round(df_lnorm1$buffI, 5)<0.5, "buffI"] <- "ur"
df_lnorm1[which(df_lnorm1$buffI!="ur"),"buffI"]<-"r"

df_lnorm1[round(df_lnorm1$misery, 5)<0.5, "misery"] <- "ur"
df_lnorm1[which(df_lnorm1$misery!="ur"),"misery"]<-"r"


df_lnorm1$agreement<-0

df_lnorm1[which( df_lnorm1$buffI=="r" & df_lnorm1$lamb_var=="r"& df_lnorm1$lamb_sk=="r" & df_lnorm1$pv=="r" & df_lnorm1$misery=="r"),"agreement"] <- "a"
df_lnorm1[which( df_lnorm1$buffI=="ur" & df_lnorm1$lamb_var=="ur"& df_lnorm1$lamb_sk=="ur" & df_lnorm1$pv=="ur" & df_lnorm1$misery=="ur"),"agreement"] <- "a"
df_lnorm1[which(df_lnorm1$agreement !="a"),"agreement"]<-"disa"



df_lnorm1$sknorm<-sapply(df_lnorm1[6:6],min_max_norm)
dis_lnorm<-df_lnorm1[(df_lnorm1$agreement=="disa"),]



df_weibb<-df5[3:10]
new<-rbind(df_lnorm1[1:8],df1_gam1[1:8],df_weibb,df_beta1[1:8])
new$sknorm<-sapply(new[6:6],min_max_norm)

disag_new<-new[(new$agreement=="disa"),]




#########################################
#######gamma#############################
 





df1_gam1<-df1_gam


df1_gam1$normalsk<-sapply(df1_gam[6:6], min_max_norm)

df1_gam1[round(df1_gam1$pv, 5)<0.5, "pv"] <- "ur"
df1_gam1[which(df1_gam1$pv!="ur"),"pv"]<-"r"


df1_gam1[round(df1_gam1$lamb_sk, 5)<0.5, "lamb_sk"] <- "ur"
df1_gam1[which(df1_gam1$lamb_sk!="ur"),"lamb_sk"]<-"r"

df1_gam1[round(df1_gam1$lamb_var, 5)<0.5, "lamb_var"] <- "ur"
df1_gam1[which(df1_gam1$lamb_var!="ur"),"lamb_var"]<-"r"

df1_gam1[round(df1_gam1$buffI, 5)<0.5, "buffI"] <- "ur"
df1_gam1[which(df1_gam1$buffI!="ur"),"buffI"]<-"r"

df1_gam1[round(df1_gam1$misery, 5)<0.5, "misery"] <- "ur"
df1_gam1[which(df1_gam1$misery!="ur"),"misery"]<-"r"



df1_gam1$agreement<-0

df1_gam1[which( df1_gam1$buffI=="r" & df1_gam1$lamb_var=="r"& df1_gam1$lamb_sk=="r" & df1_gam1$pv=="r" & df1_gam1$misery=="r"),"agreement"] <- "a"
df1_gam1[which( df1_gam1$buffI=="ur" & df1_gam1$lamb_var=="ur"& df1_gam1$lamb_sk=="ur" & df1_gam1$pv=="ur" & df1_gam1$misery=="ur"),"agreement"] <- "a"
df1_gam1[which( df1_gam1$agreement!="a"),"agreement"]<-"disa"


#############################################
#############################################
############### inv guass###########################

rand_beta<-runif(1000, min=1, max=100)










pv_beta<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_beta <- rinvGauss(N,2,x)
  std=sd(y_beta)
  return(std*100/mean(y_beta))
}

lamb_skbeta<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_beta <- rinvGauss(N,2,x)
  
  return(quantile(y_beta, 0.90)-quantile(y_beta, 0.50))/(quantile(y_beta, 0.50)-quantile(y_beta, 0.10))
}

lamb_varbeta<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_beta <- rinvGauss(N,2,x)
  return((quantile(y_beta, 0.90)-quantile(y_beta, 0.10))/(quantile(y_beta, 0.50)))
}

buffI_beta<- function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_beta <- rinvGauss(N,2,x)
  return((quantile(y_beta, 0.95)-mean(y_beta))/mean(y_beta))
}

sk_beta<-function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_beta <- rinvGauss(N,2,x)
  return(skewness( y_beta))
}


misery_beta<-function(x) {  
  set.seed(13579)                                       # Set seed for reproducibility
  N <- 1000  #sample size
  y_beta <- rinvGauss(N,2,x)
  q8<-quantile(y_beta, 0.8)
  wors<-y_beta[y_beta>q8]
  return((mean(wors)-mean(y_beta))/mean(y_beta))
}





df_beta<-data.frame(matrix(ncol=0,nrow=1000))


df_beta$pv<-sapply(rand_beta,pv_beta)
df_beta$lamb_sk<-sapply(rand_beta,  lamb_skbeta)
df_beta$lamb_var<-sapply(rand_beta,  lamb_varbeta)
df_beta$buffI<-sapply(rand_beta, buffI_beta)
df_beta$misery<-sapply(rand_beta, misery_beta)

df_beta <- as.data.frame(lapply(df_beta[1:5], min_max_norm))

df_beta$sk<-sapply(rand_beta, sk_beta)


df_beta$shape<-rand_beta






df_beta1<-df_beta



df_beta1[round(df_beta1$pv, 5)<0.5, "pv"] <- "ur"
df_beta1[which(df_beta1$pv!="ur"),"pv"]<-"r"


df_beta1[round(df_beta1$lamb_sk, 5)<0.5, "lamb_sk"] <- "ur"
df_beta1[which(df_beta1$lamb_sk!="ur"),"lamb_sk"]<-"r"

df_beta1[round(df_beta1$lamb_var, 5)<0.5, "lamb_var"] <- "ur"
df_beta1[which(df_beta1$lamb_var!="ur"),"lamb_var"]<-"r"

df_beta1[round(df_beta1$buffI, 5)<0.5, "buffI"] <- "ur"
df_beta1[which(df_beta1$buffI!="ur"),"buffI"]<-"r"

df_beta1[round(df_beta1$misery, 5)<0.5, "misery"] <- "ur"
df_beta1[which(df_beta1$misery!="ur"),"misery"]<-"r"


df_beta1$agreement<-0

df_beta1[which( df_beta1$buffI=="r" & df_beta1$lamb_var=="r"& df_beta1$lamb_sk=="r" & df_beta1$pv=="r" & df_beta1$misery=="r"),"agreement"] <- "a"
df_beta1[which( df_beta1$buffI=="ur" & df_beta1$lamb_var=="ur"& df_beta1$lamb_sk=="ur" & df_beta1$pv=="ur" & df_beta1$misery=="ur"),"agreement"] <- "a"
df_beta1[which(df_beta1$agreement !="a"),"agreement"]<-"disa"



df_beta1$sknorm<-sapply(df_beta1[6:6],min_max_norm)
df1_gam1$sknorm<-sapply(df_beta1[6:6],min_max_norm)
df5$sknorm<-sapply(df5[7:7],min_max_norm)








df_total<-rbind(df,df1_gam,df_lnorm,df_beta)

df_total[1:5]<- as.data.frame(lapply(df_total[1:5], min_max_norm))

df_total[round(df_total$pv, 5)<0.5, "pv"] <- "ur"
df_total[which(df_total$pv!="ur"),"pv"]<-"r"


df_total[round(df_total$lamb_sk, 5)<0.5, "lamb_sk"] <- "ur"
df_total[which(df_total$lamb_sk!="ur"),"lamb_sk"]<-"r"

df_total[round(df_total$lamb_var, 5)<0.5, "lamb_var"] <- "ur"
df_total[which(df_total$lamb_var!="ur"),"lamb_var"]<-"r"

df_total[round(df_total$buffI, 5)<0.5, "buffI"] <- "ur"
df_total[which(df_total$buffI!="ur"),"buffI"]<-"r"

df_total[round(df_total$misery, 5)<0.5, "misery"] <- "ur"
df_total[which(df_total$misery!="ur"),"misery"]<-"r"


df_total$agreement<-0

df_total[which( df_total$buffI=="r" & df_total$lamb_var=="r"& df_total$lamb_sk=="r" & df_total$pv=="r" & df_total$misery=="r"),"agreement"] <- "a"
df_total[which( df_total$buffI=="ur" & df_total$lamb_var=="ur"& df_total$lamb_sk=="ur" & df_total$pv=="ur" & df_total$misery=="ur"),"agreement"] <- "a"
df_total[which(df_total$agreement !="a"),"agreement"]<-"disa"
df_total$sknorm<-sapply(df_total[6:6], min_max_norm)
dis_total<-df_total[(df_total$agreement=="disa"),]
agre_total<-df_total[(df_total$agreement=="a"),]




df_total2<-rbind(df,df1_gam,df_lnorm,df_beta)
df_total2$agreement<-df_total$agreement
df_total2[1:5]<- as.data.frame(lapply(df_total2[1:5], min_max_norm))

df_total3<-df_total2[1:6]
df_total4<-df_total3[1:5]

#################################################
####################################################majority voting

majorty_total<-apply(df_total,1,function(x) names(which.max(table(x))))

df_total$maj<-majorty_total



majority_disagreement<-apply(dis_total,1,function(x) names(which.max(table(x))))
dis_total$majority<-majority_disagreement



count(dis_total$pv==dis_total$majority)


df_total2$avgvote<-apply(df_total2[1:5],1,mean)

df_total2[round(df_total2$avgvote, 5)<0.5, "avgvote"] <- "ur"
df_total2[which(df_total2$avgvote!="ur"),"avgvote"]<-"r"
df_total2$majority<-majorty_total


voting_result<-df_total2[(df_total2$avgvote!=df_total2$majority),]
   


#########################################
############################################
########sensitivity 0.4#####

df_sensitivity4<-df_total2[1:7]
df_sensitivity4$avgvote<-apply(df_sensitivity4[1:5],1,mean)




df_sensitivity4[round(df_sensitivity4$avgvote, 5)<0.4, "avgvote"] <- "ur"
df_sensitivity4[which(df_sensitivity4$avgvote!="ur"),"avgvote"]<-"r"


df_sensitivity4[round(df_sensitivity4$pv, 5)<0.4, "pv"] <- "ur"
df_sensitivity4[which(df_sensitivity4$pv!="ur"),"pv"]<-"r"


df_sensitivity4[round(df_sensitivity4$lamb_sk, 5)<0.4, "lamb_sk"] <- "ur"
df_sensitivity4[which(df_sensitivity4$lamb_sk!="ur"),"lamb_sk"]<-"r"

df_sensitivity4[round(df_sensitivity4$lamb_var, 5)<0.4, "lamb_var"] <- "ur"
df_sensitivity4[which(df_sensitivity4$lamb_var!="ur"),"lamb_var"]<-"r"

df_sensitivity4[round(df_sensitivity4$buffI, 5)<0.4, "buffI"] <- "ur"
df_sensitivity4[which(df_sensitivity4$buffI!="ur"),"buffI"]<-"r"

df_sensitivity4[round(df_sensitivity4$misery, 5)<0.4, "misery"] <- "ur"
df_sensitivity4[which(df_sensitivity4$misery!="ur"),"misery"]<-"r"


majorty_totalsens4<-apply(df_sensitivity4[1:5],1,function(x) names(which.max(table(x))))

df_sensitivity4$majvote<-majorty_totalsens4


count(df_sensitivity4$misery==df_sensitivity4$avgvote)

voting_result<-df_total2[(df_total2$avgvote!=df_total2$majority),]



#########################################
############################################
########sensitivity 0.6#####

df_sensitivity6<-df_total2[1:7]
df_sensitivity6$avgvote<-apply(df_sensitivity6[1:5],1,mean)




df_sensitivity6[round(df_sensitivity6$avgvote, 5)<0.6, "avgvote"] <- "ur"
df_sensitivity6[which(df_sensitivity6$avgvote!="ur"),"avgvote"]<-"r"


df_sensitivity6[round(df_sensitivity6$pv, 5)<0.6, "pv"] <- "ur"
df_sensitivity6[which(df_sensitivity6$pv!="ur"),"pv"]<-"r"


df_sensitivity6[round(df_sensitivity6$lamb_sk, 5)<0.6, "lamb_sk"] <- "ur"
df_sensitivity6[which(df_sensitivity6$lamb_sk!="ur"),"lamb_sk"]<-"r"

df_sensitivity6[round(df_sensitivity6$lamb_var, 5)<0.6, "lamb_var"] <- "ur"
df_sensitivity6[which(df_sensitivity6$lamb_var!="ur"),"lamb_var"]<-"r"

df_sensitivity6[round(df_sensitivity6$buffI, 5)<0.6, "buffI"] <- "ur"
df_sensitivity6[which(df_sensitivity6$buffI!="ur"),"buffI"]<-"r"

df_sensitivity6[round(df_sensitivity6$misery, 5)<0.6, "misery"] <- "ur"
df_sensitivity6[which(df_sensitivity6$misery!="ur"),"misery"]<-"r"


majorty_totalsens6<-apply(df_sensitivity6[1:5],1,function(x) names(which.max(table(x))))

df_sensitivity6$majvote<-majorty_totalsens6


count(df_sensitivity6$pv==df_sensitivity6$avgvote)

voting_result<-df_total2[(df_total2$avgvote!=df_total2$majority),]


#########################################
############################################
########sensitivity 0.3#####

df_sensitivity3<-df_total2[1:7]
df_sensitivity3$avgvote<-apply(df_sensitivity3[1:5],1,mean)




df_sensitivity3[round(df_sensitivity3$avgvote, 5)<0.3, "avgvote"] <- "ur"
df_sensitivity3[which(df_sensitivity3$avgvote!="ur"),"avgvote"]<-"r"


df_sensitivity3[round(df_sensitivity3$pv, 5)<0.3, "pv"] <- "ur"
df_sensitivity3[which(df_sensitivity3$pv!="ur"),"pv"]<-"r"


df_sensitivity3[round(df_sensitivity3$lamb_sk, 5)<0.3, "lamb_sk"] <- "ur"
df_sensitivity3[which(df_sensitivity3$lamb_sk!="ur"),"lamb_sk"]<-"r"

df_sensitivity3[round(df_sensitivity3$lamb_var, 5)<0.3, "lamb_var"] <- "ur"
df_sensitivity3[which(df_sensitivity3$lamb_var!="ur"),"lamb_var"]<-"r"

df_sensitivity3[round(df_sensitivity3$buffI, 5)<0.3, "buffI"] <- "ur"
df_sensitivity3[which(df_sensitivity3$buffI!="ur"),"buffI"]<-"r"

df_sensitivity3[round(df_sensitivity3$misery, 5)<0.3, "misery"] <- "ur"
df_sensitivity3[which(df_sensitivity3$misery!="ur"),"misery"]<-"r"


majorty_totalsens3<-apply(df_sensitivity3[1:5],1,function(x) names(which.max(table(x))))

df_sensitivity3$majvote<-majorty_totalsens3


count(df_sensitivity3$pv==df_sensitivity3$majvote)


############################################
########sensitivity 0.7#####

df_sensitivity7<-df_total2[1:7]
df_sensitivity7$avgvote<-apply(df_sensitivity7[1:5],1,mean)




df_sensitivity7[round(df_sensitivity7$avgvote, 5)<0.7, "avgvote"] <- "ur"
df_sensitivity7[which(df_sensitivity7$avgvote!="ur"),"avgvote"]<-"r"


df_sensitivity7[round(df_sensitivity7$pv, 5)<0.7, "pv"] <- "ur"
df_sensitivity7[which(df_sensitivity7$pv!="ur"),"pv"]<-"r"


df_sensitivity7[round(df_sensitivity7$lamb_sk, 5)<0.7, "lamb_sk"] <- "ur"
df_sensitivity7[which(df_sensitivity7$lamb_sk!="ur"),"lamb_sk"]<-"r"

df_sensitivity7[round(df_sensitivity7$lamb_var, 5)<0.7, "lamb_var"] <- "ur"
df_sensitivity7[which(df_sensitivity7$lamb_var!="ur"),"lamb_var"]<-"r"

df_sensitivity7[round(df_sensitivity7$buffI, 5)<0.7, "buffI"] <- "ur"
df_sensitivity7[which(df_sensitivity7$buffI!="ur"),"buffI"]<-"r"

df_sensitivity7[round(df_sensitivity7$misery, 5)<0.7, "misery"] <- "ur"
df_sensitivity7[which(df_sensitivity7$misery!="ur"),"misery"]<-"r"


majorty_totalsens7<-apply(df_sensitivity7[1:5],1,function(x) names(which.max(table(x))))

df_sensitivity7$majvote<-majorty_totalsens7


count(df_sensitivity7$pv==df_sensitivity7$majvote)
















############lets do k means clustering##################3
#install.packages("mlbench")
library("cluster")
library("factoextra")


df_total4= scale(df_total4)
# K means
k.means.fit <- kmeans(df_total4, 2)

clusplot(df_total4, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=2, lines=0)

re<-k.means.fit$cluster

df_total3$res<-re



# H.Ward
d <- dist(df_total2[1:5], method = "euclidean")
H.fit <- hclust(d, method="ward.D")
plot(H.fit)
groups <- cutree(H.fit, k=2)





#############################################3
######################################################3
##############data us 101#################################
data_h<-read.csv("Next_Generation_Simulation__NGSIM__Vehicle_Trajectories_and_Supporting_Data.csv")

