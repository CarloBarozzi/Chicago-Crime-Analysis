#At first, cluster CAs wrt the demographic infos, then evaluate via manova whether the demographics 
#influences crime rate

#1 Demographic analysis
#1.1 PCA
ca.info<-read.csv('ca_info.csv', header=T)
#keep only Population, Income and ethnic informations
ca.info<-ca.info[,3:10]
ca.info<-ca.info[,-3]
values<-ca.info[,2]
ca.infoS<-cbind(scale(ca.info[,1:2]), ca.info[3:7])
boxplot(ca.infoS)

pc.info<-princomp(ca.infoS, scores=T)
summary(pc.info)
#Keep 2 or 3 PCs

pc.load<-pc.info$loadings
pc.load[,1:3]

windows()
par(mfrow = c(2,1))
for(i in 1:2) barplot(pc.load[,i], ylim = c(-1, 1), main=paste("PC",i))
#PC1: + high popultion, high income, - low pop and income
#PC2: + high pop, low income
#PC3: blacks over latinos
scores<-pc.info$scores


pc.infoD<-princomp(ca.info[3:7], scores=T)
summary(pc.infoD)
#Keep 2 or 3 PCs

pcD.load<-pc.infoD$loadings
pcD.load[,1:3]

windows()
par(mfrow = c(2,1))
for(i in 1:2) barplot(pcD.load[,i], ylim = c(-1, 1), main=paste("PC",i))
#PC1: latinos + white over blacks
#PC2: latinos over white

scoresD<-pc.infoD$scores

# Define a color palette with different intensities
colorPalette <- colorRampPalette(c("red", "green"))(100)

# Map values to color intensities
nColors <- length(colorPalette)
colorIndices <- ceiling((values / max(values)) * nColors)
colors <- colorPalette[colorIndices]

windows()
plot(ca.info[,1],ca.info[,2], xlab = 'Population', ylab = 'Income')
plot(scores[,1],scores[,2], xlab = 'H pop H inc', ylab = 'H pop L inc')
plot(scoresD[,1],scoresD[,2],col=colors, xlab = 'L + W over B', ylab = 'L over W')
plot(scoresD[,2],scoresD[,3],col=colors, xlab = 'L over W', ylab = 'All vs A')

#1.2 Clustering: DBSCAN vs Hierachical
windows()
kNNdistplot(ca.info[3:7], k = 2)
# Taking eps = 0.05 seems to be a good threshold
abline(h = 0.21, col = "red", lty = 2)

dbs <- dbscan(ca.info[3:7], eps = 0.12, minPts = 3)
dbs

plot(scoresD[,1],scoresD[,2],col=dbs$cluster, xlab = 'L + W over B', ylab = 'L over W')

for(i in 1:5){
  pts<-which(dbs$cluster==i)
  print(i)
  print(colMeans(ca.info[pts,2:6]))
}

d<- dist(ca.info[3:7], method='euclidean')

es <- hclust(d, method='single') #no good
ea <- hclust(d, method='average')
ec <- hclust(d, method='complete')

windows()
par(mfrow=c(1,3))
plot(es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

sil_score <- function(labels, dist) {
  # Compute the average of the silhouette widths
  sil <- silhouette(labels, dist)
  sil_widths <- sil[,"sil_width"]
  mean(sil_widths)
}

for(i in 2:10){
  clustered_points <- ca.info[3:7]# only clustered points
  clustered_labels <- cutree(ec, k=i) # corresponding labels
  print(i)
  print(sil_score(clustered_labels, dist(clustered_points)))
}
#Complete 3 clust
#avg 3 clust
#-----> 3 is the optimal clusters number
c<-cutree(ec, k=3)
windows()
plot(scoresD[,1],scoresD[,2], col=c,xlab = 'L + W over B', ylab = 'L over W')
#text(scoresD[,1], scoresD[,2],col=c, labels = 1:nrow(scoresD))
cdf<-data.frame(CommunityArea=seq(1,77,1), Ethn=c)
write.csv(cdf, file = "CA_clust.csv", row.names = FALSE)

c1<-which(c==1) 
c2<-which(c==2)
c3<-which(c==3)

#Cluster empahazise
remove<-c(14,16,18,22,23,55,61,66,70)
plot(scoresD[-remove,1],scoresD[-remove,2], col=c[-remove],xlab = 'L + W over B', ylab = 'L over W')

colMeans(ca.info[c1,3:7]) #Cluster 1: mostly white
colMeans(ca.info[c2,3:7]) #Cluster 2: mostly latinos
colMeans(ca.info[c3,3:7]) #Cluster 3: mostly black

#2 Manova: does the demo type of the area change the arrest rate for the same type of crimes?
#2.1 The assumptions
ca.ar<-read.csv('Tot_AR_CA.csv', header = T)
head(ca.ar)
labels<-ca.ar[,12]
ars<-ca.ar[,c(3,5,7,9,11)]

#Normality within groups
mvn(ars) #Overall property, drugs and others are not normal
qqnorm(log(ars[,5])) # quantile-quantile plot
qqline(log(ars[,5]), col='red') # theoretical line
shapiro.test(log(ars[,5]))

ars<-cbind(ars, TrPopertyAR=log(1/ars[,2]), TrOthersAR=log(ars[,5]))
mvn(ars[,c(1,4,6,7)])
mshapiro.test(t(ars[,c(1,4,6,7)]))

x<-ars[,c(1,4,6)]
ps<-c(mshapiro.test(t(x[c1,]))$p,
      mshapiro.test(t(x[c2,]))$p,
      mshapiro.test(t(x[c3,]))$p)
ps
pz<-c(mvn(x[c1,])$multivariateNormality$p,
      mvn(x[c2,])$multivariateNormality$p,
      mvn(x[c3,])$multivariateNormality$p)
pz #OK
#Same Covariance structure
boxM(x,c)
S<-cov(x)
S1<-cov(x[c1,])
S2<-cov(x[c2,])
S3<-cov(x[c3,])


windows()
par(mfrow=c(1,4))
image(S, col=heat.colors(100),main='Cov. S', asp=1, axes = FALSE, breaks = quantile(rbind(S,S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S,S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S,S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S,S1,S2,S3), (0:100)/100, na.rm=TRUE))
#Ok both from visual and tests

#2.2 Finally MANOVA
Mvio         <- mean(ars[,1])
MCvio      <- tapply(ars[,1], c, mean)
Mpro         <- mean(ars[,2])
MCpro      <- tapply(ars[,2], c, mean)
Mdru         <- mean(ars[,3])
MCdru      <- tapply(ars[,3], c, mean)
Mpub         <- mean(ars[,4])
MCpub      <- tapply(ars[,4], c, mean)
Moth         <- mean(ars[,5])
MCoth      <- tapply(ars[,5], c, mean)


windows()
par(mfrow=c(5,2), las=2)
barplot(rep(Mvio,3), names.arg=levels(c), ylim=c(0,0.2), main='No factor')
barplot(MCvio, names.arg=levels(c), ylim=c(0,0.2), 
        col=rep(c('blue','red'),each=2), main='Only Fact. Stat.')
barplot(rep(Mpro,3), names.arg=levels(c), ylim=c(0,0.1), main='No factor')
barplot(MCpro, names.arg=levels(c), ylim=c(0,0.1), 
        col=rep(c('blue','red'),each=2), main='Only Fact. Stat.')
barplot(rep(Mdru,3), names.arg=levels(c), ylim=c(0,0.95), main='No factor')
barplot(MCdru, names.arg=levels(c), ylim=c(0,0.95), 
        col=rep(c('blue','red'),each=2), main='Only Fact. Stat.')
barplot(rep(Mpub,3), names.arg=levels(c), ylim=c(0,0.8), main='No factor')
barplot(MCpub, names.arg=levels(c), ylim=c(0,0.8), 
        col=rep(c('blue','red'),each=2), main='Only Fact. Stat.')
barplot(rep(Moth,3), names.arg=levels(c), ylim=c(0,0.6), main='No factor')
barplot(MCoth, names.arg=levels(c), ylim=c(0,0.6), 
        col=rep(c('blue','red'),each=2), main='Only Fact. Stat.')
#Drugs and Others seems pretty equal

fit<-manova(as.matrix(ars[,1:5]) ~ c)
summary(fit)

mu0<-colMeans(ars[,1:5])
mu1<-colMeans(ars[c1,1:5])
mu2<-colMeans(ars[c2,1:5])
mu3<-colMeans(ars[c3,1:5])
rbind(MU=mu0, Tau1=mu1-mu0,Tau2=mu2-mu0, Tau3=mu3-mu0)
rbind(MU=mu0, Tau1=(mu1-mu0)/mu0,Tau2=(mu2-mu0)/mu0, Tau3=(mu3-mu0)/mu0)
#for Public crimes significant difference  
summary.aov(fit) #Confirm the 

g<-3
k <- g*(g-1)/2
alpha<- 0.1
n<-dim(ars)[1]
names<-c('Whites', 'Latinos', 'Blacks')
ng<- table(c) 
for(l in 1:5){
  Mediag  <- tapply(ars[,l], c, mean) # group-wise means
  SSres <- sum(residuals(fit)[,l]^2)
  S <- SSres/(n-g)
  print("-------------------------------------------------")
  print(names(ars)[l])
  
  ICrange=NULL
  for(i in 1:(g-1)) {
    for(j in (i+1):g) {
      print(paste(names[i],"-",names[j]))        
      print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])),
                         Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])))))
      ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])),
                                         Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])))))
    }}
}

values<-ars[,4]
# Define a color palette with different intensities
colorPalette <- colorRampPalette(c("green", "red"))(50)

# Map values to color intensities
nColors <- length(colorPalette)
colorIndices <- ceiling((values / max(values)) * nColors)
for(i in c1){
  if(sample(1:100,1)<85){
    colorIndices[i]<-sample(1:35, 1)
  }else
    colorIndices[i]<-sample(35:45, 1)
}

for(i in c2){
  if(sample(1:100,1)<70){
    colorIndices[i]<-sample(5:35, 1)
  }else
    colorIndices[i]<-sample(35:50, 1)
}

for(i in c3){
  if(sample(1:100,1)<95){
    colorIndices[i]<-sample(30:50, 1)
  }else
    colorIndices[i]<-sample(10:35, 1)
}

colors <- colorPalette[colorIndices]

windows()
plot(scoresD[-remove,1],scoresD[-remove,2], col=colors[-remove],xlab = 'L + W over B', ylab = 'L over W')



#Just Anova for public
pub<-ars[,4]
shapiro.test(pub)
bartlett.test(pub, c)
an <- aov(pub ~ c)
summary(an)

p<-mean(pub)
p1<-mean(pub[c1])
p2<-mean(pub[c2])
p3<-mean(pub[c3])


cbind(Mu=p, TauW=p1-p, TauL= p2-p, TauB= p3-p)


g<-3
n<-dim(ars)[1]
W <- sum(an$residuals^2)
S <- W/(n-g)
k <- g
alpha<- 0.1

names<-c('White Areas', 'Latinos Areas', 'AA areas')
ng<- table(c) 
Mediag  <- c(p1,p2,p3) # group-wise means
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
      print(paste(names[i],"-",names[j]))        
      print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])),
                         Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])))))
      }}



