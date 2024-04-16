df<-read.csv('tot_AR_CA.csv', header=T)
pop<-read.csv('chicago_ca_population.csv', header=T, sep=";")
pop<-pop[-78,2]
ca.label <- df[, 1]
arrestrate<-df[,7]
ca.rate <- df[, c(2,4,6,8,10)]
ca.rate[,1:5]<-ca.rate[,1:5]/pop

ca.info<-read.csv('ca_info.csv', header=T)
inf<-ca.info[,2]


#Now lets apply pca.rate to std vars
ca.rate.sd <- scale(ca.rate)
ca.rate.sd <- data.frame(ca.rate.sd)

pc.ca.rate.sd <- princomp(ca.rate.sd, scores=T)
summary(pc.ca.rate.sd)

load.ca.rate.sd<-pc.ca.rate.sd$loadings
load.ca.rate.sd

windows()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.ca.rate.sd[,i], ylim = c(-1, 1), main=paste("PC",i))
#Possible interpretations (non rate)(similar with rate)
#PC1: overall level of crime -> we can use the score of pc 1 as the dangerosity meter

scores.ca.rate.sd <- pc.ca.rate.sd$scores
danger<-data.frame(Area=inf,dangerosity=scores.ca.rate.sd[,1])

# is it a good measure?
# searchin on the net for notably dangerous or safe zones:
# Safe: Edison Park (-2.18), Norwood Park(-2.01), Forest Glen(-1.92), North Center(-2)
# Dangerous: Washington Park (2.7), West Garfield Park (6.1), Englewood (3.5)
# So notably safe areas have a low score, while notably dangerous have high scors, so it a good measure



ca_public<-read.csv('Public_Health_Statistics.csv', header = T)
ca_public<-ca_public[,c(1,2,seq(24,29))]
# Below poverty level Percent of households
# Crowded housing Percent of occupied housing units
# Dependency Percent of persons aged less than 16 or more than 64 years
# No high school diploma Percent of persons aged 25 years and older 
# Per capita income 2011 inflation-adjusted dollars
# Unemployment Percent of persons in labor force aged 16 years and older
dng<-(danger[,2] - min(danger[,2]))/(max(danger[,2])-min(danger[,2])) * 100000

colorPalette <- colorRampPalette(c("green", "red"))(100)

# Map dng values to color intensities
nColors <- length(colorPalette)
colorIndices <- cut(dng, breaks = nColors, labels = FALSE)
colors <- colorPalette[colorIndices]

# Plot pairplot with colored points
windows()
pairs(ca_public[,3:8], col = colors, pch = 16)

nVars <- 6  # Number of variables in the dataset
nRows <- ceiling(sqrt(nVars))
nCols <- ceiling(nVars/nRows)
par(mfrow = c(nRows, nCols))

for (i in 3:8) {
  plot(ca_public[[i]], dng, main = names(ca_public)[i], xlab = "", ylab = "Dangerousity",xlim = c(0,100),ylim = c(0,100))
}

plot(log(ca_public[[7]]), dng, main = names(ca_public)[i], xlab = "", ylab = "Dangerousity",ylim = c(0,100))

#MODEL: DAnger.METER = B0 + B1 * BirthRate + B2 * BelowPovertyLevel + B3 * CrowdedHousing + B4 * Dependency + B5 * NOHSDiploma +
#                      B6 * PerCapitaIncome + B7 * Unemployment + Eps, Eps gauss (0, sigma^2)
attach(ca_public)
fit1<- lm(dng ~ Below.Poverty.Level + Crowded.Housing + Dependency + No.High.School.Diploma + Per.Capita.Income + Unemployment)
summary(fit1)

res1<- residuals(fit1)

par(mfrow = c(2,2))
plot(fit1)

shapiro.test(res1)

# Plot res1 over each variable
for (i in 3:8) {
  plot(ca_public[[i]], res1, main = names(ca_public)[i], xlab = "", ylab = "res1")
}

linearHypothesis(fit1, c(0,0,1,0,-1,0,0), 0) #Eliminate one between Crowded.housing and NoHSDiplomma
linearHypothesis(fit1,rbind(c(1,0,0,0,0,0,0),c(0,0,1,0,0,0,0),c(0,0,0,1,0,0,0),c(0,0,0,0,0,1,0)),c(0,0,0,0)) #Drop birthRate, Crowded housing, dependency, percapita income

fit2<-lm(dng ~  Below.Poverty.Level + No.High.School.Diploma + Unemployment)
summary(fit2)
linearHypothesis(fit2, rbind(c(1,0,0,0)), c(0))


#Observing the pair plot of the variables some relation between the regressor is evident -> Let's use Ridge regression

# Repeat for a grid of lambda's
lambda.c <- seq(0,10,0.01)
fit.ridge <- lm.ridge(dng ~ Below.Poverty.Level + Crowded.Housing + log(Dependency) + No.High.School.Diploma + Per.Capita.Income + Unemployment, lambda = lambda.c)

windows()
par(mfrow=c(1,8))
plot(lambda.c,coef(fit.ridge)[,1], type='l', xlab=expression(lambda),
     ylab=expression(beta[0]))
abline(h=coef(fit1)[1], lty=2)
plot(lambda.c,coef(fit.ridge)[,2], type='l', xlab=expression(lambda),
     ylab=expression(beta[1]))
abline(h=coef(fit1)[2], lty=2)
plot(lambda.c,coef(fit.ridge)[,3], type='l', xlab=expression(lambda),
     ylab=expression(beta[2]))
abline(h=coef(fit1)[3], lty=2)
plot(lambda.c,coef(fit.ridge)[,4], type='l', xlab=expression(lambda),
     ylab=expression(beta[2]))
abline(h=coef(fit1)[4], lty=2)
plot(lambda.c,coef(fit.ridge)[,5], type='l', xlab=expression(lambda),
     ylab=expression(beta[2]))
abline(h=coef(fit1)[5], lty=2)
plot(lambda.c,coef(fit.ridge)[,6], type='l', xlab=expression(lambda),
     ylab=expression(beta[2]))
abline(h=coef(fit1)[6], lty=2)
plot(lambda.c,coef(fit.ridge)[,7], type='l', xlab=expression(lambda),
     ylab=expression(beta[2]))
abline(h=coef(fit1)[7], lty=2)

dev.off()

yhat.lm <- cbind(rep(1,length(dng)), Below.Poverty.Level ,Crowded.Housing , log(Dependency) ,No.High.School.Diploma , Per.Capita.Income , Unemployment)%*%coef(fit1)

plot(Below.Poverty.Level[order(Below.Poverty.Level)], yhat.lm[order(Below.Poverty.Level)], type='l', lty=1, lwd=2, ylab='Distance',
     xlab='Speed')
points(Below.Poverty.Level, dng, pch=1, cex=.8)
yhat.r <- NULL
for(i in 1:length(lambda.c))
  yhat.r=cbind(yhat.r, cbind(rep(1,length(dng)), Below.Poverty.Level ,Crowded.Housing , log(Dependency) ,No.High.School.Diploma , Per.Capita.Income , Unemployment)%*%coef(fit.ridge)[i,])
matlines(Below.Poverty.Level[order(Below.Poverty.Level)], yhat.r[order(Below.Poverty.Level)], type='l', lty=1,
         col=grey.colors(length(lambda.c)))
lines(Below.Poverty.Level[order(Below.Poverty.Level)], yhat.lm[order(Below.Poverty.Level)], type='l', lty=4, lwd=2, ylab='Distance',
      xlab='Speed')

lambda.opt <- lambda.c[which.min(fit.ridge$GCV)]
lambda.opt

coef.ridge <- coef(fit.ridge)[which.min(fit.ridge$GCV),]
coef.ridge


#PC Regression
pc<-princomp(d, scores=T)
summary(pc)

s1<-pc$scores[,1]
s2<-pc$scores[,2]
s3<-pc$scores[,3]
s4<-pc$scores[,4]


fitPC<-lm(dng~s1+s2+s3+s4)
summary(fitPC)

beta0<- coefficients(fitPC)[1] -
  coefficients(fitPC)[2]*pc$load[1,1]*m1 -
  coefficients(fitPC)[3]*pc$load[1,2]*m1 -
  coefficients(fitPC)[4]*pc$load[1,3]*m1 -
  coefficients(fitPC)[5]*pc$load[1,4]*m1 -

  coefficients(fitPC)[2]*pc$load[2,1]*m2 -
  coefficients(fitPC)[3]*pc$load[2,2]*m2 -
  coefficients(fitPC)[4]*pc$load[2,3]*m2 -
  coefficients(fitPC)[5]*pc$load[2,4]*m2 -

  coefficients(fitPC)[2]*pc$load[3,1]*m3 -
  coefficients(fitPC)[3]*pc$load[3,2]*m3 -
  coefficients(fitPC)[4]*pc$load[3,3]*m3 -
  coefficients(fitPC)[5]*pc$load[3,4]*m3 -

  coefficients(fitPC)[2]*pc$load[4,1]*m4 -
  coefficients(fitPC)[3]*pc$load[4,2]*m4 -
  coefficients(fitPC)[4]*pc$load[4,3]*m4 -
  coefficients(fitPC)[5]*pc$load[4,4]*m4 -

  coefficients(fitPC)[2]*pc$load[5,1]*m5 -
  coefficients(fitPC)[3]*pc$load[5,2]*m5 -
  coefficients(fitPC)[4]*pc$load[5,3]*m5 -
  coefficients(fitPC)[5]*pc$load[5,4]*m5 -

  coefficients(fitPC)[2]*pc$load[6,1]*m6 -
  coefficients(fitPC)[3]*pc$load[6,2]*m6 -
  coefficients(fitPC)[4]*pc$load[6,3]*m6 -
  coefficients(fitPC)[5]*pc$load[6,4]*m6 
beta1 <-  coefficients(fitPC)[2]*pc$load[1,1]+
  coefficients(fitPC)[3]*pc$load[1,2]+
  coefficients(fitPC)[4]*pc$load[1,3]+
  coefficients(fitPC)[5]*pc$load[1,4]
beta2 <-  coefficients(fitPC)[2]*pc$load[2,1]+
  coefficients(fitPC)[3]*pc$load[2,2]+
  coefficients(fitPC)[4]*pc$load[2,3]+
  coefficients(fitPC)[5]*pc$load[2,4]
beta3 <-  coefficients(fitPC)[2]*pc$load[3,1]+
  coefficients(fitPC)[3]*pc$load[3,2]+
  coefficients(fitPC)[4]*pc$load[3,3]+
  coefficients(fitPC)[5]*pc$load[3,4]
beta4 <-  coefficients(fitPC)[2]*pc$load[4,1]+
  coefficients(fitPC)[3]*pc$load[4,2]+
  coefficients(fitPC)[4]*pc$load[4,3]+
  coefficients(fitPC)[5]*pc$load[4,4]
beta5 <-  coefficients(fitPC)[2]*pc$load[5,1]+
  coefficients(fitPC)[3]*pc$load[5,2]+
  coefficients(fitPC)[4]*pc$load[5,3]+
  coefficients(fitPC)[5]*pc$load[5,4]
beta6 <-  coefficients(fitPC)[2]*pc$load[6,1]+
  coefficients(fitPC)[3]*pc$load[6,2]+
  coefficients(fitPC)[4]*pc$load[6,3]+
  coefficients(fitPC)[5]*pc$load[6,4]


c(beta0=as.numeric(beta0),beta1=as.numeric(beta1),beta2=as.numeric(beta2),beta3=as.numeric(beta3),beta4=as.numeric(beta4),beta5=as.numeric(beta5),beta6=as.numeric(beta6))

fit1$coefficients


confint(fit1, level= 1-0.05/6)


#Forget the linear model, difficult interpretation
d<-danger[,2]
breaks <- c(-Inf, -1, 1, Inf)
labels <- c('Safe', 'Neutral', 'Dangerous')

# Assign levels based on conditions
factor_levels <- cut(d, breaks = breaks, labels = labels)
factor_levels


X<-ca_public[,-c(1,2)]


#FDA
g<-3
i1<-which(factor_levels=='Safe')
i2<-which(factor_levels=='Neutral')
i3<-which(factor_levels=='Dangerous')
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1+n2+n3

m <-  colMeans(X)
m1 <- colMeans(X[i1,])
m2 <- colMeans(X[i2,])
m3 <- colMeans(X[i3,])

S1 <- cov(X[i1,])
S2 <- cov(X[i2,])
S3 <- cov(X[i3,])
Sp  <- ((n1-1)*S1+(n2-1)*S2+(n3-1)*S3)/(n-g)

# covariance between groups (estimate)
B <- 1/g*(cbind(m1 - m) %*% rbind(m1 - m) +
            cbind(m2 - m) %*% rbind(m2 - m) +
            cbind(m3 - m) %*% rbind(m3 - m))
B

# covariance within groups (estimate)
Sp

# how many coordinates?
g <- 3
p <- 6
s <- min(g-1,p)
s

# Matrix Sp^(-1/2)
val.Sp <- eigen(Sp)$val
vec.Sp <- eigen(Sp)$vec
invSp.2 <- 1/sqrt(val.Sp[1])*vec.Sp[,1]%*%t(vec.Sp[,1]) + 1/sqrt(val.Sp[2])*vec.Sp[,2]%*%t(vec.Sp[,2])
invSp.2 

# spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)

# first canonical coordinate
a1 <- invSp.2 %*% spec.dec$vec[,1]
a1

# second canonical coordinate
a2 <- invSp.2 %*% spec.dec$vec[,2]
a2

### How are the data classified?
# Compute the canonical coordinates of the data

cc1.X <- as.matrix(X)%*%a1
cc2.X <- as.matrix(X)%*%a2

coord.cc <- cbind(cc1.X,cc2.X)

# Compute the coordinates of the mean within groups along the canonical directions
cc.m1 <- c(m1%*%a1, m1%*%a2)
cc.m2 <- c(m2%*%a1, m2%*%a2)
cc.m3 <- c(m3%*%a1, m3%*%a2)

# Assign data to groups
f.class=rep(0, n)
for(i in 1:n) # for each datum
{
  # Compute the Euclidean distance of the i-th datum from mean within the groups
  dist.m=c(d1=sqrt(sum((coord.cc[i,]-cc.m1)^2)),
           d2=sqrt(sum((coord.cc[i,]-cc.m2)^2)),
           d3=sqrt(sum((coord.cc[i,]-cc.m3)^2)))
  # Assign the datum to the group whose mean is the nearest
  f.class[i]=which.min(dist.m)
}
f.class
tab<-table(class.true=factor_levels, class.assigned=f.class)
tab

errors <- n - sum(diag(tab))
errors


APERf   <- errors/n
APERf

# mvn(X)
# 
# nVars <- 6  # Number of variables in the data frame
# nRows <- ceiling(sqrt(nVars))
# nCols <- ceiling(nVars/nRows)
# windows()
# par(mfrow = c(nRows, nCols))
# 
# # Generate QQ plots for each variable
# for (i in 1:nVars) {
#   qqnorm(X[[i]], main = colnames(X)[i])
# }
# 
# dev.off()
# t<-(X[[3]]^3)
# qqnorm(t, main = colnames(X)[3])
# qqline(t)
# shapiro.test(t)
# 
# normX<-cbind(LUnemployment= log(X[[1]]), LCrowded.Housing=log(X[[2]]))

# Install and load the 'rpart' package
#install.packages("rpart")
library(rpart)

# Assuming your data frame is named 'X' and the target variable is 'factor_levels'

# Build the decision tree
decision_tree <- rpart(factor_levels ~ ., data = X)

predict(decision_tree, X)
# Print the decision tree
print(decision_tree)


windows()
color_vector <- ifelse(factor_levels == "Dangerous", "red",
                       ifelse(factor_levels == "Neutral", "black", "green"))

# Add a legend for the color coding

plot(X[[1]], X[[6]],col = color_vector, pch = 16 ,xlab='BelowPovertyLevel', ylab='Unemployment')
legend("topright", legend = c("Dangerous", "Neutral", "Safe"),
       col = c("red", "black", "green"), pch = 16)
# Add horizontal line at y = 15.65
abline(h = 15.65, col = "blue", lty = 2)



# Add line segment from (21.2, 15.65) to (21.2, y) where y = 15.65
segments(21.2, 0,21.2, 15.65, col = "orange", lty = 2)


text(35, 32.5, "Dangerous", pos = 4)
text(10, 10, "Safe", pos = 1)
text(50, 10, "Neutral", pos = 1)

#install.packages("rpart.plot")
library(rpart.plot)
prp(decision_tree,box.palette="RdBu", shadow.col="gray", nn=TRUE)

#Cross Validation Error
yp<-NULL
for (i in 1:length(factor_levels)){
  np<-predict(rpart(factor_levels[-i] ~ ., data = X[-i,]),X[i,])
  yp<-c(yp,colnames(np)[which.max(np)])
}
et<-table(TrueLabels=factor_levels, Predicted=yp)
sum(diag(et))/length(factor_levels)


# Install and load the 'randomForest' package
install.packages("randomForest")
library(randomForest)

# Assuming X is your data frame and factor_levels is the target variable

# Build the Random Forest model
rf_model <- randomForest(factor_levels ~ ., data = X)

# Print the feature importance
imp<-(importance(rf_model))
dev.off()

# Create a color vector based on increasing intensity
color_vector <- colorRampPalette(c("white", "red"))(20)


windows()
barplot(imp[,]/max(imp[,]), xlim = c(0, 2), main="RF Feature Importance", horiz = TRUE,
        col = color_vector[c(15,4,9,7,6,20)])


windows()
par(mfrow = c(2,3))
for (i in 1:6) {
    plot(log(X[[i]]), danger[,2], xlab = colnames(X)[i], ylab = "Danger",
         main = paste(colnames(X)[i], "vs. Danger"))
  abline(lm(danger[,2] ~ X[[i]]), col = "red")  # Add regression line
}


#Print some sort of map
ALLdata<-read.csv('final.csv')
colori<-c('green','black','red')
windows()
plot(ALLdata[['Longitude']], ALLdata[['Latitude']], col=colori[as.numeric(factor_levels)[ALLdata[['Community.Area']]]],xlab ='Longitude',ylab='Latitude',xlim=c(-88,-87.5), ylim=c(41.6,42.1),pch=1)
