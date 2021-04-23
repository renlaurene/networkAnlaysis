# networkAnlaysis
path and network nodes analysis
head(nn$generalized.weights[[1]])

# visualization

plot(nn)


#

require(clusterGeneration)
set.seed(2)
num.vars<-8
num.obs<-1000
#arbitrary correlation matrix and random variables
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms<-runif(num.vars,-10,10)
#response variable as linear combination of random variables and random error term
y<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

require(nnet)
rand.vars<-data.frame(rand.vars)
y<-data.frame((y-min(y))/(max(y)-min(y)))
names(y)<-'y'
mod1<-nnet(rand.vars,y,size=10,linout=T)

#import function from Github
require(RCurl)
root.url<-'https://gist.github.com/fawda123'
raw.fun<-paste(
  root.url,
  '5086859/raw/17fd6d2adec4dbcf5ce750cbd1f3e0f4be9d8b19/nnet_plot_fun.r',
  sep='/'
)
script<-getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
rm('script','raw.fun')

par(mar=numeric(4),mfrow=c(1,2),family='serif')
plot(mod1,nid=F)
plot(mod1)

#example data and code from nnet function examples
ir<-rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets<-class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
samp<-c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
ir1<-nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,decay = 5e-4, maxit = 200)
#plot the model with different default values for the arguments
par(mar=numeric(4),family='serif')
plot.nnet(ir1,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.7,rel.rsc=15,
          circle.cex=10,cex=1.4,
          circle.col='brown')
