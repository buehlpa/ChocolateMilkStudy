
sample1=rnorm(50,76.8,19.8)
par(mfrow=c(2,1))
hist(sample1)
fehlendewerte=c(rep(90,28))
hist(c(sample1,fehlendewerte))

mean(c(sample1,fehlendewerte))
sd(c(sample1,fehlendewerte))





x1=70.3
x2=76.8
n1=51
n2=80
  
sd1=16.7
sd2=19.8



twosample_z<-function(x1,x2,n1,n2,sd1,sd2){z=(x1-x2)/sqrt( (sd1^2/n1) + (sd2^2/n2) );p=2*pnorm(-abs(z));return(list(z_wert=z,p_value=p))}

for (i in c(seq(76.8,100,by=0.1))){print(twosample_z(x1,i,n1,n2,sd1,sd2));print(i)}


pnorm(-1.96,0,1)
