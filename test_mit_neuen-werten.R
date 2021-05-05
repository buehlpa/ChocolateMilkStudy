sample1=rnorm(50,76.8,19.8)
par(mfrow=c(2,1))
hist(sample1)
fehlendewerte=c(rep(90,28))
hist(c(sample1,fehlendewerte))

mean(c(sample1,fehlendewerte))
sd(c(sample1,fehlendewerte))








