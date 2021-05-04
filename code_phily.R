# Data####
# Body Weight
#       Overall     CM          CHO
# Pre   73.3±18.5   70.3±16.7   76.8±19.8
# Post  74.4±18.8   71.0±16.6   77.4±20.3
bw <- rbind(c(73.3, 18.5, 70.3, 16.7, 76.8, 19.8),
            c(74.4, 18.8, 71.0, 16.6, 77.4, 20.3))
bw <- as.data.frame(bw)
colnames(bw) <- c("O_mean", "O_sd", "CM_mean", "CM_sd", "CHO_mean", "CHO_sd")
rownames(bw) <- c("Pre", "Post")
bw

bw2 <- rbind(c(73.3, 18.5, 70.3, 16.7, 82.2, 17.2),
            c(74.4, 18.8, 71.0, 16.6, 77.4, 20.3))
bw2 <- as.data.frame(bw2)
colnames(bw2) <- c("O_mean", "O_sd", "CM_mean", "CM_sd", "CHO_mean", "CHO_sd")
rownames(bw2) <- c("Pre", "Post")
bw2

# Composite Strength Score
#       Overall      CM           CHO
# Pre   173.6±81.9   166.8±73.5   179.7±89.5
# Post  185.8±80.2   187.3±75.6   184.5±85.5
css <- bw
css[1,] <- c(173.6, 81.9, 166.8, 73.5, 179.7, 89.5)
css[2,] <- c(185.8, 80.2, 187.3, 75.6, 184.5, 85.5)
css


# Bench Press
#       Overall     CM          CHO
# Pre   73.1±32.5   71.8±31.8   74.5±33.7
# Post  73.3±31.2   74.3±29.4   72.2±33.5
bp <- bw
bp[1,] <- c(73.1, 32.5, 71.8, 31.8, 74.5, 33.7)
bp[2,] <- c(73.3, 31.2, 74.3, 29.4, 72.2, 33.5)
bp


# Squat
#       Overall      CM           CHO
# Pre   104.4±51.2   100.1±43.1   108.5±58.2
# Post  116.2±50.8   115.3±46.6   117.1±55.1
squat <- bw
squat[1,] <- c(104.4, 51.2, 100.1, 43.1, 108.5, 58.2)
squat[2,] <- c(116.2, 50.8, 115.3, 46.6, 117.1, 55.1)
squat


#.####
# BSDA-library####
library(BSDA)
?tsum.test
n1 <- 51
n2 <- 52
  
  
## Body Weight####
# Pre ~ Post: Overall: p-value = 0.6726
(bw.pp.all <- tsum.test(bw[1,]$O_mean, bw[1,]$O_sd, n1+n2,
          bw[2,]$O_mean, bw[2,]$O_sd, n1+n2, var.equal = TRUE))

# Pre ~ Post: CM: p-value = 0.8323
(bw.pp.CM <- tsum.test(bw[1,]$CM_mean, bw[1,]$CM_sd, n1,
          bw[2,]$CM_mean, bw[2,]$CM_sd, n1))

# Pre ~ Post: CHO: p-value = 0.879
(bw.pp.CHO <- tsum.test(bw[1,]$CHO_mean, bw[1,]$CHO_sd, n2,
          bw[2,]$CHO_mean, bw[2,]$CHO_sd, n2))

# Pre: CM ~ CHO: p-value = 0.07456
(bw.pre.CM.CHO <- tsum.test(bw[1,]$CM_mean, bw[1,]$CM_sd, n1,
          bw[1,]$CHO_mean, bw[1,]$CHO_sd, n2))
(bw.pre.CM.CHO <- tsum.test(bw2[1,]$CM_mean, bw2[1,]$CM_sd, n1,
                            bw2[1,]$CHO_mean, bw2[1,]$CHO_sd, 50+28))

# Post: CM ~ CHO: p-value = 0.08272
(bw.pre.CM.CHO <- tsum.test(bw[2,]$CM_mean, bw[2,]$CM_sd, n1,
          bw[2,]$CHO_mean, bw[2,]$CHO_sd, n2))


## Composite Strength Score####
# Pre ~ Post: Overall: p-value = 0.2813
(css.pp.all <- tsum.test(css[1,]$O_mean, css[1,]$O_sd, n1+n2,
                        css[2,]$O_mean, css[2,]$O_sd, n1+n2))

# Pre ~ Post: CM: p-value = 0.1681
(css.pp.CM <- tsum.test(css[1,]$CM_mean, css[1,]$CM_sd, n1,
                       css[2,]$CM_mean, css[2,]$CM_sd, n1))

# Pre ~ Post: CHO: p-value = 0.7803
(css.pp.CHO <- tsum.test(css[1,]$CHO_mean, css[1,]$CHO_sd, n2,
                         css[2,]$CHO_mean, css[2,]$CHO_sd, n2))

# Pre: CM ~ CHO: p-value = 0.4256
(css.pre.CM.CHO <- tsum.test(css[1,]$CM_mean, css[1,]$CM_sd, n1,
                            css[1,]$CHO_mean, css[1,]$CHO_sd, n2))

# Post: CM ~ CHO: p-value = 0.8605
(css.pre.CM.CHO <- tsum.test(css[2,]$CM_mean, css[2,]$CM_sd, n1,
                            css[2,]$CHO_mean, css[2,]$CHO_sd, n2))


## Bench Press####
# Pre ~ Post: Overall: p-value = 0.9641
(bp.pp.all <- tsum.test(bp[1,]$O_mean, bp[1,]$O_sd, n1+n2,
                         bp[2,]$O_mean, bp[2,]$O_sd, n1+n2))

# Pre ~ Post: CM: p-value = 0.681
(bp.pp.CM <- tsum.test(bp[1,]$CM_mean, bp[1,]$CM_sd, n1,
                        bp[2,]$CM_mean, bp[2,]$CM_sd, n1))

# Pre ~ Post: CHO: p-value = 0.7278
(bp.pp.CHO <- tsum.test(bp[1,]$CHO_mean, bp[1,]$CHO_sd, n2,
                         bp[2,]$CHO_mean, bp[2,]$CHO_sd, n2))

# Pre: CM ~ CHO: p-value = 0.6766
(bp.pre.CM.CHO <- tsum.test(bp[1,]$CM_mean, bp[1,]$CM_sd, n1,
                             bp[1,]$CHO_mean, bp[1,]$CHO_sd, n2))

# Post: CM ~ CHO: p-value = 0.7358
(bp.pre.CM.CHO <- tsum.test(bp[2,]$CM_mean, bp[2,]$CM_sd, n1,
                             bp[2,]$CHO_mean, bp[2,]$CHO_sd, n2))

          
## Squat####
# Pre ~ Post: Overall: p-value = 0.09837
(squat.pp.all <- tsum.test(squat[1,]$O_mean, squat[1,]$O_sd, n1+n2,
                           squat[2,]$O_mean, squat[2,]$O_sd, n1+n2))

# Pre ~ Post: CM: p-value = 0.09037
(squat.pp.CM <- tsum.test(squat[1,]$CM_mean, squat[1,]$CM_sd, n1,
                       squat[2,]$CM_mean, squat[2,]$CM_sd, n1))

# Pre ~ Post: CHO: p-value = 0.4409
(squat.pp.CHO <- tsum.test(squat[1,]$CHO_mean, squat[1,]$CHO_sd, n2,
                        squat[2,]$CHO_mean, squat[2,]$CHO_sd, n2))

# Pre: CM ~ CHO: p-value = 0.4067
(squat.pre.CM.CHO <- tsum.test(squat[1,]$CM_mean, squat[1,]$CM_sd, n1,
                            squat[1,]$CHO_mean, squat[1,]$CHO_sd, n2))

# Post: CM ~ CHO: p-value = 0.8582
(squat.pre.CM.CHO <- tsum.test(squat[2,]$CM_mean, squat[2,]$CM_sd, n1,
                            squat[2,]$CHO_mean, squat[2,]$CHO_sd, n2))



#.####
# t.test der Differenzen####
## Composite Strength Score####
# Ist die Differenz von CM signifikant grösser als die Differenz von CHO
CM.diff <- css[2,]$CM_mean - css[1, ]$CM_mean
CHO.diff <- 5.8


#.####
# Barplot####
library(ggplot2)
df2 <- data.frame(Aufteilung=rep(c("Studie", "Allgemein"), each=5),
                  Rasse=rep(c("Weiss", "Schwarz", "Hispanisch", "Asiatisch", "Andere"),2),
                  Prozent=c(17, 41, 27, 3, 12, 48, 15, 27, 5, 5))

p <- ggplot(data=df2, aes(x=Rasse, y=Prozent, fill=Aufteilung)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Prozent), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
p + labs(title="Aufteilung von Schülern nach Rasse/Ethnie")
