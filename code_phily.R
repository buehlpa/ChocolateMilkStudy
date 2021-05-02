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
n <- 103

## Body Weight####
# Overall: p-value = 0.6726
tsum.test(bw[1,]$O_mean, bw[1,]$O_sd, n,
          bw[2,]$O_mean, bw[2,]$O_sd, n,
          var.equal = FALSE)
# CM: p-value = 0.7632
tsum.test(bw[1,]$CM_mean, bw[1,]$CM_sd, n,
          bw[2,]$CM_mean, bw[2,]$CM_sd, n,
          var.equal = FALSE)
# CHO: p-value = 0.8302
tsum.test(bw[1,]$CHO_mean, bw[1,]$CHO_sd, n,
          bw[2,]$CHO_mean, bw[2,]$CHO_sd, n,
          var.equal = FALSE)

## Composite Strength Score####
# Overall: p-value = 0.2813
tsum.test(css[1,]$O_mean, css[1,]$O_sd, n,
          css[2,]$O_mean, css[2,]$O_sd, n,
          var.equal = FALSE)

# CM: p-value = 0.04983
tsum.test(css[1,]$CM_mean, css[1,]$CM_sd, n,
          css[2,]$CM_mean, css[2,]$CM_sd, n,
          var.equal = FALSE)

# CHO: p-value = 0.6943
tsum.test(css[1,]$CHO_mean, css[1,]$CHO_sd, n,
          css[2,]$CHO_mean, css[2,]$CHO_sd, n,
          var.equal = FALSE)

## Bench Press####
# Overall: p-value = 0.9641
tsum.test(bp[1,]$O_mean, bp[1,]$O_sd, n,
          bp[2,]$O_mean, bp[2,]$O_sd, n,
          var.equal = FALSE)

# CM: p-value = 0.5586
tsum.test(bp[1,]$CM_mean, bp[1,]$CM_sd, n,
          bp[2,]$CM_mean, bp[2,]$CM_sd, n,
          var.equal = FALSE)

# CHO: p-value = 0.6238
tsum.test(bp[1,]$CHO_mean, bp[1,]$CHO_sd, n,
          bp[2,]$CHO_mean, bp[2,]$CHO_sd, n,
          var.equal = FALSE)
          
## Squat####
# Overall: p-value = 0.09837
tsum.test(squat[1,]$O_mean, squat[1,]$O_sd, n,
          squat[2,]$O_mean, squat[2,]$O_sd, n,
          var.equal = FALSE)

# CM: p-value = 0.01596
tsum.test(squat[1,]$CM_mean, squat[1,]$CM_sd, n,
          squat[2,]$CM_mean, squat[2,]$CM_sd, n,
          var.equal = FALSE)

# CHO: p-value = 0.2774
tsum.test(squat[1,]$CHO_mean, squat[1,]$CHO_sd, n,
          squat[2,]$CHO_mean, squat[2,]$CHO_sd, n,
          var.equal = FALSE)
