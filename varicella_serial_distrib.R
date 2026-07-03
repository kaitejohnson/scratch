# individual study sample size
n <- c(43,184)
N <- sum(n)
# serial interval point estimate
mu <- c(13.1,14.1)#14.2)
# standard deviaiton of point estimate
sd <- c(2.2,2.4)#1.3)

# convert values of each study to to shape and rate
shape <- (mu/sd)^2
rate <- mu/(sd^2)

# checking calculation for shape and rate correct
Gamma(mean = 13.1, sd = 2.2)
Gamma(mean = 14.1, sd = 2.4)
Gamma(mean = 14.2, sd = 1.3)

# Just check that the conversion from mean and sd to shape and rate still 
# produces the same mean when you sample from the Gamma
samples_from_gamma<- rgamma(100, shape = mean(shape), rate = mean(rate))
mean(samples_from_gamma) # 14.2
sd(samples_from_gamma) #1.7 

hist(rgamma(100, shape = shape, rate = rate))
# Visibly centered around 14

##mean and SD of shapes and rates

#weighted mean of shapes
w_mean_shape <- weighted.mean(shape, n)

library(radiant.data)
#weighted sd of shapes
w_sd_shape <- weighted.sd(shape, n)

#weighted mean of rate
w_mean_rate <- weighted.mean(rate, n)
#weighted sd of rate
w_sd_rate <- weighted.sd(rate, n)


# probably where I'm going wrong!
cpox_uncertain_gamma_si_distr <- Gamma(shape = Normal(w_mean_shape, w_sd_shape), rate = Normal(w_mean_rate, w_sd_rate), 
                                       max = 21)
plot(cpox_uncertain_gamma_si_distr)

#Attempting to plot with shapes = mean of shapes and rate = mean of rates - works but no uncertainty
cpox_uncertain_gamma_si_distr <- Gamma(shape = w_mean_shape, rate = w_mean_rate, max = 21)

samp1<- rgamma(1000, shape = w_mean_shape, rate = w_mean_rate)
mean(samp1)
sd(samp1)
#mean 13.96 slightly below 14.1 in paper

plot(cpox_uncertain_gamma_si_distr)