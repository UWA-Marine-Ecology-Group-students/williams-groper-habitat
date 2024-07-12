###power analysis bait comp

#install.packages("pwr")
#install.packages("pwr2")
library(pwr)
library(pwr2)

#####  ONE WAY ANOVA 

#n = sample size per level
#k = no. levels in factor
#f = effect size (cohen's f) want to detect
#sig.level = 0.05
#power = usually 0.8

## cohen's f - the mean difference between groups relative to the variability
## within groups

# small effect, f = 0.1
# medium effect, f = 0.25
# large effect, f = 0.4

pwr.anova.test(k = 3, f = 0.25, sig.level = 0.05, power = 0.8)
# n = 52.4 for each group (150 drops)

pwr.anova.test(k =3, f = 0.1, sig.level = 0.05, power = 0.8)
#n = 322 - 600 drops

pwr.anova.test(k = 3, f = 0.4, sig.level = 0.05, power = 0.8)
# n = 21 = 65 drops

## the interwebs say to use f = 0.5 because it indicates a moderate
## to large difference

pwr.anova.test(k=3, f=0.5, sig.level = 0.05, power = 0.8)
#n = 13.89 giving 42 drops (or 45 to round up)



### two way anova

#a = no. groups in factor a
#b = no. groups in factor b
#alpha = sig level
#size.A = sample size per group in factor A
#size.B = sample size per group in factor B
#f.A = effect size of Factor A
#f.B = effect size of Factor B
#delta.A = the smallest difference among groups in A
#delta.B = the smallest difference among groups in B
#sigma.A = s.d 
#sigma.B = s.d.
#if effect sizes of A and B are known then put them into the function
#if delta sizes are known, put f.A & f.B as NULL and put in dleta instead
#same with sigma

#effect size between the factors?
pwr.2way(a=3, b=4, alpha=0.05, size.A=21, size.B=21, f.A=0.5, f.B=0.5)
## power= 0.99999

pwr.2way(a=3, b=4, alpha=0.05, size.A=15, size.B=15, f.A=0.5, f.B=0.5)
#power = 0.999976

pwr.2way(a=3, b=4, alpha=0.05, size.A=12, size.B=12, f.A=0.5, f.B=0.5)
#power = 0.9996138

## delta - smallest difference among groups in a factor

pwr.2way(a=3, b=4, alpha=0.05, size.A=12, size.B=12, delta.A=2, delta.B=2, sigma.A=2, sigma.B=2)


## calculate sample size for two-way ANOVA models
# parameters as above
# beta = type 2 error probability
# B= iterations

ss.2way(a=3, b=4, alpha = 0.05, beta = 0.1, delta.A=1, delta.B=2, sigma.A=2,
        sigma.B=2, B=100)
#power = 0.9
#n = 26 in each group

ss.2way(a=3, b=4, alpha = 0.05, beta = 0.1, delta.A=2, delta.B=2, sigma.A=2,
        sigma.B=2, B=100)
#power = 0.9
#n = 10 in each group

ss.2way(a=3, b=4, alpha = 0.05, beta = 0.2, delta.A=2, delta.B=2, sigma.A=2,
        sigma.B=2, B=100)


## calculate sample size for one-way ANOVA models

#k - bait type levels
ss.1way(k=3, alpha=0.05, beta=0.1, f=0.5, delta=2, sigma=2, B=100)
#n = 18

#k - deployment times levels
ss.1way(k=4, alpha=0.05, beta=0.1, f=0.5, delta=2, sigma=2, B=100)
#n = 16

## power plot - one way ANOVA models

n <- seq(2, 30, by=4)
f<- 0.5
pwr.plot(n=n, k=4, f=f, alpha = 0.05)

############### Power analysis 2 factor ANOVA with interaction

#https://daniellakens.blogspot.com/2020/03/effect-sizes-and-power-for-interactions.html
#install.packages("Superpower")
library(Superpower)