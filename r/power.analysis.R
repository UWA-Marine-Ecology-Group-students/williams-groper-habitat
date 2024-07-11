###power analysis bait comp

#install.packages("pwr")
library(pwr)

#### EXAMPLES FROM CHATGPT

#   example for 2 sample t-test

effect_size <- 0.5 #Cohen's d??? - from chatgpt not sure what this is
alpha <- 0.5
power <- 0.8 #usually set to give 80% chance of detecting true difference

##calculating required sample size
pwr.t.test(d=effect_size, sig.level = alpha, power = power)

#   Example for one-way ANOVA

effect_size <- 0.5 
alpha <- 0.5
power <- 0.8
groups <- 3 #no. groups

pwr.anova.test(k=groups, f=effect_size, sig.level = alpha, power = power)

#   example for regression 
effect_size <- 1.5 # must be at least 1
alpha <- 0.5
power <- 0.8
predictors <- 3

pwr.f2.test(u = effect_size, v= predictors, sig.level = alpha, power = power) 

# example for 2 factor anova with interaction

A <- 0.1 #partial eta-sq for factor A
B <- 0.2 #partial eta-sq for Factor B
int <- 0.05 #partial eta-sq for interaction 

alpha <- 0.05
power <- 0.8

pwr.2way(a = 2, b = 3, eta2.A = A, eta2.B = B, eta2.AB = int, sig.level = alpha,
         power = power)
## a and b are number of levels for each factor

#   example 2 factor WITHOUT interaction

A <- 0.1 #partial eta-sq for factor A
B <- 0.2 #partial eta-sq for Factor B

alpha <- 0.05
power <- 0.8

#pwr.2way(a = 2, b = 3, eta2.A = A, eta2.B = B,  sig.level = alpha,
        # power = power) -- doesn't actually exist

########################################################

#   Bait comp 1 factor ANOVA

effect_size <- 0.5 # doesn't like effect size 1 or > 1
alpha <- 0.5
power <- 0.8
groups <- 3 #no. groups

pwr.anova.test(k= groups, f= effect_size, sig.level = alpha, power = power)

#effect size of 0.5 = 3.75 drops per group


