############### Power analysis 2 factor ANOVA with interaction
rm(list=ls())
#https://daniellakens.blogspot.com/2020/03/effect-sizes-and-power-for-interactions.html
#install.packages("Superpower")
#install.packages("patchwork")
library(Superpower)
library(ggplot2)
library(patchwork)


#disordinal interaction where one factor affects the other like a cross

design <- ANOVA_design(
  design = "2b*2b", 
  n = 50, 
  mu = c(1, 0, 0, 1), 
  sd = 2)

ANOVA_exact(design, alpha_level = 0.03)

#our mean difference is either 1, or a Cohen’s d of 0.5 (in which case we have 61.78% power)

#(https://designingexperiments.com/)

#1) In power analyses for ANOVA designs, you should always think of the predicted pattern of means. 
#Different patterns of means will have the same effect size, 
#and your intuition can not be relied on when predicting an effect size for ANOVA designs.
#2) Understanding how patterns of means relate to the effect you predict is essential to design an informative study.
#3) Always perform a power analysis if you want to test a predicted interaction effect, 
# and always calculate the effect size based on means, sd's, and correlations, 
# instead of plugging in a 'medium' partial eta squared. 
#4) Crossover interaction effects have large effects and can thus be studies 
# with high power in smaller samples, 
# and if your theory can predict crossover interactions, 
# such experiments might be worthwhile to design.
#5) There are some additional benefits of examining interactions 
# (risky predictions, generalizability, efficiently examining multiple main effects)
# and it would be a shame if the field is turned away from examining interactions
# because they sometimes require large samples.

# Getting started: Comparing two groups

#We are planning a two independent group experiment. 
#We are using a validated measure, and we know the standard deviation of our measure is approximately 2. Psychologists are generaly horribly bad at knowing the standard deviation of their measures, even though a very defensible position is that you are not ready to perform a power analysis without solid knowledge of the standard deviation of your measure. We are interested in observing a mean difference of 1 or more, because smaller effects would not be practically meaningful. We expect the mean in the control condition to be 0, and therefore want the mean in the intervention group to be 1 or higher.

#This means the standardized effect size is the mean difference, divided by the standard deviation, or 1/2 = 0.5. This is the Cohen's d we want to be able to detect in our study:

#Although calculating effect sizes by hand is obviously an incredibly enjoyable thing to do, 
#you might prefer using software that performs these calculations for you. 
#Here, I will use our Superpower power analysis package (developed by Aaron Caldwell and me). 
#The code below uses a function from the package that computes power analytically
#for a one-way ANOVA where all conditions are manipulated between participants. 
#In addition to the effect size, the function will compute power for any sample size per condition you enter. 
#Let's assume you have a friend who told you that they heard from someone else 
#that you now need to use 50 observations in each condition (n = 50), 
#so you plan to follow this trustworthy advice. 
#We see the code below returns a Cohen's *f* of 0.25, 
#and also tells us we would have 61.78% power if we use a preregistered alpha level of 0.03.

library(Superpower)
design <- ANOVA_design(
  design = "2b", 
  n = 50, 
  mu = c(1, 0), 
  sd = 2)
power_oneway_between(design, alpha_level = 0.03)$Cohen_f
power_oneway_between(design, alpha_level = 0.03)$power

#We therefore might want to increase our sample size for our planned study. 
#Using the `plot_power` function, we can see we would pass 90% power with 100 observations per condition.

plot_power(design, alpha_level = 0.03, min_n = 45, max_n = 150)$plot_ANOVA


# Calculating effect sizes for interactions

df <- data.frame(
  A = factor(c("A1","A1","A2","A2")),
  B = factor(c("B1","B2","B1","B2")),
  Y = c(1, 0.0, 0.1, 1)
)

p1 <- ggplot(data=df, aes(x=A, y=Y, group=B, shape=B)) +
  geom_line(linewidth = 2) +
  geom_point(linewidth = 4, fill = "white") +
  scale_shape_manual(values=c(22,21)) +
  ggtitle("disordinal interaction") +
  theme_bw()

df <- data.frame(
  A = factor(c("A1","A1","A2","A2")),
  B = factor(c("B1","B2","B1","B2")),
  Y = c(1, 0, 0.1, 0)
)
p2 <- ggplot(data=df, aes(x=A, y=Y, group=B, shape=B)) +
  geom_line(size = 2) +
  geom_point(size = 4, fill = "white") +
  scale_shape_manual(values=c(22,21)) +
  ggtitle("ordinal interaction") +
  theme_bw()

# Use patchwork to combine and plot only 1 legend without title.
combined <- p1 + p2 & theme(legend.position = "bottom", 
                            legend.title = element_blank())
combined + plot_layout(guides = "collect")

#Mathematically the interaction effect is computed as the cell mean minus the sum of the grand mean, 
#the marginal mean in each condition of one factor minus the grand mean, 
#and the marginal mean in each condition for the other factor minus grand mean (see Maxwell et al., 2017).

#Let's consider two cases comparable to the figure above, 
#one where we have a perfect disordinal interaction 
#(the means of 0 and 1 flip around in the other condition, 
#and are 1 and 0) or an ordinal interaction (the effect is present in one condition, 
#with means of 0 and 1, but there is no effect in the other condition, 
#and both means are 0). We can calcuate the interaction effect as follows. 
#First, let's look at the interaction in a 2x2 matrix:

design <- ANOVA_design(
  design = "2b*2b", 
  n = 50, 
  mu = c(1, 0, 0, 1), 
  sd = 2)

power_twoway_between(design, alpha_level = 0.03)$mean_mat

#The grand mean is (1 + 0 + 0 + 1) / 4 = 0.5.

#We can compute the marginal means for A1, A2, B1, and B2, 
#which is simply averaging per row and column, 
#which gets us for the A1 column (1+0)/2=0.5. 
#For this perfect disordinal interaction, all marginal means are 0.5. 
#This means there are no main effects. 
#There is no main effect of factor A (because the marginal means for A1 and A2 are both exactly 0.5), 
#nor is there a main effect of B.

#We can also calculate the interaction effect. 
#For each cell we take the value in the cell (e.g., for a1b1 this is 1) 
#and compute the difference between the cell mean and the additive effect of the two factors as:

#1 - (the grand mean of 0.5 + (the marginal mean of a1 minus the grand mean, or 0.5 - 0.5 = 0)
#  + (the marginal mean of b1 minus the grand mean, or 0.5 - 0.5 = 0)). 
# Thus, for each cell we get:

#a1b1: 1 - (0.5 + (0.5 -0.5) + (0.5 -0.5)) = 0.5

#a1b2: 0 - (0.5 + (0.5 -0.5) + (0.5 -0.5)) = -0.5

#a2b1: 0 - (0.5 + (0.5 -0.5) + (0.5 -0.5)) = -0.5

#a2b2: 1 - (0.5 + (0.5 -0.5) + (0.5 -0.5)) = 0.5

#Cohen's $f$ is then $f = \frac { \sqrt { \frac { 0.5^2 +-0.5^2 + -0.5^2 + 0.5^2 } { 4 } }}{ 2 } = 0.25$

#  or in R code: `sqrt(((0.5)^2 +(-0.5)^2 + (-0.5)^2 + (0.5)^2)/4)/2 = 0.25`.

#For the ordinal interaction the grand mean is (1 + 0 + 0 + 0) / 4, or 0.25.
#The marginal means are a1: 0.5, a2: 0, b1: 0.5, and b2: 0.

#Completing the calculation for all four cells for the ordinal interaction gives:

#  a1b1: 1 - (0.25 + (0.5 -0.25) + (0.5 -0.25)) = 0.25

#a1b2: 0 - (0.25 + (0.5 -0.25) + (0.0 -0.25)) = -0.25

#a2b1: 0 - (0.25 + (0.0 -0.25) + (0.5 -0.25)) = -0.25

#a2b2: 0 - (0.25 + (0.0 -0.25) + (0.0 -0.25)) = 0.25

#Cohen's $f$ is then $f = \frac { \sqrt { \frac { 0.25^2 +-0.25^2 + -0.25^2 + 0.25^2 } { 4 } }}{ 2 } = 0.125$.

#or in R code: `sqrt(((0.25)^2 +(-0.25)^2 + (-0.25)^2 + (0.25)^2)/4)/2 = 0.125`.


#We see the effect size of the cross-over interaction (*f* = 0.25) 
#is twice as large as the effect size of the ordinal interaction (*f* = 0.125). 

#If the math so far was a bit too much to follow, 
#there is an easier way to think of why the effect sizes are halved. 
#In the disordinal interaction we are comparing cells a1b1 and a2b2 against a1b2 and a2b1, 
#or (1+1)/2 vs. (0+0)/2. 
#Thus, if we see this as a *t*-test for a contrast,
#it is clear the mean difference is 1, as it was in the simple effect we started with. 
#For the ordinal interaction, we have (1+0)/2 vs. (0+0)/2, 
#so the mean difference is halved, namely 0.5. 

# Power for interactions

design <- ANOVA_design(
  design = "2b*2b", 
  n = 50, 
  mu = c(1, 0, 0, 1), 
  sd = 2)
ANOVA_exact(design, alpha_level = 0.03)

#First let's look at the Power and Effect size for the pairwise comparisons. 
#Not surprisingly, these are just the same as our original t-test,
#given that we have 50 observations per condition, 
#and our mean difference is either 1, or a Cohen's d of 0.5 (in which case we have 61.78% power) 
#or the mean difference is 0, and we have no power 
#(because there is no true effect) but we wil observe significant results 3% of the time because we set our alpha level to 0.03.

Then, let's look at the results for the ANOVA. Since there are no main effects in a perfect crossover interaction, we have a 3% Type 1 error rate. We see the power for the crossover interaction between factor a and b is 91.06%. This is much larger than the power for the simple effects. The reason is that the contrast that is equal to the test of the interaction is based on all 200 observations. Unlike the pairwise comparisons with 50 vs 50 observations, the contrast for the interaction has 100 vs 100 observations. Given that the effect size is the same (*f* = 0.25) we end up with much higher power. 
