###########################################
## TIME TO MAXN -- Maxn.all DF

##TODO - tidy

omit.zeros <- maxn.all %>% #removing drops with no bg
  filter(!is.na(titomaxn_s))%>%
  glimpse()

which(is.na(omit.zeros$titomaxn_s)) #no zeros

#visualsing to select family
# histogram
ggplot(omit.zeros, aes(x = titomaxn_s)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Time to MaxN (seconds)",
       x = "Time (seconds)", y = "Count")

#density plot
ggplot(omit.zeros, aes(x = titomaxn_s)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Time to MaxN",
       x = "Time (seconds)", y = "Density")

#by bait type

ggplot(omit.zeros, aes(x = bait, y = titomaxn_s)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Time to MaxN by Bait Type",
       x = "Bait", y = "Time (seconds)")

##modelling
#library(glmmTMB) because glmer() in lme4 package doesn't support
#gamma distribution

model <- glmmTMB(titomaxn_s ~ bait + (1 | location),
                 data = omit.zeros,
                 family = Gamma(link = "log"))

summary(model)
Anova(model)

# Post hoc test for 'bait' variable
posthoc_results <- emmeans(model, ~ bait)

# Summary of the post hoc results
summary(posthoc_results)

# Pairwise comparisons between levels of 'bait'
pairwise_comparisons <- contrast(posthoc_results, method = "pairwise")
summary(pairwise_comparisons)


# Function to calculate overdispersion
check_overdispersion <- function(model) {
  rp <- residuals(model, type = "pearson")
  rdf <- df.residual(model)
  ratio <- sum(rp^2) / rdf
  cat("Dispersion ratio:", round(ratio, 2), "\n")
  if (ratio > 1.2) {
    cat("Potential overdispersion detected.\n")
  } else {
    cat("No strong evidence of overdispersion.\n")
  }
}

check_overdispersion(model) #1.06 so tiny bit overdispersed but not enough
#to affect the model results

#residuals vs fixed

res_df <- data.frame(
  fitted = fitted(model),
  resid_pearson = residuals(model, type = "pearson")
)

ggplot(res_df, aes(x = fitted, y = resid_pearson)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Pearson Residuals vs Fitted Values",
       x = "Fitted values", y = "Pearson residuals")

#QQ plot of residuals
res_df$resid_scaled <- scale(res_df$resid_pearson)

ggplot(res_df, aes(sample = resid_scaled)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "QQ Plot of Pearson Residuals")

# more robust testing with dharma
library(DHARMa)

simres <- simulateResiduals(fittedModel = model, plot = TRUE)

testZeroInflation(simres) #test zero inflation - 
#indicating that I don't need to worry about
#zero inflation

plotResiduals(simres, form = omit.zeros$bait)

summary(model)$varcor #checking variance of random effect -- very low so doesn't
#contribute much

#install.packages("sjPlot")
library(sjPlot)
plot_model(model, type = "re", sort.est = TRUE)

#seeing if zero inflation better
model_zi <- glmmTMB(
  titomaxn_s ~ bait + (1 | location),
  data = omit.zeros,
  ziformula = ~1,  # Adds a zero-inflation component
  family = Gamma(link = "log")
)

AIC(model, model_zi)
anova(model, model_zi)

## forward stepwise
model2 <- glmmTMB(titomaxn_s ~ bait + depth_m + (1 | location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))

summary(model2)
Anova(model2)
AIC(model, model2)

summary(model)

model3 <- glmmTMB(titomaxn_s ~ bait + mean_relief + (1 | location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))

AIC(model, model3)

model4 <- glmmTMB(titomaxn_s ~ bait + scytothalia + (1|location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))

AIC(model, model4)
summary(model4)

model5 <- glmmTMB(titomaxn_s ~ bait + ecklonia + (1|location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))

AIC(model, model5)

model6 <- glmmTMB(titomaxn_s ~ bait + macroalgae + (1|location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))
AIC(model, model6)

model7 <- glmmTMB(titomaxn_s ~ bait + date + (1|location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))
AIC(model, model7)

model.nore <- glmmTMB(titomaxn_s ~ bait + location,
                      data = omit.zeros,
                      family = Gamma(link = "log"))

AIC(model, model.nore)

## sussing Time of day -- move up when done

# Convert to POSIXct time (using any dummy date, since you only care about time)
omit.zeros$time_of_day <- as.POSIXct(omit.zeros$time, format = "%H:%M:%S")

# Convert to seconds since midnight

omit.zeros$time_sec <- as.numeric(format(omit.zeros$time_of_day, "%H")) * 3600 +
  as.numeric(format(omit.zeros$time_of_day, "%M")) * 60 +
  as.numeric(format(omit.zeros$time_of_day, "%S"))


omit.zeros$time_hr <- omit.zeros$time_sec / 3600

model_with_time <- glmmTMB(titomaxn_s ~ bait + time_hr + (1 | location),
                           data = omit.zeros,
                           family = Gamma(link = "log"))

summary(model_with_time)
AIC(model, model_with_time)

#with time as factor
omit.zeros$time_block <- cut(omit.zeros$time_hr,
                             breaks = c(0, 6, 12, 18, 24),
                             labels = c("Night", "Morning", "Afternoon", "Evening"),
                             right = FALSE)

model_block <- glmmTMB(titomaxn_s ~ bait + time_block + (1 | location),
                       data = omit.zeros,
                       family = Gamma(link = "log"))


AIC(model, model_block)
# ###########################################
# ## Likelihood ratio tests on best model
# 
# lrt1.both <- glmer(maxn ~ bait + Scytothalia + (1|location) + (1|site),
#                       data = maxn.all,
#                       family = "poisson")
# lrt2.loc <- glmer(maxn ~ bait + Scytothalia + (1|location),
#                   data = maxn.all,
#                   family = "poisson")
# 
# lrt3.site <- glmer(maxn ~ bait + Scytothalia + (1|site),
#                    data = maxn.all,
#                    family = "poisson")
# 
# lrt4.nest <- glmer(maxn ~ bait + Scytothalia + (site|location),
#                    data = maxn.all,
#                    family = "poisson")
# 
# anova(lrt2.loc, lrt1.both)
# 
# anova(lrt3.site, lrt1.both)
# 
# anova(lrt2.loc, lrt3.site)
# 
# anova(lrt3.site, lrt4.nest)
# 
