############################################# 
###########################################
## TIME TO MAXN

omit.zeros <- maxn.stage %>% #removing rows with no bg
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
anova(model, model_zi) #zeroinflation mdoel not necessary

model_op <- glmmTMB(titomaxn_s ~ bait +  (1|opcode),
                    data = omit.zeros,
                    family = Gamma(link = "log"))
summary(model_op)
anova(model, model_op)
AIC(model_op)

model_nest <- glmmTMB(titomaxn_s ~ bait +  (1|location/opcode),
                      data = omit.zeros,
                      family = Gamma(link = "log"))
summary(model_nest)

model2 <- glmmTMB(titomaxn_s ~ bait + depth_m +  (1|opcode),
                  data = omit.zeros,
                  family = Gamma(link = "log"))
summary(model2)

model3 <- glmmTMB(titomaxn_s ~ bait + stage + depth_m +  (1|opcode),
                  data = omit.zeros,
                  family = Gamma(link = "log"))
summary(model3)
Anova(model3)
AIC(model3)

# Post hoc test for 'bait' variable
posthoc_results <- emmeans(model_op, ~ bait)

# Summary of the post hoc results
summary(posthoc_results)

# Pairwise comparisons between levels of 'bait'
pairwise_comparisons <- contrast(posthoc_results, method = "pairwise")
summary(pairwise_comparisons)


# Function to calculate overdispersion
check_overdispersion <- function(model) {
  rp <- residuals(model_op, type = "pearson")
  rdf <- df.residual(model_op)
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
  fitted = fitted(model_op),
  resid_pearson = residuals(model_op, type = "pearson")
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
#library(DHARMa)

simres <- simulateResiduals(fittedModel = model_op, plot = TRUE)

testZeroInflation(simres) #test zero inflation - 
#indicating that I don't need to worry about
#zero inflation

#plotResiduals(simres, form = omit.zeros$bait)

summary(model_op)$varcor #checking variance of random effect -- very low so doesn't
#contribute much

mod_stage <- glmmTMB(titomaxn_s ~ bait + stage +  (1|opcode),
                     data = omit.zeros,
                     family = Gamma(link = "log"))
summary(mod_stage)

mod_eck <- glmmTMB(titomaxn_s ~ bait + ecklonia +  (1|opcode),
                   data = omit.zeros,
                   family = Gamma(link = "log"))
summary(mod_eck)

mod_scy <- glmmTMB(titomaxn_s ~ bait + scytothalia +  (1|opcode),
                   data = omit.zeros,
                   family = Gamma(link = "log"))
summary(mod_scy)

mod_date <- glmmTMB(titomaxn_s ~ bait + date +  (1|opcode),
                    data = omit.zeros,
                    family = Gamma(link = "log"))

summary(mod_date)

mod_date_nested <- glmmTMB(titomaxn_s ~ bait  +  (1|date/opcode),
                           data = omit.zeros,
                           family = Gamma(link = "log"))
summary(mod_date_nested)

