
rm(list=ls()) # Clear memory

# install.packages('remotes')
library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
library(mgcv)
library(devtools)
library(FSSgam)
library(here)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(terra)
library(sf)
library(patchwork)

# set study name

name <- "2024_Wudjari_bait_comp"
# read in habitat data
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  glimpse()


## MaxN by STAGE

sum.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.sum.stage.rds")%>%
  dplyr::mutate(bait = as.factor(bait), location = as.factor(location), 
                site = as.factor(site))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m), 
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  left_join(habitat)%>%
  dplyr::filter(opcode != "046")%>% #no habitat data for 046
  dplyr::mutate(presence = ifelse(maxn_sum > 0, 1, 0))%>%
  clean_names()%>%
  glimpse()



# Set the predictors for modeling - don't include factors - just continuous var 
pred.vars <- c("depth_m", "macroalgae", "scytothalia", "ecklonia",
               "sessile_inverts", "mean_relief", "time_hr")


# Check the correlations between predictor variables - looking for NAs
summary(sum.stage[,pred.vars])


#checking for correlations between variables
round(cor(sum.stage[ , pred.vars]), 2)

# check individual predictors to see if any need transformed
CheckEM::plot_transformations(pred.vars = pred.vars, dat = sum.stage)

## code to export
outdir <- "output/baitcomp/sum.stage"
for (var in pred.vars) {
  png(filename = file.path(outdir, paste0(var, ".png")), width = 800, height = 600, res = 150)
  CheckEM::plot_transformations(pred.vars = var, dat = sum.stage)
  dev.off()
}


# Reset the predictor variables to remove any highly correlated variables 
# and include any transformed variables.
# removed sessile inverts & scytothalia as looked fairly zero inflated
pred.vars <- c("depth_m", "macroalgae", "ecklonia",
               "mean_relief", "time_hr") 


#Check to make sure response variables have less than 80% zeroes. 
#Full-subset GAM modelling will produce unreliable results if your data is too zero inflated.

unique.vars <- unique(as.character(sum.stage$species))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- sum.stage[which(sum.stage$species == unique.vars[i]), ]
  if(length(which(temp.dat$maxn_sum == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars  
##############################################################################
##add directory to save model outputs & set up environment for model selection
### Re-run from here down everytime

outdir <- ("output/baitcomp/sum.stage") 
out.all <- list()
var.imp <- list()

#specify cyclic or factor co-variates
cyclic.vars = c("time_hr") #circular variables that can be plotted on a clock-face
factor.vars <- c("bait") #don't include the RE factor

## Running Full Sub-set Gamms
for(i in 1:length(resp.vars)){
  use.dat = as.data.frame(sum.stage[which(sum.stage$species == resp.vars[i]),])
  print(resp.vars[i])
  
  Model1  <- gam(maxn_sum ~ s(ecklonia, k = 5, bs = 'cr') +
                   s(location, site, bs = 're'), #random effect
                 family = tw(),  data = use.dat) #check family
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars, 
                                  factor.smooth.interactions = NA,
                                  cyclic.vars = cyclic.vars,
                                  null.terms = "s(location, site, bs ='re')", #repeat R.E. here -- check
                                  k = 5)
  
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models 
  mod.table = out.list$mod.data.out 
  mod.table = mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi = cumsum(mod.table$wi.AICc)
  out.i = mod.table[which(mod.table$delta.AICc <= 20),]
  out.all = c(out.all,list(out.i))
  var.imp = c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw))
  
  for(m in 1:nrow(out.i)){
    best.model.name = as.character(out.i$modname[m])
    #png(file = here::here(paste(outdir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/")))
    png(filename = paste(outdir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name != "null"){
      par(mfrow = c(3,1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T,pages = 1,residuals = T,pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

#Tidy the model fits and importance scores.

names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- list_rbind(out.all, names_to = "species")
all.var.imp  <- as.data.frame(do.call("rbind", var.imp))


write.csv(all.mod.fits[ , -2], file = paste(outdir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(outdir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))


###############################################################################
###############################################################################
# 
# gamm1 <- gam(maxn ~ s(depth_m, k = 3, bs = 'cr') + s(Ecklonia, k = 3, bs = 'cr')+  
#                s(location, bs ='re'),#location as random effect
#              family = gaussian(link = "identity"),  data = use.dat)
# summary(gamm1)



### plottig importance scores
print(all.var.imp)

# Convert wide to long format
importance_long <- all.var.imp %>%
  pivot_longer(
    cols = everything(),        # Specify all columns to pivot
    names_to = "Feature",       # New column for feature names
    values_to = "Importance"    # New column for importance values
  )

print(importance_long)

# Heatmap for one model

ggplot(importance_long, aes(x = Feature, y = 1, fill = Importance)) +
  geom_tile() +  # Create the tile
  scale_fill_gradient(low = "white", high = "red") +  # Color gradient
  labs(
    title = "Importance Scores ",
    x = "Feature",
    y = NULL,
    fill = "Importance"
  ) +
  theme_cowplot() +
  theme(axis.text.y = element_blank(),  # Hide y-axis text (since it's just one row)
        axis.ticks.y = element_blank(),  # Hide y-axis ticks
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

