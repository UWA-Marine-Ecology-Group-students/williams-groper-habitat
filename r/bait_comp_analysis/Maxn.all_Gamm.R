###########################
## Exploratory GAMMS
##########################

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
#library(corrr)

# set study name

#name <- "2024_Wudjari_bait_comp"
name <- "bc"

# read in habitat data
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  dplyr::rename(inverts = "Sessile invertebrates", rock = "Consolidated (hard)", sand = "Unconsolidated (soft)")%>%
  glimpse()

### MaxN (all) by period 

maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  dplyr::group_by(opcode)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>% # sliced the highest maxN by opcode 
  dplyr::ungroup()%>%
  left_join(habitat)%>% #joining to habitat 
  glimpse()

##### TODO START FROM HEREEEEEEEEEEEEEEEE
# take from ChecKEM workflow - 



# Set the predictors for modeling - don't include factors - just continuous var 
pred.vars <- c("depth_m", "Macroalgae", "Scytothalia", "Ecklonia", "Sargassum",
               "Canopy", "inverts", "rock",
               "sand", "reef", "mean.relief")


# Check the correlations between predictor variables - looking for NAs
summary(maxn.all[,pred.vars])


#checking for correlations between variables
round(cor(maxn.all[ , pred.vars]), 2)

# check individual predictors to see if any need transformed
CheckEM::plot_transformations(pred.vars = pred.vars, dat = maxn.all)

#Reset the predictor variables to remove any highly correlated variables and include any transformed variables.
pred.vars <- c("depth_m", "Macroalgae", "Scytothalia", "Ecklonia", 
                "reef", "mean.relief") 


#Check to make sure response variables have less than 80% zeroes. 
#Full-subset GAM modelling will produce unreliable results if your data is too zero inflated.

unique.vars <- unique(as.character(maxn.all$species))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- maxn.all[which(maxn.all$species == unique.vars[i]), ]
  if(length(which(temp.dat$maxn == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars  
##############################################################################
##add directory to save model outputs & set up environment for model selection
### Re-run from here down everytime

outdir <- ("output/baitcomp") 
out.all <- list()
var.imp <- list()

#specify cyclic or factor co-variates
#cyclic.vars = c("time") #circular variables that can be plotted on a clock-face
factor.vars <- c("bait", "location") #don't include the RE factor

## Running Full Sub-set Gamms
for(i in 1:length(resp.vars)){
  use.dat = as.data.frame(maxn.all[which(maxn.all$species == resp.vars[i]),])
  print(resp.vars[i])
  
  Model1  <- gam(maxn ~ bait + s(mean.relief, k = 3, bs = 'cr'), #location as random effect
                 family = gaussian(link = "identity"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars, 
                                  factor.smooth.interactions = NA,
                                  #cyclic.vars = "aspect",
                                  #null.terms = "s(location, bs ='re')", #repeat R.E. here -- check
                                  k = 3)
  
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models 
  mod.table = out.list$mod.data.out 
  mod.table = mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi = cumsum(mod.table$wi.AICc)
  out.i = mod.table[which(mod.table$delta.AICc <= 2),]
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

###############################################################################
###############################################################################
# 
# gamm1 <- gam(maxn ~ s(depth_m, k = 3, bs = 'cr') + s(Ecklonia, k = 3, bs = 'cr')+  
#                s(location, bs ='re'),#location as random effect
#              family = gaussian(link = "identity"),  data = use.dat)
# summary(gamm1)

