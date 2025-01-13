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

name <- "2024_Wudjari_bait_comp"


# read in habitat data
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")

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


# Set the predictors for modeling - don't include factors - just continuous var 
pred.vars <- c("depth_m", "Macroalgae", "Scytothalia", "Ecklonia", "Sargassum",
               "Canopy", "Sessile invertebrates", "Consolidated (hard)",
               "Unconsolidated (soft)", "reef")


# Check the correlations between predictor variables - looking for NAs
summary(maxn.all[,pred.vars])


#checking for correlations between variables
round(cor(maxn.all[ , pred.vars]), 2)

# loop to check if transformations are needed - preference is no transformations

folder_path <- "plots/baitcomp/"


for (i in pred.vars) {
  png(file.path(folder_path, paste0(i,".pred.vars.png")), width = 800, height = 600)
  par(mfrow = c(3, 2))
  x <- maxn.all[ , i]
  x = as.numeric(unlist(x))
  hist((x))
  plot((x), main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x + 1))
  plot(log(x + 1))
  dev.off()
}


# checking if my response has > 80% zeros, and if it is not worth modelling
unique.vars <- unique(as.character(maxn.all$species))


test = maxn.all%>%filter(is.na(species)) #should be zero

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- maxn.all[which(maxn.all$species == unique.vars[i]), ]
  if(length(which(temp.dat$maxn == 0)) / nrow(temp.dat) < 0.8){ # Change here
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars #should be 'gouldii'

## Add the directory to save model outputs, 
#and set up the R environment for model selection.

outdir <- ("/output/baitcomp/") 
out.all <- list()
var.imp <- list()


# Run the full subset model selection
#cyclic.vars = c("time") #circular variables that can be plotted on a clock-face 
#savedir <- "model out/GAMs" #folder path for the top models to be sent to in a .csv file
#use.dat <- as.data.frame(dat) 
factor.vars <- c("location") # enter the factor variables here
#out.all     <- list() 
#var.imp     <- list()


for(i in 1:length(resp.vars)){
  use.dat = as.data.frame(maxn.all[which(maxn.all$response == resp.vars[i]),])
  print(resp.vars[i])
  
  Model1  <- gam(maxn ~ bait,
                 family = gaussian(link = "identity"),  
                 data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  max.predictors = 4,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  #cyclic.vars = cyclic.vars,
                                  k = 5)
                                  #factor.smooth.interactions = F,
                                  #smooth.smooth.interactions = F, 
                                  #null.terms = "s(den, bs ='re')"
  
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
    png(file = here::here(paste(outdir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/")))
    if(best.model.name != "null"){
      par(mfrow = c(3,1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T,pages = 1,residuals = T,pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

###########################################################################
### Manually trying

gamm1 <- gamm(maxn ~ bait + s(reef, k = 3, bs = 'cr'),
              family = gaussian(link = "identity"), 
        data = maxn.all)
  
summary(gamm1$gam)


#############################################################################
####    old example from Masters code
# Loop through the FSS function for each Taxa---- 
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- as.data.frame(dat[which(dat$species == resp.vars[i]), ])
  Model1  <- gam(maxn ~ s(moonlightModel, k = 5, bs='cr') + s(den, bs ='re'), #random effects incl.
                 family = tw(),  data = use.dat)  
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  max.predictors = 4,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  cyclic.vars = cyclic.vars,
                                  k = 5,
                                  factor.smooth.interactions = F,
                                  smooth.smooth.interactions = F, 
                                  null.terms = "s(den, bs ='re')" #repeat R.E. here 
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i   <- mod.table[which(mod.table$delta.AICc <= 100), ] 
  out.all <- c(out.all,list(out.i))
  var.imp <- c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  
  for(m in 1:nrow(out.i)){
    best.model.name = as.character(out.i$modname[m])
    png(file = paste(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model,all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Save model fits and importance scores
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits   <- do.call("rbind",out.all)
all.var.imp    <- do.call("rbind",var.imp)

write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))


