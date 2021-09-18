# Factives paper
# 5-projectivity-no-fact (certainty ratings, continuous task)
# mixture.R

# for inspiration, used tutorial from, among other places: 
# https://tinyheero.github.io/2015/10/13/mixture-model.html as inspiration)
# https://rpubs.com/MatthewPalmeri/646676


# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
require(tidyverse)
library(mixtools)
library(plotmm)
library(BetaMixture)
library(mclust)
theme_set(theme_bw())

set.seed(123)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

# load clean data  ----
cd = read.csv("../data/cd.csv") %>% 
  mutate(verb=as.factor(verb))

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y'' = (y' ?? (n ??? 1) + 0.5)/n
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
cd$betaresponse = (cd$response*(nrow(cd)-1) + .5)/nrow(cd)

## BIG LOOP THAT DOES GAUSSIAN AND BETA MIXTURE MODEL ANALYSIS IN ONE
optimalgaussians = data.frame(predicate = levels(cd$verb), clusters=-555)
colors = cbPalette

for (p in levels(cd$verb)) {
  dsub = cd %>% 
    filter(verb == p)
  
  ######################################################
  # 1.  test how many Gaussian components are justified
  mclust = Mclust(dsub$betaresponse)
  
  # print results to console
  print(p)
  print(summary(mclust))
  
  # update data frame to indicate optimal number of gaussian clusters
  optimalgaussians[optimalgaussians$predicate == p,]$clusters = mclust$G
  colors = cbPalette
  
  # plot optimal number of clusters
  mixmdl <- normalmixEM(dsub$betaresponse, k = mclust$G)
  responses <- data.frame(x=mixmdl$x)
  
  baseplot = ggplot(responses) +
    geom_histogram(aes(x, ..density..), binwidth = .01, colour = "black", fill = "white")
  
  for (i in 1:mclust$G)
  {
    baseplot = baseplot +
      stat_function(geom = "line", fun = plot_mix_comps_normal,
                  args = list(mu = mixmdl$mu[i], sigma = mixmdl$sigma[i], lam = mixmdl$lambda[i]),
                  colour = cbPalette[i], size = 1)
    
  }
  title = paste("\'",p,"\': mixed Gaussian model with ",mclust$G, " components",sep="")
  baseplot = baseplot +
    ylab("Density") + 
    xlab("Slider value") +
    scale_x_continuous(breaks=seq(0,1,by=.1)) + 
    ggtitle(title)
  filename = paste("../graphs/mixtures/",p,"_gaussian.pdf",sep="")
  ggsave(filename)
  
  ######################################################
  # 2.  plot mixtures of varying number of beta components
  for (n in 1:3) {
    # fit the model with components == n
    betamdl = BM_Fit(dsub$betaresponse, n, .01)
    print(p)
    print(betamdl)

    # extract the model parameters
    params = data.frame()
    for (j in 1:n) {
      params[1,paste("alpha_",j,sep="")] = betamdl$Iterations[nrow(betamdl$Iterations),paste("Alpha_",j,sep="")]
      params[1,paste("beta_",j,sep="")] = betamdl$Iterations[nrow(betamdl$Iterations),paste("Beta_",j,sep="")]
      params[1,paste("mix_",j,sep="")] = betamdl$Iterations[nrow(betamdl$Iterations),paste("Mixture_Param_",j,sep="")]
    }
    
    # plot the inferred beta distributions
    baseplot = ggplot(dsub) +
      geom_histogram(aes(x=betaresponse, y=..density..), binwidth = .005, colour = "black", fill = "white")

    baseplot = baseplot +
      stat_function(fun = function(x) params$mix_1*dbeta(x, params$alpha_1, params$beta_1), color = cbPalette[1], size = 1) +
      stat_function(fun = function(x) params$mix_2*dbeta(x, params$alpha_2, params$beta_2), color = cbPalette[2], size = 1) +
      stat_function(fun = function(x) params$mix_3*dbeta(x, params$alpha_3, params$beta_3), color = cbPalette[3], size = 1)
    
    title = paste("\'",p,"\': mixed Beta model with ",n, " components",sep="")
    baseplot = baseplot +
      ylab("Density") + 
      xlab("Slider value") +
      scale_x_continuous(breaks=seq(0,1,by=.1)) +
      ggtitle(title)
    filename = paste("../graphs/mixtures/",p,"_beta_",n,".pdf",sep="")
    ggsave(filename)

    }
}

optimalgaussians

# predicate clusters
# 1  acknowledge        4
# 2        admit        4
# 3     announce        5
# 4   be_annoyed        3
# 5     be_right        3
# 6      confess        6
# 7      confirm        2
# 8  demonstrate        5
# 9     discover        3
# 10   establish        6
# 11        hear        4
# 12      inform        4
# 13        know        3
# 14          MC        2
# 15     pretend        2
# 16       prove        3
# 17      reveal        4
# 18         say        5
# 19         see        3
# 20     suggest        2
# 21       think        4
