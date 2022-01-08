# Factives paper
# 5-projectivity-no-fact (certainty ratings, continuous task)
# mixture.R

# for inspiration on mixture models, used tutorial from, among other places: 
# https://tinyheero.github.io/2015/10/13/mixture-model.html as inspiration)
# https://rpubs.com/MatthewPalmeri/646676


# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(mixtools)
library(plotmm)
library(BetaMixture)
library(mclust)
theme_set(theme_classic())

set.seed(123)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
grays = gray.colors(4,start=0.2,end=0.9)

# load clean data  ----
cd = read.csv("../data/cd.csv") %>% 
  mutate(verb=as.factor(verb))

# cd = cd %>% 
  # mutate(verb = fct_recode(verb, "be-annoyed"="be_annoyed", "be-right"="be_right"))

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y'' = (y' ?? (n ??? 1) + 0.5)/n
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
cd$betaresponse = (cd$response*(nrow(cd)-1) + .5)/nrow(cd)

## SIMULATE DATA WITH 1-4 COMPONENTS

######################################################
# simulate idealized 1-component data with mean 0.85
simdata = data.frame(simvals = rnorm(266,mean=0.85,sd=0.05))
simdata = simdata %>% 
  mutate(simvals_norm = case_when(simvals < 0 ~ 0, # force vals <0 to 0
                                  simvals > 1 ~ 1, # force vals >1 to 1
                                  TRUE ~ simvals))

mclust = Mclust(simdata$simvals_norm)

# print results to console
print(summary(mclust))

# update data frame to indicate optimal number of gaussian clusters
colors = cbPalette

# mean
mean(simdata$simvals_norm)

# Figure 18b
ggplot(simdata) +
  geom_histogram(aes(simvals_norm, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.85, sd = 0.05),colour = cbPalette[1], size = 1) + 
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits = c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-1component-highmean.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/color/Figure18b.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# Figure 18b, black and white
ggplot(simdata) +
  geom_histogram(aes(simvals_norm, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.85, sd = 0.05),colour = "gray60", size = 1) + 
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits = c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-1component-highmean-bw.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/bw/Figure18b.pdf",sep="")
ggsave(filename,width=4,height=2.5)


# simulate idealized 1-component data with mean 0.4
simdata = data.frame(simvals = rnorm(266,mean=0.4,sd=0.1))
simdata = simdata %>% 
  mutate(simvals_norm = case_when(simvals < 0 ~ 0, # force vals <0 to 0
                                  simvals > 1 ~ 1, # force vals >1 to 1
                                  TRUE ~ simvals))

mclust = Mclust(simdata$simvals_norm)

# print results to console
print(summary(mclust))

# update data frame to indicate optimal number of gaussian clusters
colors = cbPalette

# mean
mean(simdata$simvals_norm)

# Figure 18a
ggplot(simdata) +
  geom_histogram(aes(simvals_norm, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.4, sd = 0.1),colour = cbPalette[1], size = 1) + 
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits = c(0,1))  +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-1component-lowmean.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/color/Figure18a.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# Figure 18a, black and white
ggplot(simdata) +
  geom_histogram(aes(simvals_norm, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.4, sd = 0.1),colour = "gray60", size = 1) + 
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits = c(0,1))  +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-1component-lowmean-bw.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/bw/Figure18a.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# simulate idealized 2-component data with mean 0.6
simdata = data.frame(simvals = c(rnorm(130,mean=0.3,sd=0.1),rnorm(136,mean=0.9,sd=0.03)))
simdata = simdata %>% 
  mutate(simvals_norm = case_when(simvals < 0 ~ 0, # force vals <0 to 0
                                  simvals > 1 ~ 1, # force vals >1 to 1
                                  TRUE ~ simvals))

mclust = Mclust(simdata$simvals_norm)

# print results to console
print(summary(mclust))

# update data frame to indicate optimal number of gaussian clusters
colors = cbPalette

# plot optimal number of clusters
mixmdl <- normalmixEM(simdata$simvals_norm, k = mclust$G)
responses <- data.frame(x=mixmdl$x)

# component 1
mixmdl$mu[1]
mixmdl$sigma[1]
mixmdl$lambda[1]

# component 2
mixmdl$mu[2]
mixmdl$sigma[2]
mixmdl$lambda[2]

# mean
mean(simdata$simvals_norm)

# Figure 18c
ggplot(responses) +
  geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = cbPalette[1], size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = cbPalette[2], size = 1) +
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-2components-lowermean.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/color/Figure18c.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# Figure 18c, black adn white
ggplot(responses) +
  geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = "gray60", size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = "gray60", size = 1) +
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-2components-lowermean-bw.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/bw/Figure18c.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# simulate idealized 2-component data with mean 0.7
simdata = data.frame(simvals = c(rnorm(61,mean=0.32,sd=0.1),rnorm(205,mean=0.82,sd=0.04)))
simdata = simdata %>% 
  mutate(simvals_norm = case_when(simvals < 0 ~ 0, # force vals <0 to 0
                                  simvals > 1 ~ 1, # force vals >1 to 1
                                  TRUE ~ simvals))
# mean
mean(simdata$simvals_norm)

mclust = Mclust(simdata$simvals_norm)

# print results to console
print(summary(mclust))

# update data frame to indicate optimal number of gaussian clusters
colors = cbPalette

# plot optimal number of clusters
mixmdl <- normalmixEM(simdata$simvals_norm, k = mclust$G)
responses <- data.frame(x=mixmdl$x)

# component 1
mixmdl$mu[1]
mixmdl$sigma[1]
mixmdl$lambda[1]

# component 2
mixmdl$mu[2]
mixmdl$sigma[2]
mixmdl$lambda[2]

# Figure 18d
ggplot(responses) +
  geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = cbPalette[1], size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = cbPalette[2], size = 1) +
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-2components-highermean.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/color/Figure18d.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# Figure 18d, black and white
ggplot(responses) +
  geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = "gray60", size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = "gray60", size = 1) +
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-2components-highermean-bw.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/bw/Figure18d.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# simulate idealized 3-component data
simdata = data.frame(simvals = c(rnorm(61,mean=0.32,sd=0.1),rnorm(100,mean=0.65,sd=0.05),rnorm(105,mean=0.85,sd=0.04)))
simdata = simdata %>% 
  mutate(simvals_norm = case_when(simvals < 0 ~ 0, # force vals <0 to 0
                                  simvals > 1 ~ 1, # force vals >1 to 1
                                  TRUE ~ simvals))
# mean
mean(simdata$simvals_norm)

mclust = Mclust(simdata$simvals_norm)

# print results to console
print(summary(mclust))

# update data frame to indicate optimal number of gaussian clusters
colors = cbPalette

# plot optimal number of clusters
mixmdl <- normalmixEM(simdata$simvals_norm, k = mclust$G)
responses <- data.frame(x=mixmdl$x)

# component 1
mixmdl$mu[1]
mixmdl$sigma[1]
mixmdl$lambda[1]

# component 2
mixmdl$mu[2]
mixmdl$sigma[2]
mixmdl$lambda[2]

# component 3
mixmdl$mu[3]
mixmdl$sigma[3]
mixmdl$lambda[3]

# Figure 18e
ggplot(responses) +
  geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = cbPalette[1], size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = cbPalette[2], size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[3], sigma = mixmdl$sigma[3], lam = mixmdl$lambda[3]),
                colour = cbPalette[3], size = 1) +  
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-3components.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/color/Figure18e.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# Figure 18e, black and white
ggplot(responses) +
  geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = "gray60", size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = "gray60", size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[3], sigma = mixmdl$sigma[3], lam = mixmdl$lambda[3]),
                colour = "gray60", size = 1) +  
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-3components-bw.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/bw/Figure18e.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# simulate idealized 3-component data
simdata = data.frame(simvals = c(rnorm(40,mean=0.1,sd=0.02),rnorm(61,mean=0.32,sd=0.1),rnorm(100,mean=0.65,sd=0.05),rnorm(65,mean=0.85,sd=0.04)))
simdata = simdata %>% 
  mutate(simvals_norm = case_when(simvals < 0 ~ 0, # force vals <0 to 0
                                  simvals > 1 ~ 1, # force vals >1 to 1
                                  TRUE ~ simvals))
# mean
mean(simdata$simvals_norm)

mclust = Mclust(simdata$simvals_norm)

# print results to console
print(summary(mclust))

# update data frame to indicate optimal number of gaussian clusters
colors = cbPalette

# plot optimal number of clusters
mixmdl <- normalmixEM(simdata$simvals_norm, k = mclust$G)
responses <- data.frame(x=mixmdl$x)

# component 1
mixmdl$mu[1]
mixmdl$sigma[1]
mixmdl$lambda[1]

# component 2
mixmdl$mu[2]
mixmdl$sigma[2]
mixmdl$lambda[2]

# component 3
mixmdl$mu[3]
mixmdl$sigma[3]
mixmdl$lambda[3]

# component 3
mixmdl$mu[4]
mixmdl$sigma[4]
mixmdl$lambda[4]

# Figure 18f
ggplot(responses) +
  geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = cbPalette[1], size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = cbPalette[2], size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[3], sigma = mixmdl$sigma[3], lam = mixmdl$lambda[3]),
                colour = cbPalette[3], size = 1) +  
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[4], sigma = mixmdl$sigma[4], lam = mixmdl$lambda[4]),
                colour = cbPalette[4], size = 1) +    
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-4components.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/color/Figure18f.pdf",sep="")
ggsave(filename,width=4,height=2.5)

# Figure 18f, black and white
ggplot(responses) +
  geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = "gray60", size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = "gray60", size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[3], sigma = mixmdl$sigma[3], lam = mixmdl$lambda[3]),
                colour = "gray60", size = 1) +  
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(mu = mixmdl$mu[4], sigma = mixmdl$sigma[4], lam = mixmdl$lambda[4]),
                colour = "gray60", size = 1) +    
  ylab("Density") + 
  xlab("Slider value") +
  scale_x_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

filename = paste("../graphs/mixtures/example-4components-bw.pdf",sep="")
ggsave(filename,width=4,height=2.5)
filename = paste("../../../papers/factives-paper/Language-figures/bw/Figure18f.pdf",sep="")
ggsave(filename,width=4,height=2.5)



# # plot best mixture for "reveal"
# dsub = cd %>% 
#   filter(verb == "reveal")
# 
# # 1.  test how many Gaussian components are justified
# mclust = Mclust(dsub$betaresponse)
# 
# # print results to console
# print(summary(mclust))
# 
# colors = cbPalette
# 
# # plot optimal number of clusters
# mixmdl <- normalmixEM(dsub$betaresponse, k = mclust$G)
# responses <- data.frame(x=mixmdl$x)
# 
# baseplot = ggplot(responses) +
#   geom_histogram(aes(x, ..density..), binwidth = .01, colour = "black", fill = "white")
# 
# print("mean")
# print(mean(dsub$betaresponse))
# 
# for (i in 1:mclust$G)
# {
#   print(paste("component ",i))
#   print("mu")
#   print(mixmdl$mu[i])
#   print("sigma")
#   print(mixmdl$sigma[i])
#   print("lambda")
#   print(mixmdl$lambda[i])
#   baseplot = baseplot +
#     stat_function(geom = "line", fun = plot_mix_comps_normal,
#                   args = list(mu = mixmdl$mu[i], sigma = mixmdl$sigma[i], lam = mixmdl$lambda[i]),
#                   colour = cbPalette[i], size = 1)
#   
# }
# baseplot = baseplot +
#   ylab("Density") + 
#   xlab("Slider value") +
#   scale_x_continuous(breaks=seq(0,1,by=.1))
# filename = paste("../graphs/mixtures/example-4components-reveal.pdf",sep="")
# ggsave(filename,width=4,height=2.5)
# 
# # plot best mixture for "discover"
# dsub = cd %>% 
#   filter(verb == "discover")
# 
# # 1.  test how many Gaussian components are justified
# mclust = Mclust(dsub$betaresponse)
# 
# # print results to console
# print(summary(mclust))
# 
# colors = cbPalette
# 
# # plot optimal number of clusters
# mixmdl <- normalmixEM(dsub$betaresponse, k = mclust$G)
# responses <- data.frame(x=mixmdl$x)
# 
# baseplot = ggplot(responses) +
#   geom_histogram(aes(x, ..density..), binwidth = .01, colour = "black", fill = "white")
# 
# print("mean")
# print(mean(dsub$betaresponse))
# 
# for (i in 1:mclust$G)
# {
#   print(paste("component ",i))
#   print("mu")
#   print(mixmdl$mu[i])
#   print("sigma")
#   print(mixmdl$sigma[i])
#   print("lambda")
#   print(mixmdl$lambda[i])
#   baseplot = baseplot +
#     stat_function(geom = "line", fun = plot_mix_comps_normal,
#                   args = list(mu = mixmdl$mu[i], sigma = mixmdl$sigma[i], lam = mixmdl$lambda[i]),
#                   colour = cbPalette[i], size = 1)
#   
# }
# baseplot = baseplot +
#   ylab("Density") + 
#   xlab("Slider value") +
#   scale_x_continuous(breaks=seq(0,1,by=.1))
# filename = paste("../graphs/mixtures/example-3components-discover.pdf",sep="")
# ggsave(filename,width=4,height=2.5)
# 
# # simulate idealized 2-component data with different mixtures
# simdata = data.frame(simvals = c(rnorm(90,mean=0.1,sd=0.03),rnorm(176,mean=0.9,sd=0.03)))
# simdata = simdata %>% 
#   mutate(simvals_norm = case_when(simvals < 0 ~ 0, # force vals <0 to 0
#                                   simvals > 1 ~ 1, # force vals >1 to 1
#                                   TRUE ~ simvals))
# 
# mclust = Mclust(simdata$simvals_norm)
# 
# # print results to console
# print(summary(mclust))
# 
# # update data frame to indicate optimal number of gaussian clusters
# colors = cbPalette
# 
# # plot optimal number of clusters
# mixmdl <- normalmixEM(simdata$simvals_norm, k = mclust$G)
# responses <- data.frame(x=mixmdl$x)
# 
# # component 1
# mixmdl$mu[1]
# mixmdl$sigma[1]
# mixmdl$lambda[1]
# 
# # component 2
# mixmdl$mu[2]
# mixmdl$sigma[2]
# mixmdl$lambda[2]
# 
# # mean
# mean(simdata$simvals_norm)
# 
# ggplot(responses) +
#   geom_histogram(aes(x, ..density..), binwidth = .01, colour = "black", fill = "white") +
#   stat_function(geom = "line", fun = plot_mix_comps_normal,
#                 args = list(mu = mixmdl$mu[1], sigma = mixmdl$sigma[1], lam = mixmdl$lambda[1]),
#                 colour = cbPalette[1], size = 1) +
#   stat_function(geom = "line", fun = plot_mix_comps_normal,
#                 args = list(mu = mixmdl$mu[2], sigma = mixmdl$sigma[2], lam = mixmdl$lambda[2]),
#                 colour = cbPalette[2], size = 1) +
#   ylab("Density") + 
#   xlab("Slider value") +
#   scale_x_continuous(breaks=seq(0,1,by=.1))
# 
# filename = paste("../graphs/mixtures/example_2components.pdf",sep="")
# ggsave(filename,width=5,height=3)







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
    geom_histogram(aes(x, ..density..), binwidth = .02, colour = "black", fill = "white")
  
  for (i in 1:mclust$G)
  {
    baseplot = baseplot +
      stat_function(geom = "line", fun = plot_mix_comps_normal,
                  args = list(mu = mixmdl$mu[i], sigma = mixmdl$sigma[i], lam = mixmdl$lambda[i]),
                  colour = cbPalette[i], size = 1)
    
  }
  # title = paste("\'",p,"\': mixed Gaussian model with ",mclust$G, " components",sep="")
  # title = p
  baseplot = baseplot +
    # geom_label(aes(x=0.5,y=10,label=p)) +
    ylab("Density") + 
    xlab("Slider value") +
    scale_x_continuous(breaks=seq(0,1,by=.1)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
    # ggtitle(title)
  filename = paste("../graphs/mixtures/",p,"-gaussian.pdf",sep="")
  ggsave(filename,width=4,height=2.5)
  
  ######################################################
  # 2.  plot mixtures of varying number of beta components
  # for (n in 1:3) {
  #   # fit the model with components == n
  #   betamdl = BM_Fit(dsub$betaresponse, n, .01)
  #   print(p)
  #   print(betamdl)
  # 
  #   # extract the model parameters
  #   params = data.frame()
  #   for (j in 1:n) {
  #     params[1,paste("alpha_",j,sep="")] = betamdl$Iterations[nrow(betamdl$Iterations),paste("Alpha_",j,sep="")]
  #     params[1,paste("beta_",j,sep="")] = betamdl$Iterations[nrow(betamdl$Iterations),paste("Beta_",j,sep="")]
  #     params[1,paste("mix_",j,sep="")] = betamdl$Iterations[nrow(betamdl$Iterations),paste("Mixture_Param_",j,sep="")]
  #   }
  # 
  #   # plot the inferred beta distributions
  #   baseplot = ggplot(dsub) +
  #     geom_histogram(aes(x=betaresponse, y=..density..), binwidth = .005, colour = "black", fill = "white")
  # 
  #   baseplot = baseplot +
  #     stat_function(fun = function(x) params$mix_1*0.3*dbeta(x, params$alpha_1, params$beta_1), color = cbPalette[1], size = 1) +
  #     stat_function(fun = function(x) params$mix_2*0.3*dbeta(x, params$alpha_2, params$beta_2), color = cbPalette[2], size = 1) +
  #     stat_function(fun = function(x) params$mix_3*0.3*dbeta(x, params$alpha_3, params$beta_3), color = cbPalette[3], size = 1)
  # 
  #   title = paste("\'",p,"\': mixed Beta model with ",n, " components",sep="")
  #   baseplot = baseplot +
  #     ylab("Density") +
  #     xlab("Slider value") +
  #     scale_x_continuous(breaks=seq(0,1,by=.1)) +
  #     ggtitle(title)
  #   filename = paste("../graphs/mixtures/",p,"-beta-",n,".pdf",sep="")
  #   ggsave(filename)
  # 
  #   }
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

