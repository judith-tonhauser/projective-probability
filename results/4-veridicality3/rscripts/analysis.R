# Factives paper
# 4-veridicality3 (inference ratings, continuous task)
# analysis.R

# What is true: Dan knows that Sophia got a tattoo.
# Does it follow that Sophia got a tattoo?

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
library(tidyverse)
library(tidybayes)
library(dichromat)
library(brms)
library(knitr)
library(extraDistr)
theme_set(theme_bw())

# load clean data  ----
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")
nrow(cd) #7252
summary(cd)

## models -----
library(emmeans)
library(lme4)
library(languageR)
library(brms)
str(cd$response)
str(cd$verb)
str(cd$workerid)
cd$workerid <- as.factor(cd$workerid)
cd$verb <- as.factor(as.character(cd$verb))

table(cd$verb)
table(cd$content)
# create item as combination of verb and content of complement
cd$item = as.factor(paste(cd$verb, cd$content))
table(cd$item)

# predict inference rating from predicate, with entailing controls as reference level, to see which 
# CC is entailed
table(cd$verb)
cd$verb <- relevel(cd$verb, ref = "entailing C")

## frequentist models ----
model <- lmer(response ~ verb + (1+verb|workerid) + (1|item), data=cd, REML=F)
summary(model) # does not converge


## Bayesian models ----

## pairwise comparison to see which predicates differ from one another
str(t$response)
str(t$verb)
str(t$workerid)
t$workerid <- as.factor(t$workerid)
model = lmer(response ~ verb + (1|workerid), data=t, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# contrast                       estimate         SE   df t.ratio p.value
# pretend - think           -0.1918637993 0.01488274 5301 -12.892  <.0001
# pretend - suggest         -0.2193548387 0.01488274 5301 -14.739  <.0001
# pretend - hear            -0.3776344086 0.01488274 5301 -25.374  <.0001
# pretend - say             -0.5566308244 0.01488274 5301 -37.401  <.0001
# pretend - announce        -0.6814695341 0.01488274 5301 -45.789  <.0001
# pretend - inform          -0.7081720430 0.01488274 5301 -47.583  <.0001
# pretend - demonstrate     -0.7208960573 0.01488274 5301 -48.438  <.0001
# pretend - confess         -0.7618279570 0.01488274 5301 -51.189  <.0001
# pretend - reveal          -0.7743369176 0.01488274 5301 -52.029  <.0001
# pretend - acknowledge     -0.7760573477 0.01488274 5301 -52.145  <.0001
# pretend - admit           -0.7765232975 0.01488274 5301 -52.176  <.0001
# pretend - establish       -0.7769534050 0.01488274 5301 -52.205  <.0001
# pretend - be_annoyed      -0.7931182796 0.01488274 5301 -53.291  <.0001
# pretend - know            -0.8031899642 0.01488274 5301 -53.968  <.0001
# pretend - confirm         -0.8141577061 0.01488274 5301 -54.705  <.0001
# pretend - discover        -0.8174910394 0.01488274 5301 -54.929  <.0001
# pretend - see             -0.8206810036 0.01488274 5301 -55.143  <.0001
# pretend - be_right        -0.8252329749 0.01488274 5301 -55.449  <.0001
# pretend - prove           -0.8273118280 0.01488274 5301 -55.589  <.0001
# think - suggest           -0.0274910394 0.01488274 5301  -1.847  0.9522
# think - hear              -0.1857706093 0.01488274 5301 -12.482  <.0001
# think - say               -0.3647670251 0.01488274 5301 -24.509  <.0001
# think - announce          -0.4896057348 0.01488274 5301 -32.898  <.0001
# think - inform            -0.5163082437 0.01488274 5301 -34.692  <.0001
# think - demonstrate       -0.5290322581 0.01488274 5301 -35.547  <.0001
# think - confess           -0.5699641577 0.01488274 5301 -38.297  <.0001
# think - reveal            -0.5824731183 0.01488274 5301 -39.137  <.0001
# think - acknowledge       -0.5841935484 0.01488274 5301 -39.253  <.0001
# think - admit             -0.5846594982 0.01488274 5301 -39.284  <.0001
# think - establish         -0.5850896057 0.01488274 5301 -39.313  <.0001
# think - be_annoyed        -0.6012544803 0.01488274 5301 -40.399  <.0001
# think - know              -0.6113261649 0.01488274 5301 -41.076  <.0001
# think - confirm           -0.6222939068 0.01488274 5301 -41.813  <.0001
# think - discover          -0.6256272401 0.01488274 5301 -42.037  <.0001
# think - see               -0.6288172043 0.01488274 5301 -42.251  <.0001
# think - be_right          -0.6333691756 0.01488274 5301 -42.557  <.0001
# think - prove             -0.6354480287 0.01488274 5301 -42.697  <.0001
# suggest - hear            -0.1582795699 0.01488274 5301 -10.635  <.0001
# suggest - say             -0.3372759857 0.01488274 5301 -22.662  <.0001
# suggest - announce        -0.4621146953 0.01488274 5301 -31.050  <.0001
# suggest - inform          -0.4888172043 0.01488274 5301 -32.845  <.0001
# suggest - demonstrate     -0.5015412186 0.01488274 5301 -33.700  <.0001
# suggest - confess         -0.5424731183 0.01488274 5301 -36.450  <.0001
# suggest - reveal          -0.5549820789 0.01488274 5301 -37.290  <.0001
# suggest - acknowledge     -0.5567025090 0.01488274 5301 -37.406  <.0001
# suggest - admit           -0.5571684588 0.01488274 5301 -37.437  <.0001
# suggest - establish       -0.5575985663 0.01488274 5301 -37.466  <.0001
# suggest - be_annoyed      -0.5737634409 0.01488274 5301 -38.552  <.0001
# suggest - know            -0.5838351254 0.01488274 5301 -39.229  <.0001
# suggest - confirm         -0.5948028674 0.01488274 5301 -39.966  <.0001
# suggest - discover        -0.5981362007 0.01488274 5301 -40.190  <.0001
# suggest - see             -0.6013261649 0.01488274 5301 -40.404  <.0001
# suggest - be_right        -0.6058781362 0.01488274 5301 -40.710  <.0001
# suggest - prove           -0.6079569892 0.01488274 5301 -40.850  <.0001
# hear - say                -0.1789964158 0.01488274 5301 -12.027  <.0001
# hear - announce           -0.3038351254 0.01488274 5301 -20.415  <.0001
# hear - inform             -0.3305376344 0.01488274 5301 -22.209  <.0001
# hear - demonstrate        -0.3432616487 0.01488274 5301 -23.064  <.0001
# hear - confess            -0.3841935484 0.01488274 5301 -25.815  <.0001
# hear - reveal             -0.3967025090 0.01488274 5301 -26.655  <.0001
# hear - acknowledge        -0.3984229391 0.01488274 5301 -26.771  <.0001
# hear - admit              -0.3988888889 0.01488274 5301 -26.802  <.0001
# hear - establish          -0.3993189964 0.01488274 5301 -26.831  <.0001
# hear - be_annoyed         -0.4154838710 0.01488274 5301 -27.917  <.0001
# hear - know               -0.4255555556 0.01488274 5301 -28.594  <.0001
# hear - confirm            -0.4365232975 0.01488274 5301 -29.331  <.0001
# hear - discover           -0.4398566308 0.01488274 5301 -29.555  <.0001
# hear - see                -0.4430465950 0.01488274 5301 -29.769  <.0001
# hear - be_right           -0.4475985663 0.01488274 5301 -30.075  <.0001
# hear - prove              -0.4496774194 0.01488274 5301 -30.215  <.0001
# say - announce            -0.1248387097 0.01488274 5301  -8.388  <.0001
# say - inform              -0.1515412186 0.01488274 5301 -10.182  <.0001
# say - demonstrate         -0.1642652330 0.01488274 5301 -11.037  <.0001
# say - confess             -0.2051971326 0.01488274 5301 -13.788  <.0001
# say - reveal              -0.2177060932 0.01488274 5301 -14.628  <.0001
# say - acknowledge         -0.2194265233 0.01488274 5301 -14.744  <.0001
# say - admit               -0.2198924731 0.01488274 5301 -14.775  <.0001
# say - establish           -0.2203225806 0.01488274 5301 -14.804  <.0001
# say - be_annoyed          -0.2364874552 0.01488274 5301 -15.890  <.0001
# say - know                -0.2465591398 0.01488274 5301 -16.567  <.0001
# say - confirm             -0.2575268817 0.01488274 5301 -17.304  <.0001
# say - discover            -0.2608602151 0.01488274 5301 -17.528  <.0001
# say - see                 -0.2640501792 0.01488274 5301 -17.742  <.0001
# say - be_right            -0.2686021505 0.01488274 5301 -18.048  <.0001
# say - prove               -0.2706810036 0.01488274 5301 -18.188  <.0001
# announce - inform         -0.0267025090 0.01488274 5301  -1.794  0.9640
# announce - demonstrate    -0.0394265233 0.01488274 5301  -2.649  0.4669
# announce - confess        -0.0803584229 0.01488274 5301  -5.399  <.0001
# announce - reveal         -0.0928673835 0.01488274 5301  -6.240  <.0001
# announce - acknowledge    -0.0945878136 0.01488274 5301  -6.356  <.0001
# announce - admit          -0.0950537634 0.01488274 5301  -6.387  <.0001
# announce - establish      -0.0954838710 0.01488274 5301  -6.416  <.0001
# announce - be_annoyed     -0.1116487455 0.01488274 5301  -7.502  <.0001
# announce - know           -0.1217204301 0.01488274 5301  -8.179  <.0001
# announce - confirm        -0.1326881720 0.01488274 5301  -8.916  <.0001
# announce - discover       -0.1360215054 0.01488274 5301  -9.140  <.0001
# announce - see            -0.1392114695 0.01488274 5301  -9.354  <.0001
# announce - be_right       -0.1437634409 0.01488274 5301  -9.660  <.0001
# announce - prove          -0.1458422939 0.01488274 5301  -9.799  <.0001
# inform - demonstrate      -0.0127240143 0.01488274 5301  -0.855  1.0000
# inform - confess          -0.0536559140 0.01488274 5301  -3.605  0.0411
# inform - reveal           -0.0661648746 0.01488274 5301  -4.446  0.0015
# inform - acknowledge      -0.0678853047 0.01488274 5301  -4.561  0.0009
# inform - admit            -0.0683512545 0.01488274 5301  -4.593  0.0008
# inform - establish        -0.0687813620 0.01488274 5301  -4.622  0.0007
# inform - be_annoyed       -0.0849462366 0.01488274 5301  -5.708  <.0001
# inform - know             -0.0950179211 0.01488274 5301  -6.384  <.0001
# inform - confirm          -0.1059856631 0.01488274 5301  -7.121  <.0001
# inform - discover         -0.1093189964 0.01488274 5301  -7.345  <.0001
# inform - see              -0.1125089606 0.01488274 5301  -7.560  <.0001
# inform - be_right         -0.1170609319 0.01488274 5301  -7.866  <.0001
# inform - prove            -0.1191397849 0.01488274 5301  -8.005  <.0001
# demonstrate - confess     -0.0409318996 0.01488274 5301  -2.750  0.3911
# demonstrate - reveal      -0.0534408602 0.01488274 5301  -3.591  0.0431
# demonstrate - acknowledge -0.0551612903 0.01488274 5301  -3.706  0.0290
# demonstrate - admit       -0.0556272401 0.01488274 5301  -3.738  0.0260
# demonstrate - establish   -0.0560573477 0.01488274 5301  -3.767  0.0235
# demonstrate - be_annoyed  -0.0722222222 0.01488274 5301  -4.853  0.0002
# demonstrate - know        -0.0822939068 0.01488274 5301  -5.529  <.0001
# demonstrate - confirm     -0.0932616487 0.01488274 5301  -6.266  <.0001
# demonstrate - discover    -0.0965949821 0.01488274 5301  -6.490  <.0001
# demonstrate - see         -0.0997849462 0.01488274 5301  -6.705  <.0001
# demonstrate - be_right    -0.1043369176 0.01488274 5301  -7.011  <.0001
# demonstrate - prove       -0.1064157706 0.01488274 5301  -7.150  <.0001
# confess - reveal          -0.0125089606 0.01488274 5301  -0.841  1.0000
# confess - acknowledge     -0.0142293907 0.01488274 5301  -0.956  1.0000
# confess - admit           -0.0146953405 0.01488274 5301  -0.987  1.0000
# confess - establish       -0.0151254480 0.01488274 5301  -1.016  1.0000
# confess - be_annoyed      -0.0312903226 0.01488274 5301  -2.102  0.8560
# confess - know            -0.0413620072 0.01488274 5301  -2.779  0.3704
# confess - confirm         -0.0523297491 0.01488274 5301  -3.516  0.0550
# confess - discover        -0.0556630824 0.01488274 5301  -3.740  0.0258
# confess - see             -0.0588530466 0.01488274 5301  -3.954  0.0117
# confess - be_right        -0.0634050179 0.01488274 5301  -4.260  0.0034
# confess - prove           -0.0654838710 0.01488274 5301  -4.400  0.0019
# reveal - acknowledge      -0.0017204301 0.01488274 5301  -0.116  1.0000
# reveal - admit            -0.0021863799 0.01488274 5301  -0.147  1.0000
# reveal - establish        -0.0026164875 0.01488274 5301  -0.176  1.0000
# reveal - be_annoyed       -0.0187813620 0.01488274 5301  -1.262  0.9995
# reveal - know             -0.0288530466 0.01488274 5301  -1.939  0.9257
# reveal - confirm          -0.0398207885 0.01488274 5301  -2.676  0.4466
# reveal - discover         -0.0431541219 0.01488274 5301  -2.900  0.2902
# reveal - see              -0.0463440860 0.01488274 5301  -3.114  0.1755
# reveal - be_right         -0.0508960573 0.01488274 5301  -3.420  0.0744
# reveal - prove            -0.0529749104 0.01488274 5301  -3.559  0.0478
# acknowledge - admit       -0.0004659498 0.01488274 5301  -0.031  1.0000
# acknowledge - establish   -0.0008960573 0.01488274 5301  -0.060  1.0000
# acknowledge - be_annoyed  -0.0170609319 0.01488274 5301  -1.146  0.9999
# acknowledge - know        -0.0271326165 0.01488274 5301  -1.823  0.9579
# acknowledge - confirm     -0.0381003584 0.01488274 5301  -2.560  0.5365
# acknowledge - discover    -0.0414336918 0.01488274 5301  -2.784  0.3670
# acknowledge - see         -0.0446236559 0.01488274 5301  -2.998  0.2326
# acknowledge - be_right    -0.0491756272 0.01488274 5301  -3.304  0.1048
# acknowledge - prove       -0.0512544803 0.01488274 5301  -3.444  0.0691
# admit - establish         -0.0004301075 0.01488274 5301  -0.029  1.0000
# admit - be_annoyed        -0.0165949821 0.01488274 5301  -1.115  0.9999
# admit - know              -0.0266666667 0.01488274 5301  -1.792  0.9645
# admit - confirm           -0.0376344086 0.01488274 5301  -2.529  0.5613
# admit - discover          -0.0409677419 0.01488274 5301  -2.753  0.3894
# admit - see               -0.0441577061 0.01488274 5301  -2.967  0.2500
# admit - be_right          -0.0487096774 0.01488274 5301  -3.273  0.1146
# admit - prove             -0.0507885305 0.01488274 5301  -3.413  0.0761
# establish - be_annoyed    -0.0161648746 0.01488274 5301  -1.086  0.9999
# establish - know          -0.0262365591 0.01488274 5301  -1.763  0.9698
# establish - confirm       -0.0372043011 0.01488274 5301  -2.500  0.5840
# establish - discover      -0.0405376344 0.01488274 5301  -2.724  0.4105
# establish - see           -0.0437275986 0.01488274 5301  -2.938  0.2668
# establish - be_right      -0.0482795699 0.01488274 5301  -3.244  0.1242
# establish - prove         -0.0503584229 0.01488274 5301  -3.384  0.0830
# be_annoyed - know         -0.0100716846 0.01488274 5301  -0.677  1.0000
# be_annoyed - confirm      -0.0210394265 0.01488274 5301  -1.414  0.9977
# be_annoyed - discover     -0.0243727599 0.01488274 5301  -1.638  0.9863
# be_annoyed - see          -0.0275627240 0.01488274 5301  -1.852  0.9511
# be_annoyed - be_right     -0.0321146953 0.01488274 5301  -2.158  0.8258
# be_annoyed - prove        -0.0341935484 0.01488274 5301  -2.298  0.7365
# know - confirm            -0.0109677419 0.01488274 5301  -0.737  1.0000
# know - discover           -0.0143010753 0.01488274 5301  -0.961  1.0000
# know - see                -0.0174910394 0.01488274 5301  -1.175  0.9998
# know - be_right           -0.0220430108 0.01488274 5301  -1.481  0.9958
# know - prove              -0.0241218638 0.01488274 5301  -1.621  0.9878
# confirm - discover        -0.0033333333 0.01488274 5301  -0.224  1.0000
# confirm - see             -0.0065232975 0.01488274 5301  -0.438  1.0000
# confirm - be_right        -0.0110752688 0.01488274 5301  -0.744  1.0000
# confirm - prove           -0.0131541219 0.01488274 5301  -0.884  1.0000
# discover - see            -0.0031899642 0.01488274 5301  -0.214  1.0000
# discover - be_right       -0.0077419355 0.01488274 5301  -0.520  1.0000
# discover - prove          -0.0098207885 0.01488274 5301  -0.660  1.0000
# see - be_right            -0.0045519713 0.01488274 5301  -0.306  1.0000
# see - prove               -0.0066308244 0.01488274 5301  -0.446  1.0000
# be_right - prove          -0.0020788530 0.01488274 5301  -0.140  1.0000

## pairwise comparison to see which predicates differ from one another
## including the entailing control stimuli

str(te$response)
str(te$verb)
str(te$workerid)
te$workerid <- as.factor(te$workerid)

means = te %>%
  group_by(verb) %>%
  summarize(Mean = mean(response)) %>%
  select(verb,Mean)
means = as.data.frame(means)

te$verb <-factor(te$verb, levels=means[order(means$Mean), "verb"])

model = lmer(response ~ verb + (1|workerid), data=te, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                          estimate         SE   df t.ratio p.value
# pretend - think              -0.1918637993 0.01400046 6417 -13.704  <.0001
# pretend - suggest            -0.2193548387 0.01400046 6417 -15.668  <.0001
# pretend - hear               -0.3776344086 0.01400046 6417 -26.973  <.0001
# pretend - say                -0.5566308244 0.01400046 6417 -39.758  <.0001
# pretend - announce           -0.6814695341 0.01400046 6417 -48.675  <.0001
# pretend - inform_Sam         -0.7081720430 0.01400046 6417 -50.582  <.0001
# pretend - demonstrate        -0.7208960573 0.01400046 6417 -51.491  <.0001
# pretend - confess            -0.7618279570 0.01400046 6417 -54.415  <.0001
# pretend - reveal             -0.7743369176 0.01400046 6417 -55.308  <.0001
# pretend - acknowledge        -0.7760573477 0.01400046 6417 -55.431  <.0001
# pretend - admit              -0.7765232975 0.01400046 6417 -55.464  <.0001
# pretend - establish          -0.7769534050 0.01400046 6417 -55.495  <.0001
# pretend - annoyed            -0.7931182796 0.01400046 6417 -56.649  <.0001
# pretend - know               -0.8031899642 0.01400046 6417 -57.369  <.0001
# pretend - confirm            -0.8141577061 0.01400046 6417 -58.152  <.0001
# pretend - discover           -0.8174910394 0.01400046 6417 -58.390  <.0001
# pretend - see                -0.8206810036 0.01400046 6417 -58.618  <.0001
# pretend - be_right_that      -0.8252329749 0.01400046 6417 -58.943  <.0001
# pretend - prove              -0.8273118280 0.01400046 6417 -59.092  <.0001
# pretend - control_good       -0.8306093190 0.01106833 6417 -75.044  <.0001
# think - suggest              -0.0274910394 0.01400046 6417  -1.964  0.9292
# think - hear                 -0.1857706093 0.01400046 6417 -13.269  <.0001
# think - say                  -0.3647670251 0.01400046 6417 -26.054  <.0001
# think - announce             -0.4896057348 0.01400046 6417 -34.971  <.0001
# think - inform_Sam           -0.5163082437 0.01400046 6417 -36.878  <.0001
# think - demonstrate          -0.5290322581 0.01400046 6417 -37.787  <.0001
# think - confess              -0.5699641577 0.01400046 6417 -40.710  <.0001
# think - reveal               -0.5824731183 0.01400046 6417 -41.604  <.0001
# think - acknowledge          -0.5841935484 0.01400046 6417 -41.727  <.0001
# think - admit                -0.5846594982 0.01400046 6417 -41.760  <.0001
# think - establish            -0.5850896057 0.01400046 6417 -41.791  <.0001
# think - annoyed              -0.6012544803 0.01400046 6417 -42.945  <.0001
# think - know                 -0.6113261649 0.01400046 6417 -43.665  <.0001
# think - confirm              -0.6222939068 0.01400046 6417 -44.448  <.0001
# think - discover             -0.6256272401 0.01400046 6417 -44.686  <.0001
# think - see                  -0.6288172043 0.01400046 6417 -44.914  <.0001
# think - be_right_that        -0.6333691756 0.01400046 6417 -45.239  <.0001
# think - prove                -0.6354480287 0.01400046 6417 -45.388  <.0001
# think - control_good         -0.6387455197 0.01106833 6417 -57.709  <.0001
# suggest - hear               -0.1582795699 0.01400046 6417 -11.305  <.0001
# suggest - say                -0.3372759857 0.01400046 6417 -24.090  <.0001
# suggest - announce           -0.4621146953 0.01400046 6417 -33.007  <.0001
# suggest - inform_Sam         -0.4888172043 0.01400046 6417 -34.914  <.0001
# suggest - demonstrate        -0.5015412186 0.01400046 6417 -35.823  <.0001
# suggest - confess            -0.5424731183 0.01400046 6417 -38.747  <.0001
# suggest - reveal             -0.5549820789 0.01400046 6417 -39.640  <.0001
# suggest - acknowledge        -0.5567025090 0.01400046 6417 -39.763  <.0001
# suggest - admit              -0.5571684588 0.01400046 6417 -39.796  <.0001
# suggest - establish          -0.5575985663 0.01400046 6417 -39.827  <.0001
# suggest - annoyed            -0.5737634409 0.01400046 6417 -40.982  <.0001
# suggest - know               -0.5838351254 0.01400046 6417 -41.701  <.0001
# suggest - confirm            -0.5948028674 0.01400046 6417 -42.485  <.0001
# suggest - discover           -0.5981362007 0.01400046 6417 -42.723  <.0001
# suggest - see                -0.6013261649 0.01400046 6417 -42.950  <.0001
# suggest - be_right_that      -0.6058781362 0.01400046 6417 -43.276  <.0001
# suggest - prove              -0.6079569892 0.01400046 6417 -43.424  <.0001
# suggest - control_good       -0.6112544803 0.01106833 6417 -55.226  <.0001
# hear - say                   -0.1789964158 0.01400046 6417 -12.785  <.0001
# hear - announce              -0.3038351254 0.01400046 6417 -21.702  <.0001
# hear - inform_Sam            -0.3305376344 0.01400046 6417 -23.609  <.0001
# hear - demonstrate           -0.3432616487 0.01400046 6417 -24.518  <.0001
# hear - confess               -0.3841935484 0.01400046 6417 -27.442  <.0001
# hear - reveal                -0.3967025090 0.01400046 6417 -28.335  <.0001
# hear - acknowledge           -0.3984229391 0.01400046 6417 -28.458  <.0001
# hear - admit                 -0.3988888889 0.01400046 6417 -28.491  <.0001
# hear - establish             -0.3993189964 0.01400046 6417 -28.522  <.0001
# hear - annoyed               -0.4154838710 0.01400046 6417 -29.676  <.0001
# hear - know                  -0.4255555556 0.01400046 6417 -30.396  <.0001
# hear - confirm               -0.4365232975 0.01400046 6417 -31.179  <.0001
# hear - discover              -0.4398566308 0.01400046 6417 -31.417  <.0001
# hear - see                   -0.4430465950 0.01400046 6417 -31.645  <.0001
# hear - be_right_that         -0.4475985663 0.01400046 6417 -31.970  <.0001
# hear - prove                 -0.4496774194 0.01400046 6417 -32.119  <.0001
# hear - control_good          -0.4529749104 0.01106833 6417 -40.925  <.0001
# say - announce               -0.1248387097 0.01400046 6417  -8.917  <.0001
# say - inform_Sam             -0.1515412186 0.01400046 6417 -10.824  <.0001
# say - demonstrate            -0.1642652330 0.01400046 6417 -11.733  <.0001
# say - confess                -0.2051971326 0.01400046 6417 -14.656  <.0001
# say - reveal                 -0.2177060932 0.01400046 6417 -15.550  <.0001
# say - acknowledge            -0.2194265233 0.01400046 6417 -15.673  <.0001
# say - admit                  -0.2198924731 0.01400046 6417 -15.706  <.0001
# say - establish              -0.2203225806 0.01400046 6417 -15.737  <.0001
# say - annoyed                -0.2364874552 0.01400046 6417 -16.891  <.0001
# say - know                   -0.2465591398 0.01400046 6417 -17.611  <.0001
# say - confirm                -0.2575268817 0.01400046 6417 -18.394  <.0001
# say - discover               -0.2608602151 0.01400046 6417 -18.632  <.0001
# say - see                    -0.2640501792 0.01400046 6417 -18.860  <.0001
# say - be_right_that          -0.2686021505 0.01400046 6417 -19.185  <.0001
# say - prove                  -0.2706810036 0.01400046 6417 -19.334  <.0001
# say - control_good           -0.2739784946 0.01106833 6417 -24.753  <.0001
# announce - inform_Sam        -0.0267025090 0.01400046 6417  -1.907  0.9460
# announce - demonstrate       -0.0394265233 0.01400046 6417  -2.816  0.3652
# announce - confess           -0.0803584229 0.01400046 6417  -5.740  <.0001
# announce - reveal            -0.0928673835 0.01400046 6417  -6.633  <.0001
# announce - acknowledge       -0.0945878136 0.01400046 6417  -6.756  <.0001
# announce - admit             -0.0950537634 0.01400046 6417  -6.789  <.0001
# announce - establish         -0.0954838710 0.01400046 6417  -6.820  <.0001
# announce - annoyed           -0.1116487455 0.01400046 6417  -7.975  <.0001
# announce - know              -0.1217204301 0.01400046 6417  -8.694  <.0001
# announce - confirm           -0.1326881720 0.01400046 6417  -9.477  <.0001
# announce - discover          -0.1360215054 0.01400046 6417  -9.716  <.0001
# announce - see               -0.1392114695 0.01400046 6417  -9.943  <.0001
# announce - be_right_that     -0.1437634409 0.01400046 6417 -10.268  <.0001
# announce - prove             -0.1458422939 0.01400046 6417 -10.417  <.0001
# announce - control_good      -0.1491397849 0.01106833 6417 -13.474  <.0001
# inform_Sam - demonstrate     -0.0127240143 0.01400046 6417  -0.909  1.0000
# inform_Sam - confess         -0.0536559140 0.01400046 6417  -3.832  0.0201
# inform_Sam - reveal          -0.0661648746 0.01400046 6417  -4.726  0.0005
# inform_Sam - acknowledge     -0.0678853047 0.01400046 6417  -4.849  0.0003
# inform_Sam - admit           -0.0683512545 0.01400046 6417  -4.882  0.0002
# inform_Sam - establish       -0.0687813620 0.01400046 6417  -4.913  0.0002
# inform_Sam - annoyed         -0.0849462366 0.01400046 6417  -6.067  <.0001
# inform_Sam - know            -0.0950179211 0.01400046 6417  -6.787  <.0001
# inform_Sam - confirm         -0.1059856631 0.01400046 6417  -7.570  <.0001
# inform_Sam - discover        -0.1093189964 0.01400046 6417  -7.808  <.0001
# inform_Sam - see             -0.1125089606 0.01400046 6417  -8.036  <.0001
# inform_Sam - be_right_that   -0.1170609319 0.01400046 6417  -8.361  <.0001
# inform_Sam - prove           -0.1191397849 0.01400046 6417  -8.510  <.0001
# inform_Sam - control_good    -0.1224372760 0.01106833 6417 -11.062  <.0001
# demonstrate - confess        -0.0409318996 0.01400046 6417  -2.924  0.2932
# demonstrate - reveal         -0.0534408602 0.01400046 6417  -3.817  0.0213
# demonstrate - acknowledge    -0.0551612903 0.01400046 6417  -3.940  0.0135
# demonstrate - admit          -0.0556272401 0.01400046 6417  -3.973  0.0119
# demonstrate - establish      -0.0560573477 0.01400046 6417  -4.004  0.0105
# demonstrate - annoyed        -0.0722222222 0.01400046 6417  -5.159  0.0001
# demonstrate - know           -0.0822939068 0.01400046 6417  -5.878  <.0001
# demonstrate - confirm        -0.0932616487 0.01400046 6417  -6.661  <.0001
# demonstrate - discover       -0.0965949821 0.01400046 6417  -6.899  <.0001
# demonstrate - see            -0.0997849462 0.01400046 6417  -7.127  <.0001
# demonstrate - be_right_that  -0.1043369176 0.01400046 6417  -7.452  <.0001
# demonstrate - prove          -0.1064157706 0.01400046 6417  -7.601  <.0001
# demonstrate - control_good   -0.1097132616 0.01106833 6417  -9.912  <.0001
# confess - reveal             -0.0125089606 0.01400046 6417  -0.893  1.0000
# confess - acknowledge        -0.0142293907 0.01400046 6417  -1.016  1.0000
# confess - admit              -0.0146953405 0.01400046 6417  -1.050  1.0000
# confess - establish          -0.0151254480 0.01400046 6417  -1.080  1.0000
# confess - annoyed            -0.0312903226 0.01400046 6417  -2.235  0.7999
# confess - know               -0.0413620072 0.01400046 6417  -2.954  0.2742
# confess - confirm            -0.0523297491 0.01400046 6417  -3.738  0.0283
# confess - discover           -0.0556630824 0.01400046 6417  -3.976  0.0117
# confess - see                -0.0588530466 0.01400046 6417  -4.204  0.0047
# confess - be_right_that      -0.0634050179 0.01400046 6417  -4.529  0.0011
# confess - prove              -0.0654838710 0.01400046 6417  -4.677  0.0006
# confess - control_good       -0.0687813620 0.01106833 6417  -6.214  <.0001
# reveal - acknowledge         -0.0017204301 0.01400046 6417  -0.123  1.0000
# reveal - admit               -0.0021863799 0.01400046 6417  -0.156  1.0000
# reveal - establish           -0.0026164875 0.01400046 6417  -0.187  1.0000
# reveal - annoyed             -0.0187813620 0.01400046 6417  -1.341  0.9992
# reveal - know                -0.0288530466 0.01400046 6417  -2.061  0.8921
# reveal - confirm             -0.0398207885 0.01400046 6417  -2.844  0.3456
# reveal - discover            -0.0431541219 0.01400046 6417  -3.082  0.2035
# reveal - see                 -0.0463440860 0.01400046 6417  -3.310  0.1111
# reveal - be_right_that       -0.0508960573 0.01400046 6417  -3.635  0.0403
# reveal - prove               -0.0529749104 0.01400046 6417  -3.784  0.0240
# reveal - control_good        -0.0562724014 0.01106833 6417  -5.084  0.0001
# acknowledge - admit          -0.0004659498 0.01400046 6417  -0.033  1.0000
# acknowledge - establish      -0.0008960573 0.01400046 6417  -0.064  1.0000
# acknowledge - annoyed        -0.0170609319 0.01400046 6417  -1.219  0.9998
# acknowledge - know           -0.0271326165 0.01400046 6417  -1.938  0.9372
# acknowledge - confirm        -0.0381003584 0.01400046 6417  -2.721  0.4349
# acknowledge - discover       -0.0414336918 0.01400046 6417  -2.959  0.2711
# acknowledge - see            -0.0446236559 0.01400046 6417  -3.187  0.1558
# acknowledge - be_right_that  -0.0491756272 0.01400046 6417  -3.512  0.0603
# acknowledge - prove          -0.0512544803 0.01400046 6417  -3.661  0.0370
# acknowledge - control_good   -0.0545519713 0.01106833 6417  -4.929  0.0002
# admit - establish            -0.0004301075 0.01400046 6417  -0.031  1.0000
# admit - annoyed              -0.0165949821 0.01400046 6417  -1.185  0.9999
# admit - know                 -0.0266666667 0.01400046 6417  -1.905  0.9467
# admit - confirm              -0.0376344086 0.01400046 6417  -2.688  0.4604
# admit - discover             -0.0409677419 0.01400046 6417  -2.926  0.2916
# admit - see                  -0.0441577061 0.01400046 6417  -3.154  0.1699
# admit - be_right_that        -0.0487096774 0.01400046 6417  -3.479  0.0670
# admit - prove                -0.0507885305 0.01400046 6417  -3.628  0.0414
# admit - control_good         -0.0540860215 0.01106833 6417  -4.887  0.0002
# establish - annoyed          -0.0161648746 0.01400046 6417  -1.155  0.9999
# establish - know             -0.0262365591 0.01400046 6417  -1.874  0.9545
# establish - confirm          -0.0372043011 0.01400046 6417  -2.657  0.4842
# establish - discover         -0.0405376344 0.01400046 6417  -2.895  0.3112
# establish - see              -0.0437275986 0.01400046 6417  -3.123  0.1838
# establish - be_right_that    -0.0482795699 0.01400046 6417  -3.448  0.0737
# establish - prove            -0.0503584229 0.01400046 6417  -3.597  0.0458
# establish - control_good     -0.0536559140 0.01106833 6417  -4.848  0.0003
# annoyed - know               -0.0100716846 0.01400046 6417  -0.719  1.0000
# annoyed - confirm            -0.0210394265 0.01400046 6417  -1.503  0.9963
# annoyed - discover           -0.0243727599 0.01400046 6417  -1.741  0.9788
# annoyed - see                -0.0275627240 0.01400046 6417  -1.969  0.9275
# annoyed - be_right_that      -0.0321146953 0.01400046 6417  -2.294  0.7617
# annoyed - prove              -0.0341935484 0.01400046 6417  -2.442  0.6537
# annoyed - control_good       -0.0374910394 0.01106833 6417  -3.387  0.0888
# know - confirm               -0.0109677419 0.01400046 6417  -0.783  1.0000
# know - discover              -0.0143010753 0.01400046 6417  -1.021  1.0000
# know - see                   -0.0174910394 0.01400046 6417  -1.249  0.9997
# know - be_right_that         -0.0220430108 0.01400046 6417  -1.574  0.9934
# know - prove                 -0.0241218638 0.01400046 6417  -1.723  0.9811
# know - control_good          -0.0274193548 0.01106833 6417  -2.477  0.6266
# confirm - discover           -0.0033333333 0.01400046 6417  -0.238  1.0000
# confirm - see                -0.0065232975 0.01400046 6417  -0.466  1.0000
# confirm - be_right_that      -0.0110752688 0.01400046 6417  -0.791  1.0000
# confirm - prove              -0.0131541219 0.01400046 6417  -0.940  1.0000
# confirm - control_good       -0.0164516129 0.01106833 6417  -1.486  0.9968
# discover - see               -0.0031899642 0.01400046 6417  -0.228  1.0000
# discover - be_right_that     -0.0077419355 0.01400046 6417  -0.553  1.0000
# discover - prove             -0.0098207885 0.01400046 6417  -0.701  1.0000
# discover - control_good      -0.0131182796 0.01106833 6417  -1.185  0.9999
# see - be_right_that          -0.0045519713 0.01400046 6417  -0.325  1.0000
# see - prove                  -0.0066308244 0.01400046 6417  -0.474  1.0000
# see - control_good           -0.0099283154 0.01106833 6417  -0.897  1.0000
# be_right_that - prove        -0.0020788530 0.01400046 6417  -0.148  1.0000
# be_right_that - control_good -0.0053763441 0.01106833 6417  -0.486  1.0000
# prove - control_good         -0.0032974910 0.01106833 6417  -0.298  1.0000
# 
# P value adjustment: tukey method for comparing a family of 21 estimates 





### Correlation between the two measures of entailment -----

# load contradictoriness means by verb
cmeans = read.csv("../../2-veridicality2/data/veridicality_means.csv")
cmeans
colnames(cmeans) = c("verb","ContradictorinessMean","ContradictorinessCILow","ContradictorinessCIHigh")
head(cmeans)
cmeans <- droplevels(subset(cmeans,cmeans$verb != "contradictory C" & cmeans$verb != "non-contrad. C"))
nrow(cmeans) #20 verbs

# merge contradictoriness item means into target data
head(t)
head(cmeans)
t = merge(t,cmeans,by=c("verb"))
head(t)
nrow(t)

means = t %>%
  group_by(verb, ContradictorinessMean) %>%
  summarize(InferenceMean = mean(response), InferenceCILow = ci.low(response), InferenceCIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(InferenceYMin = InferenceMean - InferenceCILow, InferenceYMax = InferenceMean + InferenceCIHigh, Verb = fct_reorder(verb,InferenceMean))
View(means)

model <- lm(ContradictorinessMean ~ InferenceMean, data = means)
summary(model)

ggplot(means, aes(x=InferenceMean, y=ContradictorinessMean)) + 
  geom_point() +
  geom_text(aes(label=verb),hjust=0,vjust=0) +
  geom_smooth(method="lm") +
  labs(title = paste("Adj R2 = ",signif(summary(model)$adj.r.squared, 5),
                     "Intercept =",signif(model$coef[[1]],5 ),
                     " Slope =",signif(model$coef[[2]], 5),
                     " P =",signif(summary(model)$coef[2,4], 5))) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  #scale_alpha(range = c(.3,1)) +
  #theme(legend.position="top") +
  ylab("Item mean contradictoriness rating") +
  xlab("Item mean inference rating") #+
  #theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
  ggsave("../graphs/mean-inference-by-mean-contradictoriness.pdf",height=4,width=4)

# load contradictoriness item means
cItemMeans = read.csv("../../2-veridicality2/data/veridicality_item_means.csv")
colnames(cItemMeans) = c("item","ContradictorinessItemMean","ContradictorinessItemCILow","ContradictorinessItemCIHigh")
head(cItemMeans)
nrow(cItemMeans) #400 items (20 verbs x 20 complement clauses)

# create item for inference strength data
t$item <- paste(t$verb,t$content,sep="-")
table(t$item)

# merge contradictoriness item means into target data
t = merge(t,cItemMeans,by=c("item"))
head(t)
nrow(t)
names(t)
table(t$item)

means = t %>%
  group_by(item, ContradictorinessMean) %>%
  summarize(InferenceMean = mean(response), InferenceCILow = ci.low(response), InferenceCIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(InferenceYMin = InferenceMean - InferenceCILow, InferenceYMax = InferenceMean + InferenceCIHigh, Item = fct_reorder(item,InferenceMean))
View(means)

model <- lm(ContradictorinessMean ~ InferenceMean, data = means)
summary(model)

ggplot(means, aes(x=InferenceMean, y=ContradictorinessMean)) + 
  geom_point() +
  #geom_text(aes(label=item),hjust=0,vjust=0) +
  # geom_smooth(method="lm") +
  geom_smooth() +
  labs(title = paste("Adj R2 = ",signif(summary(model)$adj.r.squared, 5),
                     "Intercept =",signif(model$coef[[1]],5 ),
                     " Slope =",signif(model$coef[[2]], 5),
                     " P =",signif(summary(model)$coef[2,4], 5))) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  #scale_alpha(range = c(.3,1)) +
  #theme(legend.position="top") +
  ylab("Item mean contradictoriness rating") +
  xlab("Item mean inference rating") #+
  #theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/by-item-mean-inference-by-mean-contradictoriness.pdf",height=4,width=9)

# restricted spline analysis
library(rms)
nrow(means)
means$Predicate = sapply(strsplit(as.character(means$Item),"-"), "[", 1)
means$Content = sapply(strsplit(as.character(means$Item),"-"), "[", 2)

m.0 = lmer(ContradictorinessMean ~ InferenceMean + (1|Predicate) + (1|Content), data=means)
m.1 = lmer(ContradictorinessMean ~ rcs(InferenceMean, 3) + (1|Predicate) + (1|Content), data=means)
m.2 = lmer(ContradictorinessMean ~ rcs(InferenceMean, 4) + (1|Predicate) + (1|Content), data=means)
summary(m.0)
summary(m.1)
summary(m.2)
anova(m.0,m.1)
anova(m.1,m.2)

m.0 = lm(ContradictorinessMean ~ InferenceMean, data=means)
m.1 = lm(ContradictorinessMean ~ rcs(InferenceMean, 3), data=means)
m.2 = lm(ContradictorinessMean ~ rcs(InferenceMean, 4), data=means)
summary(m.0)
summary(m.1)
summary(m.2)
anova(m.0,m.1)
anova(m.1,m.2)

means$DataType = "empirical"
tmp = data.frame(item=means$item,InferenceMean=means$InferenceMean,ContradictorinessMean=fitted(m.1),DataType="fitted_3knots")
bla = bind_rows(means,tmp)
tmp = data.frame(item=means$item,InferenceMean=means$InferenceMean,ContradictorinessMean=fitted(m.2),DataType="fitted_4knots")
bla = bind_rows(bla,tmp)
tmp = data.frame(item=means$item,InferenceMean=means$InferenceMean,ContradictorinessMean=fitted(m.0),DataType="fitted_linear")
bla = bind_rows(bla,tmp)

ggplot(bla, aes(x=InferenceMean, y=ContradictorinessMean,color=DataType)) + 
  geom_point() +
  #geom_text(aes(label=item),hjust=0,vjust=0) +
  # geom_smooth(method="lm") +
  geom_smooth() +
  labs(title = paste("Adj R2 = ",signif(summary(model)$adj.r.squared, 5),
                     "Intercept =",signif(model$coef[[1]],5 ),
                     " Slope =",signif(model$coef[[2]], 5),
                     " P =",signif(summary(model)$coef[2,4], 5))) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  #scale_alpha(range = c(.3,1)) +
  #theme(legend.position="top") +
  ylab("Item mean contradictoriness rating") +
  xlab("Item mean inference rating") #+
ggsave("../graphs/by-item-mean-inference-by-mean-contradictoriness-predictions.pdf",height=4,width=9)

# JD CODE STARTS HERE ----
# TL;DR: XXX
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")

# Bayesian mixed effects regression to test whether ratings differ by predicate from good controls
cd$workerid = as.factor(as.character(cd$workerid))
cd$item = as.factor(paste(as.character(cd$verb),as.character(cd$content)))
cd$content = as.factor(as.character(cd$content))
cd$isEntailing = cd$verb == "entailing C"
cd$isZeroOne = (cd$response == 0 | cd$response == 1)

# plotting slider ratings suggests we should use a zoib model
ggplot(cd, aes(x=response,fill=isEntailing)) +
  geom_histogram()

p = ggplot(cd, aes(x=response,fill=isEntailing)) +
  geom_histogram() +
  facet_wrap(~workerid)
ggsave(p, file="../graphs/subject_variability.pdf",width=25,height=25)

# set reference level to entailing controls
d = cd %>%
  droplevels() %>%
  mutate(verb = fct_relevel(verb,"entailing C"))
table(d$verb)

# run beta regression instead of zoib

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y' = (y · (n − 1) + 0.5)/n
d$betaresponse = (d$response*(nrow(d)-1) + .5)/nrow(d)

prior = get_prior(betaresponse ~ verb + (1|workerid) + (1|item),family = Beta(),data=d)

betamodel = bf(betaresponse ~ verb + (1|workerid) + (1|item),
               phi ~ verb + (1|workerid) + (1|item), # beta distribution's precision  )
               family = Beta())

m.b = brm(formula = betamodel,
        family=Beta(),
        data=d, 
        cores = 4,
        control = list(adapt_delta = .95,max_treedepth=15))

summary(m.b)

# intercept (entailing control) values
plogis(3)
exp(2.24)

# "prove" values
plogis(3 - .08)
exp(2.24 - .13)

# "confirm" values
plogis(3 - .22)
exp(2.24)

# "hear" values
plogis(3 - 2.92)
exp(2.24 - 2.05)

# "announce" values
plogis(3 - 1.51)
exp(2.24 - 1.35)

# "suggest" values
plogis(3 - 3.47)
exp(2.24 - 1.90)

# "pretend" values
plogis(3 - 4.47)
exp(2.24 - 2.13)

# visualize some of the inferred beta distributions
betadist = data.frame(x=rep(seq(0,1,by=.01),7),y=c(dprop(x=seq(0,1,by=.01),9.39,.953),dprop(x=seq(0,1,by=.01),8.25,.949),dprop(x=seq(0,1,by=.01),9.39,.941),dprop(x=seq(0,1,by=.01),1.21,.52),dprop(x=seq(0,1,by=.01),2.44,.816),dprop(x=seq(0,1,by=.01),1.4,.385),dprop(x=seq(0,1,by=.01),1.12,.187)),verb=c(rep("entailing",101),rep("prove",101),rep("confirm",101),rep("hear",101),rep("announce",101),rep("suggest",101), rep("pretend",101)))

ggplot(betadist, aes(x=x,y=y,color=verb)) +
  geom_line()

saveRDS(m.b,file="../data/beta-model-mixed.rds")

# to load model
m.b = readRDS(file="../data/beta-model-mixed.rds")

summary(m.b)
fixef(m.b) # does the same thing

# create LaTeX table
mcmcReg(m.b, pars = "b_", file="../models/brm_output.tex")

# to get stan code
stancode(m.b)

# hypothesis-testing, probing posterior model
h <- c("entailing - know" = "plogis(Intercept - verbknow) = plogis(Intercept)")
hypothesis(m.b, h) 

# hypothesis-testing, probing posterior model
q = c(q_entailing_know = "plogis(Intercept - verbknow) = plogis(Intercept)")
q_answer = hypothesis(m.b, q)
q_answer
plot(q_answer)
prop.table(table(q_answer$samples$H1 > 0)) # prob (know < entailing) = 1


# THE FOLLOWING MODEL SIMPLY WILL NOT CONVERGE, CAN IGNORE ALL COMMENTED OUT CODE, BUT LEAVING IN FOR THE TIME BEING
# # zoib model with random effects
# zoib_model <- bf(
#   response ~ verb + (1|workerid) + (1|item), # beta distribution's mean
#   zoi ~ verb + (1|workerid) + (1|item), # zero-one inflation (alpha); ie, probability of a binary rating as a function of verb
#   phi ~ verb + (1|workerid) + (1|item), # beta distribution's precision  
#   coi ~ verb + (1|workerid) + (1|item), # conditional one-inflation
#   family = zero_one_inflated_beta()
# ) 
# 
# # inspect default priors
# prior = get_prior(response ~ verb + (1|workerid) + (1|item),family = zero_one_inflated_beta(),data=d)
# 
# # default priors:
# # Intercept: student_t(3, 0, 10)                              
# # phi: gamma(0.01, 0.01)         
# # zoi: beta(1, 1)       
# # coi: beta(1, 1)                                          
# # sd: student_t(3, 0, 10)         
# 
# # fit model
# m <- brm(
#   formula = zoib_model,
#   prior = c(
#     set_prior("normal(0, 10)", class = "b")#,
#     # set_prior("normal(0, 10)", class = "b_phi")# continue here
#   ),
#   data = d,
#   cores = 4,
#   control = list(adapt_delta = .95,max_treedepth=15)#,
#   # sample_prior="only"
#   # file = here::here("zoib-ex")
# )
# # no need to run this multiple times:
# saveRDS(m,file="../data/zoib-model-mixed.rds")
# 
# 
# # load ZOIB model ----
# m <- readRDS(file="../data/zoib-model-mixed.rds")
# 
# summary(m) # see summary printed below
# stancode(m)
# plot(m)
# 
# # transform each of the posterior samples, and then re-calculate the summaries on original scale
# posterior_samples(m, pars = "b_")[,1:4] %>% 
#   mutate_at(c("b_phi_Intercept"), exp) %>% 
#   mutate_at(vars(-"b_phi_Intercept"), plogis) %>% 
#   posterior_summary() %>% 
#   as.data.frame() %>% 
#   rownames_to_column("Parameter") %>% 
#   kable(digits = 2) 
# 
# 
# # mean, zoi, and coi parameters are modeled through a logit link function; phi is modeled through a log link function
# # zoi & coi & b_intercept: [-15,15]
# # 
# 
# # |Parameter       | Estimate| Est.Error|  Q2.5| Q97.5|
# #   |:---------------|--------:|---------:|-----:|-----:|
# #   |b_Intercept     |     0.94|      0.00|  0.93|  0.94|
# #   |b_phi_Intercept |    14.30|      0.87| 12.63| 16.03|
# #   |b_zoi_Intercept |     0.37|      0.02|  0.34|  0.40|
# #   |b_coi_Intercept |     1.00|      0.00|  1.00|  1.00|
# 
# # Intercept                2.76      0.07       2.65      2.90
# # phi_Intercept            3.19      0.10       2.99      3.38
# # zoi_Intercept           -1.16      0.25      -1.61     -0.67
# # coi_Intercept         5757.12   2203.45    2822.64  11254.22
# 
# 
# # The .94 and 14.3 values are the mean and precision of the beta distribution that characterizes the bad controls that are not zeroes and ones -- this is a distribution heavily skewed towards 1
# # The .37 value is the probability that an observation will be either 0 or 1, and of these 37% endpoint values, 100% (last value) are ones. So: as expected, the bad controls are heavily 1-skewed, see also this histogram:
# ggplot(d[d$verb=="control_good",], aes(x=response)) +
#   geom_histogram()
# 
# # in principle, we can ask for each verb whether it differs from the bad controls, as follows:
# h <- c("prove - control_good" = "plogis(Intercept + verbprove) = plogis(Intercept)")
# hypothesis(m, h) # no diff for "prove"
# h <- c("be_right_that - control_good" = "plogis(Intercept + verbbe_right_that) = plogis(Intercept)")
# hypothesis(m, h) # diff for "be right"
# h <- c("see - control_good" = "plogis(Intercept + verbsee) = plogis(Intercept)")
# hypothesis(m, h) # diff for "see"
# h <- c("acknowledge - control_good" = "plogis(Intercept + verbacknowledge) = plogis(Intercept)")
# hypothesis(m, h) # diff for "acknowledge"
# 
# # plot estimated mu parameter
# plot(
#   marginal_effects(m, dpar = "mu"), 
#   points = TRUE, 
#   point_args = list(width = .05, shape = 1)
# )
# 
# # undrestand subject variability in zero/one inflation:
# length(unique(cd$workerid)) #259 subjects total
# zeroone = cd %>%
#   group_by(workerid,isZeroOne) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n)) %>%
#   ungroup() %>%
#   arrange(workerid,isZeroOne) %>%
#   filter(isZeroOne == T) 
# nrow(zeroone) #191 with at least one 0/1, ie, 259-191 = 68 people never gave an endpoint judgment. hacky way of adding these people back in for plotting:
# nozoi = data.frame(workerid=as.factor(seq(500,567,by=1)),isZeroOne=rep(TRUE,68),n = rep(0,68), freq = rep(0,68))
# zeroone = zeroone %>%
#   bind_rows(nozoi) %>%
#   mutate(workerid = fct_reorder(workerid,freq))
# 
# mean(zeroone$freq)
# median(zeroone$freq)
# # plot prop end point judgments by worker
# ggplot(zeroone, aes(x=workerid,y=freq)) +
#   geom_point()
# 

# fit linear model to compare to zoib/beta model -- same qualitative result (except for "know", which it also thinks is not different from entailing control)
summary(d %>% select(response,verb,workerid,item))
str(d %>% select(response,verb,workerid,item))

m <- brm(
  formula = response ~ verb + (1|workerid) + (1|item),
  data = d,
  cores = 4,
  control = list(adapt_delta = .95)
  # file = here::here("zoib-ex")
)
# predicates that don't come out different from entailing controls:
# see, prove, know, discover, confirm, be right
summary(m) # see summary printed below

# no need to run this multiple times:
#saveRDS(m,file="../data/linear-model-mixed.rds")

# load linear model ----
m <- readRDS(file="../data/linear-model-mixed.rds")



# let's look at pretend in particular
q = c(q_pretend_MC_mean = "Intercept + verbpretend = Intercept")
q_answer = hypothesis(m, q)
q_answer
plot(q_answer)
prop.table(table(q_answer$samples$H1 > 0)) # prob (pretend > MC) = .97
