# Prior probability work
# 2-veridicality: contradictoriness ratings

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
theme_set(theme_bw())

# load raw data
d = read.csv("../experiment.csv")
nrow(d) #8400 = 300 participants x 28 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #5.6
median(d$Answer.time_in_minutes) #4.7

d = d %>%
  select(workerid,rt,subjectGender,speakerGender,content,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,american,gender,comments,Answer.time_in_minutes)
nrow(d) #8400

# look at Turkers' comments
unique(d$comments)

# age and gender info
length(which(is.na(d$age))) #56 missing values, i.e., 2 Turkers didn't provide information
table(d$age) #19-73
median(d$age,na.rm=TRUE) #35 (of the 298 Turkers that provide age information)
table(d$gender)
#127 female, 169 male, 2 other, 2 didn't provide info

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "Chinese " & d$language != "Russian" & d$language != "Russian " & d$language != "Telugu"))
d = droplevels(d)
length(unique(d$workerid)) #296 (4 Turkers excluded)

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #286 (10 Turkers excluded, 14 excluded in total for language reasons)

## exclude Turkers based on fillers
names(d)
table(d$contentNr)
table(d$verb)

# make relevant data subsets
# control_bad: contradictory controls (YES = 1)
c.bad <- subset(d, d$verb == "control_bad")
c.bad <- droplevels(c.bad)
nrow(c.bad) #1144 / 4 contradictory controls = 286 Turkers

# control_good: noncontradictory controls (NO = 0)
c.good <- subset(d, d$verb == "control_good")
c.good <- droplevels(c.good)
nrow(c.good) #1144

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2288

# group mean on contradictory controls
round(mean(c.bad$response),2) #.92

# group mean on noncontradictory controls
round(mean(c.good$response),2) #.15

ggplot(c.bad, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.bad$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")

ggplot(c.good, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.good$response, n = 10)) +
  ylab("Response") +
  xlab("Participant")

# group means on individual controls
means = aggregate(response ~ contentNr, data=c.bad, FUN="mean")
means
# contentNr  response
# 1 control_bad1 0.9031119
# 2 control_bad2 0.9202448
# 3 control_bad3 0.9134615
# 4 control_bad4 0.9456294

means = aggregate(response ~ contentNr, data=c.good, FUN="mean")
means
# contentNr   response
# 1 control_good1 0.36003497 "Zack believes that I'm married, but I'm actually single"
# 2 control_good2 0.06311189
# 3 control_good3 0.07227273
# 4 control_good4 0.09209790

# as also suggested in the comments, some participants weren't clear on how to interpret "contradictory"
# and may have assumed that the speaker was contradicting Zack in control_good1

#################################################################
##### considering only 3 of the 4 noncontradictory controls #####
##### first analysis, see footnote in paper
c.good2 <- subset(d, d$contentNr == "control_good2" | d$contentNr == "control_good3" | d$contentNr == "control_good4")
c.good2 <- droplevels(c.good2)
nrow(c.good2) #858 / 286 participants = 3 noncontradictory controls
# group mean on 3 noncontradictory controls
round(mean(c.good2$response),2) #.08
# if c.good2 is used instead of c.good
# 11 Turkers (instead of 6) gave too high responses
# the 11 Turkers mean response to the noncontradictory controls: .66
# 11 Turkers total excluded, instead of 15
#################################################################

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2288 / 8 items = 286

# Turkers with response means on noncontradictory controls (0) more than 3sd above group mean
cg.means = aggregate(response~workerid, data=c.good, FUN="mean")
cg.means$YMin = cg.means$response - aggregate(response~workerid, data=c.good, FUN="ci.low")$response
cg.means$YMax = cg.means$response + aggregate(response~workerid, data=c.good, FUN="ci.high")$response
cg.means

c.g <- cg.means[cg.means$response > (mean(cg.means$response) + 3*sd(cg.means$response)),]
c.g
unique(length(c.g$workerid)) #6 Turkers gave high responses
mean(c.g$response) #.76

# Turkers with response means on contradictory controls (1) more than 3sd below group mean
cb.means = aggregate(response~workerid, data=c.bad, FUN="mean")
cb.means$YMin = cb.means$response - aggregate(response~workerid, data=c.bad, FUN="ci.low")$response
cb.means$YMax = cb.means$response + aggregate(response~workerid, data=c.bad, FUN="ci.high")$response
cb.means

c.b <- cb.means[cb.means$response < (mean(cb.means$response) - 3*sd(cb.means$response)),]
c.b
unique(length(c.b$workerid)) #7 Turkers gave low responses
mean(c.b$response) #.35

# how many unique Turkers did badly on the controls?
outliers <- subset(c, c$workerid %in% c.g$workerid | c$workerid %in% c.b$workerid)
outliers = droplevels(outliers)
nrow(outliers) #88 / 8 control items = 11 Turkers
table(outliers$workerid)

# look at the responses to the controls that these "outlier" Turkers did
# outliers to noncontradictory items (0)
outliers.g <- subset(c.good, c.good$workerid %in% c.g$workerid)
outliers.g = droplevels(outliers.g)
nrow(outliers.g) #24 / 4 control items = 6 Turkers
table(outliers.g$workerid)

ggplot(outliers.g, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.g$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be low (noncontradictory) but these 6 Turkers
# gave consistently high responses across the control items

# outliers to contradictory items (1)
outliers.b <- subset(c.bad, c.bad$workerid %in% c.b$workerid)
outliers.b = droplevels(outliers.b)
nrow(outliers.b) #28 / 4 control items = 7 Turkers
table(outliers.b$workerid)

ggplot(outliers.b, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.b$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be high (contradictory) but these 7 Turkers
# gave consistently low responses across the control items 

# exclude the 11 Turkers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #275 Turkers remain (186 - 11)

# clean data = cd
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #7700 / 28 items = 275 participants

# load clean data for analysis
cd = read.csv("../data/cd.csv")
nrow(cd) #7700

# age info
table(cd$age) #19-73
length(which(is.na(cd$age))) #28 missing values
median(cd$age,na.rm=TRUE) #35 (of the 270 Turkers that provide age information)
table(cd$gender)
#119 female, 153 male, 2 other, 1 didn't provide info

# target data (20 items per Turker)
names(cd)
table(cd$verb)
t <- subset(cd, cd$verb != "control_good" & cd$verb != "control_bad")
t <- droplevels(t)
nrow(t) #5500 / 20 = 275 Turkers
table(t$verb,t$content)

# mean veridicality of the verbs
means = aggregate(response~verb, data=t, FUN="mean")
means$YMin = means$response - aggregate(response~verb, data=t, FUN="ci.low")$response
means$YMax = means$response + aggregate(response~verb, data=t, FUN="ci.high")$response
means

t$verb <-factor(t$verb, levels=means[order(means$response), "verb"])

ggplot(t, aes(x=verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Predicate")
ggsave(f="../graphs/boxplot-veridicality.pdf",height=4,width=8)

agr_verb = t %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

agr_subj = t %>%
  group_by(content, verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

ggplot(agr_verb, aes(x=verb, y=Mean)) + 
  geom_point(color="black", size=4) +
  geom_point(data=agr_subj, aes(color=content)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Predicate")
ggsave("graphs/veridicality-means-byitem.pdf",height=4,width=8)

agr_content = t %>%
  group_by(content) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

ggplot(agr_content, aes(x=content, y=Mean)) + 
  geom_point(color="black", size=4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Content")
ggsave("graphs/veridicality-means-bycontent.pdf",height=8,width=6)

# veridicality rating by participant
variances = t %>%
  group_by(workerid) %>%
  summarise(VeriVar = var(response),
            VeriMean=mean(response),
            Veri.ci.low=ci.low(response),
            Veri.ci.high=ci.high(response))

variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,VeriMean),y=VeriMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=VeriMean-Veri.ci.low,ymax=VeriMean+Veri.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Mean veridicality rating")
ggsave("graphs/veridicality-subjmeans.pdf",height=3,width=6.5)

## pairwise comparison to see which predicates differ from one another
library(lsmeans)
library(lme4)
str(t$response)
str(t$verb)
str(t$workerid)
t$workerid <- as.factor(t$workerid)
model = lmer(response ~ verb + (1|workerid), data=t, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# $contrasts
# contrast                        estimate         SE   df t.ratio p.value
# pretend - suggest           -0.009127273 0.01916785 5225  -0.476  1.0000
# pretend - think             -0.013381818 0.01916785 5225  -0.698  1.0000
# pretend - hear              -0.020181818 0.01916785 5225  -1.053  1.0000
# pretend - say               -0.086945455 0.01916785 5225  -4.536  0.0010
# pretend - inform_Sam        -0.106145455 0.01916785 5225  -5.538  <.0001
# pretend - announce          -0.117709091 0.01916785 5225  -6.141  <.0001
# pretend - annoyed           -0.174945455 0.01916785 5225  -9.127  <.0001
# pretend - confess           -0.179090909 0.01916785 5225  -9.343  <.0001
# pretend - reveal            -0.192254545 0.01916785 5225 -10.030  <.0001
# pretend - acknowledge       -0.197418182 0.01916785 5225 -10.299  <.0001
# pretend - admit             -0.199381818 0.01916785 5225 -10.402  <.0001
# pretend - establish         -0.224254545 0.01916785 5225 -11.700  <.0001
# pretend - demonstrate       -0.230654545 0.01916785 5225 -12.033  <.0001
# pretend - discover          -0.256254545 0.01916785 5225 -13.369  <.0001
# pretend - confirm           -0.272327273 0.01916785 5225 -14.208  <.0001
# pretend - see               -0.274400000 0.01916785 5225 -14.316  <.0001
# pretend - know              -0.303527273 0.01916785 5225 -15.835  <.0001
# pretend - prove             -0.339272727 0.01916785 5225 -17.700  <.0001
# pretend - be_right_that     -0.440218182 0.01916785 5225 -22.966  <.0001
# suggest - think             -0.004254545 0.01916785 5225  -0.222  1.0000
# suggest - hear              -0.011054545 0.01916785 5225  -0.577  1.0000
# suggest - say               -0.077818182 0.01916785 5225  -4.060  0.0077
# suggest - inform_Sam        -0.097018182 0.01916785 5225  -5.062  0.0001
# suggest - announce          -0.108581818 0.01916785 5225  -5.665  <.0001
# suggest - annoyed           -0.165818182 0.01916785 5225  -8.651  <.0001
# suggest - confess           -0.169963636 0.01916785 5225  -8.867  <.0001
# suggest - reveal            -0.183127273 0.01916785 5225  -9.554  <.0001
# suggest - acknowledge       -0.188290909 0.01916785 5225  -9.823  <.0001
# suggest - admit             -0.190254545 0.01916785 5225  -9.926  <.0001
# suggest - establish         -0.215127273 0.01916785 5225 -11.223  <.0001
# suggest - demonstrate       -0.221527273 0.01916785 5225 -11.557  <.0001
# suggest - discover          -0.247127273 0.01916785 5225 -12.893  <.0001
# suggest - confirm           -0.263200000 0.01916785 5225 -13.731  <.0001
# suggest - see               -0.265272727 0.01916785 5225 -13.839  <.0001
# suggest - know              -0.294400000 0.01916785 5225 -15.359  <.0001
# suggest - prove             -0.330145455 0.01916785 5225 -17.224  <.0001
# suggest - be_right_that     -0.431090909 0.01916785 5225 -22.490  <.0001
# think - hear                -0.006800000 0.01916785 5225  -0.355  1.0000
# think - say                 -0.073563636 0.01916785 5225  -3.838  0.0181
# think - inform_Sam          -0.092763636 0.01916785 5225  -4.840  0.0002
# think - announce            -0.104327273 0.01916785 5225  -5.443  <.0001
# think - annoyed             -0.161563636 0.01916785 5225  -8.429  <.0001
# think - confess             -0.165709091 0.01916785 5225  -8.645  <.0001
# think - reveal              -0.178872727 0.01916785 5225  -9.332  <.0001
# think - acknowledge         -0.184036364 0.01916785 5225  -9.601  <.0001
# think - admit               -0.186000000 0.01916785 5225  -9.704  <.0001
# think - establish           -0.210872727 0.01916785 5225 -11.001  <.0001
# think - demonstrate         -0.217272727 0.01916785 5225 -11.335  <.0001
# think - discover            -0.242872727 0.01916785 5225 -12.671  <.0001
# think - confirm             -0.258945455 0.01916785 5225 -13.509  <.0001
# think - see                 -0.261018182 0.01916785 5225 -13.618  <.0001
# think - know                -0.290145455 0.01916785 5225 -15.137  <.0001
# think - prove               -0.325890909 0.01916785 5225 -17.002  <.0001
# think - be_right_that       -0.426836364 0.01916785 5225 -22.268  <.0001
# hear - say                  -0.066763636 0.01916785 5225  -3.483  0.0611
# hear - inform_Sam           -0.085963636 0.01916785 5225  -4.485  0.0013
# hear - announce             -0.097527273 0.01916785 5225  -5.088  0.0001
# hear - annoyed              -0.154763636 0.01916785 5225  -8.074  <.0001
# hear - confess              -0.158909091 0.01916785 5225  -8.290  <.0001
# hear - reveal               -0.172072727 0.01916785 5225  -8.977  <.0001
# hear - acknowledge          -0.177236364 0.01916785 5225  -9.247  <.0001
# hear - admit                -0.179200000 0.01916785 5225  -9.349  <.0001
# hear - establish            -0.204072727 0.01916785 5225 -10.647  <.0001
# hear - demonstrate          -0.210472727 0.01916785 5225 -10.981  <.0001
# hear - discover             -0.236072727 0.01916785 5225 -12.316  <.0001
# hear - confirm              -0.252145455 0.01916785 5225 -13.155  <.0001
# hear - see                  -0.254218182 0.01916785 5225 -13.263  <.0001
# hear - know                 -0.283345455 0.01916785 5225 -14.782  <.0001
# hear - prove                -0.319090909 0.01916785 5225 -16.647  <.0001
# hear - be_right_that        -0.420036364 0.01916785 5225 -21.914  <.0001
# say - inform_Sam            -0.019200000 0.01916785 5225  -1.002  1.0000
# say - announce              -0.030763636 0.01916785 5225  -1.605  0.9891
# say - annoyed               -0.088000000 0.01916785 5225  -4.591  0.0008
# say - confess               -0.092145455 0.01916785 5225  -4.807  0.0003
# say - reveal                -0.105309091 0.01916785 5225  -5.494  <.0001
# say - acknowledge           -0.110472727 0.01916785 5225  -5.763  <.0001
# say - admit                 -0.112436364 0.01916785 5225  -5.866  <.0001
# say - establish             -0.137309091 0.01916785 5225  -7.164  <.0001
# say - demonstrate           -0.143709091 0.01916785 5225  -7.497  <.0001
# say - discover              -0.169309091 0.01916785 5225  -8.833  <.0001
# say - confirm               -0.185381818 0.01916785 5225  -9.671  <.0001
# say - see                   -0.187454545 0.01916785 5225  -9.780  <.0001
# say - know                  -0.216581818 0.01916785 5225 -11.299  <.0001
# say - prove                 -0.252327273 0.01916785 5225 -13.164  <.0001
# say - be_right_that         -0.353272727 0.01916785 5225 -18.430  <.0001
# inform_Sam - announce       -0.011563636 0.01916785 5225  -0.603  1.0000
# inform_Sam - annoyed        -0.068800000 0.01916785 5225  -3.589  0.0433
# inform_Sam - confess        -0.072945455 0.01916785 5225  -3.806  0.0204
# inform_Sam - reveal         -0.086109091 0.01916785 5225  -4.492  0.0012
# inform_Sam - acknowledge    -0.091272727 0.01916785 5225  -4.762  0.0003
# inform_Sam - admit          -0.093236364 0.01916785 5225  -4.864  0.0002
# inform_Sam - establish      -0.118109091 0.01916785 5225  -6.162  <.0001
# inform_Sam - demonstrate    -0.124509091 0.01916785 5225  -6.496  <.0001
# inform_Sam - discover       -0.150109091 0.01916785 5225  -7.831  <.0001
# inform_Sam - confirm        -0.166181818 0.01916785 5225  -8.670  <.0001
# inform_Sam - see            -0.168254545 0.01916785 5225  -8.778  <.0001
# inform_Sam - know           -0.197381818 0.01916785 5225 -10.298  <.0001
# inform_Sam - prove          -0.233127273 0.01916785 5225 -12.162  <.0001
# inform_Sam - be_right_that  -0.334072727 0.01916785 5225 -17.429  <.0001
# announce - annoyed          -0.057236364 0.01916785 5225  -2.986  0.2394
# announce - confess          -0.061381818 0.01916785 5225  -3.202  0.1392
# announce - reveal           -0.074545455 0.01916785 5225  -3.889  0.0150
# announce - acknowledge      -0.079709091 0.01916785 5225  -4.158  0.0052
# announce - admit            -0.081672727 0.01916785 5225  -4.261  0.0034
# announce - establish        -0.106545455 0.01916785 5225  -5.559  <.0001
# announce - demonstrate      -0.112945455 0.01916785 5225  -5.892  <.0001
# announce - discover         -0.138545455 0.01916785 5225  -7.228  <.0001
# announce - confirm          -0.154618182 0.01916785 5225  -8.067  <.0001
# announce - see              -0.156690909 0.01916785 5225  -8.175  <.0001
# announce - know             -0.185818182 0.01916785 5225  -9.694  <.0001
# announce - prove            -0.221563636 0.01916785 5225 -11.559  <.0001
# announce - be_right_that    -0.322509091 0.01916785 5225 -16.826  <.0001
# annoyed - confess           -0.004145455 0.01916785 5225  -0.216  1.0000
# annoyed - reveal            -0.017309091 0.01916785 5225  -0.903  1.0000
# annoyed - acknowledge       -0.022472727 0.01916785 5225  -1.172  0.9998
# annoyed - admit             -0.024436364 0.01916785 5225  -1.275  0.9994
# annoyed - establish         -0.049309091 0.01916785 5225  -2.572  0.5267
# annoyed - demonstrate       -0.055709091 0.01916785 5225  -2.906  0.2860
# annoyed - discover          -0.081309091 0.01916785 5225  -4.242  0.0037
# annoyed - confirm           -0.097381818 0.01916785 5225  -5.080  0.0001
# annoyed - see               -0.099454545 0.01916785 5225  -5.189  <.0001
# annoyed - know              -0.128581818 0.01916785 5225  -6.708  <.0001
# annoyed - prove             -0.164327273 0.01916785 5225  -8.573  <.0001
# annoyed - be_right_that     -0.265272727 0.01916785 5225 -13.839  <.0001
# confess - reveal            -0.013163636 0.01916785 5225  -0.687  1.0000
# confess - acknowledge       -0.018327273 0.01916785 5225  -0.956  1.0000
# confess - admit             -0.020290909 0.01916785 5225  -1.059  1.0000
# confess - establish         -0.045163636 0.01916785 5225  -2.356  0.6943
# confess - demonstrate       -0.051563636 0.01916785 5225  -2.690  0.4357
# confess - discover          -0.077163636 0.01916785 5225  -4.026  0.0089
# confess - confirm           -0.093236364 0.01916785 5225  -4.864  0.0002
# confess - see               -0.095309091 0.01916785 5225  -4.972  0.0001
# confess - know              -0.124436364 0.01916785 5225  -6.492  <.0001
# confess - prove             -0.160181818 0.01916785 5225  -8.357  <.0001
# confess - be_right_that     -0.261127273 0.01916785 5225 -13.623  <.0001
# reveal - acknowledge        -0.005163636 0.01916785 5225  -0.269  1.0000
# reveal - admit              -0.007127273 0.01916785 5225  -0.372  1.0000
# reveal - establish          -0.032000000 0.01916785 5225  -1.669  0.9830
# reveal - demonstrate        -0.038400000 0.01916785 5225  -2.003  0.9017
# reveal - discover           -0.064000000 0.01916785 5225  -3.339  0.0948
# reveal - confirm            -0.080072727 0.01916785 5225  -4.177  0.0048
# reveal - see                -0.082145455 0.01916785 5225  -4.286  0.0030
# reveal - know               -0.111272727 0.01916785 5225  -5.805  <.0001
# reveal - prove              -0.147018182 0.01916785 5225  -7.670  <.0001
# reveal - be_right_that      -0.247963636 0.01916785 5225 -12.936  <.0001
# acknowledge - admit         -0.001963636 0.01916785 5225  -0.102  1.0000
# acknowledge - establish     -0.026836364 0.01916785 5225  -1.400  0.9979
# acknowledge - demonstrate   -0.033236364 0.01916785 5225  -1.734  0.9746
# acknowledge - discover      -0.058836364 0.01916785 5225  -3.070  0.1962
# acknowledge - confirm       -0.074909091 0.01916785 5225  -3.908  0.0139
# acknowledge - see           -0.076981818 0.01916785 5225  -4.016  0.0092
# acknowledge - know          -0.106109091 0.01916785 5225  -5.536  <.0001
# acknowledge - prove         -0.141854545 0.01916785 5225  -7.401  <.0001
# acknowledge - be_right_that -0.242800000 0.01916785 5225 -12.667  <.0001
# admit - establish           -0.024872727 0.01916785 5225  -1.298  0.9993
# admit - demonstrate         -0.031272727 0.01916785 5225  -1.632  0.9868
# admit - discover            -0.056872727 0.01916785 5225  -2.967  0.2500
# admit - confirm             -0.072945455 0.01916785 5225  -3.806  0.0204
# admit - see                 -0.075018182 0.01916785 5225  -3.914  0.0136
# admit - know                -0.104145455 0.01916785 5225  -5.433  <.0001
# admit - prove               -0.139890909 0.01916785 5225  -7.298  <.0001
# admit - be_right_that       -0.240836364 0.01916785 5225 -12.565  <.0001
# establish - demonstrate     -0.006400000 0.01916785 5225  -0.334  1.0000
# establish - discover        -0.032000000 0.01916785 5225  -1.669  0.9830
# establish - confirm         -0.048072727 0.01916785 5225  -2.508  0.5776
# establish - see             -0.050145455 0.01916785 5225  -2.616  0.4925
# establish - know            -0.079272727 0.01916785 5225  -4.136  0.0057
# establish - prove           -0.115018182 0.01916785 5225  -6.001  <.0001
# establish - be_right_that   -0.215963636 0.01916785 5225 -11.267  <.0001
# demonstrate - discover      -0.025600000 0.01916785 5225  -1.336  0.9989
# demonstrate - confirm       -0.041672727 0.01916785 5225  -2.174  0.8164
# demonstrate - see           -0.043745455 0.01916785 5225  -2.282  0.7471
# demonstrate - know          -0.072872727 0.01916785 5225  -3.802  0.0207
# demonstrate - prove         -0.108618182 0.01916785 5225  -5.667  <.0001
# demonstrate - be_right_that -0.209563636 0.01916785 5225 -10.933  <.0001
# discover - confirm          -0.016072727 0.01916785 5225  -0.839  1.0000
# discover - see              -0.018145455 0.01916785 5225  -0.947  1.0000
# discover - know             -0.047272727 0.01916785 5225  -2.466  0.6104
# discover - prove            -0.083018182 0.01916785 5225  -4.331  0.0025
# discover - be_right_that    -0.183963636 0.01916785 5225  -9.598  <.0001
# confirm - see               -0.002072727 0.01916785 5225  -0.108  1.0000
# confirm - know              -0.031200000 0.01916785 5225  -1.628  0.9872
# confirm - prove             -0.066945455 0.01916785 5225  -3.493  0.0593
# confirm - be_right_that     -0.167890909 0.01916785 5225  -8.759  <.0001
# see - know                  -0.029127273 0.01916785 5225  -1.520  0.9942
# see - prove                 -0.064872727 0.01916785 5225  -3.384  0.0828
# see - be_right_that         -0.165818182 0.01916785 5225  -8.651  <.0001
# know - prove                -0.035745455 0.01916785 5225  -1.865  0.9478
# know - be_right_that        -0.136690909 0.01916785 5225  -7.131  <.0001
# prove - be_right_that       -0.100945455 0.01916785 5225  -5.266  <.0001
# 
# P value adjustment: tukey method for comparing a family of 20 estimates 

## knowers
## The veridicality response task we used involved continuations with "...but I know that not p"
## because we assumed that "I know that not p" entails p. This assumption seems to be wrong for
## some participants for whom "know" is not entirely veridical. In the following, the analysis
## is restricted to participants for whom "know" entails the content of the complement.

cd = read.csv(file = "data/cd.csv")
nrow(cd) #7700 / 28 items = 275 participants

# know data (20 items per Turker)
table(cd$verb)
know <- subset(cd, cd$verb == "know")
know <- droplevels(know)
nrow(know) #275 (1 item per 275 Turkers)
table(know$content)

# Turkers responses to their "know" item
ggplot(know, aes(x=reorder(workerid,response),y=response)) +
  geom_point() +
  #stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  #geom_errorbar(aes(ymin=VeriMean-Veri.ci.low,ymax=VeriMean+Veri.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Mean veridicality rating")
ggsave("graphs/know-veridicality-by-subject.pdf",height=3,width=6.5)

# mean response on the contradictory controls was .92
# identify Turkers whose response to the "know" item was not lower than their mean response
# to the contradictory controls

# calculate mean response to 4 contradictory controls for each Turker
cd.contradict <- droplevels(subset(cd,cd$verb == "control_bad"))
nrow(cd.contradict) #1100 / 4 = 275

c.means = aggregate(response~workerid, data=cd.contradict, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=cd.contradict, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=cd.contradict, FUN="ci.high")$response
c.means
nrow(c.means) #275

c.know <- know[know$response >= c.means$response,]
c.know
c.know$workerid #80 Turkers
nrow(c.know) #80 Turkers

# target data is data from Turkers in c.know
t2 <- subset(t, t$workerid %in% c.know$workerid)
t2 = droplevels(t2)
nrow(t2) #1600 / 80 remaining Turkers = 20 items

# mean veridicality of the verbs
means = aggregate(response~verb, data=t2, FUN="mean")
means$YMin = means$response - aggregate(response~verb, data=t2, FUN="ci.low")$response
means$YMax = means$response + aggregate(response~verb, data=t2, FUN="ci.high")$response
means

t2$verb <-factor(t2$verb, levels=means[order(means$response), "verb"])

ggplot(t2, aes(x=verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Predicate")
ggsave(f="graphs/boxplot-veridicality-restricted.pdf",height=4,width=6.5)

# veridicality rating by participant
variances = t2 %>%
  group_by(workerid) %>%
  summarise(VeriVar = var(response),
            VeriMean=mean(response),
            Veri.ci.low=ci.low(response),
            Veri.ci.high=ci.high(response))

variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,VeriMean),y=VeriMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=VeriMean-Veri.ci.low,ymax=VeriMean+Veri.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Mean veridicality rating")
ggsave("graphs/veridicality-subjmeans-knowers.pdf",height=3,width=6.5)

## pairwise comparison to see which predicates differ from one another
str(t2$response)
str(t2$verb)
str(t2$workerid)
t2$workerid <- as.factor(t2$workerid)
model = lmer(response ~ verb + (1|workerid), data=t2, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# $contrasts
# contrast                     estimate         SE   df t.ratio p.value
# suggest - pretend           -0.001625 0.03818901 1520  -0.043  1.0000
# suggest - think             -0.015875 0.03818901 1520  -0.416  1.0000
# suggest - hear              -0.024250 0.03818901 1520  -0.635  1.0000
# suggest - say               -0.123125 0.03818901 1520  -3.224  0.1323
# suggest - inform_Sam        -0.131375 0.03818901 1520  -3.440  0.0708
# suggest - announce          -0.158250 0.03818901 1520  -4.144  0.0057
# suggest - annoyed           -0.190625 0.03818901 1520  -4.992  0.0001
# suggest - confess           -0.259125 0.03818901 1520  -6.785  <.0001
# suggest - reveal            -0.262625 0.03818901 1520  -6.877  <.0001
# suggest - admit             -0.265750 0.03818901 1520  -6.959  <.0001
# suggest - acknowledge       -0.279875 0.03818901 1520  -7.329  <.0001
# suggest - establish         -0.283750 0.03818901 1520  -7.430  <.0001
# suggest - demonstrate       -0.287500 0.03818901 1520  -7.528  <.0001
# suggest - see               -0.308250 0.03818901 1520  -8.072  <.0001
# suggest - discover          -0.344250 0.03818901 1520  -9.014  <.0001
# suggest - prove             -0.344500 0.03818901 1520  -9.021  <.0001
# suggest - confirm           -0.364750 0.03818901 1520  -9.551  <.0001
# suggest - be_right_that     -0.383250 0.03818901 1520 -10.036  <.0001
# suggest - know              -0.431375 0.03818901 1520 -11.296  <.0001
# pretend - think             -0.014250 0.03818901 1520  -0.373  1.0000
# pretend - hear              -0.022625 0.03818901 1520  -0.592  1.0000
# pretend - say               -0.121500 0.03818901 1520  -3.182  0.1483
# pretend - inform_Sam        -0.129750 0.03818901 1520  -3.398  0.0805
# pretend - announce          -0.156625 0.03818901 1520  -4.101  0.0068
# pretend - annoyed           -0.189000 0.03818901 1520  -4.949  0.0001
# pretend - confess           -0.257500 0.03818901 1520  -6.743  <.0001
# pretend - reveal            -0.261000 0.03818901 1520  -6.834  <.0001
# pretend - admit             -0.264125 0.03818901 1520  -6.916  <.0001
# pretend - acknowledge       -0.278250 0.03818901 1520  -7.286  <.0001
# pretend - establish         -0.282125 0.03818901 1520  -7.388  <.0001
# pretend - demonstrate       -0.285875 0.03818901 1520  -7.486  <.0001
# pretend - see               -0.306625 0.03818901 1520  -8.029  <.0001
# pretend - discover          -0.342625 0.03818901 1520  -8.972  <.0001
# pretend - prove             -0.342875 0.03818901 1520  -8.978  <.0001
# pretend - confirm           -0.363125 0.03818901 1520  -9.509  <.0001
# pretend - be_right_that     -0.381625 0.03818901 1520  -9.993  <.0001
# pretend - know              -0.429750 0.03818901 1520 -11.253  <.0001
# think - hear                -0.008375 0.03818901 1520  -0.219  1.0000
# think - say                 -0.107250 0.03818901 1520  -2.808  0.3509
# think - inform_Sam          -0.115500 0.03818901 1520  -3.024  0.2199
# think - announce            -0.142375 0.03818901 1520  -3.728  0.0274
# think - annoyed             -0.174750 0.03818901 1520  -4.576  0.0009
# think - confess             -0.243250 0.03818901 1520  -6.370  <.0001
# think - reveal              -0.246750 0.03818901 1520  -6.461  <.0001
# think - admit               -0.249875 0.03818901 1520  -6.543  <.0001
# think - acknowledge         -0.264000 0.03818901 1520  -6.913  <.0001
# think - establish           -0.267875 0.03818901 1520  -7.014  <.0001
# think - demonstrate         -0.271625 0.03818901 1520  -7.113  <.0001
# think - see                 -0.292375 0.03818901 1520  -7.656  <.0001
# think - discover            -0.328375 0.03818901 1520  -8.599  <.0001
# think - prove               -0.328625 0.03818901 1520  -8.605  <.0001
# think - confirm             -0.348875 0.03818901 1520  -9.135  <.0001
# think - be_right_that       -0.367375 0.03818901 1520  -9.620  <.0001
# think - know                -0.415500 0.03818901 1520 -10.880  <.0001
# hear - say                  -0.098875 0.03818901 1520  -2.589  0.5140
# hear - inform_Sam           -0.107125 0.03818901 1520  -2.805  0.3531
# hear - announce             -0.134000 0.03818901 1520  -3.509  0.0571
# hear - annoyed              -0.166375 0.03818901 1520  -4.357  0.0023
# hear - confess              -0.234875 0.03818901 1520  -6.150  <.0001
# hear - reveal               -0.238375 0.03818901 1520  -6.242  <.0001
# hear - admit                -0.241500 0.03818901 1520  -6.324  <.0001
# hear - acknowledge          -0.255625 0.03818901 1520  -6.694  <.0001
# hear - establish            -0.259500 0.03818901 1520  -6.795  <.0001
# hear - demonstrate          -0.263250 0.03818901 1520  -6.893  <.0001
# hear - see                  -0.284000 0.03818901 1520  -7.437  <.0001
# hear - discover             -0.320000 0.03818901 1520  -8.379  <.0001
# hear - prove                -0.320250 0.03818901 1520  -8.386  <.0001
# hear - confirm              -0.340500 0.03818901 1520  -8.916  <.0001
# hear - be_right_that        -0.359000 0.03818901 1520  -9.401  <.0001
# hear - know                 -0.407125 0.03818901 1520 -10.661  <.0001
# say - inform_Sam            -0.008250 0.03818901 1520  -0.216  1.0000
# say - announce              -0.035125 0.03818901 1520  -0.920  1.0000
# say - annoyed               -0.067500 0.03818901 1520  -1.768  0.9688
# say - confess               -0.136000 0.03818901 1520  -3.561  0.0482
# say - reveal                -0.139500 0.03818901 1520  -3.653  0.0356
# say - admit                 -0.142625 0.03818901 1520  -3.735  0.0268
# say - acknowledge           -0.156750 0.03818901 1520  -4.105  0.0067
# say - establish             -0.160625 0.03818901 1520  -4.206  0.0044
# say - demonstrate           -0.164375 0.03818901 1520  -4.304  0.0029
# say - see                   -0.185125 0.03818901 1520  -4.848  0.0002
# say - discover              -0.221125 0.03818901 1520  -5.790  <.0001
# say - prove                 -0.221375 0.03818901 1520  -5.797  <.0001
# say - confirm               -0.241625 0.03818901 1520  -6.327  <.0001
# say - be_right_that         -0.260125 0.03818901 1520  -6.812  <.0001
# say - know                  -0.308250 0.03818901 1520  -8.072  <.0001
# inform_Sam - announce       -0.026875 0.03818901 1520  -0.704  1.0000
# inform_Sam - annoyed        -0.059250 0.03818901 1520  -1.551  0.9925
# inform_Sam - confess        -0.127750 0.03818901 1520  -3.345  0.0940
# inform_Sam - reveal         -0.131250 0.03818901 1520  -3.437  0.0715
# inform_Sam - admit          -0.134375 0.03818901 1520  -3.519  0.0553
# inform_Sam - acknowledge    -0.148500 0.03818901 1520  -3.889  0.0154
# inform_Sam - establish      -0.152375 0.03818901 1520  -3.990  0.0105
# inform_Sam - demonstrate    -0.156125 0.03818901 1520  -4.088  0.0071
# inform_Sam - see            -0.176875 0.03818901 1520  -4.632  0.0007
# inform_Sam - discover       -0.212875 0.03818901 1520  -5.574  <.0001
# inform_Sam - prove          -0.213125 0.03818901 1520  -5.581  <.0001
# inform_Sam - confirm        -0.233375 0.03818901 1520  -6.111  <.0001
# inform_Sam - be_right_that  -0.251875 0.03818901 1520  -6.595  <.0001
# inform_Sam - know           -0.300000 0.03818901 1520  -7.856  <.0001
# announce - annoyed          -0.032375 0.03818901 1520  -0.848  1.0000
# announce - confess          -0.100875 0.03818901 1520  -2.641  0.4733
# announce - reveal           -0.104375 0.03818901 1520  -2.733  0.4043
# announce - admit            -0.107500 0.03818901 1520  -2.815  0.3464
# announce - acknowledge      -0.121625 0.03818901 1520  -3.185  0.1470
# announce - establish        -0.125500 0.03818901 1520  -3.286  0.1114
# announce - demonstrate      -0.129250 0.03818901 1520  -3.384  0.0837
# announce - see              -0.150000 0.03818901 1520  -3.928  0.0133
# announce - discover         -0.186000 0.03818901 1520  -4.871  0.0002
# announce - prove            -0.186250 0.03818901 1520  -4.877  0.0002
# announce - confirm          -0.206500 0.03818901 1520  -5.407  <.0001
# announce - be_right_that    -0.225000 0.03818901 1520  -5.892  <.0001
# announce - know             -0.273125 0.03818901 1520  -7.152  <.0001
# annoyed - confess           -0.068500 0.03818901 1520  -1.794  0.9638
# annoyed - reveal            -0.072000 0.03818901 1520  -1.885  0.9418
# annoyed - admit             -0.075125 0.03818901 1520  -1.967  0.9153
# annoyed - acknowledge       -0.089250 0.03818901 1520  -2.337  0.7081
# annoyed - establish         -0.093125 0.03818901 1520  -2.439  0.6320
# annoyed - demonstrate       -0.096875 0.03818901 1520  -2.537  0.5552
# annoyed - see               -0.117625 0.03818901 1520  -3.080  0.1922
# annoyed - discover          -0.153625 0.03818901 1520  -4.023  0.0092
# annoyed - prove             -0.153875 0.03818901 1520  -4.029  0.0090
# annoyed - confirm           -0.174125 0.03818901 1520  -4.560  0.0010
# annoyed - be_right_that     -0.192625 0.03818901 1520  -5.044  0.0001
# annoyed - know              -0.240750 0.03818901 1520  -6.304  <.0001
# confess - reveal            -0.003500 0.03818901 1520  -0.092  1.0000
# confess - admit             -0.006625 0.03818901 1520  -0.173  1.0000
# confess - acknowledge       -0.020750 0.03818901 1520  -0.543  1.0000
# confess - establish         -0.024625 0.03818901 1520  -0.645  1.0000
# confess - demonstrate       -0.028375 0.03818901 1520  -0.743  1.0000
# confess - see               -0.049125 0.03818901 1520  -1.286  0.9993
# confess - discover          -0.085125 0.03818901 1520  -2.229  0.7821
# confess - prove             -0.085375 0.03818901 1520  -2.236  0.7779
# confess - confirm           -0.105625 0.03818901 1520  -2.766  0.3807
# confess - be_right_that     -0.124125 0.03818901 1520  -3.250  0.1231
# confess - know              -0.172250 0.03818901 1520  -4.510  0.0012
# reveal - admit              -0.003125 0.03818901 1520  -0.082  1.0000
# reveal - acknowledge        -0.017250 0.03818901 1520  -0.452  1.0000
# reveal - establish          -0.021125 0.03818901 1520  -0.553  1.0000
# reveal - demonstrate        -0.024875 0.03818901 1520  -0.651  1.0000
# reveal - see                -0.045625 0.03818901 1520  -1.195  0.9998
# reveal - discover           -0.081625 0.03818901 1520  -2.137  0.8369
# reveal - prove              -0.081875 0.03818901 1520  -2.144  0.8333
# reveal - confirm            -0.102125 0.03818901 1520  -2.674  0.4483
# reveal - be_right_that      -0.120625 0.03818901 1520  -3.159  0.1575
# reveal - know               -0.168750 0.03818901 1520  -4.419  0.0018
# admit - acknowledge         -0.014125 0.03818901 1520  -0.370  1.0000
# admit - establish           -0.018000 0.03818901 1520  -0.471  1.0000
# admit - demonstrate         -0.021750 0.03818901 1520  -0.570  1.0000
# admit - see                 -0.042500 0.03818901 1520  -1.113  0.9999
# admit - discover            -0.078500 0.03818901 1520  -2.056  0.8785
# admit - prove               -0.078750 0.03818901 1520  -2.062  0.8755
# admit - confirm             -0.099000 0.03818901 1520  -2.592  0.5115
# admit - be_right_that       -0.117500 0.03818901 1520  -3.077  0.1938
# admit - know                -0.165625 0.03818901 1520  -4.337  0.0025
# acknowledge - establish     -0.003875 0.03818901 1520  -0.101  1.0000
# acknowledge - demonstrate   -0.007625 0.03818901 1520  -0.200  1.0000
# acknowledge - see           -0.028375 0.03818901 1520  -0.743  1.0000
# acknowledge - discover      -0.064375 0.03818901 1520  -1.686  0.9809
# acknowledge - prove         -0.064625 0.03818901 1520  -1.692  0.9801
# acknowledge - confirm       -0.084875 0.03818901 1520  -2.222  0.7863
# acknowledge - be_right_that -0.103375 0.03818901 1520  -2.707  0.4237
# acknowledge - know          -0.151500 0.03818901 1520  -3.967  0.0114
# establish - demonstrate     -0.003750 0.03818901 1520  -0.098  1.0000
# establish - see             -0.024500 0.03818901 1520  -0.642  1.0000
# establish - discover        -0.060500 0.03818901 1520  -1.584  0.9905
# establish - prove           -0.060750 0.03818901 1520  -1.591  0.9900
# establish - confirm         -0.081000 0.03818901 1520  -2.121  0.8458
# establish - be_right_that   -0.099500 0.03818901 1520  -2.605  0.5012
# establish - know            -0.147625 0.03818901 1520  -3.866  0.0167
# demonstrate - see           -0.020750 0.03818901 1520  -0.543  1.0000
# demonstrate - discover      -0.056750 0.03818901 1520  -1.486  0.9956
# demonstrate - prove         -0.057000 0.03818901 1520  -1.493  0.9953
# demonstrate - confirm       -0.077250 0.03818901 1520  -2.023  0.8931
# demonstrate - be_right_that -0.095750 0.03818901 1520  -2.507  0.5783
# demonstrate - know          -0.143875 0.03818901 1520  -3.767  0.0239
# see - discover              -0.036000 0.03818901 1520  -0.943  1.0000
# see - prove                 -0.036250 0.03818901 1520  -0.949  1.0000
# see - confirm               -0.056500 0.03818901 1520  -1.479  0.9958
# see - be_right_that         -0.075000 0.03818901 1520  -1.964  0.9165
# see - know                  -0.123125 0.03818901 1520  -3.224  0.1323
# discover - prove            -0.000250 0.03818901 1520  -0.007  1.0000
# discover - confirm          -0.020500 0.03818901 1520  -0.537  1.0000
# discover - be_right_that    -0.039000 0.03818901 1520  -1.021  1.0000
# discover - know             -0.087125 0.03818901 1520  -2.281  0.7474
# prove - confirm             -0.020250 0.03818901 1520  -0.530  1.0000
# prove - be_right_that       -0.038750 0.03818901 1520  -1.015  1.0000
# prove - know                -0.086875 0.03818901 1520  -2.275  0.7518
# confirm - be_right_that     -0.018500 0.03818901 1520  -0.484  1.0000
# confirm - know              -0.066625 0.03818901 1520  -1.745  0.9727
# be_right_that - know        -0.048125 0.03818901 1520  -1.260  0.9995
# 
# P value adjustment: tukey method for comparing a family of 20 estimates 
