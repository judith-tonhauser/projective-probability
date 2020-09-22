# Factives paper
# 5-projectivity-no-fact (certainty ratings, continuous task)
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
library(emmeans)
library(lme4)
library(padr)
library(performance)
library(MuMIn)
theme_set(theme_bw())


# load clean data  ----
cd = read.csv("../data/cd.csv")
nrow(cd) #6916

# create item as combination of predicate and complement clause
cd$item = as.factor(paste(cd$verb,cd$content))

# LME model predicting rating from predicate
table(cd$verb)
cd$verb <- relevel(cd$verb, ref = "MC")
m = lmer(response ~ verb + (1+verb|workerid) + (1|content), data = cd, REML=F)
summary(m)

# reorder verb by mean
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response)) %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))
means
levels(means$verb)

cd$verb <- factor(cd$verb, levels = unique(levels(means$verb)))
levels(cd$verb) 

# pairwise comparison
str(cd$response)
str(cd$verb)
cd$verb <- as.factor(cd$verb)
str(cd$workerid)
cd$workerid <- as.factor(cd$workerid)
model = lmer(response ~ verb + (1|workerid) + (1|item), data=cd, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# $contrasts
# contrast                      estimate         SE     df t.ratio p.value
# MC - pretend              -0.042020639 0.01916977  49.97  -2.192  0.8098
# MC - be_right             -0.071607572 0.01916501  50.13  -3.736  0.0542
# MC - think                -0.083783582 0.01916492  50.07  -4.372  0.0089
# MC - suggest              -0.111608001 0.01912903  50.69  -5.834  0.0001
# MC - say                  -0.133890950 0.01912899  50.72  -6.999  <.0001
# MC - prove                -0.193307193 0.01914129  50.50 -10.099  <.0001
# MC - confirm              -0.231812154 0.01917434  50.04 -12.090  <.0001
# MC - establish            -0.251595754 0.01913785  50.56 -13.146  <.0001
# MC - demonstrate          -0.381538510 0.01913109  50.66 -19.943  <.0001
# MC - announce             -0.471695123 0.01912825  50.73 -24.660  <.0001
# MC - confess              -0.527021744 0.01916157  50.13 -27.504  <.0001
# MC - admit                -0.544807503 0.01912812  50.71 -28.482  <.0001
# MC - reveal               -0.592362151 0.01912462  50.78 -30.974  <.0001
# MC - acknowledge          -0.613560616 0.01916529  50.13 -32.014  <.0001
# MC - hear                 -0.634910506 0.01912981  50.72 -33.190  <.0001
# MC - discover             -0.667312302 0.01913513  50.63 -34.874  <.0001
# MC - inform               -0.693266524 0.01915643  50.26 -36.190  <.0001
# MC - see                  -0.697664502 0.01914503  50.43 -36.441  <.0001
# MC - know                 -0.752791183 0.01913805  50.55 -39.335  <.0001
# MC - be_annoyed           -0.773522289 0.01915574  50.30 -40.381  <.0001
# pretend - be_right        -0.029586933 0.02175318 352.08  -1.360  0.9989
# pretend - think           -0.041762942 0.02175315 351.11  -1.920  0.9404
# pretend - suggest         -0.069587361 0.02172153 363.43  -3.204  0.1553
# pretend - say             -0.091870311 0.02172148 363.91  -4.229  0.0051
# pretend - prove           -0.151286554 0.02173230 359.55  -6.961  <.0001
# pretend - confirm         -0.189791515 0.02176139 349.96  -8.721  <.0001
# pretend - establish       -0.209575115 0.02172928 360.73  -9.645  <.0001
# pretend - demonstrate     -0.339517871 0.02172331 362.84 -15.629  <.0001
# pretend - announce        -0.429674484 0.02172086 363.99 -19.782  <.0001
# pretend - confess         -0.485001104 0.02175018 352.32 -22.299  <.0001
# pretend - admit           -0.502786864 0.02172074 363.80 -23.148  <.0001
# pretend - reveal          -0.550341512 0.02171763 365.20 -25.341  <.0001
# pretend - acknowledge     -0.571539977 0.02175347 351.98 -26.274  <.0001
# pretend - hear            -0.592889867 0.02172222 363.73 -27.294  <.0001
# pretend - discover        -0.625291662 0.02172689 361.93 -28.780  <.0001
# pretend - inform          -0.651245884 0.02174563 354.69 -29.948  <.0001
# pretend - see             -0.655643862 0.02173562 358.16 -30.164  <.0001
# pretend - know            -0.710770543 0.02172942 360.62 -32.710  <.0001
# pretend - be_annoyed      -0.731501649 0.02174507 355.26 -33.640  <.0001
# be_right - think          -0.012176009 0.02174894 353.91  -0.560  1.0000
# be_right - suggest        -0.040000428 0.02171732 366.37  -1.842  0.9600
# be_right - say            -0.062283378 0.02171730 366.84  -2.868  0.3344
# be_right - prove          -0.121699621 0.02172813 362.42  -5.601  <.0001
# be_right - confirm        -0.160204582 0.02175720 352.72  -7.363  <.0001
# be_right - establish      -0.179988182 0.02172509 363.62  -8.285  <.0001
# be_right - demonstrate    -0.309930938 0.02171914 365.74 -14.270  <.0001
# be_right - announce       -0.400087551 0.02171664 366.95 -18.423  <.0001
# be_right - confess        -0.455414171 0.02174594 355.15 -20.942  <.0001
# be_right - admit          -0.473199931 0.02171652 366.75 -21.790  <.0001
# be_right - reveal         -0.520754579 0.02171338 368.20 -23.983  <.0001
# be_right - acknowledge    -0.541953044 0.02174927 354.77 -24.918  <.0001
# be_right - hear           -0.563302934 0.02171802 366.67 -25.937  <.0001
# be_right - discover       -0.595704730 0.02172270 364.85 -27.423  <.0001
# be_right - inform         -0.621658951 0.02174146 357.50 -28.593  <.0001
# be_right - see            -0.626056929 0.02173140 361.04 -28.809  <.0001
# be_right - know           -0.681183610 0.02172529 363.46 -31.354  <.0001
# be_right - be_annoyed     -0.701914716 0.02174087 358.10 -32.285  <.0001
# think - suggest           -0.027824419 0.02171720 365.42  -1.281  0.9995
# think - say               -0.050107369 0.02171718 365.89  -2.307  0.7508
# think - prove             -0.109523612 0.02172804 361.46  -5.041  0.0001
# think - confirm           -0.148028572 0.02175712 351.80  -6.804  <.0001
# think - establish         -0.167812173 0.02172500 362.66  -7.724  <.0001
# think - demonstrate       -0.297754928 0.02171905 364.77 -13.709  <.0001
# think - announce          -0.387911542 0.02171658 365.95 -17.862  <.0001
# think - confess           -0.443238162 0.02174589 354.19 -20.383  <.0001
# think - admit             -0.461023921 0.02171643 365.78 -21.229  <.0001
# think - reveal            -0.508578569 0.02171334 367.18 -23.422  <.0001
# think - acknowledge       -0.529777035 0.02174921 353.82 -24.358  <.0001
# think - hear              -0.551126924 0.02171787 365.74 -25.377  <.0001
# think - discover          -0.583528720 0.02172260 363.89 -26.863  <.0001
# think - inform            -0.609482942 0.02174134 356.58 -28.033  <.0001
# think - see               -0.613880920 0.02173129 360.11 -28.249  <.0001
# think - know              -0.669007601 0.02172518 362.52 -30.794  <.0001
# think - be_annoyed        -0.689738707 0.02174078 357.16 -31.726  <.0001
# suggest - say             -0.022282950 0.02168553 379.04  -1.028  1.0000
# suggest - prove           -0.081699192 0.02169639 374.37  -3.766  0.0285
# suggest - confirm         -0.120204153 0.02172556 364.09  -5.533  <.0001
# suggest - establish       -0.139987753 0.02169334 375.64  -6.453  <.0001
# suggest - demonstrate     -0.269930509 0.02168737 377.89 -12.446  <.0001
# suggest - announce        -0.360087123 0.02168490 379.13 -16.605  <.0001
# suggest - confess         -0.415413743 0.02171425 366.68 -19.131  <.0001
# suggest - admit           -0.433199502 0.02168475 378.95 -19.977  <.0001
# suggest - reveal          -0.480754150 0.02168168 380.41 -22.173  <.0001
# suggest - acknowledge     -0.501952616 0.02171755 366.31 -23.113  <.0001
# suggest - hear            -0.523302505 0.02168625 378.86 -24.131  <.0001
# suggest - discover        -0.555704301 0.02169097 376.91 -25.619  <.0001
# suggest - inform          -0.581658523 0.02170974 369.17 -26.793  <.0001
# suggest - see             -0.586056501 0.02169969 372.89 -27.008  <.0001
# suggest - know            -0.641183182 0.02169352 375.49 -29.556  <.0001
# suggest - be_annoyed      -0.661914288 0.02170912 369.83 -30.490  <.0001
# say - prove               -0.059416243 0.02169635 374.86  -2.739  0.4254
# say - confirm             -0.097921204 0.02172555 364.55  -4.507  0.0016
# say - establish           -0.117704804 0.02169334 376.11  -5.426  <.0001
# say - demonstrate         -0.247647560 0.02168737 378.36 -11.419  <.0001
# say - announce            -0.337804173 0.02168484 379.66 -15.578  <.0001
# say - confess             -0.393130793 0.02171422 367.16 -18.105  <.0001
# say - admit               -0.410916553 0.02168471 379.46 -18.950  <.0001
# say - reveal              -0.458471201 0.02168165 380.91 -21.146  <.0001
# say - acknowledge         -0.479669666 0.02171752 366.79 -22.087  <.0001
# say - hear                -0.501019556 0.02168617 379.41 -23.103  <.0001
# say - discover            -0.533421352 0.02169092 377.43 -24.592  <.0001
# say - inform              -0.559375573 0.02170970 369.66 -25.766  <.0001
# say - see                 -0.563773551 0.02169966 373.39 -25.981  <.0001
# say - know                -0.618900232 0.02169349 375.98 -28.529  <.0001
# say - be_annoyed          -0.639631338 0.02170909 370.31 -29.464  <.0001
# prove - confirm           -0.038504961 0.02173640 360.15  -1.771  0.9731
# prove - establish         -0.058288561 0.02170419 371.50  -2.686  0.4651
# prove - demonstrate       -0.188231317 0.02169820 373.73  -8.675  <.0001
# prove - announce          -0.278387930 0.02169568 374.99 -12.831  <.0001
# prove - confess           -0.333714550 0.02172508 362.71 -15.361  <.0001
# prove - admit             -0.351500310 0.02169558 374.77 -16.201  <.0001
# prove - reveal            -0.399054958 0.02169251 376.20 -18.396  <.0001
# prove - acknowledge       -0.420253423 0.02172836 362.36 -19.341  <.0001
# prove - hear              -0.441603313 0.02169707 374.69 -20.353  <.0001
# prove - discover          -0.474005109 0.02170182 372.75 -21.842  <.0001
# prove - inform            -0.499959330 0.02172053 365.19 -23.018  <.0001
# prove - see               -0.504357308 0.02171049 368.84 -23.231  <.0001
# prove - know              -0.559483989 0.02170436 371.35 -25.777  <.0001
# prove - be_annoyed        -0.580215095 0.02171994 365.81 -26.713  <.0001
# confirm - establish       -0.019783600 0.02173331 361.39  -0.910  1.0000
# confirm - demonstrate     -0.149726356 0.02172736 363.49  -6.891  <.0001
# confirm - announce        -0.239882969 0.02172486 364.68 -11.042  <.0001
# confirm - confess         -0.295209590 0.02175419 352.98 -13.570  <.0001
# confirm - admit           -0.312995349 0.02172471 364.51 -14.407  <.0001
# confirm - reveal          -0.360549997 0.02172167 365.86 -16.599  <.0001
# confirm - acknowledge     -0.381748462 0.02175744 352.66 -17.546  <.0001
# confirm - hear            -0.403098352 0.02172625 364.40 -18.554  <.0001
# confirm - discover        -0.435500148 0.02173092 362.60 -20.041  <.0001
# confirm - inform          -0.461454369 0.02174966 355.33 -21.217  <.0001
# confirm - see             -0.465852348 0.02173966 358.81 -21.429  <.0001
# confirm - know            -0.520979028 0.02173343 361.29 -23.971  <.0001
# confirm - be_annoyed      -0.541710134 0.02174904 355.95 -24.907  <.0001
# establish - demonstrate   -0.129942756 0.02169515 375.00  -5.989  <.0001
# establish - announce      -0.220099369 0.02169266 376.24 -10.146  <.0001
# establish - confess       -0.275425990 0.02172202 363.93 -12.680  <.0001
# establish - admit         -0.293211749 0.02169254 376.04 -13.517  <.0001
# establish - reveal        -0.340766397 0.02168946 377.49 -15.711  <.0001
# establish - acknowledge   -0.361964862 0.02172535 363.54 -16.661  <.0001
# establish - hear          -0.383314752 0.02169403 375.97 -17.669  <.0001
# establish - discover      -0.415716548 0.02169871 374.07 -19.159  <.0001
# establish - inform        -0.441670769 0.02171751 366.39 -20.337  <.0001
# establish - see           -0.446068748 0.02170745 370.08 -20.549  <.0001
# establish - know          -0.501195428 0.02170132 372.61 -23.095  <.0001
# establish - be_annoyed    -0.521926534 0.02171688 367.05 -24.033  <.0001
# demonstrate - announce    -0.090156613 0.02168671 378.48  -4.157  0.0068
# demonstrate - confess     -0.145483234 0.02171609 366.04  -6.699  <.0001
# demonstrate - admit       -0.163268993 0.02168659 378.27  -7.529  <.0001
# demonstrate - reveal      -0.210823641 0.02168350 379.75  -9.723  <.0001
# demonstrate - acknowledge -0.232022106 0.02171937 365.68 -10.683  <.0001
# demonstrate - hear        -0.253371996 0.02168808 378.19 -11.683  <.0001
# demonstrate - discover    -0.285773792 0.02169275 376.29 -13.174  <.0001
# demonstrate - inform      -0.311728014 0.02171154 368.55 -14.358  <.0001
# demonstrate - see         -0.316125992 0.02170150 372.26 -14.567  <.0001
# demonstrate - know        -0.371252672 0.02169533 374.84 -17.112  <.0001
# demonstrate - be_annoyed  -0.391983778 0.02171092 369.20 -18.055  <.0001
# announce - confess        -0.055326620 0.02171357 367.26  -2.548  0.5718
# announce - admit          -0.073112380 0.02168406 379.57  -3.372  0.0981
# announce - reveal         -0.120667028 0.02168102 381.01  -5.566  <.0001
# announce - acknowledge    -0.141865493 0.02171682 366.93  -6.533  <.0001
# announce - hear           -0.163215383 0.02168556 379.49  -7.526  <.0001
# announce - discover       -0.195617179 0.02169026 377.55  -9.019  <.0001
# announce - inform         -0.221571400 0.02170904 369.77 -10.206  <.0001
# announce - see            -0.225969378 0.02169903 373.47 -10.414  <.0001
# announce - know           -0.281096059 0.02169282 376.11 -12.958  <.0001
# announce - be_annoyed     -0.301827165 0.02170843 370.42 -13.904  <.0001
# confess - admit           -0.017785759 0.02171345 367.07  -0.819  1.0000
# confess - reveal          -0.065340407 0.02171039 368.45  -3.010  0.2478
# confess - acknowledge     -0.086538873 0.02174624 355.04  -3.979  0.0134
# confess - hear            -0.107888762 0.02171498 366.96  -4.968  0.0002
# confess - discover        -0.140290558 0.02171962 365.17  -6.459  <.0001
# confess - inform          -0.166244780 0.02173840 357.79  -7.648  <.0001
# confess - see             -0.170642758 0.02172837 361.32  -7.853  <.0001
# confess - know            -0.225769439 0.02172222 363.77 -10.393  <.0001
# confess - be_annoyed      -0.246500545 0.02173779 358.41 -11.340  <.0001
# admit - reveal            -0.047554648 0.02168088 380.81  -2.193  0.8226
# admit - acknowledge       -0.068753113 0.02171675 366.70  -3.166  0.1709
# admit - hear              -0.090103003 0.02168545 379.27  -4.155  0.0068
# admit - discover          -0.122504799 0.02169011 377.37  -5.648  <.0001
# admit - inform            -0.148459021 0.02170898 369.52  -6.839  <.0001
# admit - see               -0.152856999 0.02169890 373.28  -7.044  <.0001
# admit - know              -0.207983680 0.02169272 375.88  -9.588  <.0001
# admit - be_annoyed        -0.228714786 0.02170827 370.26 -10.536  <.0001
# reveal - acknowledge      -0.021198465 0.02171367 368.09  -0.976  1.0000
# reveal - hear             -0.042548355 0.02168234 380.77  -1.962  0.9275
# reveal - discover         -0.074950151 0.02168705 378.81  -3.456  0.0768
# reveal - inform           -0.100904373 0.02170584 370.99  -4.649  0.0009
# reveal - see              -0.105302351 0.02169580 374.73  -4.854  0.0003
# reveal - know             -0.160429031 0.02168964 377.33  -7.397  <.0001
# reveal - be_annoyed       -0.181160138 0.02170524 371.63  -8.346  <.0001
# acknowledge - hear        -0.021349890 0.02171823 366.62  -0.983  1.0000
# acknowledge - discover    -0.053751686 0.02172293 364.79  -2.474  0.6289
# acknowledge - inform      -0.079705907 0.02174171 357.42  -3.666  0.0400
# acknowledge - see         -0.084103885 0.02173165 360.97  -3.870  0.0199
# acknowledge - know        -0.139230566 0.02172545 363.46  -6.409  <.0001
# acknowledge - be_annoyed  -0.159961672 0.02174108 358.05  -7.358  <.0001
# hear - discover           -0.032401796 0.02169165 377.25  -1.494  0.9963
# hear - inform             -0.058356018 0.02171044 369.47  -2.688  0.4634
# hear - see                -0.062753996 0.02170037 373.22  -2.892  0.3186
# hear - know               -0.117880676 0.02169422 375.79  -5.434  <.0001
# hear - be_annoyed         -0.138611782 0.02170981 370.14  -6.385  <.0001
# discover - inform         -0.025954222 0.02171514 367.61  -1.195  0.9998
# discover - see            -0.030352200 0.02170507 371.33  -1.398  0.9985
# discover - know           -0.085478881 0.02169893 373.88  -3.939  0.0154
# discover - be_annoyed     -0.106209987 0.02171446 368.32  -4.891  0.0003
# inform - see              -0.004397978 0.02172387 363.74  -0.202  1.0000
# inform - know             -0.059524659 0.02171765 366.27  -2.741  0.4237
# inform - be_annoyed       -0.080255765 0.02173323 360.85  -3.693  0.0366
# see - know                -0.055126681 0.02170760 369.95  -2.540  0.5784
# see - be_annoyed          -0.075857787 0.02172323 364.40  -3.492  0.0691
# know - be_annoyed         -0.020731106 0.02171705 366.90  -0.955  1.0000

# JD CODE STARTS HERE ----
# TL;DR: all verbs are different from main clause (non-projecting) controls (called "control")
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")
table(cd$verb)

# Bayesian mixed effects regression to test whether ratings differ by predicate from good controls
cd$workerid = as.factor(as.character(cd$workerid))
cd$item = as.factor(paste(as.character(cd$verb),as.character(cd$content)))
cd$content = as.factor(as.character(cd$content))
cd$isMC = cd$verb == "MC"
cd$isZeroOne = (cd$response == 0 | cd$response == 1)

p = ggplot(cd, aes(x=response,fill=isMC)) +
  geom_histogram() +
  facet_wrap(~workerid)
ggsave(p, file="../graphs/subject_variability.pdf",width=25,height=25)


# set reference level to main clause controls
d = cd %>%
  droplevels() %>%
  mutate(verb = fct_relevel(verb,"MC"))
table(d$verb)

# run beta regression

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y'' = (y' ?? (n ??? 1) + 0.5)/n
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
d$betaresponse = (d$response*(nrow(d)-1) + .5)/nrow(d)

prior = get_prior(betaresponse ~ verb + (1|workerid) + (1|item),family = Beta(),data=d)
prior

betamodel = bf(betaresponse ~ verb + (1|workerid) + (1|item),
               phi ~ verb + (1|workerid) + (1|item), # beta distribution's precision  )
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=d, 
          cores = 4,
          control = list(adapt_delta = .95,max_treedepth=15))

summary(m.b)

saveRDS(m.b,file="../data/beta-model-mixed.rds")

# to load model
m.b = readRDS(file="../data/beta-model-mixed.rds")

# to get stan code
stancode(m.b)

summary(m.b)
fixef(m.b) # does the same thing

# create LaTeX table
mcmcReg(m.b, pars = "b_", file="../models/brm_output.tex")

# hypothesis-testing, probing posterior model
q = c(q_pretend_MC = "plogis(verbpretend-Intercept) = plogis(Intercept)")
q_answer = hypothesis(m.b, q)
q_answer
plot(q_answer)
prop.table(table(q_answer$samples$H1 > 0)) 

# posterior_samples(m.b, pars = "b_") %>%
#   mutate_at(c("b_phi_Intercept",paste("b_",c(dimnames(fixef(m.b))[[1]])[23:42],sep="")), exp) %>%
#   mutate_at(c("b_Intercept",paste("b_",c(dimnames(fixef(m.b))[[1]])[3:22],sep="")), plogis) %>%
#   posterior_summary() %>%
#   as.data.frame() %>%
#   rownames_to_column("Parameter") %>%
#   kable(digits = 2)


# run alternate beta model with factivity fixed effect
# create factivity variable
d = d %>% 
  mutate(predicate_type = as.factor(case_when(verb %in% c("know", "reveal","see","discover","be_annoyed") ~ "factive",
                                     verb == "MC" ~ "control",
                                     TRUE ~ "non-factive")))

betamodel.fact = bf(betaresponse ~ verb + predicate_type + (1|workerid) + (1|item),
               phi ~ verb + (1|workerid) + (1|item), # beta distribution's precision  )
               family = Beta())

m.b.fact = brm(formula = betamodel.fact,
          family=Beta(),
          data=d, 
          cores = 4,
          control = list(adapt_delta = .95,max_treedepth=15))

summary(m.b.fact)

saveRDS(m.b.fact,file="../data/beta-model-fact-mixed.rds")

# to load model
m.b.fact = readRDS(file="../data/beta-model-fact-mixed.rds")



##################################
# fit linear model, first Bayesian then frequentist -- same qualitative result (except pretend's lower bound is now inluded in 95% credible interval)
summary(d %>% select(response,verb,workerid,item))
str(d %>% select(response,verb,workerid,item))

m <- brm(
  formula = response ~ verb + (1|workerid) + (1|item),
  data = d,
  cores = 4,
  control = list(adapt_delta = .95)
  # file = here::here("zoib-ex")
)
# no need to run this multiple times:
saveRDS(m,file="../data/linear-model-mixed.rds")

# load linear model ----
m <- readRDS(file="../data/linear-model-mixed.rds")

summary(m) # see summary printed below

# let's look at pretend in particular
q = c(q_pretend_MC_mean = "Intercept + verbpretend = Intercept")
q_answer = hypothesis(m, q)
q_answer
plot(q_answer)
prop.table(table(q_answer$samples$H1 > 0)) # prob (pretend > MC) = .97

# fit frequentist linear model for comparison
m = lmer(response ~ verb + (1|workerid) + (1|item), data = d)
summary(m)
check_model(m) # shows some non-normality of residuals as well as violation of homoscedasticity assumption
m = lmerTest::lmer(response ~ verb + (1|workerid) + (1|item), data = d)
summary(m)

# fit frequentist linear models with verb (m.verb) and factivity (m.fact) predictor, compare
d$verb_noMC = fct_relevel(d$verb,"pretend")
d_noMC = d %>% 
  filter(verb != "MC") %>% 
  mutate(predicate_type = fct_relevel(predicate_type,"non-factive")) %>% 
  droplevels()
m.verb = lmer(response ~ verb_noMC + (1|workerid) + (1|item), data = d_noMC)
summary(m.verb)
m.fact = lmer(response ~ predicate_type + (1|workerid) + (1|item), data = d_noMC)
summary(m.fact)
check_model(m.fact) 

# rank models by two different measures of model quality -- in both cases, verb model is better than binary factivity model
# rank models by BIC
model.sel(m.verb,m.fact, rank = BIC)
# rank models by AIC
model.sel(m.verb,m.fact, rank = AIC)

# To compute marginal R^2 (variance explained by fixed effects) and conditional R^2 (variance explained by fixed and random effects):
r.squaredGLMM(m.verb) 
# marginal R^2 (variance explained by fixed effects): .47
# conditional R^2 (variance explained by fixed and random effects jointly): .54

r.squaredGLMM(m.fact) 
# marginal R^2 (variance explained by fixed effects): .18
# conditional R^2 (variance explained by fixed and random effects jointly): .54

