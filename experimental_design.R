######################################################################################
################ Comparing the means of two groups (or only one) #####################
#####################################################################################

# library(dplyr) 
library(ggplot2)
library(ggpubr)

data("ToothGrowth")
?ToothGrowth

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
ToothGrowth$supp <- as.factor(ToothGrowth$supp)
levels(ToothGrowth$supp)=c("Orange Juice","Ascorbic Acid")

head(ToothGrowth)

ToothGrowth$len[ToothGrowth$supp=="Orange Juice"]
ToothGrowth$len[ToothGrowth$supp=="Ascorbic Acid"]

# Simple numerical comparison

mg1=mean(ToothGrowth$len[ToothGrowth$supp=="Orange Juice"])
mg2=mean(ToothGrowth$len[ToothGrowth$supp=="Ascorbic Acid"])

mg1
mg2

mg1-mg2


# Graphical comparison

# compare different delivery methods groups
e <- ggplot(ToothGrowth, aes(x = supp, y = len))

# add a title in ggplot
e + geom_boxplot() + 
  ggtitle("Teeth length by delivery methods") 

# use different colors
e + geom_boxplot(aes(color = supp)) + 
  ggtitle("Teeth length by delivery methods") 

# fill the box and add names to axes
e + geom_boxplot(aes(fill = supp)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) + 
  ggtitle("Teeth length by delivery methods") +
  xlab("Delivery method") + ylab("Teeth length")


# compare different dose groups
e2 <- ggplot(ToothGrowth, aes(x = dose, y = len))
e2 + geom_boxplot(aes(fill = dose)) +
  scale_fill_manual(values = c("red", "green", "yellow")) + 
  ggtitle("Teeth length by dose") +
  xlab("Dose") + ylab("Teeth length")


# Graphical comparison considering two factors
e <- ggplot(ToothGrowth, aes(x = supp, y = len))
e + geom_boxplot(aes(fill = dose)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) + 
  ggtitle("Teeth length by delivery method and dose combinations") +
  xlab("Delivery method and dose Combinations") + ylab("Teeth length")

e2 <- ggplot(ToothGrowth, aes(x = dose, y = len))
e2 + geom_boxplot(aes(fill = supp)) +
  scale_fill_manual(values = c("red", "yellow")) + 
  ggtitle("Teeth length by delivery method and dose combinations") +
  xlab("Delivery method and dose Combinations") + ylab("Teeth length") 


# Between-group variation and within-group variation

attach(ToothGrowth)

s1=subset(ToothGrowth, ToothGrowth$dose=="0.5")
s2=subset(ToothGrowth, ToothGrowth$dose=="1")
s3=subset(ToothGrowth, ToothGrowth$dose=="2")

mean(s1$len)
mean(s2$len)
mean(s3$len)

# within-group variation
sd(s1$len)
sd(s2$len)
sd(s3$len)

# Between-group variation

total_mean=mean(c(mean(s1$len), mean(s2$len), mean(s3$len)))
total_mean
mean(ToothGrowth$len) # the same

sd(c(mean(s1$len), mean(s2$len), mean(s3$len)))
sd(ToothGrowth$len) # not the same

###########################################################################
############################ parametric tests #############################
###########################################################################


########################################################################
### one-sample t-test ##################################################
########################################################################

set.seed(127)
?rnorm

x<-rnorm(1000, mean = 0, sd = 20) # generate some data

#############################################################################
# assumption 1: the data are normally distributed ##########################
#############################################################################

hist(x)

# how to check normality manually with other tests

library(tseries)
jarque.bera.test(x)
shapiro.test(x)

jarque.bera.test(x)

library(nortest) # Anderson-Darling normality test
ad.test(x)

library(nortest) # Cramer-von Mises normality test
cvm.test(x)

library(nortest) #  Lilliefors (Kolmogorov-Smirnov) normality test
lillie.test(x)

library(nortest) # Shapiro-Francia normality test
sf.test(x)

library(nortest)
pearson.test(x) # Pearson chi-square normality test


#############################################################################
# assumption 2: The dependent variable should not contain any outliers #####
#############################################################################

# How to check for outliers

boxplot(x)
outliers_values=boxplot.stats(x)$out
outliers_values


# IQR to Detect Outliers

summary(x)
summary(x)[5]
summary(x)[2]
IQR=summary(x)[5]-summary(x)[2]
IQR # Interquartile range

up_out=summary(x)[5]+1.5*IQR
down_out=summary(x)[2]-1.5*IQR

up_out
down_out

max(x)
min(x)

which(x>up_out)
which(x<down_out)

out_data=c(which(x<down_out), which(x>up_out))
out_data

length(x)
x[out_data]

x_new=x[-out_data]
length(x_new)

boxplot(x_new)
outliers_values_new=boxplot.stats(x_new)$out
outliers_values_new

# Using Z-scores to Detect Outliers

z=(x-mean(x))/sd(x)
summary(z)
boxplot(z)

which(z > 2.5)
which(z < -2.5)

out_data_z=c(which(z > 2.5), which(z < -2.5))
out_data_z

x_new2=x[-out_data_z]
length(x_new2)


# create a function to find and remove outliers automatically using z score

remove_outliers_z=function(x, value=2.5){
  z=(x-mean(x))/sd(x)
  out_data_z=c(which(z > value), which(z < -value))
  x_new=x[-out_data_z]
  print(length(x_new))
  boxplot(x_new)
  summary(x_new)
  par(mfrow=c(1,2))
  boxplot(x_new, main="New Distribution BoxPlot")
  hist(x_new, main="New Distribution Histogram")
}

remove_outliers_iqr=function(x){
  IQR=summary(x)[5]-summary(x)[2]
  up_out=summary(x)[5]+1.5*IQR
  down_out=summary(x)[2]-1.5*IQR
  out_data=c(which(x<down_out), which(x>up_out))
  x_new=x[-out_data]
  print(length(x_new))
  boxplot(x_new)
  summary(x_new)}

?runif
?rbeta

# x=runif(300, min=0, max=100)
# x=sample(1:10000, 1000, replace=T)

x=rbeta(1000, 3, 0.6)

hist(x)
boxplot(x)

remove_outliers_z(x)
remove_outliers_z(x,3)
remove_outliers_z(x,1)

boxplot(x)
remove_outliers_iqr(x)

########################################################################
# One sample t-test - One-tailed hypothesis tests
########################################################################

?t.test

t.test(x, mu = 0, alternative = "greater")
t.test(x, mu = 0, alternative = "greater",  conf.level = 0.90) # set a different 1????? level


# One sample t-test - Using a Two-tailed hypothesis tests
t.test(x, mu = 0, alternative = "two.sided")
t.test(x, mu = 0, alternative = "two.sided", conf.level = 0.90) # set a different 1????? level


# Real Example

cats_sample_weight=rnorm(49, mean=4, sd=5)
hist(cats_sample_weight)

# we can check the outliers before to do the test
boxplot(cats_sample_weight)

sd(cats_sample_weight)
mean(cats_sample_weight)
t=(mean(cats_sample_weight)-7)/(sd(cats_sample_weight)/7)

t.test(cats_sample_weight, mu = 7, alternative = "two.sided")

# if we know sd pop - for example is equal to sd sample
library(DescTools)
?ZTest

sd_pop=sd(cats_sample_weight)
ZTest(cats_sample_weight, mu=7, sd_pop=sd_pop)


#############################################################################
## unpaired two samples t test with similar variance (homoscedastic case) ###
#############################################################################

# create two random groups
x1<-rnorm(1000, mean=3, sd=0.98)
x2<-rnorm(1000, mean=4, sd=1)

boxplot(x1, main="First group")
boxplot(x2, main="Second group")

# assumption 1: the data are normally distributed
shapiro.test(x1)
shapiro.test(x2)

# assumption 2: samples have "equal" variances
# rough check: in general, the ratio between the SDs must not be greater than 2
sd(x1)
sd(x2)
sd(x2)/sd(x1)  

# testing the homogeneity of variances
# low P-values rejection H0, high P-values I accept homogeneity variances
# need of dataframe

values=c(x1,x2)
length(values)
group_label=c(rep("A",length(values)/2),rep("B",length(values)/2))
group_label=as.factor(group_label)
class(group_label)

data=cbind(values, group_label)
head(data)
tail(data)

bartlett.test(values, group_label)
fligner.test(values, group_label)

# the null is that the means are equal
t.test(x1, x2, alternative = "two.sided", var.equal = TRUE)

# nice function: get the function "rquery.t.test" 
source('http://www.sthda.com/upload/rquery_t_test.r')
rquery.t.test(x1, x2)


###################################################################################
## Assumption n. 1 violated - The samples (or one) are not normally distributed ###
###################################################################################
###################################################################################
## Unpaired two samples t test when the normality is not present ##################
###################################################################################

#############################################################################
############# Solution n.1 if normality is not present ######################
#############################################################################
## Try to transform the data to make them normal and respect assumption 1 ###
#############################################################################

# use log to transform but pay attention to zeros
# The natural log function is frequently used to rescale data for statistical
# and graphical analysis. This can be accomplished in R via the use of the 
# log() function which can be mapped across a vector or data frame. 
# The resulting series of values will be transformed, reducing the
# visual distance between observations that are orders of magnitude
# apart (eg. 10, 100, 1000 would be adjacent to each other visually).
# Rescaling data through a natural log transformation reduces the 
# impact a few excessively large data points have when calculating 
# a trend-line through the sample.


v = c(100,10,5,2,1,0.5,0.1,0.05,0.01,0.001,0.0001)
hist(v)
hist(log(v))
shapiro.test(v)
shapiro.test(log(v))

# Box-Cox transformation

library(caret)
?cars
head(cars)
cars
hist(datasets::cars$dist)
shapiro.test(datasets::cars$dist)

distBCMod <- caret::BoxCoxTrans(cars$dist)   # try to make dist normal
print(distBCMod)
dist_new=predict(distBCMod, cars$dist)   # transform the distance using lambda
hist(dist_new)
shapiro.test(dist_new)

# if you make the same transformation to both groups, now you can use t-test

#############################################################################
############# Solution n.2 if normality is not present ######################
#############################################################################
############# Use a non-parametric test, i.e. wilcox.test ###################
#############################################################################

x5<-rnorm(100, mean=2, sd=0.9)
x5<-c(x, 10,20) # add some outliers
hist(x5)
boxplot(x5)
shapiro.test(x5)

x6<-rnorm(100, mean=4, sd=1)
boxplot(x6)
shapiro.test(x6)

wilcox.test(x5,x6)

# rquery.t.test automatically performs the wilcox.test when normality is not met
source('http://www.sthda.com/upload/rquery_t_test.r')
rquery.t.test(x5, x6)


###################################################################################
## Assumption n. 2 violated - The samples have differerent variance ###############
###################################################################################
## Unpaired two samples t test with dissimilar variances (heteroscedastic case) ###
###################################################################################
# The two samples are normally distributed but the variances are unequal #########
###################################################################################

x3<-rnorm(100, mean=2, sd=0.9)
x4<-rnorm(100, mean=2, sd=3)

values2=c(x3,x4)
length(values2)
group_label2=c(rep("C",length(values2)/2),rep("D",length(values2)/2))
group_label2=as.factor(group_label2)
class(group_label2)

data2=cbind(values2, group_label2)
head(data2)
tail(data2)

bartlett.test(values2, group_label2)
fligner.test(values2, group_label2)

t.test(x3, x4, alternative = "two.sided", var.equal = FALSE)

# rquery.t.test automatically performs the Welch Two Sample t-test
rquery.t.test(x3, x4)

##############################################################################
################# Paired two samples t test ##################################
##############################################################################

# two dependent samples

# assumption: the difference d=x-y is normally distributed

t1<-rnorm(100, mean=10, sd=2)
t2<-rnorm(100, mean=15, sd=3)

differences=t1-t2
hist(differences)
shapiro.test(differences)

boxplot(t1)
boxplot(t2)

t.test(t1, t2, paired=TRUE, alternative = "two.sided")

# rquery.t.test automatically check normality of differences
rquery.t.test(t1, t2, paired=TRUE)
# real example

library(tidyverse)
library(ggpubr)
library(rstatix)

data("mice2", package = "datarium")
head(mice2, 3)
?mice2 # contains the weight of 10 mice before and after the treatment.

# Transform into long data: 
# gather the before and after values in the same column
# mice2.long <- mice2 %>%
#   gather(key = "group", value = "weight", before, after)
# head(mice2.long, 10)
# ?tidyr::gather

par(mfrow=c(1,2))
boxplot(mice2$before, main="Before", ylim=c(0,500))
boxplot(mice2$after, main="After", ylim=c(0,500))

mice2$after-mice2$before

hist(mice2$after-mice2$before) # it may appear not normal
hist(mice2$after-mice2$before,10)
shapiro.test(mice2$after-mice2$before)

res <- t.test(mice2$after,mice2$before, paired = TRUE)
res
###########################################################################
######################### Non-parametric tests ############################
###########################################################################

##############################################
### One-sample Wilcoxon signed rank test #####
##############################################

set.seed(127)

my_data <- data.frame(
  name = paste0(rep("M_", 30), 1:30),
  weight = round(rnorm(10, 20, 12), 3)
)

my_data

hist(my_data$weight)
shapiro.test(my_data$weight)

boxplot(my_data$weight, ylab = "Weight (g)")

# We want to know, if the average weight of the mice differs from 25g 

res <- wilcox.test(my_data$weight, mu = 25)
res 

# ties = pairs of data that have the same ordinal values
# the average weight of the mice is significantly different from 25g 

#########################################################
### Two-samples Mann-Whitney-Wilcoxon rank-sum test #####
#########################################################

# independent groups

women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5,48)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4, 60) 

hist(women_weight)
hist(men_weight)

shapiro.test(women_weight)
shapiro.test(men_weight)

my_data <- data.frame( 
  group = rep(c("Woman", "Man"), each = 10),
  weight = c(women_weight,  men_weight)
)

my_data

# does the median of women's weight differ from the median of men's weight?

library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("red", "pink"),
          ylab = "Weight", xlab = "Groups")

res <- wilcox.test(women_weight, men_weight)
res

##################################################
### Wilcoxon matched-pairs signed rank test  #####
##################################################

# dependent groups

# Differences between paired samples should be distributed symmetrically 
# around the median.

# Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)

# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
)

my_data

# We want to know, if there is any significant difference in the median 
# weights before and after treatment?

library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "Weight", xlab = "Groups")

# Subset weight data before treatment
before <- subset(my_data,  group == "before", weight,
                 drop = TRUE)
# subset weight data after treatment
after <- subset(my_data,  group == "after", weight,
                drop = TRUE)

# Plot paired data
library(PairedData)
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

res <- wilcox.test(before, after, paired = TRUE)
res   # median weight of the mice before treatment is significantly different from the median weight after treatment


#####################################################################################
################### Comparing the means of more than two groups #####################
#####################################################################################

###########################################################################
############ Parametric tests for many groups #############################
###########################################################################

##################################
######## ANOVA BETWEEN ###########
##################################

####################################################
############# One-way ANOVA ########################
####################################################

# http://www.sthda.com/english/wiki/one-way-anova-test-in-r

# Results from an experiment to compare yields (amount produced) 
# (as measured by dried weight of plants) obtained 
# under a control and two different treatment conditions.

# We want to know if there is any significant difference between 
# the average weights of plants in the 3 experimental conditions.

?PlantGrowth

dim(PlantGrowth)
head(PlantGrowth)
levels(PlantGrowth$group)    # factor with three treatments

library(ggpubr)
?ggboxplot
ggboxplot(PlantGrowth, x = "group", y = "weight", 
          fill="group", order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

# plot the distribution of the groups and check normality 

par(mfrow=c(2,2))

hist(PlantGrowth$weight[PlantGrowth$group=="ctrl"], col="blue", 
     border="black", prob = TRUE, xlab = "Weight", main = "Control", breaks=6)
lines(density(PlantGrowth$weight[PlantGrowth$group=="ctrl"]), lwd = 2, col = "red")

hist(PlantGrowth$weight[PlantGrowth$group=="trt1"],col="blue", 
     border="black", prob = TRUE, xlab = "Weight", main = "Treatment 1", breaks=6)
lines(density(PlantGrowth$weight[PlantGrowth$group=="trt1"]), lwd = 2, col = "red")

hist(PlantGrowth$weight[PlantGrowth$group=="trt2"], col="blue",
     border="black", prob = TRUE, xlab = "Weight", main = "Treatment 2", breaks=6)
lines(density(PlantGrowth$weight[PlantGrowth$group=="trt2"]), lwd = 2, col = "red")

library(ggplot2)
library(lattice)

?densityplot
densityplot(~ PlantGrowth$weight, group = group, data = PlantGrowth, auto.key = TRUE)
# auto.key automatically insert the legend
ggplot(PlantGrowth) + geom_density(aes(x = PlantGrowth$weight, fill = group), alpha = 0.2)

shapiro.test(PlantGrowth$weight[PlantGrowth$group=="ctrl"])
shapiro.test(PlantGrowth$weight[PlantGrowth$group=="trt1"])
shapiro.test(PlantGrowth$weight[PlantGrowth$group=="trt2"])

# check homogeneity of variances

bartlett.test(PlantGrowth$weight, PlantGrowth$group)
fligner.test(PlantGrowth$weight, PlantGrowth$group)

# Compute the analysis of variance
mod <- aov(weight ~ group, data = PlantGrowth)
# dim(PlantGrowth)

# Summary of the analysis
summary(mod)

# There are significant differences between the groups highlighted with "*"

# Multiple pairwise-comparison between the means of groups
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.
# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between specific pairs of group are statistically significant.

# Tukey multiple pairwise-comparisons

TUKEY=TukeyHSD(mod)
TUKEY

library(multcompView)
plot(TUKEY , las=1 , col="brown")

# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence
# interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.
# It can be seen from the output, that only the difference between
# trt2 and trt1 is significant with an adjusted p-value of 0.012.

# alternative (adjusted by the Benjamini-Hochberg method)
?pairwise.t.test
pairwise.t.test(PlantGrowth$weight, PlantGrowth$group,
                p.adjust.method = "BH")


# Alternative to Check the homogeneity of variance assumption

plot(mod, 1)
# Points 17, 15, 4 are detected as outliers, which can severely 
# affect normality and homogeneity of variance. It can be useful to remove 
# outliers to meet the test assumptions.

# alternative to Check the homogeneity of variance assumption
library(car)
leveneTest(weight ~ group, data = PlantGrowth)
# there is no evidence to suggest that the variance across groups is
# statistically significantly different



# Alternative to  Check the normality assumption

# The normal probability plot of residuals is used to check 
# the assumption that the residuals are normally distributed. 
# It should approximately follow a straight line.

plot(mod, 2)

# alternative to Check the normality assumption
# Extract the residuals
aov_residuals <- residuals(object = mod )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

####################################################
############# Two-way ANOVA ########################
####################################################

# http://www.sthda.com/english/wiki/two-way-anova-test-in-r

set.seed(1234)

?ToothGrowth

data(ToothGrowth)
ToothGrowth$dose=as.factor(ToothGrowth$dose)
ToothGrowth$supp=as.factor(ToothGrowth$supp)

# Convert dose as a factor and recode the levels
# as "D0.5", "D1", "D2"
ToothGrowth$dose <- factor(ToothGrowth$dose, 
                       levels = c(0.5, 1, 2),
                       labels = c("D0.5", "D1", "D2"))

# We want to know if tooth length depends on supp and dose.

table(ToothGrowth$supp, ToothGrowth$dose)

# We have 2X3 design cells with the factors being supp and dose and 10 subjects in each cell. Here, we have a balanced design. 

library("ggpubr")
ggboxplot(ToothGrowth, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

ggline(ToothGrowth, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))


# Box plot with two factor variables
boxplot(len ~ supp * dose, data=ToothGrowth, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="Tooth Length")

# Two-way interaction plot
?interaction.plot
interaction.plot(x.factor = ToothGrowth$dose, trace.factor = ToothGrowth$supp, 
                 response = ToothGrowth$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

# x.factor: the factor to be plotted on x axis.
# trace.factor: the factor to be plotted as lines
# response: a numeric variable giving the response
# type: the type of plot. Allowed values include p (for point only), l (for line only) and b (for both point and line).

mod2 <- aov(len ~ supp + dose, data = ToothGrowth)
summary(mod2)


# Two-way ANOVA with interaction effect
mod3 <- aov(len ~ supp * dose, data = ToothGrowth)
summary(mod3)

TUKEY=TukeyHSD(mod3, which = "dose")
TUKEY

library(multcompView)
plot(TUKEY , las=1 , col="brown")

####################################################################
################## ANOVA BETWEEN EXERCISE ##########################
####################################################################

# Time: Survival time of the animal
# poison: Type of poison used: factor level: 1,2 and 3
# treat: Type of treatment used: factor level: 1,2 and 3

library(BHH2)
data(poison.data)
?poison.data

dd=poison.data

dim(dd)
head(dd)
attach(dd)

levels(treat)
levels(poison)

e <- ggplot(dd, aes(x = treat, y = y))
e + geom_boxplot(aes(fill = poison)) +
  ggtitle("Survival time of the animal according to poison and tratment") +
  xlab("poison and tratment Combinations") + ylab(" Survival time ")

table(poison, treat)

shapiro.test(dd$y[dd$poison=="I" & dd$treat=="A"])
shapiro.test(dd$y[dd$poison=="I" & dd$treat=="B"])
shapiro.test(dd$y[dd$poison=="I" & dd$treat=="C"])
shapiro.test(dd$y[dd$poison=="I" & dd$treat=="D"])
# skip other normality assumptions check

# not the right way
bartlett.test(dd$y, dd$poison) # only for a factor
bartlett.test(dd$y, dd$treat) # only for a factor

# extend to all the cells, we have 12 cells because the levels combinations are 4*3
combo=rep(1:12, each=4)
combo

dd2=cbind(dd, combo)

attach(dd2)
dd2
dd2$combo=as.factor(dd2$combo)
levels(dd2$combo)

bartlett.test(dd2$y, dd2$combo) # no homoscedasticity, a non-parametric test is more appropriate

# come back to the previous dataset, dd2 was just for chechink

mod_ex <- aov(y ~ treat*poison, data = dd)
summary(mod_ex)

TUKEY1=TukeyHSD(mod_ex, which = "poison")
TUKEY2=TukeyHSD(mod_ex, which = "treat")
TUKEY1
TUKEY2

library(multcompView)
plot(TUKEY1, las=1 , col="brown")
plot(TUKEY2, las=1 , col="brown")

# all contrasts
mod_ex2 <- aov(y ~ combo, data = dd2)
summary(mod_ex2)
TUKEY3=TukeyHSD(mod_ex2)
TUKEY3
plot(TUKEY3)
####################################################################
######################### ANOVA WITHIN #############################
####################################################################

library(tidyverse)
library(ggpubr)
library(rstatix)

library(datarium)
data("selfesteem", package = "datarium") 
?selfesteem

dd=as.data.frame(selfesteem)
attach(dd)
head(dd)
dim(dd)

# use long format
subj <- rep(1:nrow(dd), each=3)
subj

time <- rep(c("time1", "time2", "time3"), nrow(dd))
time

a=cbind(dd$t1,dd$t2,dd$t3)
a

y=as.vector(t(a))
y

data <- data.frame(factor(subj), factor(time), y)
names(data) <- c("subj", "time", "y")
data

# plot the data
g = ggplot(data, aes(x=time, y=y))
g + geom_boxplot(aes(fill=time)) + ggtitle("Self-Esteem Score Data over time")

# density
ggplot(data) + geom_density(aes(x = y, fill = time), alpha = 0.8)

# normality check
shapiro.test(t1)
shapiro.test(t2)
shapiro.test(t3)

# sfericity check
bartlett.test(y,time)

# sfericity check - alternative but requires notions of MANOVA
mod <- lm(cbind(t1,t2,t3) ~ 1, data=dd)
SSD(mod) #Functions to compute matrix of residual sums of squares and products,
         # or the estimated variance matrix for multivariate linear models
estVar(mod)
mauchly.test(mod, X=~1)

# perform ANOVA within
# Error(id/time) is used to divide the error variance into 4 different clusters, 
# which therefore takes into account of the repeated measures. 
mod_rep <- aov(y ~ time + Error(subj/time), data)
summary(mod_rep)

# Tuckey post-hoc test
require(MASS)         ## for oats data set
require(nlme)         ## for lme()
require(multcomp)     ## for multiple comparison stuff

?lme
Lme.mod <- lme(y ~ time, random = ~1 | subj/time, data)
anova(Lme.mod)
summary(Lme.mod)

summary(glht(Lme.mod, linfct=mcp(time="Tukey")))

####################################################################
################## ANOVA WITHIN EXERCISE ###########################
####################################################################

# generate random vectors

data=matrix(rnorm(250, mean=100, sd=40), 50, 5)
data=round(data,2)

sex=c(rep("M",25), rep("F",25))
sex=as.factor(sex)

id=c(1:50)

data=cbind(id,data, sex)
colnames(data)=c("id", paste("Time", c(1:5)), "sex")

head(data)
dim(data)

data=as.data.frame(data)

data$id=as.factor(data$id)
data$sex=as.factor(data$sex)

data

# nice way to get the long format

library(reshape)

data_long = reshape(data, 
            direction = "long",
            varying = list(names(data)[2:6]),
            v.names = "Value",
            idvar = c("id", "sex"),
            timevar = "Year",
            times = 1:5)
data_long

# recent alternative

library(reshape2)
data_long2 <- melt(data, id.vars = c("id", "sex"))
data_long2

# plot the data using data_long

head(data_long)
g = ggplot(data_long, aes(x=as.factor(Year), y=Value))
g + geom_boxplot(aes(fill=Year)) + ggtitle("Blood Pressure over time")

# density
ggplot(data_long) + geom_density(aes(x = Value, fill = as.factor(Year)), alpha = 0.5)

# normality check
shapiro.test(data_long$Value[data_long$Year==1])
shapiro.test(data_long$Value[data_long$Year==2])
shapiro.test(data_long$Value[data_long$Year==3])
shapiro.test(data_long$Value[data_long$Year==4])
shapiro.test(data_long$Value[data_long$Year==5])

# sfericity check
bartlett.test(data_long$Value,data_long$Year)

# perform ANOVA within
mod_rep <- aov(Value ~ Year + Error(id/Year), data_long)
summary(mod_rep)


###########################################################################
############################### Bivariate Outliers ########################
###########################################################################

# not important here given that we have only one continuos outcome
# that's just for student's knowledge

library(aplpack)
?bagplot

bagplot(cbind(x,y),pch=16,cex=1)

###########################################################################
################ Mixed ANOVA with balanced designs ########################
###########################################################################

# In a sleep psychophysiology laboratory, the effects of a new psychotherapy (PT) 
# and a pharmacological treatment (FT) for the treatment of insomnia are being tested. 
# 10 sleepless people with problems in the falling asleep phase were selected: 
# 5 are treated with PT and 5 with FT. The day before the start of treatment (time 0),
# 30 days and finally 60 days, the number of hours slept by each individual is recorded.

# The dependent variable is the number of hours slept (hours), 
# while the predictors are time and treatment

# we need the long format

sleep <- read.csv2("sonno-long.csv")
sleep

colnames(sleep)=c("subject", "time", "treat","hours")
attach(sleep)

sleep$subject <- as.factor(sleep$subject)
sleep$time <- as.factor(sleep$time)
sleep$treat <- as.factor(sleep$treat)

levels(sleep$treat)
levels(sleep$time)

?tapply
with(sleep, tapply(hours, list(treat,time), mean))

# recent alternative
library(dplyr)
?summarise
sleep %>% group_by(time, treat) %>% summarise(mean=mean(hours))

# in the formula you must specify how to divide the error variance using the Error option;
# inside Error it will be necessary to specify that, for each level of the "time" factor,
# the observations are always carried out on the same subjects.

# aov works only with balanced designs
mod_aov_rep = aov(hours ~ treat * time + Error(subject/time), data=sleep)
summary(mod_aov_rep)

# TukeyHSD Mixed ANOVA
Lme.mod <- lme(hours ~ treat * time, random = ~1 | subject/time, sleep)
anova(Lme.mod)  # same results as before
summary(Lme.mod)
# Tuckey test? .... you should do all the combinations manually and use the usal code for Tukey


# Mauchly's Test of Sphericity 
# the variances of differences between all combinations of related conditions 
# (or group levels) are equal. 
# we can use the function anova_test

library(rstatix)
res <- anova_test(data = sleep, dv = hours, wid = subject, within = time)
res
# The null hypothesis is that the variances of the group differences are equal. 
# Thus, a significant p-value (p <= 0.05) indicates that the variances of 
# group differences are not equal.


###############################################################################
############# Mixed ANOVA with tidyverse, for every design #################### 
###############################################################################

# example from: https://www.datanovia.com/en/lessons/mixed-anova-in-r/

library(tidyverse)
library(ggpubr)
library(rstatix)

# Wide format
set.seed(123)
data("anxiety", package = "datarium")

anxiety
tail(anxiety)
?datarium::anxiety
# The data provide the anxiety score, measured at three time points, 
# of three groups of individuals practicing physical exercises at different 
# levels (grp1: basal, grp2: moderate and grp3: high)

?sample_n_by
# just to check the treatments
anxiety %>% sample_n_by(group, size = 3)

# Gather the columns t1, t2 and t3 into long format.
# Convert id and time into factor variables
?tidyr::gather
anxiety <- anxiety %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)

# Inspect some random rows of the data by groups
set.seed(123)
anxiety %>% sample_n_by(group, time, size = 2)

# Summary statistics for each cell
anxiety %>%
  group_by(time, group) %>%
  get_summary_stats(score, type = "mean_sd")

bxp <- ggboxplot(
  anxiety, x = "time", y = "score",
  color = "group", palette = "jco"
)
bxp

# check outliers
anxiety %>%
  group_by(time, group) %>%
  identify_outliers(score)

# check normality
anxiety %>%
  group_by(time, group) %>%
  shapiro_test(score)

# Homogeneity of variance of the between-subject factor (group)
?levene_test
anxiety %>%
  group_by(time) %>%
  levene_test(score ~ group)

# Homogeneity of covariances of the between-subject factor (group) 
?box_m
box_m(anxiety[, "score", drop = FALSE], anxiety$group)


# Two-way mixed ANOVA test
res.aov <- anova_test(
  data = anxiety, dv = score, wid = id,
  between = group, within = time
)

get_anova_table(res.aov)
?get_anova_table
?anova_summary
# measure of effect size that permits comparisons of results across 
# both between-subjects and within-subjects designs

# Effect of group at each time point
one.way <- anxiety %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between group levels
pwc <- anxiety %>%
  group_by(time) %>%
  pairwise_t_test(score ~ group, p.adjust.method = "bonferroni")
pwc

# Effect of time at each level of exercises group
one.way2 <- anxiety %>%
  group_by(group) %>%
  anova_test(dv = score, wid = id, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# Pairwise comparisons between time points at each group levels
# Paired t-test is used because we have repeated measures by time
pwc2 <- anxiety %>%
  group_by(group) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) 
pwc2

# Procedure for non-significant two-way interaction
anxiety %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

###########################################################################
############ NON-parametric tests for many groups #########################
###########################################################################

###########################################################################
###################### Kruskal-Wallis test ################################ 
###########################################################################

# http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

# Non-parametric alternative to one-way ANOVA test

# let's create some data for three groups

?rnorm

a=rnorm(100, mean = 0, sd = 1)
b=runif(100, min=0, max=2)
c=rbeta(100, 2, 2)
d=sample(1:5, 100, replace=TRUE)

a
b
c
d

data=cbind(a,b,c,d)
data

hist(a,prob=TRUE)
lines(density(a), lty="dotted")
shapiro.test(a)

hist(b,prob=TRUE)
lines(density(b), lty="dotted")
shapiro.test(b)

hist(c,prob=TRUE)
lines(density(c), lty="dotted")
shapiro.test(c)

hist(d,prob=TRUE)
lines(density(d), lty="dotted")
shapiro.test(d)

dd=c(a,b,c,d)

group=c(rep("g1",100),rep("g2",100),rep("g3",100),rep("g4",100))

group=as.factor(group)

class(group)
levels(group)

dataset=as.data.frame(cbind(dd,as.factor(group)))
dataset

library(ggplot2)
g = ggplot(dataset, aes(x=group, y=dd))
g + geom_boxplot(aes(x=group, fill=group))

?kruskal.test
kruskal.test(dd ~ group, data = dataset)

# test post-hoc 
# Adjust P-values for Multiple Comparisons
# The "BH" of Benjamini, Hochberg, and Yekutieli control the false discovery rate, 
# the expected proportion of false discoveries amongst the rejected hypotheses.
# The false discovery rate is a less stringent condition than the family-wise error rate,
# so these methods are more powerful than the others.
# Benjamini, Y., and Yekutieli, D. (2001). The control of the false discovery rate
# in multiple testing under dependency. Annals of Statistics 29, 1165-1188.

?pairwise.wilcox.test
pairwise.wilcox.test(dd, group, p.adjust.method = "BH", paired = FALSE)

###########################################################################
################################# Question ################################ 
###########################################################################

# Does a Non-parametric alternative to two-way ANOVA test exist?

# Interesting discussion here: https://stats.stackexchange.com/questions/41934/non-parametric-alternative-for-2-way-anova


###########################################################################
###################### Friedman test  ####################################
###########################################################################

# from https://www.datanovia.com/en/lessons/friedman-test-in-r/

# friedman.test is a Non-parametric alternative ANOVA for repeated measures
?friedman.test 

data("selfesteem", package = "datarium")
head(selfesteem, 3)

# another way to get the long format
?gather

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(selfesteem, 9)
tail(selfesteem, 9)

selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")

library(ggplot2)
library(ggpubr)
ggboxplot(selfesteem, x = "time", y = "score", add = "jitter")

res.fried <- selfesteem %>% friedman_test(score ~ time |id)
res.fried

# Effect size
# The Kendall's W can be used as the measure of the Friedman test effect size. 
# It is calculated as follow : W = X2/N(K-1); where W is the Kendall's W value; X2
# is the Friedman test statistic value; N is the sample size. k is the number 
# of measurements per subject (M. T. Tomczak and Tomczak 2014).
# The Kendall's W coefficient assumes the value from 0 (indicating no relationship)
# to 1 (indicating a perfect relationship).
# Kendall's W uses the Cohen's interpretation guidelines of 0.1 - < 0.3 (small effect),
# 0.3 - < 0.5 (moderate effect) and >= 0.5 (large effect). 
# Confidence intervals are calculated by bootstap.
selfesteem %>% friedman_effsize(score ~ time |id)

# pairwise comparisons
pwc <- selfesteem %>%
  wilcox_test(score ~ time, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
ggboxplot(selfesteem, x = "time", y = "score", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

###########################################################################
################################# Question ################################ 
###########################################################################

# Does a Non-parametric alternative to mixed ANOVA test exist?

# Interesting discussion here: https://stats.stackexchange.com/questions/304264/non-parametric-test-equivalent-to-mixed-anova

###########################################################################
########## Non-parametric alternative to mixed ANOVA ######################
###########################################################################

library(nparLD)

############################## Code for Figure 1 ##############################

### Plot (left) for Figure 1 ###
x<-seq(-4.5,4.5,0.001)
plot(x,pnorm(x,mean=0), type="l",lwd=2,xlab="x",ylab="",cex.lab=1.3,axes=F,xlim=c(-4.2,4.2))
points(x,pnorm(x,mean=2), type="l",lwd=2)
points(x,pnorm(x,1,sqrt(1/2)),type="l",lwd=2)
axis(1,labels=FALSE)
axis(2,labels=TRUE,font=2)
H<-expression(H(x))
Fj<-expression(F[j](x))
Fs<-expression(F[s](x))
pj<-expression(p[j]<p[s])
text(2.2,0.8,H,cex=1.3)
text(-1.1,0.4,Fj,cex=1.3)
text(2.7,0.4,Fs,cex=1.3)
text(-4,0.95,"(a)",cex=1.3)
text(-3,0.95,pj,cex=1.6)

### Plot (right) for Figure 1 ###
x<-seq(-4.5,4.5,0.001)
plot(x,pnorm(x,mean=0), type="l",lwd=2,xlab="x",ylab="",cex.lab=1.3,axes=F,xlim=c(-4.2,4.2))
points(x,pnorm(x,mean=2), type="l",lwd=2)
points(x,pnorm(x,1,sqrt(1/2)),type="l",lwd=2)
axis(1,labels=FALSE)
axis(2,labels=TRUE,font=2)
H<-expression(H(x))
Fj<-expression(F[j](x))
Fs<-expression(F[s](x))
pj<-expression(p[j]>p[s])
text(2.2,0.8,H,cex=1.3)
text(-1.1,0.4,Fs,cex=1.3)
text(2.6,0.4,Fj,cex=1.3)
text(-4,0.95,"(b)",cex=1.3)
text(-3,0.95,pj,cex=1.6)

####################### Opening the Package #######################

library("nparLD")

############################ Example 1 ############################

data("dental")

### Box plot for Figure 2 ###
boxplot(resp~time, data=dental, lwd=2, xlab="time",
        font.lab=2,cex.lab=2, main="Box Plots")

### 95% confidence interval plot for Figure 2 ###
ex.f1np<-nparLD(resp~time, data=dental,
                subject="subject", description=FALSE)
plot(ex.f1np)

### Summary of the statistical analysis ###
summary(ex.f1np)

### Multiple comparisons as in Table 4 ###
### 810 is for ages 8 vs 10 ###
### 812 is for ages 8 vs 12 ###
### 814 is for ages 8 vs 14 ###

m810<-which(((dental$time==8)+(dental$time==10))==1)
m812<-which(((dental$time==8)+(dental$time==12))==1)
m814<-which(((dental$time==8)+(dental$time==14))==1)

ex.f1np810<-nparLD(resp~time, data=dental[m810,],
                   subject="subject", description=FALSE)
ex.f1np812<-nparLD(resp~time, data=dental[m812,],
                   subject="subject", description=FALSE)
ex.f1np814<-nparLD(resp~time, data=dental[m814,],
                   subject="subject", description=FALSE)

summary(ex.f1np810)
summary(ex.f1np812)
summary(ex.f1np814)

### Statistical analysis using the nlme package ###
library("nlme")
ex.f1lme<-lme(resp ~ time, data = dental, random = ~ 1 | subject)
summary(ex.f1lme)

### Multiple comparisons using nlme ###
ex.f1lme810<-lme(resp ~ time, data = dental[m810,],
                 random = ~ 1 | subject)
ex.f1lme812<-lme(resp ~ time, data = dental[m810,],
                 random = ~ 1 | subject)
ex.f1lme814<-lme(resp ~ time, data = dental[m810,],
                 random = ~ 1 | subject)

summary(ex.f1lme810)
summary(ex.f1lme812)
summary(ex.f1lme814)

############################ Example 2 ############################

data("rat")

### Box plot for Figure 3 ###
boxplot(resp~group*time, data=rat,
        names=FALSE,col=c("grey",2,3),lwd=2)
axis(1,at=2,labels="Time 0",font=2,cex=2)
axis(1,at=5,labels="Time 1",font=2,cex=2)
axis(1,at=8,labels="Time 2",font=2,cex=2)
axis(1,at=11,labels="Time 3",font=2,cex=2)
axis(1,at=14,labels="Time 4",font=2,cex=2)
legend(2,190,c("Control","Thiour","Thyrox"),
       lwd=c(3,3,3),col=c("grey",2,3),cex=2)

### 95% confidence interval plot for Figure 3 ###
ex.f1f1np<-nparLD(resp~time*group, data=rat,
                  subject="subject", description=FALSE)
plot(ex.f1f1np)

### Summary of the statistical analysis ###
summary(ex.f1f1np)

############################ Example 3 ############################

data("respiration")
par(mfrow=c(1,2))

### Box plot (left) for Figure 4 ###
boxplot(resp~treatment*time,data=respiration[which(center==1),],
        names=FALSE,ylim=c(-1,5),col=c("grey",2),lwd=2,main="Center 1")
axis(1,at=2,labels="Time 1",font=2,cex=2)
axis(1,at=5,labels="Time 2",font=2,cex=2)
axis(1,at=8,labels="Time 3",font=2,cex=2)
axis(1,at=11,labels="Time 4",font=2,cex=2)
axis(1,at=14,labels="Time 5",font=2,cex=2)
legend(2,5,c("Treat A","Treat P"),
       lwd=c(2,2),col=c("grey",2),cex=1.2)

### Box plot (right) for Figure 4 ###
boxplot(resp~treatment*time,data=respiration[which(center==2),],
        names=FALSE,ylim=c(-1,5),col=c("grey",2),lwd=2,main="Center 2")
axis(1,at=2,labels="Time 1",font=2,cex=2)
axis(1,at=5,labels="Time 2",font=2,cex=2)
axis(1,at=8,labels="Time 3",font=2,cex=2)
axis(1,at=11,labels="Time 4",font=2,cex=2)
axis(1,at=14,labels="Time 5",font=2,cex=2)
legend(2,5,c("Treat A","Treat P"),
       lwd=c(2,2),col=c("grey",2),cex=1.2)

### 95% confidence interval plot for Figure 4 ###
ex.f2f1np<-nparLD(resp~time*center*treatment,
                  data=respiration, subject="patient", description=FALSE)
plot(ex.f2f1np)

### Summary of the statistical analysis ###
print(ex.f2f1np)

### Statistical analysis using the nlme package ###
library("nlme")

ex.f2f1lme <- lme(resp ~ time*treatment*center,
                  data = respiration,random = ~ 1 | patient)
summary(ex.f2f1lme)







