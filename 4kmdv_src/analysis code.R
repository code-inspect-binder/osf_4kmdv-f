#set things up
library(tidyr)
library(Rmisc)
library(car)
library(ggplot2)

#STUDY 1

#make sure condition is a factor
study1$Condition <- as.factor(study1$Condition)

#convert to long format, get descriptives
study1$ID <- factor(study1$ID)
study1_long <- gather(study1, item, prediction, PredictQ1:PredictQ6, factor_key=TRUE)
study1_long$Condition <- as.factor(study1_long$Condition)
group.CI(prediction ~ Condition + AgeGroup, data=study1_long, ci = 0.95)

#binomial logistic regression testing for effect of condition
model1 <- glm(cbind(PredictTotal, PredictTrials-PredictTotal) ~ Condition, data = study1, family=binomial)
summary(model1)
Anova(model1, type="III", test="Wald")

#saving condition only, effect of age
model2 <- glm(cbind(PredictTotal, 6-PredictTotal) ~ Age, data = subset(study1, Condition==4), family=binomial)
summary(model2)
Anova(model2, type="III", test="Wald")

#compare each group to chance
#harmful
harmful <- glm(cbind(PredictTotal, PredictTrials-PredictTotal) ~ 1, data = subset(study1, Condition==1), family=binomial)
Anova(harmful, type="III", test="Wald")
#odds ratio
exp(harmful$coefficients)
#CI for odds ratio
exp(confint(harmful))

#prosocial
prosocial <- glm(cbind(PredictTotal, PredictTrials-PredictTotal) ~ 1, data = subset(study1, Condition==2), family=binomial)
Anova(prosocial, type="III", test="Wald")

#saving (3-year-olds)
saving3s <- glm(cbind(PredictTotal, PredictTrials-PredictTotal) ~ 1, data = subset(study1, Condition==4 & AgeGroup==0), family=binomial)
Anova(saving3s, type="III", test="Wald")

#friendship
friendship <- glm(cbind(PredictTotal, PredictTrials-PredictTotal) ~ 1, data = subset(study1, Condition==3), family=binomial)
Anova(friendship, type="III", test="Wald")
#odds ratio
exp(friendship$coefficients)
#CI for odds ratio
exp(confint(friendship))

#saving (4-year-olds)
saving4s <- glm(cbind(PredictTotal, PredictTrials-PredictTotal) ~ 1, data = subset(study1, Condition==4 & AgeGroup==1), family=binomial)
Anova(saving4s, type="III", test="Wald")
#odds ratio
exp(saving4s$coefficients)
#CI for odds ratio
exp(confint(saving4s))

#plot
study1_long$Condition <- as.numeric(study1_long$Condition)
study1_long$Condition[study1_long$AgeGroup==1] <- "5"
study1_long$Condition <- as.factor(study1_long$Condition)
plot1 <- ggplot(study1_long, aes(x=Condition, y=prediction)) + 
  stat_summary(fun.y=mean, position=position_dodge(), geom="bar", fill="deepskyblue2") + 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=.1, position=position_dodge(.9)) + 
  geom_hline(yintercept=.5, linetype=2) + 
  scale_x_discrete(labels=c("Harmful", "Prosocial","Friendship","Saving (3s)","Saving (4s)")) +
  ylim(c(0,1)) + theme_bw() + xlab("Condition") + ylab("Proportion of intragroup predictions") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(aes(x=.6, y=.98, xend=5.4, yend=.98), size=.1) +
  annotate("text",x=3,y=1,label="***", size=6) +
  annotate("text",x=5,y=.85,label="***", size=6) +
  annotate("text",x=3,y=.9,label="***", size=6) + 
  annotate("text",x=1,y=.55,label="**", size=6)
plot1

#STUDY 2

#convert to long format, get descriptives
study2$ID <- factor(study2$ID)
study2$Behavior <- factor(study2$Behavior)
study2$Context <- factor(study2$Context)
study2_long <- gather(study2, item, prediction, PredictQ1:PredictQ6, factor_key=TRUE)
group.CI(prediction ~ Behavior + Context, data=study2_long, ci = 0.95)

#binomial logistic regression testing for effects of behavior and context
model3 <- glm(cbind(PredictMatch, NumQs-PredictMatch) ~ Behavior + Context + Behavior*Context, data = study2, family=binomial)
Anova(model3, type="III", test="Wald")

#effect of context in harmful conditions
harmmodel <- glm(cbind(PredictMatch, NumQs-PredictMatch) ~ Context, data = subset(study2, Behavior=="Harmful"), family=binomial)
Anova(harmmodel, type="III", test="Wald")

#effect of context in prosocial conditions
promodel <- glm(cbind(PredictMatch, NumQs-PredictMatch) ~ Context, data = subset(study2, Behavior=="Prosocial"), family=binomial)
Anova(promodel, type="III", test="Wald")

#comparisons to chance
#intergroup harm
interharm <- glm(cbind(PredictMatch, 6-PredictMatch) ~ 1, data = subset(study2, Behavior=="Harmful" & Context=="Intergroup"), family=binomial)
Anova(interharm, type="III", test="Wald")
#odds ratio
exp(interharm$coefficients)
#CI for odds ratio
exp(confint(interharm))

#intragroup harm
intraharm <- glm(cbind(PredictMatch, 6-PredictMatch) ~ 1, data = subset(study2, Behavior=="Harmful" & Context=="Intragroup"), family=binomial)
Anova(intraharm, type="III", test="Wald")

#intergroup prosocial
interpro <- glm(cbind(PredictMatch, 6-PredictMatch) ~ 1, data = subset(study2, Behavior=="Prosocial" & Context=="Intergroup"), family=binomial)
Anova(interpro, type="III", test="Wald")

#intragroup prosocial
intrapro <- glm(cbind(PredictMatch, 6-PredictMatch) ~ 1, data = subset(study2, Behavior=="Prosocial" & Context=="Intragroup"), family=binomial)
Anova(intrapro, type="III", test="Wald")
#odds ratio
exp(intrapro$coefficients)
#CI for odds ratio
exp(confint(intrapro))

#plot
plot2 <- ggplot(study2_long, aes(x=Behavior, y=prediction, group=Context, fill=Context)) + 
  stat_summary(fun.y=mean, position=position_dodge(), geom="bar") + 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=.1, position=position_dodge(.9)) + 
  geom_hline(yintercept=.5, linetype=2) + xlab("Behavior") +
  ylim(c(0,1)) + theme_bw() + ylab("Proportion of predictions matching observed evidence") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text",x=.77,y=.75,label="***", size=6) +
  geom_segment(aes(x=.6, y=.88, xend=2.4, yend=.88), size=.1) +
  annotate("text",x=1.5,y=.9,label="**", size=6) +
  annotate("text",x=2.23,y=.73,label="**", size=6)
plot2

#STUDY 2B

#convert to long format, get descriptives
study2b$ID <- factor(study2b$ID)
study2b$Behavior <- factor(study2b$Behavior)
study2b_long <- gather(study2b, item, prediction, PredictQ1:PredictQ6, factor_key=TRUE)
group.CI(prediction ~ Behavior, data=study2b_long, ci = 0.95)

#compare each condition to chance
baseharm <- glm(cbind(PredictTotal, NumQs-PredictTotal) ~ 1, data = subset(study2b, Behavior=="harmful"), family=binomial)
Anova(baseharm, type="III", test="Wald")
basepro <- glm(cbind(PredictTotal, NumQs-PredictTotal) ~ 1, data = subset(study2b, Behavior=="prosocial"), family=binomial)
Anova(basepro, type="III", test="Wald")