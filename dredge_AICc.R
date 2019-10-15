# always run this at the beggining
options(repos = c(CRAN = "http://cran.rstudio.com"))

rm(list = ls())
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.


install.packages("tidyverse")
install.packages("lme4")
install.packages("lmerTest")
install.packages("MuMIn")
install.packages("metafor")
install.packages("Matrix")
install.packages("mplot")
install.packages("shiny")
install.packages("googleVis")
install.packages("tidyverse")
install.packages("texreg")
install.packages("glmulti")
install.packages("devtools")
#libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)
library(metafor)
library(Matrix)
library("mplot")
library("shiny")
library(googleVis)
library("tidyverse")
library(broom)
library(texreg)
library(glmulti)
library(rJava)
library(psych)
library(foreign)
library(lme4)
library(MASS)
# which model best describe your data?
#genotypic selection analysis to quantify natural selection acting on each trait (while controlling for other traits) 
#and assess whether plant defenses alter the strength or direction of natural selection imposed on these traits. 
#While this may sound complicated, it turns out that a single multiple regression is all that's required to do this: 
#relative fitness is the response variable and standardized traits (i.e. mean of 0 and standard deviation of 1), treatments, and their interactions are the predictors. 
#This multiple regression regression approach is a common way of measuring natural selection in nature (see Lande and Arnold 1983, Rausher 1992, Stinchcombe 2002).

##load the data from excel
#data_all<-read.spss(file.choose(),to.data.frame=TRUE)
head(data)
colnames(data)
colnames(data)[6]<-"Inattention"
colnames(data)[16]<-"Chicken_span"
attach(data)
class(Gender)
Gender<-factor(Gender)
class(FNG)
FNG<-factor(FNG)
########################################## INFELXIBILITY ##################################################################
options(na.action = "na.fail")

vars<-c("FNG","Gender","Age","Vineland",
        "Inattention","Hyperactivity","Anxiety","Inflexibility")
inflx<- data[vars]

inflx<-na.omit(inflx)

inflex_glm1<-glm(Inflexibility~FNG+Gender+Age+Vineland+
                 Inattention+Hyperactivity+Anxiety+
                 Inattention:FNG+Hyperactivity:FNG+Anxiety:FNG, data=inflx)

inflex_dredge<-dredge(inflex_glm1, trace=T, rank="AICc", beta = "sd", extra = "adjR^2")
options(na.action = "na.omit") 

# extract values
summary(inflex_dredge)

# select the top 3 models
good_models<-get.models(subset(inflex_dredge,delta<=2),subset=T)
good_models

#see how many there are
good_models[1]
good_models[2]
good_models[3]

#model selection table
par(mar=c(3,5,6,4))
plot(inflex_dredge,labAsExpr = TRUE)

# table of top models

inflex.table<-model.sel(subset(inflex_dredge,delta<=2))
table<-as.data.frame(inflex.table)[7:11]
table

# clean up values
table[,2:3]<-round(table[,2:3],2)
table[,4:5]<-round(table[,4:5],3)
#changes names
names(table)[1]="K"
table$Model<-rownames(table)
# TABLEs
for(i in 1:nrow(table))
  table$Model[i]<- as.character(formula(paste(good_models[i])))[3]
table
# reorder 
table<-table[,c(6,1,2,3,4,5)]
for(i in 1:nrow(table))table$Model[i]<-as.character(formula)

#importance of the coefficients
importance(inflex_dredge)

# write to a file, here a comma seperated values format
# make sure your working directory is properly specified
write.csv(table,"Inflexibility.csv", row.names = F)
?write.csv
#' MuMIn also calculates model averaged parameter estimates
#' using the model.avg function. Here you have several options
#' most importantly is "revised.var" ALWAYS USE IT.

# Model average using all candidate models
top_models<-summary(model.avg(inflex_dredge,delta <= 2)) #, revised.var=FALSE))
top_models

# you can save this as a table

impo_Inflex<-importance(inflex_dredge)
barplot(t(impo_Inflex), main="Inflexibility")

#---------------------------- Plotting ----------------------------------------

#### this is a mess 
library(devtools)
devtools::install_github("strengejacke/sjPlot")
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

coef_inflex<-coef(top_models)
barplot(coef_inflex,main="Inflexibility coefficients") # FIX me


#----------------------------- G MULTI -----------------------------------#
library("glmulti")

gmulti_inflex<-glmulti(Inflexibility~.,method="h",
                         level=2,fitfunction = myglm , crit = "aicc", plotty = TRUE,
                         marginality = TRUE,data=inflx)


# library('utils')
# retain_var <- c('FNG','Gender','Age','Vineland',
#                   'Inattention','Hyperactivity','Anxiety') #set variables interested for testing
# excl_inter_effects <- c('Gender','Age','Vineland') #set variables to exclude
# set_depth <- 2 #generate 2 way interaction to exclude explicitly
# excl_inter_form <- t(combn(excl_inter_effects,set_depth)) 
# Paste <- function(x) paste(x, collapse = ":") 
# excl_inter_form <- apply(excl_inter_form, 1, Paste)
# 
# rm(list = c('Paste','set_depth','excl_inter_effects')) #clean up

# ----- Generate formula incorporating exclusion ----- 

glm_formula <- as.formula(paste("y ~",paste(retain_var, collapse= "+"),"-",paste(excl_inter_form, collapse= "-")))

rma.glmulti.ran <- function (glm_formula, data, random, ...) 


myglm(Inflexibility~.,data=inflx)

summary(gmulti_inflex)

print.glmulti(gmulti_inflex)

weightable(gmulti_inflex)

#plotting
plot(gmulti_inflex,type = "p") #IC profile
plot(gmulti_inflex,type = "s") ##each term estimated importance
plot(gmulti_inflex,type = "w") # ranked relative evidence of the model
plot(gmulti_inflex,type = "r") # plotting from the best to the worst model

bestmodel<-gmulti_inflex@formulas[3]
importances<-summary(gmulti_inflex)$modelweights

plot(gmulti_inflex,importances)

#compare output
summary(gmulti_inflex@objects[[1]])
summary(gmulti_inflex, method)
summary(gmulti_inflex, coef)
gmulti_inflex@formulas

# other summaries
aicc_values=summary(gmulti_inflex)$icvalues
complex=gmulti_inflex@K
all_models=gmulti_inflex@objects
#model complexity
plot(aicc_values,complex)

# model averaginng
coef(gmulti_inflex)
?coef.glmulti

predict(gmulti_inflex, newdata =inflx,se.fit = T)

#compare models the top 3
top<-weightable(gmulti_inflex)
top <- top[top$aicc <= min(top$aicc) + 2,]
top


# COEFFICIENTS SELECTION
eval(metafor:::.glmulti)
inflx_coeff<-as.data.frame(coef(gmulti_inflex))

barplot(mmi$Importance)


mmi <- as.data.frame(coef(gmulti_inflex))
mmi <- data.frame(Estimate=mmi$Est, SE=sqrt(mmi$Uncond), Importance=mmi$Importance, row.names=row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2*pnorm(abs(mmi$z), lower.tail=FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(mmi, 4)



#---------------------------------------- M plot ---------------------------------------##


fullmodel=glm(Inflexibility~FNG+Gender+Age_Mo+Vineland+
               Inattention+Hyperactivity+Anxiety+
               Inattention:FNG+Hyperactivity:FNG+Anxiety:FNG, data=inflx)

round(summary(fullmodel)$coef,2)

# to double check
step_inflex=step(fullmodel,trace=1,initial.stepwise = TRUE)

vis.art=vis(fullmodel,B=150,redundant = TRUE,nbest = "all")

print(vis.art)

# plotting 
plot(vis.art, which = "vip") #, interactive=TRUE)
plot(vis.art, which = "lvk")
plot(vis.art, which="boot")
plot(vis.art, interactive = FALSE, highlight = "Anxiety", which = "lvk")

#  adaptive fence  #
af.art=af(fullmodel, B=150, n.c = 50)
plot(af.art, interactive = FALSE, best.only = TRUE)

# plot all models
mplot(fullmodel,vis.art,af.art)

# -------------------------------------------- MODEL AVERAGING ------------------------------
install.packages("leaps")
# INFLEXIBILITY

# chose the top 3 models







############################################# Social Understanding ######################################
vars<-c("FNG","Gender","Age","Vineland",
        "Inattention","Hyperactivity","Anxiety",
        "Social_Understanding")
SU<-data[vars]

# -------------------------- GMULTI ----------------------------------- #
glmSU_behav<-glmulti(Social_Understanding~FNG+Gender+Age+Vineland+
                    Inattention+Hyperactivity+Anxiety+
                    Inattention:FNG+Hyperactivity:FNG+Anxiety:FNG, data=SU,
                  na.action = "na.omit", crit = "aicc", plotty = TRUE,marginality = TRUE)

summary(glmSU_behav)

print.glmulti(glmSU_behav)

#plotting
plot(glmSU_behav,type = "p") #IC profile
plot(glmSU_behav,type = "s") ##each term estimated importance
plot(glmSU_behav,type = "w") # ranked relative evidence of the model
plot(glmSU_behav,type = "r") # plotting from the best to the worst model

bestmodel<-glmSU_behav@formulas[3]
importances<-summary(glmSU_behav)$modelweights

plot(glmSU_behav,importances)


#compare output
summary(glmSU_behav@objects[[1]])
summary(glmSU_behav, method)
summary(glmSU_behav, coef)
glmSU_behav@formulas

#compare models the top 3
top<-weightable(glmSU_behav)
top <- top[top$aicc <= min(top$aicc) + 2,]
top


# COEFFICIENTS ESTIMATE

coef(glmSU_behav)

mmi <- as.data.frame(coef(glmSU_behav))
mmi <- data.frame(Estimate=mmi$Est, SE=sqrt(mmi$Uncond), Importance=mmi$Importance, row.names=row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2*pnorm(abs(mmi$z), lower.tail=FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(mmi, 4)


#---------------------------------------- M plot ---------------------------------------##


SU_fullmodel=glm(Social_Understanding~FNG+Gender+Age_Mo+Vineland+
                Inattention+Hyperactivity+Anxiety+
                Inattention:FNG+Hyperactivity:FNG+Anxiety:FNG, data=SU)

round(summary(SU_fullmodel)$coef,2)

# to double check
step_SU=step(SU_fullmodel,trace=1,initial.stepwise = TRUE)

vis.art_SU=vis(SU_fullmodel,B=150,redundant = TRUE,nbest = "all")

print(vis.art_SU)

# plotting 
plot(vis.art_SU, which = "vip") #, interactive=TRUE)
plot(vis.art_SU, which = "lvk")
plot(vis.art_SU, which="boot")
plot(vis.art_SU, interactive = FALSE, highlight = "Anxiety", which = "lvk")


#  adaptive fence  #
af.art=af(SU_fullmodel, B=150, n.c = 50)
plot(af.art, interactive = FALSE, best.only = TRUE)

# plot all models
mplot(SU_fullmodel,vis.art,af.art)



####################################### SOCIAL wITHDRAWAL #############################################

vars<-c("FNG","Gender","Age_Mo","Vineland",
        "Inattention","Hyperactivity","Anxiety",
        "Social_Withdrawal")

sw_data<-data[vars]


# ------------------------------ G multi  ----------------------------------------------------

gmltiSW_behav<-glmulti(Social_Withdrawal~FNG+Gender+Age_Mo+Vineland+
                    Inattention+Hyperactivity+Anxiety+
                    Inattention:FNG+Hyperactivity:FNG+Anxiety:FNG, data=sw_data,
                  na.action = "na.omit", crit = "aicc", plotty = TRUE,marginality = TRUE)


summary(gmltiSW_behav)

print.glmulti(gmltiSW_behav)

#plotting
plot(gmltiSW_behav,type = "p") #IC profile
plot(gmltiSW_behav,type = "s") ##each term estimated importance
plot(gmltiSW_behav,type = "w") # ranked relative evidence of the model
plot(gmltiSW_behav,type = "r") # plotting from the best to the worst model

bestmodel<-gmltiSW_behav@formulas[3]
importances<-summary(gmltiSW_behav)$modelweights

plot(gmltiSW_behav,importances)


#compare output
summary(gmltiSW_behav@objects[[1]])
summary(gmltiSW_behav, method)
summary(gmltiSW_behav, coef)
gmltiSW_behav@formulas

#compare models the top 3
top<-weightable(gmltiSW_behav)
top <- top[top$aicc <= min(top$aicc) + 2,]
top


# COEFFICIENTS ESTIMATE

coef(gmltiSW_behav)

mmi <- as.data.frame(coef(gmltiSW_behav))
mmi <- data.frame(Estimate=mmi$Est, SE=sqrt(mmi$Uncond), Importance=mmi$Importance, row.names=row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2*pnorm(abs(mmi$z), lower.tail=FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(mmi, 4)


#---------------------------------------- M plot ---------------------------------------##


Sw_fullmodel=glm(Social_Withdrawal~FNG+Gender+Age_Mo+Vineland+
                   Inattention+Hyperactivity+Anxiety+
                   Inattention:FNG+Hyperactivity:FNG+Anxiety:FNG, data=sw_data)

round(summary(Sw_fullmodel)$coef,2)

# to double check
step_SW=step(Sw_fullmodel,trace=1,initial.stepwise = TRUE)

vis.art_SW=vis(Sw_fullmodel,B=150,redundant = TRUE,nbest = "all")

print(vis.art_SW)

# plotting 
plot(vis.art_SW, which = "vip") #, interactive=TRUE)
plot(vis.art_SW, which = "lvk")
plot(vis.art_SW, which="boot")
plot(vis.art_SW, interactive = FALSE, highlight = "Anxiety", which = "lvk")


#  adaptive fence  #
af.art_SW=af(SW_fullmodel, B=150, n.c = 50)
plot(af.art_SW, interactive = FALSE, best.only = TRUE)

# plot all models
mplot(Sw_fullmodel,vis.art,af.art)


################################### MODEL AVERAGING OR SUMMARIES ################################


