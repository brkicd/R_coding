# plotting the coefficients

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(emmeans)
library(dplyr)
# set the theme

theme_set(theme_sjplot())

plot_model(summary(inflex_avg))

library(emmeans)
library(jtools)
install.packages("ggstance")
library(ggstance)
plot_summs(inflex_glm, scale=TRUE,inner_ci_level = .9, sort="magnitude")

# this is the one
Inflex_model<- data.frame (Variable =rownames(estim_inflex),
                           Coefficient=estim_inflex[,1],
                           SE=estim_inflex[,2],
                           CI_low=estim_inflex[,6],
                           CI_high=estim_inflex[,7],
                           modelName="Inflexibility coefficients")

###### change names 
class(Inflex_model$Variable)
library(plyr)
Inflex_model$Variable<-revalue(Inflex_model$Variable, c("FNGsyptic"= "FNG"))
Inflex_model$Variable<-revalue(Inflex_model$Variable, c("FNGsyptic:Hyperactivity"= "FNG:Hyperactivity"))
Inflex_model$Variable<-revalue(Inflex_model$Variable, c("FNGsyptic:Inattention"= "FNG:Inattention"))
Inflex_model$Variable<-revalue(Inflex_model$Variable, c("Anxiety:FNGsyptic"= "FNG:Anxiety"))


#Specify the width of your confidence intervals
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier

par(mar = c(8, 8, 5, 5))

Inflex_model_filtered<-Inflex_model%>%
  filter(CI_high>0)
Inflex_model_filtered

p1_i<-ggplot(Inflex_model_filtered, aes(colour="Inflexibility"))

p1_i<-p1_i+geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                              ymax = Coefficient + SE*interval1),
                          lwd = 2, position = position_dodge(width = 1/2)) 
  #geom_hline(yintercept=which(Inflex_model$Coefficient=='-2', linetype=2))

p1_i <- p1_i + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")+
  geom_hline(aes(fill=Variable),yintercept = 0,colour="grey50", linetype = 2) 
#p1_i<-p1_1+geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50")
p1_i <- p1_i + coord_flip() + xlab("")+ylab("Inflexibility ratio (95% CI)")+theme_classic()+
  theme(plot.title=element_text(size=16,face="bold"),
        #axis.text.y=element_blank(size=10),
        #axis.ticks.y=element_blank(size=10),
        axis.text.x=element_text(size=12,face="bold"),
        axis.text.y=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))
p1_i<- p1_i + ggtitle("Inflexibility")+scale_color_manual(values =c("#DC143C"))

print(p1_i)  # The trick to these is position_dodge().

#dev.off()
# library("dplyr")
# library("tidyr")
# install.packages("broom.mixed")
# library("broom.mixed")
# p1_i+geom_vline(xintercept=0,lty=2)
# print(p1_i) 
# 
# # # or alternatively
# g<-ggplot(estim_inflex, aes(vars, Estimate)) +
#   theme(axis.title.x =element_text(margin=margin(0,20,0,0)))+
#   theme(axis.title.x = element_blank())+
# 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
# 
#   geom_errorbar(aes(ymin=CI_low, ymax=CI_high),
#                 lwd=1, width=0, position = position_dodge(width = 0.5)) +
#   labs(x ="")+
#   geom_point(size=4, pch=21,position = position_dodge(width = 0.5)) +
# 
#   theme(panel.background = element_rect(fill = "white"))
# g
# 
# # or even 
# 
# par(mar = c(8, 8, 5, 5))
# inflex_plot<-estim_inflex_2 %>%
#   filter(vars!="(Intercept)")%>%
#   ggplot(aes(vars,Estimate))+
#   geom_bar(position =position_dodge(), stat="identity", fill = "#DC143C", size=.3)+
#   #geom_errorbar(aes(ymin=Estimate - SE*interval2, ymax=Estimate + SE*interval2), 
#   geom_errorbar(aes(ymin=CI_low, ymax=CI_high),
#                 #lwd=1,
#                 width=0.2, position = position_dodge(width = 0.5))+
#   geom_point(data=estim_inflex_2 %>% dplyr::filter(sig=="padj<.05"), aes(col=sig), colour="red", size=2)+
#   #scale_color_manual(values=c("red")) +
#   coord_flip()+
#   xlab("")+
#   ylab("Beta values")+
#   ggtitle("Model averaged coefficients")
# inflex_plot+ theme(
#   axis.text.y=element_text(face="bold",size=12),
#   axis.text.x=element_text(size=10),
#   panel.background = element_blank(),
#   axis.line = element_line(size = 0.5,linetype = "solid",colour = "black"),
#   plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0), size=12, face="bold"))



############################################ SOCIAL WITHDRAWAL ################################
#change names without typos

SW_summ<-summary(SW_avg)
CI_SW<-as.data.frame(confint(SW_avg))
impo_SW<-importance(SW_avg)
coef(SW_avg)

# Put estimates and CI together 
#estim_SW<-as.data.frame(SW_summ$coefmat.full)

estim_SW<-as.data.frame(SW_summ$coefmat.subset)
estim_SW$CI_low<-CI_SW$`2.5 %`
estim_SW$CI_high<-CI_SW$`97.5 %`
estim_SW$vars<-rownames(estim_SW)

estim_SW
colnames(estim_SW)
colnames(estim_SW)[2]<-"SE"
colnames(estim_SW)[3]<-"AdjSE"
colnames(estim_SW)[5]<-"p-value"


# this is the one
estim_SW_3<- data.frame (Variable =rownames(estim_SW),
                           Coefficient=estim_SW[,1],
                           SE=estim_SW[,2],
                           CI_low=estim_SW[,6],
                           CI_high=estim_SW[,7],
                           modelName="SW")

# change row names
estim_SW_3$Variable<-revalue(estim_SW_3$Variable, c("FNGsyptic"= "FNG"))
estim_SW_3$Variable<-revalue(estim_SW_3$Variable, c("FNGsyptic:Hyperactivity"= "FNG:Hyperactivity"))
estim_SW_3$Variable<-revalue(estim_SW_3$Variable, c("FNGsyptic:Inattention"= "FNG:Inattention"))
estim_SW_3$Variable<-revalue(estim_SW_3$Variable, c("Anxiety:FNGsyptic"= "FNG:Anxiety"))


# get rid of the intercept
SW_3<-estim_SW_3 %>%
  filter(Variable!="(Intercept)")
droplevels()


SW_plot<-ggplot(SW_3, aes(colour="Social Withdrawal"))

SW_plot<-SW_plot+geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                              ymax = Coefficient + SE*interval1),
                          lwd = 2, position = position_dodge(width = 1/2)) 
#geom_hline(yintercept=which(Inflex_model$Coefficient=='-2', linetype=2))

SW_plot <- SW_plot + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                   ymax = Coefficient + SE*interval2),
                               lwd = 1/2, position = position_dodge(width = 1/2),
                               shape = 21, fill = "WHITE")+
  geom_hline(aes(fill=Variable),yintercept = 0,colour="grey50", linetype = 2) 
#p1_i<-p1_1+geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50")
SW_plot <- SW_plot + coord_flip() + xlab("")+ylab("Social Withdrawal ratio (95% CI)")+theme_classic()+
  theme(plot.title=element_text(size=16,face="bold"),
        #axis.text.y=element_blank(size=10),
        #axis.ticks.y=element_blank(size=10),
        axis.text.x=element_text(size=12,face="bold"),
        axis.text.y=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))
SW_plot<- SW_plot + ggtitle("Social Withdrawal")+scale_color_manual(values =c("#20B2AA"))

print(SW_plot) 


################################# Social Understanding ##################################

# take a look at the data
estim_SU
# see if you have to change vars names
rownames(estim_SU)

# prepare the data
SU_model<- data.frame (Variable =rownames(estim_SU),
                         Coefficient=estim_SU[,1],
                         SE=estim_SU[,2],
                         CI_low=estim_SU[,6],
                         CI_high=estim_SU[,7],
                         modelName="SU")


#change names without typos
SU_model$Variable<-revalue(SU_model$Variable, c("FNGsyptic"= "FNG"))
SU_model$Variable<-revalue(SU_model$Variable, c("FNGsyptic:Hyperactivity"= "FNG:Hyperactivity"))
SU_model$Variable<-revalue(SU_model$Variable, c("FNGsyptic:Inattention"= "FNG:Inattention"))
SU_model$Variable<-revalue(SU_model$Variable, c("Anxiety:FNGsyptic"= "FNG:Anxiety"))


# plot the coefficients
SU_model_f<-SU_model%>%
  filter(Variable!="(Intercept)")%>%
droplevels()

SU_i<-ggplot(SU_model_f, aes(colour="Social Understanding"))

SU_i<-SU_i+geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                              ymax = Coefficient + SE*interval1),
                          lwd = 2, position = position_dodge(width = 1/2)) 
#geom_hline(yintercept=which(Inflex_model$Coefficient=='-2', linetype=2))

SU_i <- SU_i + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                   ymax = Coefficient + SE*interval2),
                               lwd = 1/2, position = position_dodge(width = 1/2),
                               shape = 21, fill = "WHITE")+
  geom_hline(aes(fill=Variable),yintercept = 0,colour="grey50", linetype = 2) 
#p1_i<-p1_1+geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50")
SU_i <- SU_i + coord_flip() + xlab("")+ylab("Social Understanding (95% CI)")+theme_classic()+
  theme(plot.title=element_text(size=16,face="bold"),
        #axis.text.y=element_blank(size=10),
        #axis.ticks.y=element_blank(size=10),
        axis.text.x=element_text(size=12,face="bold"),
        axis.text.y=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))
SU_i<- SU_i + ggtitle("Social Understanding")+scale_color_manual(values =c("#4682B4"))

print(SU_i) 

dev.off()



