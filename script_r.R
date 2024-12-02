##CFA on dataset with only hsc
##Dataset
##reading dataset
rm(list=ls())
library(readxl)
X <- read_excel("")

View(X)
##missing
##counting missing
totalcells = prod(dim(X))
print("Total number of cells ")
print(totalcells)


##calculating the number of cells with na
missingcells = sum(is.na(X))
print("Missing value cells")
print(missingcells)

# calculating percentage of missing values
percentage = (missingcells * 100 )/(totalcells)
print("Percentage of missing values' cells")
print (percentage) ##4.7% of missing 

##CFA and measurement invariance 
library(lavaan)
mod3Fbif <- 
  "
  EOE =~ HSC_4 + HSC_6 + HSC_8 + HSC_9 + HSC_12
  LST =~ HSC_2 + HSC_7 + HSC_11
  AES =~ HSC_1 + HSC_3 + HSC_5 + HSC_10
  HSC =~ HSC_1 + HSC_2 + HSC_3 + HSC_4 + HSC_5 + HSC_6 + 
HSC_7 + HSC_8 + HSC_9 + HSC_10 + HSC_11 + HSC_12
"
fitmod3Fbif <- cfa(mod3Fbif, std.lv = TRUE, orthogonal = TRUE, missing = "ML", data = X, group = "Group")

summary(fitmod3Fbif, fit.measures=TRUE)
## item 11 in mothers negative variance 

##metric invariance
mod3Fbif <- 
  "
  EOE =~ HSC_4 + HSC_6 + HSC_8 + HSC_9 + HSC_12
  LST =~ HSC_2 + HSC_7 + HSC_11
  AES =~ HSC_1 + HSC_3 + HSC_5 + HSC_10
  HSC =~ HSC_1 + HSC_2 + HSC_3 + HSC_4 + HSC_5 + HSC_6 +
HSC_7 + HSC_8 + HSC_9 + HSC_10 + HSC_11 + HSC_12
"

fit2mod3Fbif <- cfa(mod3Fbif, std.lv=TRUE, orthogonal = TRUE, missing = "ML", data = X, group = "Group",  group.equal = c("loadings"))

summary(fit2mod3Fbif, fit.measures=TRUE)

##scalar invariance 
mod3Fbif <- 
  "EOE =~ HSC_4 + HSC_6 + HSC_8 + HSC_9 + HSC_12
  LST =~ HSC_2 + HSC_7 + HSC_11
  AES =~ HSC_1 + HSC_3 + HSC_5 + HSC_10
  HSC =~ HSC_1 + HSC_2 + HSC_3 + HSC_4 + HSC_5 + HSC_6 +
HSC_7 + HSC_8 + HSC_9 + HSC_10 + HSC_11 + HSC_12
"
fit3mod3Fbif <- cfa(mod3Fbif, std.lv=TRUE, orthogonal = TRUE, missing = "ML", data = X, group = "Group",  group.equal = c("intercepts", "loadings"))

summary(fit3mod3Fbif, fit.measures=TRUE)


##uploading the complete dataset for regression analyses
rm(list=ls())
library(readxl)
M <- read_excel("")
View(M)
##creating subscales 
##fathers 
##hsc scales
M$hsc_f <- (M$HSC_F_1+M$HSC_F_2+M$HSC_F_3+M$HSC_F_4+M$HSC_F_5+M$HSC_F_6+M$HSC_F_7+M$HSC_F_8+M$HSC_F_9+M$HSC_F_10+M$HSC_F_11+M$HSC_F_12)/12
M$eoe_f <- (M$HSC_F_4+M$HSC_F_6+M$HSC_F_8+M$HSC_F_9+M$HSC_F_12)/5
M$lst_f <- (M$HSC_F_2+M$HSC_F_7+M$HSC_F_11)/3
M$aes_f <- (M$HSC_F_1+M$HSC_F_3+M$HSC_F_5+M$HSC_F_10)/4

##mothers 
##hsc scales
M$hsc_m <- (M$HSC_M_1+M$HSC_M_2+M$HSC_M_3+M$HSC_M_4+M$HSC_M_5+M$HSC_M_6+M$HSC_M_7+M$HSC_M_8+M$HSC_M_9+M$HSC_M_10+M$HSC_M_11+M$HSC_M_12)/12
M$eoe_m <- (M$HSC_M_4+M$HSC_M_6+M$HSC_M_8+M$HSC_M_9+M$HSC_M_12)/5
M$lst_m <- (M$HSC_M_2+M$HSC_M_7+M$HSC_M_11)/3
M$aes_m <- (M$HSC_M_1+M$HSC_M_3+M$HSC_M_5+M$HSC_M_10)/4

View(M)
##reliability
library(psych)
##hsc fathers
alpha(M[,c(57:68)]) 
omega(M[,c(57:68)]) 

##hsc mothers
alpha(M[,c(131:142)]) 
omega(M[,c(131:142)]) 

##FATHERS
##creating subscales fathers 
#parents
M$accept_f <- (M$Parents_F_1+M$Parents_F_2+M$Parents_F_4+M$Parents_F_10+M$Parents_F_13+M$Parents_F_18+M$Parents_F_20+M$Parents_F_24)/8

summary(M$accept_f) ##1.0 - 2.62
M$accept_fr <- (5 - M$accept_f)
View(M$accept_fr)
summary(M$accept_fr)

M$rejec_f <- (M$Parents_F_6R+M$Parents_F_9R+M$Parents_F_17R+M$Parents_F_22R)/4
M$parent_f <-(M$Parents_F_1+M$Parents_F_2+M$Parents_F_3R+M$Parents_F_4+M$Parents_F_5R+M$Parents_F_6R+M$Parents_F_7R+M$Parents_F_8R+M$Parents_F_9R+M$Parents_F_10+M$Parents_F_11R+M$Parents_F_12R+M$Parents_F_13+M$Parents_F_14+M$Parents_F_15R+M$Parents_F_16R+M$Parents_F_17R+M$Parents_F_18+M$Parents_F_19R+M$Parents_F_20+M$Parents_F_21R+M$Parents_F_22R+M$Parents_F_23R+M$Parents_F_24)/24

#pediatric symptoms 
M$inter_f<-(M$PED_F_3+M$PED_F_6+M$PED_F_7)/3
M$exter_f<-(M$PED_F_1+M$PED_F_5+M$PED_F_8+M$PED_F_9+M$PED_F_2+M$PED_F_4)/6

##mothers 
M$accept_m <- (M$Parents_M_1+M$Parents_M_2+M$Parents_M_4+M$Parents_M_10+M$Parents_M_13+M$Parents_M_18+M$Parents_M_20+M$Parents_M_24)/8
summary(M$accept_m)
M$accept_mr <- (5 - M$accept_m)
View(M$accept_fr)
summary(M$accept_mr)

M$rejec_m <- (M$Parents_M_6R+M$Parents_M_9R+M$Parents_M_17R+M$Parents_M_22R)/4
M$parent_m <- (M$Parents_M_1+M$Parents_M_2+M$Parents_M_3R+M$Parents_M_4+M$Parents_M_5R+M$Parents_M_6R+M$Parents_M_7R+M$Parents_M_8R+M$Parents_M_9R+M$Parents_M_10+M$Parents_M_11R+M$Parents_M_12R+M$Parents_M_13+M$Parents_M_14+M$Parents_M_15R+M$Parents_M_16R+M$Parents_M_17R+M$Parents_M_18+M$Parents_M_19R+M$Parents_M_20+M$Parents_M_21R+M$Parents_M_22R+M$Parents_M_23R+M$Parents_M_24)/24


hist(M$accept_f)
hist(M$accept_m)
View(M)
library(psych)
##acceptance fathers
alpha(M[,c(10,12,16,28,34,43,47,55)])
omega(M[,c(10,12,16,28,34,43,47,55)])

##rejection fathers
alpha(M[,c(21, 27, 42, 52)])
omega(M[,c(21, 27, 42, 52)])

##acceptance mothers
alpha(M[,c(84, 86, 90, 102, 108, 117, 121, 129)])
omega(M[,c(84, 86, 90, 102, 108, 117, 121, 129)])
omega(M[,c(84, 86, 90, 102, 108, 117, 121, 129)])

##rejection mothers
alpha(M[,c(95, 101, 116, 126)])


##exploration of rejection scale 
##rejection fathers
par(mfrow = c(2, 2))
##item rejection padre
hist(M$Parents_F_6R, main = "Item 6 father")
hist(M$Parents_F_9R, main = "Item 9 father")
hist(M$Parents_F_17R, main = "Item 17 father")
hist(M$Parents_F_22R, main = "Item 22 father")
dev.off()
##item rejection madre

hist(M$Parents_M_6R, main = "Item 6 mother")
hist(M$Parents_M_9R, main = "Item 9 mother")
hist(M$Parents_M_17R, main = "Item 17 mother")
hist(M$Parents_M_22R, main = "Item 22 mother")

summary(M$rejec_f)
sd(M$rejec_f, na.rm = TRUE)

summary(M$rejec_m)
sd(M$rejec_m, na.rm = TRUE)


library(psych)
##hsc fathers
alpha(M[,c(57:68)]) 
omega(M[,c(57:68)]) 

##hsc mothers
alpha(M[,c(131:142)])
omega(M[,c(131:142)]) 


#pediatric symptoms 
M$inter_m<-(M$PED_M_3+M$PED_M_6+M$PED_M_7)/3
M$exter_m<-(M$PED_M_1+M$PED_M_5+M$PED_M_8+M$PED_M_9+M$PED_M_2+M$PED_M_4)/6

##composite score internalizing fathers and mothers 
M$inter_tot <-(M$inter_f+M$inter_m)/2
M$exter_tot <-(M$exter_f+M$exter_m)/2
View(M$exter_tot)
summary(M$exter_tot)
M$parent_tot <-(M$parent_f+M$parent_m)/2
##bivariate associations among all variables of interest
##composite score ES fathers and mothers
M$hsc_tot <- (M$hsc_f+M$hsc_m)/2

library(psych)
View(M)
##alpha composite score internalizing
alpha(M[,c(71,74,75,145,148,149)]) 
omega(M[,c(71,74,75,145,148,149)]) 
##alpha composite score externalizing
alpha(M[,c(69,70,72,73,76,77)])
omega(M[,c(69,70,72,73,76,77)])

###fathers
#internalizing
alpha(M[,c(71,74,75)]) 
omega(M[,c(71,74,75)]) 
##alpha composite score externalizing
alpha(M[,c(69,70,72,73,76,77)])
omega(M[,c(69,70,72,73,76,77)])

##mothers
##internalizing 
alpha(M[,c(145,148,149)]) 
omega(M[,c(145,148,149)]) 
##alpha composite score externalizing
alpha(M[,c(143,147,150,151,146,144)])
omega(M[,c(143,147,150,151,146,144)])




##association mothers' and fathers' hsc
subset<-M[,c("hsc_f", "hsc_m", "eoe_f", "lst_f", "aes_f", "eoe_m", "lst_m", "aes_m", "accept_mr", "accept_fr", "inter_m", "inter_f", "exter_m", "exter_f","Age_child", "Gen_child")]
nrow(subset)


cor_data=round(cor(subset, use = "complete.obs", method = "pearson"),2)
View(cor_data)
library(corrplot)
corrplot(cor_data, type = "upper", addCoef.col = 'black', number.cex=0.60)
nrow( na.omit( subset ) )
library(sjPlot)
tab_corr(subset, digits = 2, triangle = "lower", file = "tabella_fathers_mothers_new.doc")
getwd()



##descriptives statistics
summary(M$Age_child)
sd(M$Age_child, na.rm = TRUE)
table(M$Gen_child)/length(M$Gen_b_M)

summary(M$Mother_Age)
sd(M$Mother_Age, na.rm = TRUE)

summary(M$Father_age)
sd(M$Father_age, na.rm = TRUE)

table(M$Nationality_F)/length(M$Nationality_F)
table(M$Degree_F)/length(M$Degree_F)
table(M$Married_F)/length(M$Married_F) 

table(M$Degree_M)/length(M$Degree_M)
table(M$Married_M)/length(M$Married_M)

table(M$Work_F)/length(M$Work_F)
table(M$Work_M)/length(M$Work_M)

summary(M$hsc_m)
sd(M$hsc_m, na.rm = TRUE) 
summary(M$hsc_f)
sd(M$hsc_f, na.rm = TRUE) 

summary(M$parent_m)
sd(M$parent_m, na.rm = TRUE) 

summary(M$parent_f)
sd(M$parent_f, na.rm = TRUE) 

summary(M$inter_m)
sd(M$inter_m, na.rm = TRUE) 

summary(M$inter_f)
sd(M$inter_f, na.rm = TRUE) 

summary(M$exter_m)
sd (M$exter_m, na.rm = TRUE)

summary(M$exter_f) 
sd(M$exter_f, na.rm = TRUE)

mod0 <-lm(inter_tot ~ accept_mr, data = M)
summary(mod0) 

mod1 <-lm(inter_tot ~ accept_mr + hsc_tot + Age_child + Gen_child, data = M)
summary(mod1) 
library(lm.beta)
lm.beta(mod1)

mod2 <- lm(inter_tot ~ accept_mr * hsc_tot + Age_child + Gen_child, data = M)
summary(mod2)

library(lm.beta)
lm.beta(mod2)

AIC(mod0)
AIC(mod1)
AIC(mod2)

library(MuMIn)
model.sel(mod0, mod1, mod2)

##exploring the significant interaction effect
quantile(M$hsc_tot, probs = c(0.30,0.70), na.rm = TRUE)

##plotting the significative interaction 
library(ggeffects)
citation("ggplot2")
library(ggplot2)
GGdata <-  ggpredict(mod2, terms = c("accept_mr","hsc_tot [4.041667, 4.875000]")) 

View(GGdata)
citation("MuMIn")

p1 <-ggplot(GGdata, aes(x,predicted, ymin=conf.low,
                        ymax=conf.high,fill=group))+
  theme_bw()+geom_ribbon(alpha=.3)+
  geom_line(aes(color=group))+labs(color="Child ES",fill="Child ES")+
  scale_fill_discrete(labels=c("30°","70°"))+
  scale_color_discrete(labels=c("30°","70°"))+
  xlab("Maternal acceptance")+ylab("Internalizing problems")
plot<-p1 + theme(panel.grid.minor= element_blank())+
  scale_x_continuous(expand = expansion(mult=c(0,0)))

ggsave("figure_1.png", plot=plot, width=6, height=4, dpi=300)


mod333<- lm(exter_tot ~ accept_mr, data = M)
summary(mod333) 

mod3 <- lm(exter_tot ~ accept_mr + hsc_tot + Age_child + Gen_child, data = M)
summary(mod3)

library(lm.beta)
lm.beta(mod3)
mod33 <-lm(exter_tot ~ accept_mr * hsc_tot + Age_child + Gen_child, data = M)
summary(mod33)


AIC(mod333)
AIC(mod3)
AIC(mod33)

library(MuMIn)
model.sel(mod333, mod3, mod33)

