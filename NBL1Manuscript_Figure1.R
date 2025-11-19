# Code Script for Nbl1 Manuscript: NBL1 Correlates with Renal Phenotypes in 
# Mouse Models of Kidney Disease, but is Not Causal.

# Figure 1. Correlation Between Serum NBL1 Concentrations and GFR, ACR in 
# DO-XLAS Mice.

# Spearman's correlation coefficients were calculated to determine how serum 
# NBL1 concentrations correlate with ACR and GFR in DO-XLAS Cohort 1, DO-XLAS 
# Cohort 2, and DO-XLAS Cohort 2 Females at various timepoints.

###############################################################################
# Install Packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("multcomp")
install.packages("readxl")

# Load Packages
library("tidyverse")
library("dplyr")
library("ggplot2")
library("multcomp")
library("readxl")

###############################################################################
# COHORT 1

# Cohort 1 Correlations with Sex Combined


# 10w NBL1 Correlations with 14w GFR
# Load Data
NBL1_10_GFR_14 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/10wNBL1&14wGFRCohort1.xlsx")

#Correlations between GFR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_10_GFR_14, mapping=aes(x=as.numeric(GFR_14w_rankz),y=as.numeric(NBL1_10w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("orange"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("14w GFR (ml/min/100g b.w.)")+ggtitle("Correlation Between 14w GFR & 10w NBL1 in Cohort 1 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_10_GFR_14$NBL1_10w_rankz), as.numeric(NBL1_10_GFR_14$GFR_14w_rankz))
view(Significance)
#	p-value = 0.001086508, SIGNIFICANT

#Spearman correlation coefficient
GFR_14w_1 <- as.numeric(NBL1_10_GFR_14$GFR_14w_rankz)
NBL1_10w_1 <- as.numeric(NBL1_10_GFR_14$NBL1_10w_rankz)
cor.test(GFR_14w_1,NBL1_10w_1,method=c("spearman"))
# S = 298163, P-value = 0.00353, rho = -0.22734621


# 10w NBL1 Correlations with 6w ACR
# Load Data
NBL1_10_ACR_6 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/10wNBL1&6wACRCohort1.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_10_ACR_6, mapping=aes(x=as.numeric(ACR_6w_rankz),y=as.numeric(NBL1_10w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("orange"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("6w ACR")+ggtitle("Correlation Between 6w ACR & 10w NBL1 in Cohort 1 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_10_ACR_6$NBL1_10w_rankz), as.numeric(NBL1_10_ACR_6$ACR_6w_rankz))
view(Significance)
#	p-value = 0.0005884068, SIGNIFICANT

#Spearman correlation coefficient
ACR_6w_1 <- as.numeric(NBL1_10_ACR_6$ACR_6w_rankz)
NBL1_10w_1 <- as.numeric(NBL1_10_ACR_6$NBL1_10w_rankz)
cor.test(ACR_6w_1,NBL1_10w_1,method=c("spearman"))
# S = 403659, p-value = 0.0004641, rho = 0.2823524


## 10w NBL1 Correlations with 10w ACR
# Load Data
NBL1_10_ACR_10 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/10wNBL1&10wACRCohort1.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_10_ACR_10, mapping=aes(x=as.numeric(ACR_10w_rankz),y=as.numeric(NBL1_10w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("orange"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("10w ACR")+ggtitle("Correlation Between 10w ACR & 10w NBL1 in Cohort 1 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_10_ACR_10$NBL1_10w_rankz), as.numeric(NBL1_10_ACR_10$ACR_10w_rankz))
view(Significance)
#	p-value = 3.453959e-06, SIGNIFICANT

#Spearman correlation coefficient
ACR_10w_1 <- as.numeric(NBL1_10_ACR_10$ACR_10w_rankz)
NBL1_10w_1 <- as.numeric(NBL1_10_ACR_10$NBL1_10w_rankz)
cor.test(ACR_10w_1,NBL1_10w_1,method=c("spearman"))
# S = 301950, p-value = 1.335e-06, rho = 0.3932361


## 10w NBL1 Correlations with 15w ACR
# Load Data
NBL1_10_ACR_15 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/10wNBL1&15wACRCohort1.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_10_ACR_15, mapping=aes(x=as.numeric(ACR_15w_rankz),y=as.numeric(NBL1_10w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("orange"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("15w ACR")+ggtitle("Correlation Between 15w ACR & 10w NBL1 in Cohort 1 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_10_ACR_15$NBL1_10w_rankz), as.numeric(NBL1_10_ACR_15$ACR_15w_rankz))
view(Significance)
#	p-value = 2.511128e-08, SIGNIFICANT

#Spearman correlation coefficient
ACR_15w_1 <- as.numeric(NBL1_10_ACR_15$ACR_15w_rankz)
NBL1_10w_1 <- as.numeric(NBL1_10_ACR_15$NBL1_10w_rankz)
cor.test(ACR_15w_1,NBL1_10w_1,method=c("spearman"))
# S = 329662, p-value = 1.841e-07, rho = 0.4139082


## 15w NBL1 Correlations with 14w GFR
# Load Data
NBL1_15_GFR_14 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/15wNBL1&14wGFRCohort1.xlsx")

#Correlations between GFR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_15_GFR_14, mapping=aes(x=as.numeric(GFR_14w_rankz),y=as.numeric(NBL1_15w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("orange"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("14w GFR")+ggtitle("Correlation Between 14w GFR & 15w NBL1 in Cohort 1 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_15_GFR_14$NBL1_15w_rankz), as.numeric(NBL1_15_GFR_14$GFR_14w_rankz))
view(Significance)
#	p-value = 0.01813899, SIGNIFICANT

#Spearman correlation coefficient
GFR_14w_1 <- as.numeric(NBL1_15_GFR_14$GFR_14w_rankz)
NBL1_15w_1 <- as.numeric(NBL1_15_GFR_14$NBL1_15w_rankz)
cor.test(GFR_14w_1,NBL1_15w_1,method=c("spearman"))
# S = 253003, p-value = 0.01306, rho = -0.2392627


## 15w NBL1 Correlations with 6w ACR
# Load Data
NBL1_15_ACR_6 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/15wNBL1&6wACRCohort1.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_15_ACR_6, mapping=aes(x=as.numeric(ACR_6w_rankz),y=as.numeric(NBL1_15w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("orange"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("6w ACR")+ggtitle("Correlation Between 6w ACR & 15w NBL1 in Cohort 1 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_15_ACR_6$NBL1_15w_rankz), as.numeric(NBL1_15_ACR_6$ACR_6w_rankz))
view(Significance)
#	p-value = 0.001277906, SIGNIFICANT

#Spearman correlation coefficient
ACR_6w_1 <- as.numeric(NBL1_15_ACR_6$ACR_6w_rankz)
NBL1_15w_1 <- as.numeric(NBL1_15_ACR_6$NBL1_15w_rankz)
cor.test(ACR_6w_1,NBL1_15w_1,method=c("spearman"))
# S = 330803, p-value = 1.699e-05, rho = 0.3489162


## 15w NBL1 Correlations with 10w ACR
# Load Data
NBL1_15_ACR_10 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/15wNBL1&10wACRCohort1.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_15_ACR_10, mapping=aes(x=as.numeric(ACR_10w_rankz),y=as.numeric(NBL1_15w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("orange"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("10w ACR")+ggtitle("Correlation Between 10w ACR & 15w NBL1 in Cohort 1 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_15_ACR_10$NBL1_15w_rankz), as.numeric(NBL1_15_ACR_10$ACR_10w_rankz))
view(Significance)
#	p-value = 5.921771e-06, SIGNIFICANT

#Spearman correlation coefficient
ACR_10w_1 <- as.numeric(NBL1_15_ACR_10$ACR_10w_rankz)
NBL1_15w_1 <- as.numeric(NBL1_15_ACR_10$NBL1_15w_rankz)
cor.test(ACR_10w_1,NBL1_15w_1,method=c("spearman"))
# S = 257542, p-value = 1.896e-07, rho = 0.4245905


## 15w NBL1 Correlations with 15w ACR
# Load Data
NBL1_15_ACR_15 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/15wNBL1&15wACRCohort1.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_15_ACR_15, mapping=aes(x=as.numeric(ACR_15w_rankz),y=as.numeric(NBL1_15w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("orange"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("15w ACR")+ggtitle("Correlation Between 15w ACR & 15w NBL1 in Cohort 1 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_15_ACR_15$NBL1_15w_rankz), as.numeric(NBL1_15_ACR_15$ACR_15w_rankz))
view(Significance)
#	p-value = 0.0001531397, SIGNIFICANT

#Spearman correlation coefficient
ACR_15w_1 <- as.numeric(NBL1_15_ACR_15$ACR_15w_rankz)
NBL1_15w_1 <- as.numeric(NBL1_15_ACR_15$NBL1_15w_rankz)
cor.test(ACR_15w_1,NBL1_15w_1,method=c("spearman"))
# S = 310088, p-value = 3.223e-06, rho = 0.3768833


# Plotting Spearman Correlation Coefficients for All Cohort 1 Males & Females in a Double Sided Bar Graph
rhoCohort1all <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 1/rhoCohort1all.xlsx")
factor_dat <- rhoCohort1all |>
  mutate(
    label = factor(
      Comparison,
      levels = rhoCohort1all$Comparison
    ) |> fct_rev()
  )

ggplot(factor_dat, aes(x = label, y = Correlation_Coefficient, fill = Comparison)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "Comparison", y = "Correlation Coefficient", fill = "Comparison") +
  theme_minimal()
###############################################################################
# COHORT 2

# Cohort 2 Correlations with Sex Combined


## 15w NBL1 Correlations with 14w GFR
# Load Data
NBL1_15_GFR_14 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/15wNBL1&14wGFRCohort2.xlsx")

#Correlations between GFR and NBL1 by time point in Males & Females
Correlation<-ggplot(NBL1_15_GFR_14, mapping=aes(x=as.numeric(GFR_14w_rankz),y=as.numeric(NBL1_15w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("14w GFR (ml/min/100g b.w.)")+ggtitle("Correlation Between 14w GFR & 15w NBL1 in Cohort 2 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_15_GFR_14$NBL1_15w_rankz), as.numeric(NBL1_15_GFR_14$GFR_14w_rankz))
view(Significance)
#	p-value = 0.01391675, SIGNIFICANT

#Spearman correlation coefficient
GFR_14w_2 <- as.numeric(NBL1_15_GFR_14$GFR_14w_rankz)
NBL1_15w_2 <- as.numeric(NBL1_15_GFR_14$NBL1_15w_rankz)
cor.test(GFR_14w_2,NBL1_15w_2,method=c("spearman"))
# S = 1169569, p-value = 0.05005, rho = -0.1450825


## 15w NBL1 Correlations with 6w ACR
# Load Data
NBL1_15_ACR_6 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/15wNBL1&6wACRCohort2.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females
Correlation<-ggplot(NBL1_15_ACR_6, mapping=aes(x=as.numeric(ACR_6w_rankz),y=as.numeric(NBL1_15w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("6w ACR")+ggtitle("Correlation Between 6w ACR & 15w NBL1 in Cohort 2 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_15_ACR_6$NBL1_15w_rankz), as.numeric(NBL1_15_ACR_6$ACR_6w_rankz))
view(Significance)
#	p-value = 0.09210962, SIGNIFICANT

#Spearman correlation coefficient
ACR_6w_2 <- as.numeric(NBL1_15_ACR_6$ACR_6w_rankz)
NBL1_15w_2 <- as.numeric(NBL1_15_ACR_6$NBL1_15w_rankz)
cor.test(ACR_6w_2,NBL1_15w_2,method=c("spearman"))
# S = 1219670, p-value = 0.1121, rho = 0.1121286


## 15w NBL1 Correlations with 10w ACR
# Load Data
NBL1_15_ACR_10 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/15wNBL1&10wACRCohort2.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females (10w+15w)
Correlation<-ggplot(NBL1_15_ACR_10, mapping=aes(x=as.numeric(ACR_10w_rankz),y=as.numeric(NBL1_15w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("10w ACR")+ggtitle("Correlation Between 10w ACR & 15w NBL1 in Cohort 2 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_15_ACR_10$NBL1_15w_rankz), as.numeric(NBL1_15_ACR_10$ACR_10w_rankz))
view(Significance)
#	p-value = 0.3656459, NOT SIGNIFICANT

#Spearman correlation coefficient
ACR_10w_2 <- as.numeric(NBL1_15_ACR_10$ACR_10w_rankz)
NBL1_15w_2 <- as.numeric(NBL1_15_ACR_10$NBL1_15w_rankz)
cor.test(ACR_10w_2,NBL1_15w_2,method=c("spearman"))
# S = 1243862, p-value = 0.2534, rho = 0.08093551


## 15w NBL1 Correlations with 15w ACR
# Load Data
NBL1_15_ACR_15 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/15wNBL1&15wACRCohort2.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females
Correlation<-ggplot(NBL1_15_ACR_15, mapping=aes(x=as.numeric(ACR_15w_rankz),y=as.numeric(NBL1_15w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("15w NBL1")+ylab("15w ACR")+ggtitle("Correlation Between 15w ACR & 15w NBL1 in Cohort 2 Males & Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_15_ACR_15$NBL1_15w_rankz), as.numeric(NBL1_15_ACR_15$ACR_15w_rankz))
view(Significance)
#	p-value = 0.111223, NOT SIGNIFICANT

#Spearman correlation coefficient
ACR_15w_2 <- as.numeric(NBL1_15_ACR_15$ACR_15w_rankz)
NBL1_15w_2 <- as.numeric(NBL1_15_ACR_15$NBL1_15w_rankz)
cor.test(ACR_15w_2,NBL1_15w_2,method=c("spearman"))
# S = 1093193, p-value = 0.02965, rho = -0.1630593


# Plotting Spearman Correlation Coefficients for All Cohort 2 Males & Females in a Double Sided Bar Graph
rhoCohort2all <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/rhoCohort2all.xlsx")
factor_dat <- rhoCohort2all |>
  mutate(
    label = factor(
      Comparison,
      levels = rhoCohort2all$Comparison
    ) |> fct_rev()
  )

ggplot(factor_dat, aes(x = label, y = Correlation_Coefficient, fill = Comparison)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "Comparison", y = "Correlation Coefficient", fill = "Comparison") +
  theme_minimal()
###############################################################################
# COHORT 2

# Cohort 2 Correlations Females Only


## 26w NBL1 Correlations with 14w GFR
# Load Data
NBL1_26_GFR_14 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/26wNBL1&14wGFRCohort2.xlsx")

#Correlations between GFR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_26_GFR_14, mapping=aes(x=as.numeric(GFR_14w_rankz),y=as.numeric(NBL1_26w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("26w NBL1")+ylab("14w GFR (ml/min/100g b.w.)")+ggtitle("Correlation Between 14w GFR & 26w NBL1 in Cohort 2 Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_26_GFR_14$NBL1_26w_rankz), as.numeric(NBL1_26_GFR_14$GFR_14w_rankz))
view(Significance)
#	p-value = 0.6994351, NOT SIGNIFICANT

#Spearman correlation coefficient
GFR_14w_2 <- as.numeric(NBL1_26_GFR_14$GFR_14w_rankz)
NBL1_26w_2 <- as.numeric(NBL1_26_GFR_14$NBL1_26w_rankz)
cor.test(GFR_14w_2,NBL1_26w_2,method=c("spearman"))
# S = 387072, p-value = 0.5183, rho = -0.05715558


## 26w NBL1 Correlations with 24w GFR
# Load Data
NBL1_26_GFR_24 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/26wNBL1&24wGFRCohort2.xlsx")

#Correlations between GFR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_26_GFR_24, mapping=aes(x=as.numeric(GFR_24w_rankz),y=as.numeric(NBL1_26w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("26w NBL1")+ylab("24w GFR (ml/min/100g b.w.)")+ggtitle("Correlation Between 24w GFR & 26w NBL1 in Cohort 2 Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_26_GFR_24$NBL1_26w_rankz), as.numeric(NBL1_26_GFR_24$GFR_24w_rankz))
view(Significance)
#	p-value = 0.9418224, NOT SIGNIFICANT

#Spearman correlation coefficient
GFR_24w_2 <- as.numeric(NBL1_26_GFR_24$GFR_24w_rankz)
NBL1_26w_2 <- as.numeric(NBL1_26_GFR_24$NBL1_26w_rankz)
cor.test(GFR_24w_2,NBL1_26w_2,method=c("spearman"))
# S = 300894, p-value = 0.8348, rho = -0.01915072


## 26w NBL1 Correlations with 6w ACR
# Load Data
NBL1_26_ACR_6 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/26wNBL1&6wACRCohort2.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_26_ACR_6, mapping=aes(x=as.numeric(ACR_6w_rankz),y=as.numeric(NBL1_26w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("26w NBL1")+ylab("6w ACR")+ggtitle("Correlation Between 6w ACR & 26w NBL1 in Cohort 2 Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_26_ACR_6$NBL1_26w_rankz), as.numeric(NBL1_26_ACR_6$ACR_6w_rankz))
view(Significance)
#	p-value = 0.1073769, NOT SIGNIFICANT

#Spearman correlation coefficient
ACR_6w_2 <- as.numeric(NBL1_26_ACR_6$ACR_6w_rankz)
NBL1_26w_2 <- as.numeric(NBL1_26_ACR_6$NBL1_26w_rankz)
cor.test(ACR_6w_2,NBL1_26w_2,method=c("spearman"))
# S = 284074, p-value = 0.03434, rho = 0.1872092


## 26w NBL1 Correlations with 10w ACR
# Load Data
NBL1_26_ACR_10 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/26wNBL1&10wACRCohort2.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_26_ACR_10, mapping=aes(x=as.numeric(ACR_10w_rankz),y=as.numeric(NBL1_26w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("26w NBL1")+ylab("10w ACR")+ggtitle("Correlation Between 10w ACR & 26w NBL1 in Cohort 2 Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_26_ACR_10$NBL1_26w_rankz), as.numeric(NBL1_26_ACR_10$ACR_10w_rankz))
view(Significance)
#	p-value = 0.6621208, NOT SIGNIFICANT

#Spearman correlation coefficient
ACR_10w_2 <- as.numeric(NBL1_26_ACR_10$ACR_10w_rankz)
NBL1_26w_2 <- as.numeric(NBL1_26_ACR_10$NBL1_26w_rankz)
cor.test(ACR_10w_2,NBL1_26w_2,method=c("spearman"))
# S = 379219, p-value = 0.6867, rho = -0.03570736


## 26w NBL1 Correlations with 15w ACR
# Load Data
NBL1_26_ACR_15 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/26wNBL1&15wACRCohort2.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_26_ACR_15, mapping=aes(x=as.numeric(ACR_15w_rankz),y=as.numeric(NBL1_26w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("26w NBL1")+ylab("15w ACR")+ggtitle("Correlation Between 15w ACR & 26w NBL1 in Cohort 2 Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_26_ACR_15$NBL1_26w_rankz), as.numeric(NBL1_26_ACR_15$ACR_15w_rankz))
view(Significance)
#	p-value = 0.3719104, NOT SIGNIFICANT

#Spearman correlation coefficient
ACR_15w_2 <- as.numeric(NBL1_26_ACR_15$ACR_15w_rankz)
NBL1_26w_2 <- as.numeric(NBL1_26_ACR_15$NBL1_26w_rankz)
cor.test(ACR_15w_2,NBL1_26w_2,method=c("spearman"))
# S = 344132, p-value = 0.3549, rho = 0.08148242


## 26w NBL1 Correlations with 25w ACR
# Load Data
NBL1_26_ACR_25 <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/22-06 NBL1 KO/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/26wNBL1&25wACRCohort2.xlsx")

#Correlations between ACR and NBL1 by time point in Males & Females 
Correlation<-ggplot(NBL1_26_ACR_25, mapping=aes(x=as.numeric(ACR_25w_rankz),y=as.numeric(NBL1_26w_rankz), color = Cohort, size=2))+geom_point() + xlim(-3,3) + ylim (-3,3) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="black")+xlab("26w NBL1")+ylab("25w ACR")+ggtitle("Correlation Between 25w ACR & 26w NBL1 in Cohort 2 Females")
Correlation

#Determining if this is significant
Significance <- cor.test(as.numeric(NBL1_26_ACR_25$NBL1_26w_rankz), as.numeric(NBL1_26_ACR_25$ACR_25w_rankz))
view(Significance)
#	p-value = 0.7431077, NOT SIGNIFICANT

#Spearman correlation coefficient
ACR_25w_2 <- as.numeric(NBL1_26_ACR_25$ACR_25w_rankz)
NBL1_26w_2 <- as.numeric(NBL1_26_ACR_25$NBL1_26w_rankz)
cor.test(ACR_25w_2,NBL1_26w_2,method=c("spearman")) 
# S = 243445, p-value = 0.8963, rho = -0.01239692


# Plotting Spearman Correlation Coefficients for Cohort 2 26w Females in a Double Sided Bar Graph
rhoCohort226F <- read_excel("Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/22-06 NBL1 KO/NBL1 Manuscript/20-01 DO-XLAS Figures/Correlations/Cohort 2/rhoCohort226F.xlsx")
rhoCohort226F <- rhoCohort226F
factor_dat <- rhoCohort226F |>
  mutate(
    label = factor(
      Comparison,
      levels = rhoCohort226F$Comparison
    ) |> fct_rev()
  )

ggplot(factor_dat, aes(x = label, y = Correlation_Coefficient, fill = Comparison)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "Comparison", y = "Correlation Coefficient", fill = "Comparison") +
  theme_minimal()
