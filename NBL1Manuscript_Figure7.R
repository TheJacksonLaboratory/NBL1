# Figure 7. Detection of Htr6 and Nbl1 using Fluorescent in situ Hybridization.

# Htr6 and Nbl1 RNA expresson in glomeruli from male Nbl1 HET mice with or 
# without XLAS, following cisplatin treatment, or PBS control.

###############################################################################
# Install Packages
install.packages("tidyverse")
install.packages("multcomp")
install.packages("lme4")
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("performance")

# Load Packages
library(tidyverse)
library(multcomp)
library(lme4)
library(readxl)
library(dplyr)
library(ggplot2)
library(performance)

###############################################################################
# Load Data
data <- read_excel("~/Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/22-06 NBL1 KO/22-06 Kidneys/RNAscope/RNAscopeAnalysis_R.xlsx")

## Subset Data
# Glomerular By Treatment
data_G_NA <-subset(data, Area == "G" & Treatment == "NA")
data_G_Cis <-subset(data, Area == "G" & Treatment == "Cisplatin")
data_G_PBS <-subset(data, Area == "G" & Treatment == "Saline")

###############################################################################
## Looking at specifically Htr6 glomerular expression, broken up by treatment.

# Glomerular Htr6, NA
glom_plot_htr6_NA <- ggplot(data_G_NA, aes(x = Genotype, y = Htr6_Green, color = SampleName, shape = SampleName)) +
  geom_point() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0)) +
  scale_shape_manual(values = 1:19) + # Manually specify shapes
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Htr6 Glomerular Expression by Genotype in Nbl1 KO Mice with XLAS",
       x = "Genotype",
       y= "Htr6 RNA Expression")
print(glom_plot_htr6_NA)
## Using a Linear Mixed Model Approach
# Let's start by looking at Htr6 Glomerular Expression, NA
# First run the model assuming genotype does not play a role.
Percent.null = lmer(Htr6_Green ~ (1|SampleName), data=data_G_NA, REML=FALSE)
# Then run the model adding the genotype
Percent.model = lmer(Htr6_Green ~ Genotype + (1|SampleName), data=data_G_NA, REML=FALSE)
# Compare the models to see if they are significantly different. 
anova(Percent.null,Percent.model)
# Data: data_G_NA
# Models:
# Percent.null: Htr6_Green ~ (1 | SampleName)
# Percent.model: Htr6_Green ~ Genotype + (1 | SampleName)
# npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)
# Percent.null     3 400.53 406.81 -197.27    394.53                     
# Percent.model    4 400.83 409.20 -196.41    392.83 1.7052  1     0.1916
# Get R2 values
r2_values <- r2(Percent.model)
print(r2_values)
# Conditional R2: 0.562
# Marginal R2: 0.152
## Genotype does not affect Htr6 Glomerular Expression in this group (Chi-square(1)=1.7052, p=0.1916)

# Glomerular Htr6, Cis
glom_plot_htr6_Cis <- ggplot(data_G_Cis, aes(x = Genotype, y = Htr6_Green, color = SampleName, shape = SampleName)) +
  geom_point() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0)) +
  scale_shape_manual(values = 1:19) + # Manually specify shapes
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Htr6 Glomerular Expression by Genotype in Nbl1 KO Mice Treated with Cisplatin",
       x = "Genotype",
       y= "Htr6 RNA Expression")
print(glom_plot_htr6_Cis)
## Using a Linear Mixed Model Approach
# Let's start by looking at Htr6 Glomerular Expression, Cis
# First run the model assuming genotype does not play a role.
Percent.null = lmer(Htr6_Green ~ (1|SampleName), data=data_G_Cis, REML=FALSE)
# Then run the model adding the genotype
Percent.model = lmer(Htr6_Green ~ Genotype + (1|SampleName), data=data_G_Cis, REML=FALSE)
# Compare the models to see if they are significantly different. 
anova(Percent.null,Percent.model)
# Data: data_G_Cis
# Models:
# Percent.null: Htr6_Green ~ (1 | SampleName)
# Percent.model: Htr6_Green ~ Genotype + (1 | SampleName)
# npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)
# Percent.null     3 540.68 547.43 -267.34    534.68                     
# Percent.model    4 541.99 550.98 -266.99    533.99 0.6919  1     0.4055
# Get R2 values
r2_values <- r2(Percent.model)
print(r2_values)
# Conditional R2: 0.272
# Marginal R2: 0.033
## Genotype does not affect Htr6 Glomerular Expression in this group (Chi-square(1)=0.6919, p=0.4055)

# Glomerular Htr6, PBS
glom_plot_htr6_PBS <- ggplot(data_G_PBS, aes(x = Genotype, y = Htr6_Green, color = SampleName, shape = SampleName)) +
  geom_point() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0)) +
  scale_shape_manual(values = 1:19) + # Manually specify shapes
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Htr6 Glomerular Expression by Genotype in Nbl1 KO Mice Treated with PBS",
       x = "Genotype",
       y= "Htr6 RNA Expression")
print(glom_plot_htr6_PBS)
## Using a Linear Mixed Model Approach
# Let's start by looking at Htr6 Glomerular Expression, PBS
# First run the model assuming genotype does not play a role.
Percent.null = lmer(Htr6_Green ~ (1|SampleName), data=data_G_PBS, REML=FALSE)
# Then run the model adding the genotype
Percent.model = lmer(Htr6_Green ~ Genotype + (1|SampleName), data=data_G_PBS, REML=FALSE)
# Compare the models to see if they are significantly different. 
anova(Percent.null,Percent.model)
#Data: data_G_PBS
#Models:
#  Percent.null: Htr6_Green ~ (1 | SampleName)
#Percent.model: Htr6_Green ~ Genotype + (1 | SampleName)
#npar    AIC    BIC  logLik -2*log(L) Chisq Df Pr(>Chisq)
#Percent.null     3 450.85 457.13 -222.42    444.85                    
#Percent.model    4 451.49 459.87 -221.75    443.49 1.353  1     0.2448
#Get R2 values
r2_values <- r2(Percent.model)
print(r2_values)
# Conditional R2: 0.421
# Marginal R2: 0.098
## Genotype does not affect Htr6 Glomerular Expression in this group (Chi-square(1)=1.353, p=0.2448)

###############################################################################
## Looking at specifically Nbl1 glomerular expression, broken up by treatment.

# Glomerular Nbl1, NA
glom_plot_nbl1_NA <- ggplot(data_G_NA, aes(x = Genotype, y = Nbl1_Red, color = SampleName, shape = SampleName)) +
  geom_point() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0)) +
  scale_shape_manual(values = 1:19) + # Manually specify shapes
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Nbl1 Glomerular Expression by Genotype in Nbl1 KO Mice with XLAS",
       x = "Genotype",
       y= "Nbl1 RNA Expression")
print(glom_plot_nbl1_NA)
## Using a Linear Mixed Model Approach
# Let's start by looking at Glomerular Nbl1 Expression, NA
# First run the model assuming genotype does not play a role.
Percent.null = lmer(Nbl1_Red ~ (1|SampleName), data=data_G_NA, REML=FALSE)
# Then run the model adding the genotype
Percent.model = lmer(Nbl1_Red ~ Genotype + (1|SampleName), data=data_G_NA, REML=FALSE)
# Compare the models to see if they are significantly different. 
anova(Percent.null,Percent.model)
# Data: data_G_NA
# Models:
# Percent.null: Nbl1_Red ~ (1 | SampleName)
# Percent.model: Nbl1_Red ~ Genotype + (1 | SampleName)
# npar    AIC    BIC  logLik -2*log(L) Chisq Df Pr(>Chisq)  
# Percent.null     3 420.69 426.98 -207.35    414.69                      
# Percent.model    4 417.73 426.11 -204.87    409.73 4.963  1     0.0259 *
# Get R2 values
r2_values <- r2(Percent.model)
print(r2_values)
# Conditional R2: 0.275
# Marginal R2: 0.197
## Genotype does affect Glomerular Nbl1 Expression (Chi-square(1)=4.963, p=0.0259*)

# Glomerular Nbl1, Cis
glom_plot_nbl1_Cis <- ggplot(data_G_Cis, aes(x = Genotype, y = Nbl1_Red, color = SampleName, shape = SampleName)) +
  geom_point() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0)) +
  scale_shape_manual(values = 1:19) + # Manually specify shapes
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Nbl1 Glomerular Expression by Genotype in Nbl1 KO Mice Treated with Cisplatin",
       x = "Genotype",
       y= "Nbl1 RNA Expression")
print(glom_plot_nbl1_Cis)
## Using a Linear Mixed Model Approach
# Let's start by looking at Glomerular Nbl1 Expression, Cis
# First run the model assuming genotype does not play a role.
Percent.null = lmer(Nbl1_Red ~ (1|SampleName), data=data_G_Cis, REML=FALSE)
# Then run the model adding the genotype
Percent.model = lmer(Nbl1_Red ~ Genotype + (1|SampleName), data=data_G_Cis, REML=FALSE)
# Compare the models to see if they are significantly different. 
anova(Percent.null,Percent.model)
# Data: data_G_Cis
# Models:
# Percent.null: Nbl1_Red ~ (1 | SampleName)
# Percent.model: Nbl1_Red ~ Genotype + (1 | SampleName)
# npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)    
# Percent.null     3 588.90 595.65 -291.45    582.90                         
# Percent.model    4 574.04 583.03 -283.02    566.04 16.866  1  4.011e-05 ***
# Get R2 values
r2_values <- r2(Percent.model)
print(r2_values)
# Conditional R2: NA
# Marginal R2: 0.364
## Genotype does affect Glomerular Nbl1 Expression (Chi-square(1)=16.866, p=4.011e-05***)

# Glomerular Nbl1, PBS ## Check significance & try removing outlier S-630-22
glom_plot_nbl1_PBS <- ggplot(data_G_PBS, aes(x = Genotype, y = Nbl1_Red, color = SampleName, shape = SampleName)) +
  geom_point() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0)) +
  scale_shape_manual(values = 1:19) + # Manually specify shapes
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Nbl1 Glomerular Expression by Genotype in Nbl1 KO Mice Treated with PBS",
       x = "Genotype",
       y= "Nbl1 RNA Expression")
print(glom_plot_nbl1_PBS)
## Using a Linear Mixed Model Approach
# Let's start by looking at Glomerular Nbl1 Expression, PBS
# First run the model assuming genotype does not play a role.
Percent.null = lmer(Nbl1_Red ~ (1|SampleName), data=data_G_PBS, REML=FALSE)
# Then run the model adding the genotype
Percent.model = lmer(Nbl1_Red ~ Genotype + (1|SampleName), data=data_G_PBS, REML=FALSE)
# Compare the models to see if they are significantly different. 
anova(Percent.null,Percent.model)
# Data: data_G_PBS
# Models:
# Percent.null: Nbl1_Red ~ (1 | SampleName)
# Percent.model: Nbl1_Red ~ Genotype + (1 | SampleName)
# npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)  
# Percent.null     3 406.02 412.30 -200.01    400.02                       
# Percent.model    4 402.25 410.62 -197.12    394.25 5.7722  1    0.01628 *
r2_values <- r2(Percent.model)
print(r2_values)
# Conditional R2: 0.643
# Marginal R2: 0.422
## Genotype does affect Glomerular Nbl1 Expression (Chi-square(1)=5.7722, p=0.01628*)



