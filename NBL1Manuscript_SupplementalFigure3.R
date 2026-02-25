# Supplemental Figure 3. Quantifying MME and Apoptosis in Male Nbl1 Knockout 
# Mice with XLAS

# Analyzing MME Area and Apoptosis (Cleaved Caspase 3 Signal) in Nbl1 HET & WT 
# mice with XLAS, using ImageJ and a Linear Mixed Model.

###############################################################################
# Install Packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("lme4")
install.packages("performance")

# Load Packages
library(ggplot2)
library(dplyr)
library(readxl)
library(lme4)
library(performance)

###############################################################################
## Looking at MME

# Load Data
MMEresults <- read.csv("~/Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/22-06 NBL1 KO/NBL1 Manuscript/Rei's KO Figures/MMEresults.csv")

#Run the model assuming that genotype does not play a role
FAR2.null=lmer(mmeArea ~ (1|ID), data=MMEresults, REML=FALSE)

#Run the model adding the genotype 
FAR2.model=lmer(mmeArea ~ Genotype + (1|ID), data=MMEresults, REML = FALSE)

#Compare the models to see if they are significantly different
anova(FAR2.null,FAR2.model)
# P-value = 0.4701

# Get R² values
r2_values <- r2(FAR2.model)
print(r2_values)
# Conditional R2: 0.177
# Marginal R2: 0.010

# Calculate summary statistics
summary_stats <- MMEresults %>%
  group_by(Genotype) %>%
  summarise(
    mean = mean(mmeArea, na.rm = TRUE),
    sem = sd(mmeArea, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# Plot
ggplot(MMEresults, aes(x = Genotype, y = mmeArea, color = Genotype)) +
  geom_jitter(
    aes (color = Genotype, shape = factor(ID)),
    width = 0.2,
    size = 2,
    show.legend = TRUE
  ) +
  geom_point(data = summary_stats, aes(y = mean), shape = 18, size = 4, color = "black") + # Mean
  geom_errorbar(data = summary_stats, aes(y = mean, ymin = mean - sem, ymax = mean + sem), width = 0.2, color = "black") + # SEM
  scale_color_manual(values = c("orange", "purple")) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_shape_manual(values = 0:25) +
  theme_minimal()+
  labs (
    x = "Genotype",
    y = "MME Area"
  )

###############################################################################
## Looking at Apoptosis

#Load Data 
Caspase <-  read_excel("~/Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/22-06 NBL1 KO/NBL1 Manuscript/Rei's KO Figures/2206caspaseResults (1).xlsx")

#Run the model assuming that genotype dpes not play a role
FAR2.null=lmer(CaspaseperDapi ~ (1|Sample), data=Caspase, REML=FALSE)

#Run the model adding the genotype 
FAR2.model=lmer(CaspaseperDapi ~ Genotype + (1|Sample), data=Caspase, REML = FALSE)

#Compare the models to see if they are significantly different
anova(FAR2.null,FAR2.model)
# P-value = 0.3758

# Get R² values
r2_values <- r2(FAR2.model)
print(r2_values)
# Conditional R2: 0584
# Marginal R2: 0.046

# Calculate summary statistics
summary_stats <- Caspase %>%
  group_by(Genotype) %>%
  summarise(
    mean = mean(CaspaseperDapi, na.rm = TRUE),
    sem = sd(CaspaseperDapi, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# Plot
ggplot(Caspase, aes(x = Genotype, y = CaspaseperDapi, color = Genotype)) +
  geom_jitter(
    aes (color = Genotype, shape = factor(Sample)),
    width = 0.2,
    size = 2,
    show.legend = TRUE
  ) + # Dot plot
  geom_point(data = summary_stats, aes(y = mean), shape = 18, size = 4, color = "black") + # Mean
  geom_errorbar(data = summary_stats, aes(y = mean, ymin = mean - sem, ymax = mean + sem), width = 0.2, color = "black") + # SEM
  scale_color_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = 0:25) +
  theme_minimal()+
  labs (
    x = "Genotype",
    y = "Cleaved Caspase 3 Signal"
  )

