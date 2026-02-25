# Figure 3. Testing Causality using an Nbl1 Knockout Mouse with Cisplatin.

# Blood Urea Nitrogen (BUN) was calcualted for Nbl1 HET & WT mice treated with
# Cisplaitn or PBS (Control).

###############################################################################
# Install Packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lme4")
install.packages("emmeans")
install.packages("readxl")

# Load Packages
library(ggplot2)
library(dplyr)
library(lme4)
library(emmeans)
library(readxl)

###############################################################################
# Blood Urea Nitrogen (BUN)

# Load Data
BUN_2206 <- read_excel("~/Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/22-06 NBL1 KO/NBL1 Manuscript/Cisplatin/BUN2105.xlsx")

# Calculate Summary Statistics
summary_stats <- BUN_2206 %>%
  group_by(Treatment, Genotype) %>%
  summarise(
    mean = mean(BUN, na.rm = TRUE),
    sem = sd(BUN, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# Plot Results
ggplot(BUN_2206, aes(x = Treatment, y = BUN, color = Genotype)) +
  geom_jitter(position = position_dodge (width = 0.6), size = 3, alpha = 0.7) + # Dot plot
  geom_point(data = summary_stats, aes(x = Treatment, y = mean, group = Genotype), position = position_dodge(width = 0.6), shape = 18, size = 4, color = "black") + # Mean
  geom_errorbar(data = summary_stats, aes(x = Treatment, y = mean, ymin = mean - sem, ymax = mean + sem, group = Genotype), position = position_dodge(width = 0.6), width = 0.2, color = "black") + # SEM
  scale_color_manual(values = c("orange", "purple")) +
  coord_cartesian(ylim = c(0, 250)) +
  ylab("BUN (mg/dL)") +
  xlab("Genotype") +
  ggtitle("BUN Concentrations per Treatment")

# Obtaining P-Values
# Subset for Treatment A
model_A <- lm(BUN ~ Genotype, data = subset(BUN_2206, Treatment == "Cisplatin"), REML = FALSE)

# Subset for Treatment B
model_B <- lm(BUN ~ Genotype, data = subset(BUN_2206, Treatment == "PBS"), REML = FALSE)

# Summaries with p-values
summary(model_A) # p-value = 0.495839, r2 = 0.03639
summary(model_B) # p-value = 0.334, r2 = 0.06667

# Function to calculate R²
get_r2 <- function(data) {
  model <- lm(BUN ~ Genotype, data = data)
  summary(model)$r.squared
}

# Split the data by Treatment
treatment_groups <- split(BUN_2206, BUN_2206$Treatment)

# Calculate R²
r2_values <- sapply(treatment_groups, get_r2)

# Print results
print(r2_values)
# Cisplatin: 0.03639354
# PBS: 0.06666667