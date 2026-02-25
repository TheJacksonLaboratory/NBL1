# Supplemental Figure 7. Correlations Between Renal Clcnka Expression and Serum 
# NBL1 Concentrations, Renal Nbl1 Expression in 15-week DO-XLAS Females

# Exploring correlations between renal clcnka expression and serum NBL1 
# concentrations as well as renal clcnka expression and renal Nbl1 expression 
# in 15-week DO-XLAS Females.

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

# Load Data
CombinedCohort <- read_excel("~/Library/CloudStorage/OneDrive-TheJacksonLaboratory/Korstanje Lab/20-01 DO-XLAS/20-01 Assays/Combined Distributions/Normalized Gene Expression/Cohort1&2_NormalizedGeneExpression_Crocc_Clcnka.xlsx")

# Subset Data
# Subset 15w Females
cohort12F <- CombinedCohort[CombinedCohort$sex == 0, ]

cohort12F$Nbl1_15w_Rankz_CombinedCohort <- as.numeric(cohort12F$Nbl1_15w_Rankz_CombinedCohort)
cohort12F$Nbl1_ENSMUSG00000041120 <- as.numeric(cohort12F$Nbl1_ENSMUSG00000041120)
cohort12F$Clcnka_ENSMUSG00000033770 <- as.numeric(cohort12F$Clcnka_ENSMUSG00000033770)

###############################################################################
## Renal Clcnka Expression & Serum NBL1 Concentrations

# Correlations between Clcnka Expression and RankZ Transformed 15w Nbl1 Levels
# for Both Cohorts
Correlation <- ggplot(cohort12F, mapping=aes(x=as.numeric(Clcnka_ENSMUSG00000033770),y=as.numeric(Nbl1_15w_Rankz_CombinedCohort), size=1))+geom_point() + xlim(min(cohort12F$Clcnka_ENSMUSG00000033770), max(cohort12F$Clcnka_ENSMUSG00000033770)) + ylim(min(cohort12F$Nbl1_15w_Rankz_CombinedCohort), max(cohort12F$Nbl1_15w_Rankz_CombinedCohort)) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="blue", size = 0.5, se = FALSE)+xlab("RankZ (Clcnka Expression)")+ylab("RankZ (serum NBL1)")+ggtitle("Correlation Between Clcnka Expression and 15w NBL1 Levels in DO-XLAS Females")
Correlation

# Determining if this is significant
Significance <- cor.test(as.numeric(cohort12F$Clcnka_ENSMUSG00000033770), as.numeric(cohort12F$Nbl1_15w_Rankz_CombinedCohort))
view(Significance)
# P-value = 0.0124861, SIGNIFICANT

# Get R² values
R2 <- Significance$estimate^2
R2
# R2 = 0.03512985

###############################################################################
## Renal Clcnka Expression & Renal Nbl1 Expression

# Correlations between Clcnka Expression and RankZ Transformed 15w Nbl1 Expression
# for Both Cohorts
Correlation <- ggplot(cohort12F, mapping=aes(x=as.numeric(Clcnka_ENSMUSG00000033770),y=as.numeric(Nbl1_ENSMUSG00000041120), size=1))+geom_point() + xlim(min(cohort12F$Clcnka_ENSMUSG00000033770), max(cohort12F$Clcnka_ENSMUSG00000033770)) + ylim(min(cohort12F$Nbl1_ENSMUSG00000041120), max(cohort12F$Nbl1_ENSMUSG00000041120)) +scale_color_manual(values=c("purple"))+geom_smooth(method="lm",color="blue", size = 0.5, se = FALSE)+xlab("RankZ (Nbl1 Expression")+ylab("RankZ (Clcnka Expression)")+ggtitle("Correlation Between Clcnka Expression and Nbl1 Expression in DO-XLAS Females")
Correlation

# Determining if this is significant
Significance <- cor.test(as.numeric(cohort12F$Clcnka_ENSMUSG00000033770), as.numeric(cohort12F$Nbl1_ENSMUSG00000041120))
view(Significance)
# p-value = 0.001643992, SIGNIFICANT

# Get R² values
R2 <- Significance$estimate^2
R2
# R2 = 0.0370477

