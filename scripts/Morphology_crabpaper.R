library(tidyverse)
library(MASS)
library(klaR)
library(DMwR2)
library(factoextra)
library(ggplot2)
library(agricolae)
library(ggpubr)
library(lmodel2)
library(smatr)
library(ggsmatr)
library(devtools)
library(ggpubr)

# Read the data
Raw <- read.csv("Morphology_paper.csv", header = TRUE, na.strings = TRUE)
Raw$Species <- as.factor(Raw$Species)

# Select the variables of interest for PCA
variables <- c("CH", "CL", "CLDL", "CLPL", "CLPW", "CRDL", "CRPL", "CRPW", "CWA", "CWW",  "CWP",  "ED", "RML" , "RMW" , "LML" , "LMW","PFCD", "P2ML", "P2MW", "P5ML", "P5MW", "CrabID")

data_subset <- Raw[, variables]
data_subset$Species <- Raw$Species
data_subset2 <- knnImputation(data_subset[,c (1:21)])

# Check the structure of the data
str(data_subset)

# Identify columns with NAs after coercion
data_subset_numeric <- data_subset[, c(1:21)]
data_subset_numeric[] <- lapply(data_subset_numeric, function(x) as.numeric(as.character(x)))
na_columns <- sapply(data_subset_numeric, function(x) sum(is.na(x)))
print(na_columns[na_columns > 0])

# Inspect and clean problematic columns
# Example: cleaning CLPW column
unique(data_subset$CLPW)
data_subset$CLPW <- as.numeric(as.character(data_subset$CLPW))

# Re-coerce the entire subset again after cleaning
data_subset_numeric <- data_subset[, c(1:21)]
data_subset_numeric[] <- lapply(data_subset_numeric, function(x) as.numeric(as.character(x)))

# Check for NAs again
na_columns <- sapply(data_subset_numeric, function(x) sum(is.na(x)))
print(na_columns[na_columns > 0])

# Apply knnImputation to the cleaned numeric data
data_subset2 <- knnImputation(data_subset_numeric)

# Check the result
str(data_subset2)

#Add metadata
data_subset2$Species <- Raw$Species
data_subset2$Sex <- Raw$Sex
data_subset2$CrabID <- Raw$CrabID

# Create a vector of y-values
y_values <- c("CH", "CL", "CLDL", "CLPL", "CLPW", "CRDL", "CRPL", "CRPW", "CWA", "CWW",  "CWP",  "ED", "RML" , "RMW" , "LML" , "LMW","PFCD", "P2ML", "P2MW", "P5ML", "P5MW")

# Create an empty list to store the ggplot objects
plot_list <- list()

for (y_value in y_values) {
  plot <- ggplot(data = data_subset2, aes(x = CWW, y = .data[[y_value]], color = Species)) +
    geom_point(shape = 21, size = 1, aes(fill = Species), color = "black", show.legend = FALSE) +
    geom_text(aes(label = CrabID), vjust = -0.5, size = 3) +  # Add labels for CrabID
    theme_classic() +
    scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
    scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C"))
  plot_list[[y_value]] <- plot
}

# Create a combined plot using cowplot's plot_grid function
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 3)

# Print or visualize the combined plot
print(combined_plot)

# PCA
pca_result1 <- prcomp((data_subset2[,c (1:21)]), scale = TRUE)
summary(pca_result1)
fviz_eig(pca_result1, addlabels = TRUE)
pca_scores1 <- as.data.frame(pca_result1$x)

# Fit the linear regression models
M2_SR <- lm(CH ~ CWW, data = data_subset2)
M3_SR <- lm(CL ~ CWW, data = data_subset2)
M4_SR <- lm(CLDL ~ CWW, data = data_subset2)
M5_SR <- lm(CLPL ~ CWW, data = data_subset2)
M6_SR <- lm(CLPW~ CWW, data = data_subset2)
M7_SR <- lm(CRDL ~ CWW, data = data_subset2)
M8_SR <- lm(CRPL ~ CWW, data = data_subset2)
M9_SR <- lm(CRPW ~ CWW, data = data_subset2)
M10_SR <- lm(CWA ~ CWW, data = data_subset2)
M11_SR <- lm(CWP ~ CWW, data = data_subset2)
M12_SR <- lm(ED ~ CWW, data = data_subset2)
M13_SR <- lm(RML ~ CWW, data = data_subset2)
M14_SR <- lm(RMW ~ CWW, data = data_subset2)
M15_SR <- lm(LML ~ CWW, data = data_subset2)
M16_SR <- lm(LMW ~ CWW, data = data_subset2)
M17_SR <- lm(PFCD ~ CWW, data = data_subset2)
M18_SR <- lm(P2ML ~ CWW, data = data_subset2)
M19_SR <- lm(P2MW ~ CWW, data = data_subset2)
M20_SR <- lm(P5ML ~ CWW, data = data_subset2)
M21_SR <- lm(P5MW ~ CWW, data = data_subset2)


# Create a data frame with the residuals
Example_residuals <- data.frame(M2_SR$residuals,
                                M3_SR$residuals, M4_SR$residuals,
                                M5_SR$residuals, M6_SR$residuals, M7_SR$residuals,
                                M8_SR$residuals, M9_SR$residuals, M10_SR$residuals,
                                M11_SR$residuals, M12_SR$residuals, M13_SR$residuals,
                                M14_SR$residuals, M15_SR$residuals, M16_SR$residuals,
                                M17_SR$residuals, M18_SR$residuals, M19_SR$residuals,
                                M20_SR$residuals, M21_SR$residuals
)


# PCA
pca_result2 <- prcomp(Example_residuals, scale = TRUE)
summary(pca_result2)
fviz_eig(pca_result2, addlabels = TRUE)
pca_scores2 <- as.data.frame(pca_result2$x)
pca_result2$rotation

# Loadings for PC1 and PC2
loadings <- pca_result2$rotation[, 1:2]

# Print the loadings
print(loadings)

# Merge "Sex" variable back to pca_scores2 based on row names (sample identifiers)
rownames(pca_scores2) <- pca_scores2$Row.names
pca_scores2 <- merge(pca_scores2, data_subset2[c("Sex", "Species")], by = "row.names", all.x = TRUE)

pcaplot <-ggplot(data = pca_scores2, aes(x = PC1, y = PC2, col = Species, shape = Sex, lab)) +
geom_point(size = 3) +
theme_classic() +
geom_hline(yintercept = 0, linetype = "dotted") +
geom_vline(xintercept = 0, linetype = "dotted") +
scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
labs(x = "Principal Component 1 (24.6 % of total variation)", y = "Principal Component 2 (19.6 % of total variation)") + 
scale_x_continuous(limits = c(-9, 9)) +  # Set x-axis limits
scale_y_continuous(limits = c(-9, 9))  # Set y-axis limits

pcaplot

#PC1 model 
pc1_model <- aov(PC1 ~ Species * Sex, data = pca_scores2)
pc1_model_nosex <- aov(PC1 ~ Species, data = pca_scores2)

summary (pc1_model)
TukeyHSD(pc1_model)
TukeyHSD(pc1_model_nosex)

#PC2 model
PC2_model <- aov(PC2 ~ Species * Sex, data = pca_scores2)
pc2_model_nosex <- aov(PC2 ~ Species, data = pca_scores2)

summary (PC2_model)
TukeyHSD(PC2_model)
TukeyHSD(pc2_model_nosex)

######################

#LDA
lda_model <- MASS::lda(data_subset2$Species ~ ., data_subset2[,c (1:21)])
lda_model$prior
lda_model$svd

lda_prediction <- predict(lda_model)
str(lda_prediction)
lda_prediction$x
lda_scores2 <- merge(lda_prediction$x, data_subset2["Species"], by = "row.names", all.x = TRUE)

#Things of interest
lda_model$scaling

# Merge "Sex" variable back to lda_scores2 based on row names (sample identifiers)
rownames(lda_scores2) <- lda_scores2$Row.names
lda_scores2 <- merge(lda_prediction$x, data_subset2[c("Sex", "Species")], by = "row.names", all.x = TRUE)

ldaplot <- ggplot(data = lda_scores2, aes(x = LD1, y = LD2, col = Species, shape = Sex)) +
  geom_point(size = 3) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "Linear Discriminant 1 (76.6 % of total variation)", y = "Linear Discriminant 2 (23.3 % of total variation") + 
  scale_x_continuous(limits = c(-9, 9)) +  # Set x-axis limits
  scale_y_continuous(limits = c(-9, 9))  # Set y-axis limits

ldaplot

ggarrange(pcaplot,ldaplot)


# Fit LDA model
lda_model <- MASS::lda(data_subset2$Species ~ ., data_subset2[, c(1:21)])

# Print the proportion of variance explained by each linear discriminant
cat("Proportion of Variance Explained by LDA1:", lda_model$svd[1]^2 / sum(lda_model$svd^2), "\n")
cat("Proportion of Variance Explained by LDA2:", lda_model$svd[2]^2 / sum(lda_model$svd^2), "\n")

######################
#Smatr standardised major axis regression - Table 

#Global tests

# For CL
sma_CH <- sma(formula = CH ~ CWW + Species, data = data_subset2)
summary(sma_CH)

sma_CH_both <- sma(formula = CH ~ CWW * Species, data = data_subset2)
summary(sma_CH_both)

# For CL
sma_CL <- sma(formula = CL ~ CWW + Species, data = data_subset2)
summary(sma_CL)

sma_CL_both <- sma(formula = CL ~ CWW * Species, data = data_subset2)
summary(sma_CL_both)

# For CLDL
sma_CLDL <- sma(formula = CLDL ~ CWW + Species, data = data_subset2)
summary(sma_CLDL)

sma_CLDL_both <- sma(formula = CLDL ~ CWW * Species, data = data_subset2)
summary(sma_CLDL_both)

# For CLPL
sma_CLPL <- sma(formula = CLPL ~ CWW + Species, data = data_subset2)
summary(sma_CLPL)

sma_CLPL_both <- sma(formula = CLPL ~ CWW * Species, data = data_subset2)
summary(sma_CLPL_both)

# For CLPW
sma_CLPW <- sma(formula = CLPW ~ CWW + Species, data = data_subset2)
summary(sma_CLPW)

sma_CLPW_both <- sma(formula = CLPW ~ CWW * Species, data = data_subset2)
summary(sma_CLPW_both)

# For CRDL
sma_CRDL <- sma(formula = CRDL ~ CWW + Species, data = data_subset2)
summary(sma_CRDL)

sma_CRDL_both <- sma(formula = CRDL ~ CWW * Species, data = data_subset2)
summary(sma_CRDL_both)

# For CRPL
sma_CRPL <- sma(formula = CRPL ~ CWW + Species, data = data_subset2)
summary(sma_CRPL)

sma_CRPL_both <- sma(formula = CRPL ~ CWW * Species, data = data_subset2)
summary(sma_CRPL_both)

# For CRPW
sma_CRPW <- sma(formula = CRPW ~ CWW + Species, data = data_subset2)
summary(sma_CRPW)

sma_CRPW_both <- sma(formula = CRPW ~ CWW * Species, data = data_subset2)
summary(sma_CRPW_both)

# For CWA
sma_CWA <- sma(formula = CWA ~ CWW + Species, data = data_subset2)
summary(sma_CWA)

sma_CWA_both <- sma(formula = CWA ~ CWW * Species, data = data_subset2)
summary(sma_CWA_both)

# For CWP
sma_CWP <- sma(formula = CWP ~ CWW + Species, data = data_subset2)
summary(sma_CWP)

sma_CWP_both <- sma(formula = CWP ~ CWW * Species, data = data_subset2)
summary(sma_CWP_both)

# For ED
sma_ED <- sma(formula = ED ~ CWW + Species, data = data_subset2)
summary(sma_ED)

sma_ED_both <- sma(formula = ED ~ CWW * Species, data = data_subset2)
summary(sma_ED_both)

# For RML
sma_RML <- sma(formula = RML ~ CWW + Species, data = data_subset2)
summary(sma_RML)

sma_RML_both <- sma(formula = RML ~ CWW * Species, data = data_subset2)
summary(sma_RML_both)

# For RMW
sma_RMW <- sma(formula = RMW ~ CWW + Species, data = data_subset2)
summary(sma_RMW)

sma_RMW_both <- sma(formula = RMW ~ CWW * Species, data = data_subset2)
summary(sma_RMW_both)

# For LML
sma_LML <- sma(formula = LML ~ CWW + Species, data = data_subset2)
summary(sma_LML)

sma_LML_both <- sma(formula = LML ~ CWW * Species, data = data_subset2)
summary(sma_LML_both)

# For LMW
sma_LMW <- sma(formula = LMW ~ CWW + Species, data = data_subset2)
summary(sma_LMW)

sma_LMW_both <- sma(formula = LMW ~ CWW * Species, data = data_subset2)
summary(sma_LMW_both)

# For PFCD
sma_PFCD <- sma(formula = PFCD ~ CWW + Species, data = data_subset2)
summary(sma_PFCD)

sma_PFCD_both <- sma(formula = PFCD ~ CWW * Species, data = data_subset2)
summary(sma_PFCD_both)

# For P2ML
sma_P2ML <- sma(formula = P2ML ~ CWW + Species, data = data_subset2)
summary(sma_P2ML)

sma_P2ML_both <- sma(formula = P2ML ~ CWW * Species, data = data_subset2)
summary(sma_P2ML_both)

# For P2MW
sma_P2MW <- sma(formula = P2MW ~ CWW + Species, data = data_subset2)
summary(sma_P2MW)

sma_P2MW_both <- sma(formula = P2MW ~ CWW * Species, data = data_subset2)
summary(sma_P2MW_both)

# For P5ML
sma_P5ML <- sma(formula = P5ML ~ CWW + Species, data = data_subset2)
summary(sma_P5ML)

sma_P5ML_both <- sma(formula = P5ML ~ CWW * Species, data = data_subset2)
summary(sma_P5ML_both)

# For P5MW
sma_P5MW <- sma(formula = P5MW ~ CWW + Species, data = data_subset2)
summary(sma_P5MW)

sma_P5MW_both <- sma(formula = P5MW ~ CWW * Species, data = data_subset2)
summary(sma_P5MW_both)

######################

#Pairwise smatr testing

# Subset creation
AMI_AO_subset <- subset(data_subset2, Species %in% c("A orbitospinus ", "A montivagus I"))
AMI_AMII_subset <- subset(data_subset2, Species %in% c("A montivagus I", "A montivagus II"))
AMII_AO_subset <- subset(data_subset2, Species %in% c("A orbitospinus ", "A montivagus II"))

# CH
sma_CH_AMI_AO <- sma(formula = CH ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CH_AMI_AO)

sma_CH_AMI_AMII <- sma(formula = CH ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CH_AMI_AMII)

sma_CH_AMII_AO <- sma(formula = CH ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CH_AMII_AO)

# CL
sma_CL_AMI_AO <- sma(formula = CL ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CL_AMI_AO)

sma_CL_AMI_AMII <- sma(formula = CL ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CL_AMI_AMII)

sma_CL_AMII_AO <- sma(formula = CL ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CL_AMII_AO)

# CLDL
sma_CLDL_AMI_AO <- sma(formula = CLDL ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CLDL_AMI_AO)

sma_CLDL_AMI_AMII <- sma(formula = CLDL ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CLDL_AMI_AMII)

sma_CLDL_AMI_AMII_both <- sma(formula = CLDL ~ CWW * Species, data = AMI_AMII_subset)
summary(sma_CLDL_AMI_AMII_both)

sma_CLDL_AMII_AO <- sma(formula = CLDL ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CLDL_AMII_AO)

sma_CLDL_AMII_AO <- sma(formula = CLDL ~ CWW * Species, data = AMII_AO_subset)
summary(sma_CLDL_AMII_AO)

# CLPL
sma_CLPL_AMI_AO <- sma(formula = CLPL ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CLPL_AMI_AO)

sma_CLPL_AMI_AMII <- sma(formula = CLPL ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CLPL_AMI_AMII)

sma_CLPL_AMI_AMII_both <- sma(formula = CLPL ~ CWW * Species, data = AMI_AMII_subset)
summary(sma_CLPL_AMI_AMII_both)

sma_CLPL_AMII_AO <- sma(formula = CLPL ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CLPL_AMII_AO)

sma_CLPL_AMII_AO_both <- sma(formula = CLPL ~ CWW * Species, data = AMII_AO_subset)
summary(sma_CLPL_AMII_AO_both)

# CLPW
sma_CLPW_AMI_AO <- sma(formula = CLPW ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CLPW_AMI_AO)

sma_CLPW_AMI_AMII <- sma(formula = CLPW ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CLPW_AMI_AMII)

sma_CLPW_AMI_AMII_both <- sma(formula = CLPW ~ CWW * Species, data = AMI_AMII_subset)
summary(sma_CLPW_AMI_AMII_both)

sma_CLPW_AMII_AO <- sma(formula = CLPW ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CLPW_AMII_AO)

sma_CLPW_AMII_AO_both <- sma(formula = CLPW ~ CWW * Species, data = AMII_AO_subset)
summary(sma_CLPW_AMII_AO_both)

# CRDL
sma_CRDL_AMI_AO <- sma(formula = CRDL ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CRDL_AMI_AO)

sma_CRDL_AMI_AO_both <- sma(formula = CRDL ~ CWW * Species, data = AMI_AO_subset)
summary(sma_CRDL_AMI_AO_both)

sma_CRDL_AMI_AMII <- sma(formula = CRDL ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CRDL_AMI_AMII)

sma_CRDL_AMII_AO <- sma(formula = CRDL ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CRDL_AMII_AO)

sma_CRDL_AMII_AO_both <- sma(formula = CRDL ~ CWW * Species, data = AMII_AO_subset)
summary(sma_CRDL_AMII_AO_both)

# CRPL
sma_CRPL_AMI_AO <- sma(formula = CRPL ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CRPL_AMI_AO)

sma_CRPL_AMI_AMII <- sma(formula = CRPL ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CRPL_AMI_AMII)

sma_CRPL_AMII_AO <- sma(formula = CRPL ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CRPL_AMII_AO)

sma_CRPL_AMII_AO_both <- sma(formula = CRPL ~ CWW * Species, data = AMII_AO_subset)
summary(sma_CRPL_AMII_AO_both)

# CRPW
sma_CRPW_AMI_AO <- sma(formula = CRPW ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CRPW_AMI_AO)

sma_CRPW_AMI_AMII <- sma(formula = CRPW ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CRPW_AMI_AMII)

sma_CRPW_AMII_AO <- sma(formula = CRPW ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CRPW_AMII_AO)

sma_CRPW_AMII_AO_both <- sma(formula = CRPW ~ CWW * Species, data = AMII_AO_subset)
summary(sma_CRPW_AMII_AO_both)

# CWA
sma_CWA_AMI_AO <- sma(formula = CWA ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CWA_AMI_AO)

sma_CWA_AMI_AMII <- sma(formula = CWA ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CWA_AMI_AMII)

sma_CWA_AMII_AO <- sma(formula = CWA ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CWA_AMII_AO)

sma_CWA_AMII_AO_both <- sma(formula = CWA ~ CWW * Species, data = AMII_AO_subset)
summary(sma_CWA_AMII_AO_both)

# CWW
sma_CWW_AMI_AO <- sma(formula = CWW ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CWW_AMI_AO)

sma_CWW_AMI_AMII <- sma(formula = CWW ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CWW_AMI_AMII)

sma_CWW_AMII_AO <- sma(formula = CWW ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CWW_AMII_AO)

# CWP
sma_CWP_AMI_AO <- sma(formula = CWP ~ CWW + Species, data = AMI_AO_subset)
summary(sma_CWP_AMI_AO)

sma_CWP_AMI_AMII <- sma(formula = CWP ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_CWP_AMI_AMII)

sma_CWP_AMI_AMII_both <- sma(formula = CWP ~ CWW * Species, data = AMI_AMII_subset)
summary(sma_CWP_AMI_AMII_both)

sma_CWP_AMII_AO <- sma(formula = CWP ~ CWW + Species, data = AMII_AO_subset)
summary(sma_CWP_AMII_AO)

# ED
sma_ED_AMI_AO <- sma(formula = ED ~ CWW + Species, data = AMI_AO_subset)
summary(sma_ED_AMI_AO)

sma_ED_AMI_AMII <- sma(formula = ED ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_ED_AMI_AMII)

sma_ED_AMI_AMII <- sma(formula = ED ~ CWW * Species, data = AMI_AMII_subset)
summary(sma_ED_AMI_AMII)

sma_ED_AMII_AO <- sma(formula = ED ~ CWW + Species, data = AMII_AO_subset)
summary(sma_ED_AMII_AO)

# RML
sma_RML_AMI_AO <- sma(formula = RML ~ CWW + Species, data = AMI_AO_subset)
summary(sma_RML_AMI_AO)

sma_RML_AMI_AMII <- sma(formula = RML ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_RML_AMI_AMII)

sma_RML_AMII_AO <- sma(formula = RML ~ CWW + Species, data = AMII_AO_subset)
summary(sma_RML_AMII_AO)

sma_RML_AMII_AO_both <- sma(formula = RML ~ CWW * Species, data = AMII_AO_subset)
summary(sma_RML_AMII_AO_both)

# RMW
sma_RMW_AMI_AO <- sma(formula = RMW ~ CWW + Species, data = AMI_AO_subset)
summary(sma_RMW_AMI_AO)

sma_RMW_AMI_AMII <- sma(formula = RMW ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_RMW_AMI_AMII)

sma_RMW_AMII_AO <- sma(formula = RMW ~ CWW + Species, data = AMII_AO_subset)
summary(sma_RMW_AMII_AO)

# LML
sma_LML_AMI_AO <- sma(formula = LML ~ CWW + Species, data = AMI_AO_subset)
summary(sma_LML_AMI_AO)

sma_LML_AMI_AMII <- sma(formula = LML ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_LML_AMI_AMII)

sma_LML_AMI_AMII_both <- sma(formula = LML ~ CWW * Species, data = AMI_AMII_subset)
summary(sma_LML_AMI_AMII_both)

sma_LML_AMII_AO <- sma(formula = LML ~ CWW + Species, data = AMII_AO_subset)
summary(sma_LML_AMII_AO)

sma_LML_AMII_AO_both <- sma(formula = LML ~ CWW * Species, data = AMII_AO_subset)
summary(sma_LML_AMII_AO_both)

# LMW
sma_LMW_AMI_AO <- sma(formula = LMW ~ CWW + Species, data = AMI_AO_subset)
summary(sma_LMW_AMI_AO)

sma_LMW_AMI_AMII <- sma(formula = LMW ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_LMW_AMI_AMII)

sma_LMW_AMII_AO <- sma(formula = LMW ~ CWW + Species, data = AMII_AO_subset)
summary(sma_LMW_AMII_AO)

# PFCD
sma_PFCD_AMI_AO <- sma(formula = PFCD ~ CWW + Species, data = AMI_AO_subset)
summary(sma_PFCD_AMI_AO)

sma_PFCD_AMI_AMII <- sma(formula = PFCD ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_PFCD_AMI_AMII)

sma_PFCD_AMII_AO <- sma(formula = PFCD ~ CWW + Species, data = AMII_AO_subset)
summary(sma_PFCD_AMII_AO)

# P2ML
sma_P2ML_AMI_AO <- sma(formula = P2ML ~ CWW + Species, data = AMI_AO_subset)
summary(sma_P2ML_AMI_AO)

sma_P2ML_AMI_AO_both <- sma(formula = P2ML ~ CWW * Species, data = AMI_AO_subset)
summary(sma_P2ML_AMI_AO_both)

sma_P2ML_AMI_AMII <- sma(formula = P2ML ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_P2ML_AMI_AMII)

sma_P2ML_AMII_AO <- sma(formula = P2ML ~ CWW + Species, data = AMII_AO_subset)
summary(sma_P2ML_AMII_AO)

sma_P2ML_AMII_AO_both <- sma(formula = P2ML ~ CWW * Species, data = AMII_AO_subset)
summary(sma_P2ML_AMII_AO_both)

# P2MW
sma_P2MW_AMI_AO <- sma(formula = P2MW ~ CWW + Species, data = AMI_AO_subset)
summary(sma_P2MW_AMI_AO)

sma_P2MW_AMI_AMII <- sma(formula = P2MW ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_P2MW_AMI_AMII)

sma_P2MW_AMII_AO <- sma(formula = P2MW ~ CWW + Species, data = AMII_AO_subset)
summary(sma_P2MW_AMII_AO)

# P5ML
sma_P5ML_AMI_AO <- sma(formula = P5ML ~ CWW + Species, data = AMI_AO_subset)
summary(sma_P5ML_AMI_AO)

sma_P5ML_AMI_AO_both <- sma(formula = P5ML ~ CWW * Species, data = AMI_AO_subset)
summary(sma_P5ML_AMI_AO_both)

sma_P5ML_AMI_AMII <- sma(formula = P5ML ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_P5ML_AMI_AMII)

sma_P5ML_AMII_AO <- sma(formula = P5ML ~ CWW + Species, data = AMII_AO_subset)
summary(sma_P5ML_AMII_AO)

sma_P5ML_AMII_AO_both <- sma(formula = P5ML ~ CWW * Species, data = AMII_AO_subset)
summary(sma_P5ML_AMII_AO_both)

# P5MW
sma_P5MW_AMI_AO <- sma(formula = P5MW ~ CWW + Species, data = AMI_AO_subset)
summary(sma_P5MW_AMI_AO)

sma_P5MW_AMI_AO_both <- sma(formula = P5MW ~ CWW * Species, data = AMI_AO_subset)
summary(sma_P5MW_AMI_AO_both)

sma_P5MW_AMI_AMII <- sma(formula = P5MW ~ CWW + Species, data = AMI_AMII_subset)
summary(sma_P5MW_AMI_AMII)

sma_P5MW_AMII_AO <- sma(formula = P5MW ~ CWW + Species, data = AMII_AO_subset)
summary(sma_P5MW_AMII_AO)

sma_P5MW_AMII_AO_both <- sma(formula = P5MW ~ CWW * Species, data = AMII_AO_subset)
summary(sma_P5MW_AMII_AO_both)

################
#Plots on key variables from LDA and SMATR testing

# Create individual plots for each SMA model
sma_CH_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "CH", sma.fit = sma_CH_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "CH") +
  theme(legend.position = "none")

sma_PFCD_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "PFCD", sma.fit = sma_PFCD_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "PFCD") +
  theme(legend.position = "none")

sma_CRDL_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "CRDL", sma.fit = sma_CRDL_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "CRDL") +
  theme(legend.position = "none")

sma_P2MW_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "P2MW", sma.fit = sma_P2MW_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "P2MW") +
  theme(legend.position = "none")

sma_P5ML_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "P5ML", sma.fit = sma_P5ML_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "P5ML") +
  theme(legend.position = "none")

sma_P5MW_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "P5MW", sma.fit = sma_P5MW_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "P5MW") +
  theme(legend.position = "none")


#Based on smatr global tests

sma_CLDL_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "CLDL", sma.fit = sma_CRDL_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "CLDL") +
  theme(legend.position = "none")

sma_CRDL_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "CRDL", sma.fit = sma_CRDL_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "CRDL") +
  theme(legend.position = "none")

sma_LML_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "LML", sma.fit = sma_LML_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "LML") +
  theme(legend.position = "none")

sma_RML_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "RML", sma.fit = sma_RML_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "RML") +
  theme(legend.position = "none")

sma_P5ML_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "P5ML", sma.fit = sma_P5ML_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "P5ML") +
  theme(legend.position = "none")

sma_P5MW_plot <- ggsmatr(data_subset2, groups = "Species", xvar = "CWW", yvar = "P5MW", sma.fit = sma_P5MW_both) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_fill_manual(values = c("#00B4D8", "#E5989B", "#C9182C")) +
  scale_shape_manual(values = c("M" = 19, "F" = 17)) +  # Define symbols for Male and Female
  labs(x = "CWW", y = "P5MW") +
  theme(legend.position = "none")

Combined_plot <- ggarrange(sma_CH_plot,sma_PFCD_plot,sma_P2MW_plot, sma_CRDL_plot, sma_P5ML_plot, sma_P5MW_plot, sma_CLDL_plot, sma_LML_plot, sma_RML_plot, 
                      ncol = 3, nrow = 3)


Combined_plot 


########################################


#End of analysis