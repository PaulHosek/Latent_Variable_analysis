dat <- read.csv("data_lvm_4.csv", header = TRUE, sep = ";")


library(psych)
library(ltm)
dat_competence <- data.frame(CO01 = dat$CO01, CO02 = dat$CO02, CO03 = dat$CO03,
                             CO04 = dat$CO04, CO05 = dat$CO05, CO06 = dat$CO06,
                             CO07 = dat$CO07, CO08 = dat$CO08, CO09 = dat$CO09,
                             CO10 = dat$CO10)

dat_autonomy <- data.frame(AU01 = dat$AU01, AU02 = dat$AU02, AU03 = dat$AU03,
                           AU04 = dat$AU04, AU05 = dat$AU05, AU06 = dat$AU06,
                           AU07 = dat$AU07, AU08 = dat$AU08, AU09 = dat$AU09,
                           AU10 = dat$AU10)

dat_relatedness <- data.frame(RE01 = dat$RE01, RE02 = dat$RE02, RE03 = dat$RE03,
                              RE04 = dat$RE04, RE05 = dat$RE05, RE06 = dat$RE06,
                              RE07 = dat$RE07, RE08 = dat$RE08, RE09 = dat$RE09,
                              RE10 = dat$RE10)

### IRT Anylsis ###

## Fitting a graded response model

SDT_graded_model_competence <- grm(dat_competence, Hessian = TRUE)
SDT_graded_model_autonomy <- grm(dat_autonomy, Hessian = TRUE)
SDT_graded_model_relatedness <- grm(dat_relatedness, Hessian = TRUE)

## Test for Unidimensionality
poly_competence <- polychoric(dat_competence)$rho
poly_autonomy <- polychoric(dat_autonomy)$rho
poly_relatedness <- polychoric(dat_relatedness)$rho

png("Scree Plots.png", width = 1000, height = 600)
par(mfrow = c(1,3))
fa.parallel(poly_competence, fa = "pc", n.obs = nrow(dat_competence), main = "Scree Plot - Competence",
            cor = "poly")
fa.parallel(poly_autonomy, fa = "pc", n.obs = nrow(dat_autonomy), main = "Scree Plot - Autonomy",
            cor = "poly")
fa.parallel(poly_relatedness, fa = "pc", n.obs = nrow(dat_relatedness), main = "Scree Plot - Relatedness",
            cor = "poly")
dev.off()

# Exact Eigenvalues
fa.parallel(poly_competence, fa = "pc", n.obs = nrow(dat_competence))$pc.values
fa.parallel(poly_autonomy, fa = "pc", n.obs = nrow(dat_autonomy))$pc.values
fa.parallel(poly_relatedness, fa = "pc", n.obs = nrow(dat_relatedness))$pc.values


## Equality of Item discriminators

sum_scores_competence <- apply(dat_competence, 1, sum)
sum_scores_autonomy <- apply(dat_autonomy, 1, sum)
sum_scores_relatedness <- apply(dat_relatedness, 1, sum)

item_rest_competence <- c()
item_rest_autonomy <- c()
item_rest_relatedness <- c()

# for(i in 1:nrow(dat_competence)){
#   item_rest_competence[i] <- cor(dat_competence[,i], sum_scores_competence - dat_competence[,i])
#   item_rest_autonomy[i] <- cor(dat_autonomy[,i], sum_scores_autonomy - dat_autonomy[,i])
#   item_rest_relatedness[i] <- cor(dat_relatedness[,i], sum_scores_relatedness - dat_relatedness[,i])
# }
# 
# # Rough estimate via histogramm
# par(mfrow = c(1,3))
# hist(item_rest_competence, main = "Competence")
# hist(item_rest_autonomy, main = "Autonomy")
# hist(item_rest_relatedness, main = "Relatedness")


# Likelihhod ratio test with equal discriminatior, constrauned vs non constrained
anova(grm(dat_competence, Hessian = TRUE, constrained = TRUE), grm(dat_competence, Hessian = TRUE))
anova(grm(dat_autonomy, Hessian = TRUE, constrained = TRUE), grm(dat_autonomy, Hessian = TRUE))
anova(grm(dat_relatedness, Hessian = TRUE, constrained = TRUE), grm(dat_relatedness, Hessian = TRUE))

## Item Curves and Test Information

# Item Information
png("Item Information Curves.png", width = 1000, height = 600)
par(mfrow = c(1,3))
plot(SDT_graded_model_competence, type = "IIC", main = "Competence")
plot(SDT_graded_model_autonomy, type = "IIC", main = "Autonomy")
plot(SDT_graded_model_relatedness, type = "IIC", main = "Relatedness")
dev.off()

# Test Information
png("Test Information Curves.png", width = 1000, height = 600)
par(mfrow = c(1,3))
plot(SDT_graded_model_competence, type = "IIC", items = 0, main = "Competence")
plot(SDT_graded_model_autonomy, type = "IIC", items = 0, main = "Autonomy")
plot(SDT_graded_model_relatedness, type = "IIC", items = 0, main = "Relatedness")
dev.off()