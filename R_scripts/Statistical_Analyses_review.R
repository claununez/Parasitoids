################
# Project: Forewing length to estimate body weight in Ichneumonidae
# Authors: Claudia Nuïez-Penichet, Marlon Cobos
# Process: Statistical analyses
# Date: 10/10/2019
################

# Directory
setwd("C:/Users/Marlon/Documents/R/Parasitoides")

# Pacakges
library(HDoutliers)

# Read data
paras <- read.csv("tabla_parasitoides_corr2.csv")

# Exploring the data looking for outliers
sex <- as.factor(paras$Sex)
cols <- setNames(c("blue", "red"), levels(sex))
pchs <- setNames(c(1, 19), levels(sex))

## Plotting and exploring outliers
par(cex = 1, mar = c(4, 4, 1.3, 0.5))
plot(paras[, 6:5], pch = pchs[sex], col = cols[sex], xlab = "Wing length (mm)",
     ylab = "Mass (mg)", las = 1)
legend("bottomright", c("Female", "Male"), pch = pchs,
       col = cols, bg = "white")

outs <- HDoutliers(paras[, 6:5], transform = FALSE)
points(paras[outs, 6:5], col = "grey45", cex = 2.5)


# Cleaning the data
paras_clean <- paras[-outs, ]


# Normality test (Shapiro-Wilk's test)
n_test <- apply(paras_clean[, c("Wing.length..mm.", "Weight..mg.")], 2,
                shapiro.test)

# t-test between countries
Ecuador <- paras_clean[paras_clean$Country == 1, ]
Venezuela <- paras_clean[paras_clean$Country == 2, ]
WL_t.test <- t.test(Ecuador$Wing.length..mm.,
                    Venezuela$Wing.length..mm., alternative = "less")

WE_t.test <- t.test(Ecuador$Weight..mg., Venezuela$Weight..mg.,
                    alternative = "less")

# t-test between sex
Female <- paras_clean[paras_clean$Sex == 1, ]
Male <- paras_clean[paras_clean$Sex == 2, ]
WL_Sex_t.test <- t.test(Female$Wing.length..mm., Male$Wing.length..mm.,
                        alternative = "less")

WE_Sex_t.test <- t.test(Female$Weight..mg., Male$Weight..mg.,
                        alternative = "less")

# t-test between strategy
Idio <- paras_clean[paras_clean$Strategy == "idiobiont", ]
Koin <- paras_clean[paras_clean$Strategy == "koinobiont", ]
WL_Str_t.test <- t.test(Idio$Wing.length..mm., Koin$Wing.length..mm.,
                        alternative = "less")

WE_Str_t.test <- t.test(Idio$Weight..mg., Koin$Weight..mg.,
                        alternative = "less")


# Boxplot representation
par(cex = 0.73, mar = c(4, 4, 0, 0), mfrow = c(2, 3))

boxplot(paras_clean$Weight..mg. ~ paras_clean$Country, xlab = "",
        ylab = "Mass (mg)", las = 1, names = c("Ecuador", "Venezuela") )
boxplot(paras_clean$Weight..mg.~paras_clean$Sex, xlab = "",
        ylab = "", las = 1, names = c("Female","Male"))
boxplot(paras_clean$Weight..mg.~paras_clean$Strategy, xlab = "",
        ylab = "", las = 1, names = c("Koinobiont", "Idiobiont"))

boxplot(paras_clean$Wing.length..mm. ~ paras_clean$Country, xlab = "Country",
        ylab = "Wing length (mm)", las = 1, names = c("Ecuador", "Venezuela") )
boxplot(paras_clean$Wing.length..mm.~paras_clean$Sex, xlab = "Sex",
        ylab = "", las = 1,names = c("Female", "Male") )
boxplot(paras_clean$Wing.length..mm.~paras_clean$Strategy, xlab = "Strategy",
        ylab = "", las = 1, names = c("Koinobiont", "Idiobiont"))


# Exporting summary figure
jpeg(filename = "Weight_Wing_length_summary_plot1a.jpg",
     width = 16.6, height = 20, units = "cm", res = 600)
lmat <- matrix(c(rep(1, 3), 2, 3, 4, 5, 6, 7), 3, 3, byrow = TRUE)
layout(lmat, heights = c(2, 1, 1))
par(cex = 0.7, mar = c(5, 4, 0, 0))
plot(paras[, 6:5], pch = pchs[sex], col = cols[sex], xlab = "Wing length (mm)",
     ylab = "Mass (mg)", las = 1)
points(paras[outs, 6:5], col = "grey45", cex = 2.5)
legend("bottomright", c("Female", "Male"), pch = pchs,
       col = cols, bg = "white", cex = 0.9, bty = "n")
legend("topleft", legend = "(a) Overall relationship", bty = "n", cex = 1.2)

par(cex = 0.7, mar = c(4, 4, 0, 0))
boxplot(paras_clean$Weight..mg. ~ paras_clean$Country, xlab = "",
        ylab = "Mass (mg)", las = 1, names = c("Ecuador", "Venezuela") )
legend("topright", legend = "(b)", bty = "n", cex = 1.2)
boxplot(paras_clean$Weight..mg.~paras_clean$Sex, xlab = "",
        ylab = "", las = 1, names = c("Female","Male"))
legend("topright", legend = "(c)", bty = "n", cex = 1.2)
boxplot(paras_clean$Weight..mg.~paras_clean$Strategy, xlab = "",
        ylab = "", las = 1, names = c("Koinobiont", "Idiobiont"))
legend("topright", legend = "(d)", bty = "n", cex = 1.2)

boxplot(paras_clean$Wing.length..mm. ~ paras_clean$Country, xlab = "Country",
        ylab = "Wing length (mm)", las = 1, names = c("Ecuador", "Venezuela") )
legend("topright", legend = "(e)", bty = "n", cex = 1.2)
boxplot(paras_clean$Wing.length..mm.~paras_clean$Sex, xlab = "Sex",
        ylab = "", las = 1,names = c("Female", "Male") )
legend("topright", legend = "(f)", bty = "n", cex = 1.2)
boxplot(paras_clean$Wing.length..mm.~paras_clean$Strategy, xlab = "Strategy",
        ylab = "", las = 1, names = c("Koinobiont", "Idiobiont"))
legend("topright", legend = "(g)", bty = "n", cex = 1.2)
dev.off()

# Testing effect of country and sex in relationships 
## Ancova Country (effects of elevation)
paras_anc <- paras_clean
paras_anc$Country <- as.factor(paras_anc$Country)

### First model
fit1 <- aov(Weight..mg. ~ Wing.length..mm. * Country, data = paras_anc)
summary(fit1)

### Second model
fit2 <- aov(Weight..mg. ~ Wing.length..mm. + Country, data = paras_anc)
summary(fit2)

### Comparing the two models
anova(fit1, fit2)

## Ancova Sex (effects of sex on relationship)
paras_anc$Sex <- as.factor(paras_anc$Sex)

### First model
fit3 <- aov(Weight..mg. ~ Wing.length..mm. * Sex, data = paras_anc)
summary(fit3)

### Second model
fit4 <- aov(Weight..mg. ~ Wing.length..mm. + Sex, data = paras_anc)
summary(fit4)

### Comparing the two models
anova(fit3, fit4)

## Ancova Strategy 
paras_anc$Strategy <- as.factor(paras_anc$Strategy)

### First model
fit5 <- aov(Weight..mg. ~ Wing.length..mm. * Strategy, data = paras_anc)
summary(fit5)

### Second model
fit6 <- aov(Weight..mg. ~ Wing.length..mm. + Strategy, data = paras_anc)
summary(fit6)

### Comparing the two models
anova(fit5, fit6)


# Dividing the data in training and testing
set.seed(1)
train_all <- paras_clean[sample(1:dim(paras_clean)[1], dim(paras_clean) * 0.75), ]
train_female <- Female[sample(1:dim(Female)[1], dim(Female) * 0.75), ]
train_male <- Male[sample(1:dim(Male)[1], dim(Male) * 0.75), ]
train_idio <- Idio[sample(1:dim(Idio)[1], dim(Idio) * 0.75), ]
train_koin <- Koin[sample(1:dim(Koin)[1], dim(Koin) * 0.75), ]

test_all <- paras_clean[!row.names(paras_clean) %in% row.names(train_all), ]
test_female <- Female[!row.names(Female) %in% row.names(train_female), ]
test_male <- Male[!row.names(Male) %in% row.names(train_male), ]
test_idio <- Idio[!row.names(Idio) %in% row.names(train_idio), ]
test_koin <- Koin[!row.names(Koin) %in% row.names(train_koin), ]


# Regressions
## training
regre_train_all <- lm(log(train_all$Weight..mg.) ~
                        log(train_all$Wing.length..mm.))
summary(regre_train_all)
regre_train_female <- lm(log(train_female$Weight..mg.) ~
                           log(train_female$Wing.length..mm.))
summary(regre_train_female)
regre_train_male <- lm(log(train_male$Weight..mg.) ~
                         log(train_male$Wing.length..mm.))
summary(regre_train_male)
regre_train_idio <- lm(log(train_idio$Weight..mg.) ~
                           log(train_idio$Wing.length..mm.))
summary(regre_train_idio)
regre_train_koin <- lm(log(train_koin$Weight..mg.) ~
                         log(train_koin$Wing.length..mm.))
summary(regre_train_koin)

## testing
### calling function
source("https://raw.githubusercontent.com/claununez/Parasitoids/master/R_scripts/ichneumonidae_wwl.R")

### calculating weights with testing data and the function
weight_all <- ichneumonidae_wwl(wing_length = test_all$Wing.length..mm.,
                                units_wing_length = "mm")
weight_female <- ichneumonidae_wwl(wing_length = test_female$Wing.length..mm.,
                                   units_wing_length = "mm", sex_or_strategy = "f")
weight_male <- ichneumonidae_wwl(wing_length = test_male$Wing.length..mm.,
                                 units_wing_length = "mm", sex_or_strategy = "m")
weight_idio <- ichneumonidae_wwl(wing_length = test_idio$Wing.length..mm.,
                                   units_wing_length = "mm", sex_or_strategy = "idi")
weight_koin <- ichneumonidae_wwl(wing_length = test_koin$Wing.length..mm.,
                                 units_wing_length = "mm", sex_or_strategy = "koi")

### correlation between calculated and measured testing weights
weight_cor_test <- cor.test(weight_all, test_all$Weight..mg.)
weight_cor_test_female <- cor.test(weight_female, test_female$Weight..mg.)
weight_cor_test_male <- cor.test(weight_male, test_male$Weight..mg.)
weight_cor_test_idio <- cor.test(weight_idio, test_idio$Weight..mg.)
weight_cor_test_koin <- cor.test(weight_koin, test_koin$Weight..mg.)


# Plotting regressions
strat <- paras_clean$Strategy
colss <- c("orange", "brown")

jpeg(filename = "Wing_length_Weight_rel_summary_review.jpg", width = 16.6, height = 16,
     units = "cm", res = 600)
par(mar = c(4, 4, 0.5, 0.5), mfrow = c(2, 2))

## log Weight/Wing lengths
par(cex = 0.7)
plot(log(train_all$Wing.length..mm.), log(train_all$Weight..mg.),
     pch = pchs[sex], col = cols[sex],
     ylab = "Natural logarithm of mass (mg)",
     xlab = "Natural logarithm of wing length (mm)", las = 1)
abline(lm(log(train_all$Weight..mg.) ~
            log(train_all$Wing.length..mm.) ), lty = 1, lwd = 1, col = "black")
legend("topleft", legend = "(a)", bty = "n", cex = 1.2)
legend("bottomright", c("Female", "Male"), pch = pchs,
       col = cols, bg = "white", cex = 0.9, bty = "n")

plot(log(train_all$Wing.length..mm.), log(train_all$Weight..mg.),
     pch = pchs[strat], col = colss[strat],
     ylab = "", xlab = "Natural logarithm of wing length (mm)", las = 1)
abline(lm(log(train_all$Weight..mg.) ~
            log(train_all$Wing.length..mm.) ), lty = 1, lwd = 1, col = "black")
legend("topleft", legend = "(b)", bty = "n", cex = 1.2)
legend("bottomright", c("Idiobiont", "Koinobiont"), pch = pchs,
       col = colss, bg = "white", cex = 0.9, bty = "n")

## Calculated/Measured data
xm <- max(max(weight_female), max(weight_male))
ym <- max(max(test_male$Weight..mg), max(test_female$Weight..mg))
plot(test_male$Weight..mg., weight_male, ylab = "Calculated mass (mg)", xlim = c(0, xm),
     ylim = c(0, ym), xlab = "Measured mass (mg)", las = 1, col = cols[2], pch = pchs[2])
points(test_female$Weight..mg., weight_female, col = cols[1], pch = pchs[1])
abline(lm(weight_all ~ test_all$Weight..mg.), lty = 2, lwd = 1, col = "black")
legend("topleft", legend = "(c)", bty = "n", cex = 1.2)

xm <- max(max(weight_idio), max(weight_koin))
ym <- max(max(test_idio$Weight..mg), max(test_koin$Weight..mg))
plot(test_idio$Weight..mg., weight_idio, ylab = "", xlim = c(0, xm),
     ylim = c(0, ym), xlab = "Measured mass (mg)", las = 1, col = colss[1], pch = pchs[1])
points(test_koin$Weight..mg., weight_koin, col = colss[2], pch = pchs[2])
abline(lm(weight_all ~ test_all$Weight..mg.), lty = 2, lwd = 1, col = "black")
legend("topleft", legend = "(d)", bty = "n", cex = 1.2)

dev.off()


# per Subfamily with more than 20 measurements
unique(as.character(paras_clean$Subfamily))

subs <- c("Campopleginae", "Cryptinae", "Ichneumoninae", "Pimplinae", "Orthocentrinae",
          "Banchinae", "Tryphoninae", "Mesochorinae", "Cremastinae")

regs <- lapply(subs, function(x) {
  sub <- paras_clean[as.character(paras_clean$Subfamily) == x, ]
  lm(log(sub$Weight..mg.) ~ log(sub$Wing.length..mm.))
})

regs


# plotting regresions
lwl <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs, "Wing.length..mm."])
lw <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs, "Weight..mg."])
cols <- c("#000000", "#A50F0F", "#0D2B94", "#2EB471", "#501C88", "#CC8D16",
          "#B721D5", "#22EA1E", "#795E20")

lwlo <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs[5], "Wing.length..mm."])
lwo <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs[5], "Weight..mg."])
lwlm <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs[8], "Wing.length..mm."])
lwm <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs[8], "Weight..mg."])


jpeg(filename = "Wing_length_Weight_rel_subfam1.jpg", width = 16.6, height = 8,
     units = "cm", res = 600)
par(mar = c(4, 4.5, 0, 0), mfrow = c(1, 2))

## all
par(cex = 0.7)
plot(lwl, lw, pch = 1, col = "grey78", ylab = "Natural logarithm of mass (mg)",
     xlab = "Natural logarithm of wing length (mm)", las = 1)
prs <- lapply(1:length(regs), function(x) {
  abline(regs[[x]], lty = 1, lwd = 1, col = cols[x])
})
legend("bottomright", subs, lty = 1, col = cols, bg = "white",
       bty = "n", cex = 0.7)
legend("topleft", legend = "(a)", bty = "n", cex = 1.2)

## by subfamily
plot(lwl, lw, pch = 1, col = "grey90", ylab = "",
     xlab = "Natural logarithm of wing length (mm)", las = 1)
points(lwlo, lwo, col = cols[5]); points(lwlm, lwm, col = cols[8])
prs <- lapply(c(5, 8), function(x) {
  abline(regs[[x]], lty = 1, lwd = 1, col = cols[x])
})
legend("bottomright", subs[c(5, 8)], pch = 1, lty = 1, col = cols[c(5, 8)], bg = "white",
       bty = "n", cex = 0.7)
legend("topleft", legend = "(b)", bty = "n", cex = 1.2)

dev.off()


# table for showing results from regressions
## from general regression
regt <- list(regre_train_all, regre_train_female, regre_train_male)
reg_tot <- lapply(regt, function(x) {
  sum <- summary(x)
  pp <- sum$coefficients[2, 4]
  pval <- ifelse(pp < 0.001, "< 0.001", as.character(round(pp, 3)))
  data.frame(Fstatistic = round(sum$fstatistic[1], 2), df = round(sum$df[2]),
             R2 = round(sum$adj.r.squared, 2), pvalue = pval,
             Residual_error = round(sum$sigma, 2),
             Equation_derived = paste0("lw = ", round(sum$coefficients[2, 1], 5),
                                       " x lwl - ", abs(round(sum$coefficients[1, 1], 5))))
})


reg_res <- lapply(regs, function(x) {
  sum <- summary(x)
  pp <- sum$coefficients[2, 4]
  pval <- ifelse(pp < 0.001, "< 0.001", as.character(round(pp, 3)))
  data.frame(Fstatistic = round(sum$fstatistic[1], 2), df = round(sum$df[2]),
             R2 = round(sum$adj.r.squared, 2), pvalue = pval,
             Residual_error = round(sum$sigma, 2),
             Equation_derived = paste0("lw = ", round(sum$coefficients[2, 1], 5),
                                       " x lwl - ", abs(round(sum$coefficients[1, 1], 5))))
  
})

regtable <- rbind(data.frame(Data = c("Combined data", "Female data", "Male data"),
                             do.call(rbind, reg_tot)),
                  data.frame(Data = subs, do.call(rbind, reg_res)))
colnames(regtable) <- c("Data", "F-statistic", "df", "R2", "p-value",
                        "Residual error", "Equation derived")

write.csv(regtable, "Summary_regressions.csv", row.names = FALSE)