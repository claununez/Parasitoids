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
n_test <- apply(paras_clean[, c("Wing.length.(mm)", "Weight.(mg)")], 2,
                shapiro.test)

# t-test between countries
Ecuador <- paras_clean[paras_clean$Country == 1, ]
Venezuela <- paras_clean[paras_clean$Country == 2, ]
WL_t.test <- t.test(Ecuador$Wing.length.(mm),
                    Venezuela$Wing.length.(mm), alternative = "less")

WE_t.test <- t.test(Ecuador$Weight.(mg), Venezuela$Weight.(mg),
                    alternative = "less")

# t-test between sex
Female <- paras_clean[paras_clean$Sex == 1, ]
Male <- paras_clean[paras_clean$Sex == 2, ]
WL_Sex_t.test <- t.test(Female$Wing.length.(mm), Male$Wing.length.(mm),
                        alternative = "less")

WE_Sex_t.test <- t.test(Female$Weight.(mg), Male$Weight.(mg),
                        alternative = "less")

# t-test between strategy
Idio <- paras_clean[paras_clean$Strategy == "idiobiont", ]
Koin <- paras_clean[paras_clean$Strategy == "koinobiont", ]
WL_Str_t.test <- t.test(Idio$Wing.length.(mm), Koin$Wing.length.(mm),
                        alternative = "less")

WE_Str_t.test <- t.test(Idio$Weight.(mg), Koin$Weight.(mg),
                        alternative = "less")


# Boxplot representation
par(cex = 0.73, mar = c(4, 4, 0, 0), mfrow = c(2, 3))

boxplot(paras_clean$Weight.(mg) ~ paras_clean$Country, xlab = "",
        ylab = "Mass (mg)", las = 1, names = c("Ecuador", "Venezuela") )
boxplot(paras_clean$Weight.(mg)~paras_clean$Sex, xlab = "",
        ylab = "", las = 1, names = c("Female","Male"))
boxplot(paras_clean$Weight.(mg)~paras_clean$Strategy, xlab = "",
        ylab = "", las = 1, names = c("Koinobiont", "Idiobiont"))

boxplot(paras_clean$Wing.length.(mm) ~ paras_clean$Country, xlab = "Country",
        ylab = "Wing length (mm)", las = 1, names = c("Ecuador", "Venezuela") )
boxplot(paras_clean$Wing.length.(mm)~paras_clean$Sex, xlab = "Sex",
        ylab = "", las = 1,names = c("Female", "Male") )
boxplot(paras_clean$Wing.length.(mm)~paras_clean$Strategy, xlab = "Strategy",
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
boxplot(paras_clean$Weight.(mg) ~ paras_clean$Country, xlab = "",
        ylab = "Mass (mg)", las = 1, names = c("Ecuador", "Venezuela") )
legend("topright", legend = "(b)", bty = "n", cex = 1.2)
boxplot(paras_clean$Weight.(mg)~paras_clean$Sex, xlab = "",
        ylab = "", las = 1, names = c("Female","Male"))
legend("topright", legend = "(c)", bty = "n", cex = 1.2)
boxplot(paras_clean$Weight.(mg)~paras_clean$Strategy, xlab = "",
        ylab = "", las = 1, names = c("Koinobiont", "Idiobiont"))
legend("topright", legend = "(d)", bty = "n", cex = 1.2)

boxplot(paras_clean$Wing.length.(mm) ~ paras_clean$Country, xlab = "Country",
        ylab = "Wing length (mm)", las = 1, names = c("Ecuador", "Venezuela") )
legend("topright", legend = "(e)", bty = "n", cex = 1.2)
boxplot(paras_clean$Wing.length.(mm)~paras_clean$Sex, xlab = "Sex",
        ylab = "", las = 1,names = c("Female", "Male") )
legend("topright", legend = "(f)", bty = "n", cex = 1.2)
boxplot(paras_clean$Wing.length.(mm)~paras_clean$Strategy, xlab = "Strategy",
        ylab = "", las = 1, names = c("Koinobiont", "Idiobiont"))
legend("topright", legend = "(g)", bty = "n", cex = 1.2)
dev.off()


# Dividing the data in training and testing
set.seed(1)
train_all <- paras_clean[sample(1:dim(paras_clean)[1], dim(paras_clean) * 0.75), ]
train_female <- Female[sample(1:dim(Female)[1], dim(Female) * 0.75), ]
train_male <- Male[sample(1:dim(Male)[1], dim(Male) * 0.75), ]

test_all <- paras_clean[!row.names(paras_clean) %in% row.names(train_all), ]
test_female <- Female[!row.names(Female) %in% row.names(train_female), ]
test_male<- Male[!row.names(Male) %in% row.names(train_male), ]


# Regressions
## training
regre_train_all <- lm(log(train_all$Weight.(mg)) ~
                              log(train_all$Wing.length.(mm)))
summary(regre_train_all)
regre_train_female <- lm(log(train_female$Weight.(mg)) ~
                                 log(train_female$Wing.length.(mm)))
summary(regre_train_female)
regre_train_male <- lm(log(train_male$Weight.(mg)) ~
                               log(train_male$Wing.length.(mm)))
summary(regre_train_male)

## testing
### calling function
source("https://raw.githubusercontent.com/claununez/Parasitoids/master/R_scripts/ichneumonidae_wwl.R")

### calculating weights with testing data and the function
weight_all <- ichneumonidae_wwl(wing_length = test_all$Wing.length.(mm),
                                units_wing_length = "mm")
weight_female <- ichneumonidae_wwl(wing_length = test_female$Wing.length.(mm),
                                   units_wing_length = "mm", sex = "f")
weight_male <- ichneumonidae_wwl(wing_length = test_male$Wing.length.(mm),
                                 units_wing_length = "mm", sex = "m")

### correlation between calculated and measured testing weights
weight_cor_test <- cor.test(weight_all, test_all$Weight.(mg))
weight_cor_test_female <- cor.test(weight_female, test_female$Weight.(mg))
weight_cor_test_male <- cor.test(weight_male, test_male$Weight.(mg))


# Plotting regressions
jpeg(filename = "Wing_length_Weight_rel_summary1.jpg", width = 16.6, height = 8,
     units = "cm", res = 600)
par(mar = c(4, 4.5, 0, 0), mfrow = c(1, 2))

## log Weight/Wing lengths
par(cex = 0.7)
plot(log(train_all$Wing.length.(mm)), log(train_all$Weight.(mg)),
     pch = pchs[sex], col = cols[sex],
     ylab = "Natural logarithm of mass (mg)",
     xlab = "Natural logarithm of wing length (mm)", las = 1)
abline(lm(log(train_all$Weight.(mg)) ~
                  log(train_all$Wing.length.(mm)) ), lty = 1, lwd = 1, col = "black")
legend("topleft", legend = "(a)", bty = "n", cex = 1.2)

## Calculated/Measured data
plot(test_all$Weight.(mg), weight_all, ylab = "Calculated mass (mg)",
     xlab = "Measured mass (mg)", las = 1)
abline(lm(weight_all ~ test_all$Weight.(mg)), lty = 2, lwd = 1, col = "black")
legend("topleft", legend = "(b)", bty = "n", cex = 1.2)

dev.off()


# per Subfamily with more than 20 measurements
unique(as.character(paras_clean$Subfamily))

subs <- c("Campopleginae", "Cryptinae", "Ichneumoninae", "Pimplinae", "Orthocentrinae",
          "Banchinae", "Tryphoninae", "Mesochorinae", "Cremastinae")

regs <- lapply(subs, function(x) {
  sub <- paras_clean[as.character(paras_clean$Subfamily) == x, ]
  lm(log(sub$Weight.(mg)) ~ log(sub$Wing.length.(mm)))
})

regs


# plotting regresions
lwl <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs, "Wing.length.(mm)"])
lw <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs, "Weight.(mg)"])
cols <- c("#000000", "#A50F0F", "#0D2B94", "#2EB471", "#501C88", "#CC8D16",
          "#B721D5", "#22EA1E", "#795E20")

lwlo <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs[5], "Wing.length.(mm)"])
lwo <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs[5], "Weight.(mg)"])
lwlm <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs[8], "Wing.length.(mm)"])
lwm <- log(paras_clean[as.character(paras_clean$Subfamily) %in% subs[8], "Weight.(mg)"])


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