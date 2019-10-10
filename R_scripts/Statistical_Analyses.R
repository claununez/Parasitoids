###############
# Project: Body weight to estimate forewing in Ichneumonidae
# Authors: Marina Mazon, Claudia Nuñez-Penichet, Marlon Cobos 
# Process: Statistical analyses
# Date: 10/10/2019
################

# Directory
setwd("C:/D/Courses/R_class/Final_Project/Final_presentation")

# Read data
paras <- read.csv("tabla_parasitoides_corr2.csv", header = T, dec = ".")

# Exploring the data looking for outliers
paras$simb1 <- paras$Sex
paras$simb1[paras$simb1 == 1] <- 1
paras$simb1[paras$simb1 == 2] <- 19
paras$col <- paras$Sex
paras$col[paras$col == 1] <- "blue"
paras$col[paras$col == 2] <- "red"

# Plotting and exporting figure
jpeg(filename = "Weight.Wings_lengths.jpg",
     width = 22, height = 17, units = "cm", res = 600)

par(cex = 1, mar = c(4, 4, 1.3, 0.5))
plot(paras$Weight.mg, paras$Length_of_the_wins.mm, pch = paras$simb1,
     col = paras$col, xlab = "Weight (mg)", ylab = "Wings lengths (mm)", las = 1)
legend(58, 4, c("Female", "Male"),
       pch = c(1,19), col = c("blue", "red"), bg = "white", bty = "n")

dev.off()

# Cleaning the data
paras1 <- paras[paras$Weight.mg > 30 & 
                                paras$Length_of_the_wins.mm < 16, ]
paras_clean <- paras[!rownames(paras) %in% rownames(paras1), ]

# Normality test (Shapiro-Wilk's test) 
n_test <- apply(paras_clean[, c("Length_of_the_wins.mm", "Weight.mg")], 2, 
                shapiro.test)

# t-test between countries
Ecuador <- paras_clean[paras_clean$Country == 1, ]
Venezuela <- paras_clean[paras_clean$Country == 2, ]
WL_t.test <- t.test(Ecuador$Length_of_the_wins.mm, 
                    Venezuela$Length_of_the_wins.mm, alternative = "less")

WE_t.test <- t.test(Ecuador$Weight.mg, Venezuela$Weight.mg, 
                    alternative = "less")

# t-test between sex
Female <- paras_clean[paras_clean$Sex == 1, ]
Male <- paras_clean[paras_clean$Sex == 2, ]
WL_Sex_t.test <- t.test(Female$Length_of_the_wins.mm, Male$Length_of_the_wins.mm, 
                    alternative = "less")

WE_Sex_t.test <- t.test(Female$Weight.mg, Male$Weight.mg, 
                    alternative = "less")

# Boxplot representation
jpeg(filename = "Wings_lengths.Country.jpg", width = 8, height = 10, 
     units = "cm", res = 600)
par(cex = 0.73, mar = c(4, 4, 1.3, 0.5) )
boxplot(paras_clean$Length_of_the_wins.mm ~ paras_clean$Country, xlab = "Country",
        ylab = "Wings lengths (mm)", las = 1, names = c("Ecuador", "Venezuela") ) 
legend("topright", legend = "p > 0.05" )
dev.off()

jpeg(filename = "Weight.Country.jpg", width = 8, height = 10, units = "cm", 
     res = 600)
par(cex = 0.73, mar = c(4, 4, 1.3, 0.5) )
boxplot(paras_clean$Weight.mg ~ paras_clean$Country, xlab = "Country",
        ylab = "Weight (mg)", las = 1, names = c("Ecuador", "Venezuela") )  
legend("topright", legend = "p > 0.05" )
dev.off()

jpeg(filename = "Wings.lengths.Sex.jpg", width = 8, height = 10, units = "cm", 
     res = 600)
par(cex = 0.73, mar = c(4, 4, 1.3, 0.5) )
boxplot(paras$Length_of_the_wins.mm~paras$Sex, xlab = "Sex",
        ylab = "Wings lengths (mm)", las = 1,names = c("Female", "Male") )
legend("topright", legend = "p > 0.05" ) 
dev.off()

jpeg(filename = "Weight.Sex.jpg", width = 8, height = 10, units = "cm", 
     res = 600)
par(cex = 0.73, mar = c(4, 4, 1.3, 0.5) )
boxplot(paras$Weight.mg~paras$Sex, xlab = "Sex",
        ylab = "Weight (mg)", las = 1, names = c("Female","Male") ) 
legend("topright", legend = "p > 0.05" )
dev.off()

# Testing correlation
## raw data
cor.test(paras_clean$Weight.mg, paras_clean$Length_of_the_wins.mm)

## log transformed data
cor.test(log(paras_clean$Weight.mg), log(paras_clean$Length_of_the_wins.mm))

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
regre_train_all <- lm(log(train_all$Weight.mg) ~ 
                        log(train_all$Length_of_the_wins.mm))
summary(regre_train_all)
regre_train_female <- lm(log(train_female$Weight.mg) ~ 
                           log(train_female$Length_of_the_wins.mm))
summary(regre_train_female)
regre_train_male <- lm(log(train_male$Weight.mg) ~ 
                         log(train_male$Length_of_the_wins.mm))
summary(regre_train_male)

## testing
### calling function
source("https://raw.githubusercontent.com/claununez/Parasitoids/master/R_scripts/ichneumonidae_wwl.R")

### calculating weights with testing data and the function
weight_all <- ichneumonidae_wwl(wing_length = test_all$Length_of_the_wins.mm, 
                                units_wing_length = "mm")
weight_female <- ichneumonidae_wwl(wing_length = test_female$Length_of_the_wins.mm, 
                                   units_wing_length = "mm", sex = "f")
weight_male <- ichneumonidae_wwl(wing_length = test_male$Length_of_the_wins.mm, 
                                 units_wing_length = "mm", sex = "m")

### correlation between calculated and measured testing weights
weight_t_test <- cor.test(weight_all, test_all$Weight.mg)
weight_t_test_female <- cor.test(weight_female, test_female$Weight.mg)
weight_t_test_male <- cor.test(weight_male, test_male$Weight.mg)

# Plotting regressions 
## Weight/Wing lengths
jpeg(filename = "Wings.lengths.Weight1.jpg", width = 8, height = 7, units = "cm",
     res = 600)
par(cex = 0.7, mar = c(4, 4, 1.3, 0.5) )
plot(train_all$Length_of_the_wins.mm, train_all$Weight.mg,
     pch = paras$simb1, col = paras$col, cex = 0.8,
     ylab = "Weight (mg)", xlab = "Wings lengths (mm)",las = 1)
legend("bottomright", c("Female", "Male", "All"), pch = c(1, 16, NA), 
       lty = c(2, 1, 1), col = c("blue", "red", "black"), 
       bg = "white", bty = "n", cex = 0.9) 
abline(lm(train_female$Weight.mg ~
            train_female$Length_of_the_wins.mm ), lty = 2, lwd = 1, col = "blue")
abline(lm(train_male$Weight.mg ~ train_male$Length_of_the_wins.mm ), lty = 1, 
       lwd = 1, col = "red")
abline(lm(train_all$Weight.mg ~
            train_all$Length_of_the_wins.mm ), lty = 1, lwd = 1, col = "black")
dev.off()

## log Weight/Wing lengths
jpeg(filename = "Wings.lengths.log.Weight1.jpg", width = 8, height = 7, 
     units = "cm", res = 600)
par(cex = 0.7, mar = c(4, 4, 1.3, 0.5) )
plot(log(train_all$Length_of_the_wins.mm), log(train_all$Weight.mg),
     pch = paras$simb1, col = paras$col, cex = 0.8,
     ylab = "Natural logarithm of Weight (mg)", 
     xlab = "Natural logarithm of Wings lengths (mm)",las = 1)
abline(lm(log(train_female$Weight.mg) ~
            log(train_female$Length_of_the_wins.mm) ), lty = 2, lwd = 1, col = "blue")
abline(lm(log(train_male$Weight.mg) ~ log(train_male$Length_of_the_wins.mm) ), lty = 1, 
       lwd = 1, col = "red")
abline(lm(log(train_all$Weight.mg) ~
            log(train_all$Length_of_the_wins.mm) ), lty = 1, lwd = 1, col = "black")
dev.off()

## Calculated/Measured data
jpeg(filename = "Testing data2.jpg", width = 8, height = 7, units = "cm", 
     res = 600)
par(cex = 0.7, mar = c(4, 4, 1.3, 0.5) )
plot(test_all$Weight.mg, weight_all, cex = 0.8, ylab = "Calculated weights (mg)", 
     xlab = "Measured weights (mg)",las = 1) 
legend("bottomright", "Testing data", pch = 1, lty = 2, col = "black", 
       bg = "white", bty = "n", cex = 0.9)
abline(lm(weight_all ~ test_all$Weight.mg), lty = 2, lwd = 1, col = "black")
dev.off()