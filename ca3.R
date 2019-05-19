df <- read.csv("ca3_dataset.csv")
str(df)
View(df)                                         
suraj <- subset(df, select = c(Statistical.Indicator, Dairy.products, Bakery.products))
View(suraj)
str(suraj)
suraj$Statistical.Indicator <- as.character(suraj$Statistical.Indicator)
View(suraj)
str(suraj)

normal_daru <- shapiro.test(suraj$Dairy.products)
normal_daru
normal_daru1 <- shapiro.test(suraj$Bakery.products)
normal_daru1


chisq.test(suraj$Dairy.products)
chisq.test(suraj$Bakery.products)


dhope1 <- mean(suraj$Dairy.products)
dhope1
dhope2 <- mean(suraj$Bakery.products)
dhope2

sd1 <- sd(suraj$Dairy.products)
sd1
sd2 <- sd(suraj$Bakery.products)
sd2
avg_sd = (sd1 + sd2)/2
avg_sd
delta1 <- (dhope1 - dhope2)/avg_sd
delta1

library(pwr)
power.t.test(delta = delta1, n = NULL, sig.level = 0.05, power = 0.90, type = "two.sample", alternative = "two.sided")
power_suraj <- pwr.t.test(d = delta1,
                          sig.level = 0.05,
                          power = 0.99,
                          type = "two.sample",
                          alternative = "two.sided")
power_suraj
plot(power_suraj)

cohen.ES(test = c("chisq"), size = c("small"))

res_suraj <- cor.test(suraj$Dairy.products, suraj$Bakery.products,
                      method = "spearman")
res_suraj
