data(iris)
fit <- lm(Petal.Width ~ Petal.Length, data=iris)
class(fit)

methods(class=class(fit))
summary(fit)

coefficients(fit) # model coefficients
predict(fit) # fitted predictions
predict(fit, newdata=data.frame(Petal.Length=seq(1, 2, by=0.1)))
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
influence(fit) # regression diagnostics

par(mfrow=c(2,2))
plot(fit)
#Multiple Regression
fit2 <- lm(Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width, data=iris)
summary(fit2) # show results

anova(fit, fit2)

fit2int <- lm(Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width + Petal.Length:Sepal.Length, data=iris)
anova(fit2, fit2int)

fit3 <- lm(Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width + Species, data=iris)
summary(fit3)

anova(fit2, fit3)

library('xtable')
print(xtable(fit3), type="html")

print(anova(fit3))

iris2 <- iris

iris2$virginica <- iris$Species == "virginica"
fit4 <- glm(virginica ~ Petal.Width + Petal.Length + Sepal.Length + Sepal.Width, data=iris2, family=binomial)
plot(fit4)

plot(Petal.Width ~ Petal.Length, col=c("black", "red", "blue")[Species], pch=(15:17)[Species], xlab="Petal Length (cm)", ylab="Petal Width (cm)", data=iris)
newx <- data.frame(Petal.Length=seq(min(iris$Petal.Length), max(iris$Petal.Length), length.out=100))
conf.interval <- predict(fit, newdata=newx, interval="confidence")
pred.interval <- predict(fit, newdata=newx, interval="prediction")
lines(conf.interval[, "fit"] ~ newx[, 1], lty=1, lw=3)
lines(conf.interval[, "lwr"] ~ newx[, 1], lty=2)
lines(conf.interval[, "upr"] ~ newx[, 1], lty=2)
lines(pred.interval[, "lwr"] ~ newx[, 1], lty=3)
lines(pred.interval[, "upr"] ~ newx[, 1], lty=3)
legend("topleft", legend=c(levels(iris$Species), "CI", "PI"), col=c("black", "red", "blue", "black", "black"), pch=c(15:17, -1, -1), lty=c(-1, -1, -1, 2, 3))