x <- c(1:10)
y <- c(1:10)
df <- data.frame(x,y)


m <- lm(formula = y ~ x, data=df)
summary(m)
plot(df$x, df$y)
abline(m)

ptest<-data.frame(x=7)
predict(m,ptest, type="response")

x <- c(1,2,3,4,5)
y <- c(0.2, 2, 2.3, 7,4)

#