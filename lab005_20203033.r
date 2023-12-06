data <- read.table("data.txt", header=FALSE)
colnames(data) <- c("V1", "V2", "V3")

model <- lm(V3 ~ V2, data)

row <- nrow(data)
print(row)

alpha <- as.numeric(round(coef(model)[1],3))
beta <- as.numeric(round(coef(model)[2],3))
print(c(alpha,beta))

summary_model <- summary(model)
p_value_alpha <- round(summary_model$coefficients["(Intercept)", "Pr(>|t|)"],3)
p_value_beta <- round(summary_model$coefficients["V2", "Pr(>|t|)"],3)
print(c(p_value_alpha,p_value_beta))

r_squared <- round(summary_model$r.squared,3)
print(r_squared)

new_data <- data.frame(V2 = 70)
confidence_interval <- round(predict(model, newdata = new_data, interval = "confidence", level = 0.95),3)
print(confidence_interval)

prediction_interval <- round(predict(model, newdata = new_data, interval = "prediction", level = 0.95),3)
print(prediction_interval)


png("res.bmp")
plot(resid(model), xlab="V2", ylab="Standardized Residual", main="Standardized Residuals")
dev.off()