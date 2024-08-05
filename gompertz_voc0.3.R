# Importing necessary libraries


library(readxl)
library(nlme)
library(ggplot2)
library(boot)
library(dplyr)

# Read data from Excel
dataf <- read_excel("basevve.xlsx")

# Gompertz function
gompertz <- function(t, A, B, C) {
  A * exp(-exp(-B * (t - C)))
}

gompertz2 <- function(t, cat, A, B, C, coef_A, coef_B, coef_C) {
  (A + cat * coef_A) * exp(-exp(-(B + cat * coef_B) * (t - C))) + A * exp(-exp(-B * (t - (C + cat * coef_C))))
}

Gomp_second_der <- function(x, a, k, t) {
  (k * exp(-k * (x - t)) - k) * a * k * exp(-exp(-k * (x - t)) - k * (x - t))
}

# Nonlinear fit
fit_v <- nls(V ~ gompertz(Edad, A, B, C), data = dataf, start = list(A = 14, B = 0.09, C = 10))
fit_ve <- nls(VE ~ gompertz(Edad, A, B, C), data = dataf, start = list(A = 14, B = 0.09, C = 10))

# Fit results
print(summary(fit_v))
print(summary(fit_ve))

# Create dummy vectors
dummy_vector1 <- rep(1, length(dataf$V))
dummy_vector2 <- rep(0, length(dataf$VE))

# Create a new data frame
nuevo_df <- data.frame(
  Col1 = c((dataf$V / coef(fit_v)[1]), (dataf$VE / coef(fit_ve)[1])),
  Col2 = c(dummy_vector1, dummy_vector2),
  Col3 = c(dataf$Edad, dataf$Edad)
)
nuevo_df_v <- data.frame(
  Col1 = dataf$V / coef(fit_v)[1],
  Col2 = dataf$Edad
)
nuevo_df_ve <- data.frame(
  Col1 = dataf$VE / coef(fit_ve)[1],
  Col2 = dataf$Edad
)

print(head(nuevo_df))

# Fit the model with group differences
group_dif <- nls(Col1 ~ gompertz2(Col3, Col2, A, B, C, coef_A, coef_B, coef_C), data = nuevo_df, start = list(A = 14, B = 0.09, C = 10, coef_A = 0, coef_B = 0, coef_C = 0))

print(summary(group_dif))

# Extract residuals
residuos <- residuals(group_dif)
residuos_df <- data.frame(Residuos = residuos)

# Plot residuals
plot6 <- ggplot(residuos_df, aes(x = seq_along(Residuos), y = Residuos)) +
  geom_point() +
  labs(title = "Residuals of Gompertz Model", x = "Index", y = "Residuals") +
  theme_minimal()
ggsave("plot6.png", plot6, width = 6, height = 4, dpi = 300)

# # Bootstrap function
# statistic <- function(data, indices) {
#   d <- data[indices, ]
#   model <- nls(Col1 ~ gompertz(Col2, A, B, C), start = list(A = 1, B = 0.1, C = 7), data = d)
#   return(coef(model))
# }

# Define range of x-values for extrapolation
extrapolation_range <- seq(0, 100, by = 1)

# Plot V
plot1 <- ggplot() +
  geom_point(data = dataf, aes(x = Edad, y = V), color = "blue", alpha = 0.5, size = 1) +
  geom_line(aes(x = extrapolation_range, y = gompertz(extrapolation_range, coef(fit_v)[1], coef(fit_v)[2], coef(fit_v)[3])), color = "blue", linewidth = 1.5) +
  labs(x = "Age", y = "GV") +
  theme_light()
ggsave("plot1.png", plot1, width = 6, height = 4, dpi = 300)

# Plot VE
plot2 <- ggplot() +
  geom_point(data = dataf, aes(x = Edad, y = VE), color = "red", alpha = 0.5, size = 1) +
  geom_line(aes(x = extrapolation_range, y = gompertz(extrapolation_range, coef(fit_ve)[1], coef(fit_ve)[2], coef(fit_ve)[3])), color = "red", linewidth = 1.5) +
  labs(x = "Age", y = "EV") +
  theme_light()
ggsave("plot2.png", plot2, width = 6, height = 4, dpi = 300)

# Plot second derivative of V
plot3 <- ggplot() +
  geom_line(aes(x = extrapolation_range, y = Gomp_second_der(extrapolation_range, coef(fit_v)[1], coef(fit_v)[2], coef(fit_v)[3])), color = "blue", linewidth = 1.5) +
  labs(x = "Age", y = "a") +
  ggtitle("Second Derivative of Gompertz Fit for GV") +
  theme_light()
ggsave("plot3.png", plot3, width = 6, height = 4, dpi = 300)

# Plot second derivative of VE
plot4 <- ggplot() +
  geom_line(aes(x = extrapolation_range, y = Gomp_second_der(extrapolation_range, coef(fit_ve)[1], coef(fit_ve)[2], coef(fit_ve)[3])), color = "red", linewidth = 1.5) +
  labs(x = "Age", y = "a") +
  ggtitle("Second Derivative of Gompertz Fit for EV") +
  theme_light()
ggsave("plot4.png", plot4, width = 6, height = 4, dpi = 300)

# Plot normalized curves for V and VE
plot5 <- ggplot() +
  geom_line(aes(x = extrapolation_range, 
                y = (gompertz(extrapolation_range, coef(fit_v)[1], coef(fit_v)[2], coef(fit_v)[3])) / coef(fit_v)[1]), 
            color = "blue", linewidth = 1.5) +
  geom_line(aes(x = extrapolation_range, 
                y = (gompertz(extrapolation_range, coef(fit_ve)[1], coef(fit_ve)[2], coef(fit_ve)[3])) / coef(fit_ve)[1]), 
            color = "red", linewidth = 1.5) +
  labs(x = "Age", y = "Normalized Value") +
  ggtitle("Normalized Curve Fit for GV and VE") +
  theme_light() +
  annotate("text", x = 34, 
           y = 0.75, 
           label = "GV", color = "blue", hjust = 1) +
  annotate("text", x = 10, 
           y = 0.75, 
           label = "VE", color = "red", hjust = 1)

ggsave("plot5.png", plot5, width = 6, height = 4, dpi = 300)
# Find the time of minimum second derivative for V
posicion_min_segunda_der_v <- which.min(Gomp_second_der(extrapolation_range, coef(fit_v)[1], coef(fit_v)[2], coef(fit_v)[3]))
tiempo_min_segunda_der_v <- extrapolation_range[posicion_min_segunda_der_v]
print(paste("The time at which the minimum second derivative for V occurs is:", tiempo_min_segunda_der_v))

# Find the time of minimum second derivative for VE
posicion_min_segunda_der_ve <- which.min(Gomp_second_der(extrapolation_range, coef(fit_ve)[1], coef(fit_ve)[2], coef(fit_ve)[3]))
tiempo_min_segunda_der_ve <- extrapolation_range[posicion_min_segunda_der_ve]
print(paste("The time at which the minimum second derivative for VE occurs is:", tiempo_min_segunda_der_ve))





