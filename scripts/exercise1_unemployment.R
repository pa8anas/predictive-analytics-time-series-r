# 1.a
# Creation of a time series
unemployment <- c(13.6, 13.8, 13.8, 13.1, 12.5, 12.4, 12.4, 12.8, 13.0, 13.0, 13.0, 13.0,
                  13.6, 13.6, 13.0, 12.1, 11.6, 11.4, 11.7, 11.9, 12.1, 11.9, 11.7, 11.7,
                  12.3, 12.4, 12.2, 11.6, 11.2, 11.1, 11.2, 11.3, 11.1, 10.8, 10.5, 10.6,
                  11.2, 11.5, 11.5, 10.9)

# Creation of object time series
unemp_ts <- ts(unemployment, start = c(2022, 1), frequency = 12)

# 1. Timeline
plot(unemp_ts, 
     main = "Unemployment Rate in Spain (Jan. 2022 - Apr. 2025)",
     xlab = "Year",
     ylab = "Percentage (%)",
     col = "darkred",
     lwd = 2,
     ylim = c(10, 15))
grid(col = "gray", lty = "dotted")

# 2. Check of stationary series
# Calculation of first differences
diff_unemp <- diff(unemp_ts)

# Graph of first differences
plot(diff_unemp, 
     main = "First Differences (Stagnation Index)",
     xlab = "Year",
     ylab = "Difference",
     col = "darkblue",
     lwd = 2)
grid(col = "gray", lty = "dotted")
abline(h = 0, col = "red", lty = 2)

# 3. Seasonality analysis 
decomp <- decompose(unemp_ts)

# Graphical representation of decomposition
par(mfrow = c(4, 1), mar = c(3, 4, 2, 2))
plot(decomp$trend, main = "Trend", col = "darkgreen", lwd = 2)
plot(decomp$seasonal, main = "Seasonality", col = "darkblue", lwd = 2)
plot(decomp$random, main = "Random Element", col = "purple", lwd = 2)
plot(unemp_ts, main = "Real Series", col = "darkred", lwd = 2)
par(mfrow = c(1, 1))

# 4. Εποχικότητα - boxplot ανά μήνα
months <- factor(cycle(unemp_ts), 
                 levels = 1:12,
                 labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

boxplot(unemp_ts ~ months,
        main = "Seasonal Unemployment Pattern (By Month)",
        xlab = "Month",
        ylab = "Unemployment Percentage (%)",
        col = "lightblue",
        border = "darkblue")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

# 5. Autocorrelation (ACF) for seasonality
acf(unemp_ts, 
    main = "Autocorrelation Function (ACF)",
    lag.max = 36,
    col = "darkred",
    lwd = 2)
grid(col = "gray", lty = "dotted")

# 1.b
# First difference application
diff_unemp <- diff(unemp_ts)

# Graph of first differences
plot(diff_unemp, 
     main = "First Differences of Unemployment (Stationary Series)",
     xlab = "Year",
     ylab = "Difference",
     col = "darkblue",
     lwd = 2)
abline(h = 0, col = "red", lty = 2)
grid(col = "gray", lty = "dotted")

# Autocorrelation Functions (ACF) and Partial Autocorrelation Functions (PACF)
par(mfrow = c(1, 2))

# ACF
acf(diff_unemp, 
    lag.max = 24,
    main = "ACF: First Differences",
    col = "darkred",
    lwd = 2)

# PACF
pacf(diff_unemp, 
     lag.max = 24,
     main = "PACF: First Differences",
     col = "darkgreen",
     lwd = 2)
par(mfrow = c(1, 1))

# Προσαρμογή μοντέλων
arma21 <- arima(diff_unemp, order = c(2, 0, 1))  # ARMA(2,1) for the differences
arma11 <- arima(diff_unemp, order = c(1, 0, 1))  # ARMA(1,1) for the differences

# Summary of models
cat("Summary ARMA(2,1):\n")
print(summary(arma21))

cat("\nSummary ARMA(1,1):\n")
print(summary(arma11))

# Balance check
check_residuals <- function(model) {
  par(mfrow = c(1, 2))
  acf(residuals(model), main = "ACF Residuals", col = "purple")
  pacf(residuals(model), main = "PACF Residuals", col = "orange")
  par(mfrow = c(1, 1))
}

cat("\nBalance Check ARMA(2,1):\n")
check_residuals(arma21)

cat("\nBalance Check ARMA(1,1):\n")
check_residuals(arma11)

# Καλύτερο μοντέλο
best_model <- arma21

# Πρόβλεψη 6 μηνών
forecast_values <- predict(best_model, n.ahead = 6)

# Γραφική παράσταση
plot(diff_unemp, 
     main = "Πρόβλεψη Πρώτων Διαφορών (ARMA(2,1))",
     xlab = "Έτος",
     ylab = "Διαφορά",
     xlim = c(2022, 2025.5),
     col = "darkblue",
     lwd = 2)
lines(ts(forecast_values$pred, start = end(diff_unemp), frequency = 12), 
      col = "red", lwd = 2)
legend("topright", 
       legend = c("Πραγματικές", "Προβλέψεις"),
       col = c("darkblue", "red"),
       lwd = 2)

#1.d
# --- ΠΡΟΣΑΡΜΟΓΗ ARIMA(2,1,1) ---
arima_model <- arima(unemp_ts, order = c(2, 1, 1))

# --- ΑΝΑΛΥΣΗ ΑΠΟΤΕΛΕΣΜΑΤΩΝ ---
# Συντελεστές και p-values
coefs <- arima_model$coef
se <- sqrt(diag(arima_model$var.coef))
z_stats <- coefs / se
p_values <- 2 * (1 - pnorm(abs(z_stats)))
results <- data.frame(
  Estimate = coefs,
  Std.Error = se,
  z.value = z_stats,
  p.value = p_values
)
print(results)

# --- ΕΛΕΓΧΟΣ ΚΑΤΑΛΟΙΠΩΝ ---
par(mfrow = c(1, 2))
acf(resid(arima_model), main = "ACF Καταλοίπων")
pacf(resid(arima_model), main = "PACF Καταλοίπων")
par(mfrow = c(1, 1))
# Ljung-Box test (12 lags)
box_test <- Box.test(resid(arima_model), type = "Ljung-Box", lag = 12)
print(box_test)

# --- ΠΡΟΒΛΕΨΗ 12 ΜΗΝΕΣ ΜΠΡΟΣΤΑ ---
forecast_values <- predict(arima_model, n.ahead = 12)

# --- ΔΙΑΓΡΑΜΜΑ ---
plot(unemp_ts, 
     main = "Πρόβλεψη Ανεργίας (ARIMA(2,1,1))",
     xlim = c(2022, 2026),
     ylim = c(10, 14),
     ylab = "Ποσοστό Ανεργίας (%)",
     xlab = "Έτος")
# Προβλέψεις (κόκκινη γραμμή)
lines(ts(forecast_values$pred, start = end(unemp_ts) + c(0, 1), frequency = 12), col = "red", lwd = 2)
# Διαστήματα εμπιστοσύνης (μπλε διακεκομμένες)
lines(ts(forecast_values$pred + 1.96 * forecast_values$se, start = end(unemp_ts) + c(0, 1), frequency = 12), col = "blue", lty = 2)
lines(ts(forecast_values$pred - 1.96 * forecast_values$se, start = end(unemp_ts) + c(0, 1), frequency = 12), col = "blue", lty = 2)
# Υπόμνημα
legend("topright", 
       legend = c("Πραγματικά", "Πρόβλεψη", "95% Διάστημα"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 2),
       lwd = 2)

# --- Επιστροφή σε μονοπλότ ---
par(mfrow = c(1, 1))

#1.ζ

# --- Αρχικό μοντέλο (ARIMA(2,1,1)) ---
arima_211 <- arima(unemp_ts, order = c(2, 1, 1))
aic_211 <- AIC(arima_211)
lb_211 <- Box.test(resid(arima_211), type = "Ljung-Box", lag = 12)$p.value

# --- Διευρυμένα μοντέλα ---
arima_311 <- arima(unemp_ts, order = c(3, 1, 1))
aic_311 <- AIC(arima_311)
lb_311 <- Box.test(resid(arima_311), type = "Ljung-Box", lag = 12)$p.value

arima_212 <- arima(unemp_ts, order = c(2, 1, 2))
aic_212 <- AIC(arima_212)
lb_212 <- Box.test(resid(arima_212), type = "Ljung-Box", lag = 12)$p.value

arima_221 <- arima(unemp_ts, order = c(2, 2, 1))
aic_221 <- AIC(arima_221)
lb_221 <- Box.test(resid(arima_221), type = "Ljung-Box", lag = 12)$p.value

# --- Συγκεντρωτικός πίνακας ---
models <- c("ARIMA(2,1,1)", "ARIMA(3,1,1)", "ARIMA(2,1,2)", "ARIMA(2,1,2)")
aic_values <- c(aic_211, aic_311, aic_212, aic_221)
lb_pvalues <- c(lb_211, lb_311, lb_212, lb_221)
summary_table <- data.frame(Model = models, AIC = aic_values, LjungBox_p = lb_pvalues)
print(summary_table)

# --- Έλεγχος καταλοίπων του καλύτερου μοντέλου (με το μικρότερο AIC) ---
best_model_index <- which.min(aic_values)
best_model_name <- models[best_model_index]
cat("Καλύτερο μοντέλο:", best_model_name, "\n")
if(best_model_index == 1) best_model <- arima_211
if(best_model_index == 2) best_model <- arima_311
if(best_model_index == 3) best_model <- arima_212
if(best_model_index == 4) best_model <- arima_221

# Διάγραμμα υπολοίπων
par(mfrow = c(1, 2))
acf(resid(best_model), main = paste("ACF Καταλοίπων", best_model_name))
pacf(resid(best_model), main = paste("PACF Καταλοίπων", best_model_name))
par(mfrow = c(1, 1))

#1.η

# Πρόβλεψη 24 μηνών μπροστά με το τελικό μοντέλο
forecast_values <- predict(arima_311, n.ahead = 24)

# Προσάρμοσε το plotting στις ανάγκες σου:
plot(unemp_ts,
     xlim = c(2022, 2027),
     ylim = c(min(unemp_ts, forecast_values$pred - 1.96*forecast_values$se), max(unemp_ts, forecast_values$pred + 1.96*forecast_values$se)),
     main = "Πρόβλεψη ποσοστού ανεργίας (24 μήνες μπροστά)",
     ylab = "Ποσοστό ανεργίας (%)",
     xlab = "Έτος")
lines(ts(forecast_values$pred, start = end(unemp_ts) + c(0, 1), frequency = 12), col = "red", lwd = 2)
lines(ts(forecast_values$pred + 1.96 * forecast_values$se, start = end(unemp_ts) + c(0, 1), frequency = 12), col = "blue", lty = 2)
lines(ts(forecast_values$pred - 1.96 * forecast_values$se, start = end(unemp_ts) + c(0, 1), frequency = 12), col = "blue", lty = 2)
legend("topright",
       legend = c("Ιστορικά", "Πρόβλεψη", "95% Διάστημα"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 2), lwd = 2)

