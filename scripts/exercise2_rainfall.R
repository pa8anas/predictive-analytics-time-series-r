#2.a

data <- read.csv("C:/Users/pathana/Documents/predictive analytics/open-meteo-37.93N22.83E37m.csv", skip = 2)
head(data)


names(data)
head(data)

data$date <- as.Date(substr(data$time, 1, 10))

data$year <- format(data$date, "%Y")
data$month <- format(data$date, "%m")


monthly_rain <- aggregate(data[,"rain..mm."], 
                          by = list(year = data$year, month = data$month),
                          FUN = sum, na.rm = TRUE)
names(monthly_rain)[3] <- "rain_sum"

# Δημιουργία χρονοσειράς
ts_rain <- ts(monthly_rain$rain_sum, start = c(as.numeric(monthly_rain$year[1]), as.numeric(monthly_rain$month[1])), frequency = 12)
plot(ts_rain, main = "Μηνιαία Βροχόπτωση Ζευγολατιού Κορινθίας (2016 - 2025)", ylab = "mm", xlab = "Έτος")

#2.β
# Διάγραμμα αυτοσυσχέτισης (ACF)
acf(ts_rain, main = "ACF Μηνιαίας Βροχόπτωσης")


# Διάγραμμα μερικής αυτοσυσχέτισης (PACF)
pacf(ts_rain, main = "PACF Μηνιαίας Βροχόπτωσης")

# 2.γ: Πρώτες εποχικες διαφορές και ανάλυση στασιμότητας

# Υπολογισμός διαφορών
seas_diff <- diff(ts_rain, lag = 12)

# Γράφημα σειράς διαφορών
plot(seas_diff, 
     main = "Πρώτες Εποχικές Διαφορές (Lag=12)",
     ylab = "Διαφορά (mm)",
     xlab = "Έτος")
grid(col = "gray", lty = "dotted")

# ACF/PACF εποχικών διαφορών
par(mfrow = c(1, 2))
acf(seas_diff, 
    lag.max = 36,
    main = "ACF: Εποχικές Διαφορές",
    col = "darkblue")
pacf(seas_diff, 
     lag.max = 36,
     main = "PACF: Εποχικές Διαφορές",
     col = "darkred")
par(mfrow = c(1, 1))


# Υπολογισμός μέσου όρου και διακύμανσης ανά έτος
years <- unique(floor(time(seas_diff)))
means <- tapply(seas_diff, floor(time(seas_diff)), mean)
vars <- tapply(seas_diff, floor(time(seas_diff)), var)

# Έλεγχος σταθερότητας
cat("Μέσοι Όροι ανά Έτος:\n", round(means, 2), "\n")
cat("Διακυμάνσεις ανά Έτος:\n", round(vars, 2), "\n")

if (max(vars)/min(vars) < 4 && diff(range(means)) < 2*sd(seas_diff)) {
  cat("Στάσιμη Σειρά (σταθερός μέσος και διακύμανση)")
} else {
  cat("Μη Στάσιμη Σειρά")
}

#2.ε
# Διπλή διαφοροποίηση
double_diff <- diff(diff(ts_rain, lag = 12), differences = 1)

# Γράφημα
plot(double_diff, 
     main = "Διπλή Διαφοροποίηση (Πρώτες + Εποχικές Διαφορές)",
     ylab = "Διαφορά (mm)",
     xlab = "Έτος",
     col = "darkblue",
     lwd = 2)
grid(col = "gray", lty = "dotted")


par(mfrow = c(1, 2))

# ACF διπλών διαφορών
acf(double_diff, 
    lag.max = 36,
    main = "ACF: Διπλές Διαφορές",
    col = "darkred")

# PACF διπλών διαφορών
pacf(double_diff, 
     lag.max = 36,
     main = "PACF: Διπλές Διαφορές",
     col = "darkgreen")

par(mfrow = c(1, 1))

#2.στ
# Προσαρμογή SARIMA(2,1,1)(1,1,1)[12]
fit_sarima <- stats::arima(
  ts_rain,
  order = c(2, 1, 1),
  seasonal = list(
    order = c(1, 1, 1),
    period = 12
  )
)

# Εμφάνιση αποτελεσμάτων
cat("Παράμετροι Μοντέλου:\n")
print(fit_sarima$coef)

cat("\nΣτατιστικά:\n")
cat("AIC:", fit_sarima$aic, "\n")
cat("log-likelihood:", fit_sarima$loglik, "\n")


# Εξαγωγή καταλοίπων
residuals_sarima <- residuals(fit_sarima)

# 1. Δημιουργία ACF plot
acf_plot <- function(res, main = "ACF Καταλοίπων") {
  n <- length(res)
  acf_vals <- acf(res, plot = FALSE)$acf[-1]
  lags <- 1:length(acf_vals)
  
  plot(lags, acf_vals, type = "h", main = main, 
       ylim = c(-1, 1), xlab = "Lag", ylab = "ACF")
  abline(h = 0, col = "black")
  abline(h = c(-1.96, 1.96)/sqrt(n), col = "blue", lty = 2)
}

# 2. Δημιουργία PACF plot
pacf_plot <- function(res, main = "PACF Καταλοίπων") {
  n <- length(res)
  pacf_vals <- pacf(res, plot = FALSE)$acf
  lags <- 1:length(pacf_vals)
  
  plot(lags, pacf_vals, type = "h", main = main, 
       ylim = c(-1, 1), xlab = "Lag", ylab = "PACF")
  abline(h = 0, col = "black")
  abline(h = c(-1.96, 1.96)/sqrt(n), col = "blue", lty = 2)
}

# 3. Ιστόγραμμα και QQ-plot
par(mfrow = c(1, 3))
acf_plot(residuals_sarima)
pacf_plot(residuals_sarima)
hist(residuals_sarima, main = "Ιστόγραμμα Καταλοίπων", col = "lightblue")
par(mfrow = c(1, 1))

# 4. Ljung-Box test
ljung_box_test <- function(res, lag = 24) {
  n <- length(res)
  lb_test <- Box.test(res, lag = lag, type = "Ljung-Box")
  cat("Ljung-Box test (lag =", lag, "):\n")
  cat("X-squared =", lb_test$statistic, " p-value =", lb_test$p.value, "\n")
  if (lb_test$p.value > 0.05) {
    cat("Δεν υπάρχει αυτοσυσχέτιση (καλά καταλοιπα)\n")
  } else {
    cat("Υπάρχει αυτοσυσχέτιση (προβληματικά καταλοιπα)\n")
  }
}

ljung_box_test(residuals_sarima)

# Πρόβλεψη για 12 μήνες
future_forecast <- predict(fit_sarima, n.ahead = 12)

# Υπολογισμός χρονικών στιγμών πρόβλεψης
last_obs_time <- tail(time(ts_rain), 1)  # Χρονική στιγμή τελευταίας παρατήρησης
forecast_dates <- last_obs_time + (1:12)/12  # Προσθήκη 1/12 για κάθε νέο σημείο

# Οπτικοποίηση
plot(ts_rain, 
     main = "Πρόβλεψη Μηνιαίας Βροχόπτωσης",
     xlab = "Έτος",
     ylab = "Βροχόπτωση (mm)",
     xlim = c(start(ts_rain)[1], last_obs_time + 13/12),
     ylim = c(0, max(c(ts_rain, future_forecast$pred + 1.96*future_forecast$se), na.rm = TRUE)))

# Προσθήκη γραμμής που συνδέει το τελευταίο σημείο με την πρώτη πρόβλεψη
last_obs_value <- tail(ts_rain, 1)
lines(c(last_obs_time, forecast_dates[1]), 
      c(last_obs_value, future_forecast$pred[1]), 
      col = "red", lwd = 2)

# Προσθήκη προβλέψεων
lines(forecast_dates, future_forecast$pred, col = "red", lwd = 2)
lines(forecast_dates, future_forecast$pred + 1.96 * future_forecast$se, 
      col = "blue", lty = 2)
lines(forecast_dates, future_forecast$pred - 1.96 * future_forecast$se, 
      col = "blue", lty = 2)

legend("topleft",
       legend = c("Παρατηρήσεις", "Πρόβλεψη", "95% Διάστημα Εμπιστοσύνης"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 2),
       cex = 0.8)


#2.ζ
# Διαγνωστικά γραφήματα
residuals_sarima <- residuals(fit_sarima)

par(mfrow = c(2, 2))
plot(residuals_sarima, main = "Κατάλοιπα ανά Χρόνο", ylab = "Κατάλοιπα")
abline(h = 0, col = "red")
acf(residuals_sarima, main = "ACF Καταλοίπων", col = "darkblue")
pacf(residuals_sarima, main = "PACF Καταλοίπων", col = "darkred")
qqnorm(residuals_sarima, main = "Q-Q Plot")
qqline(residuals_sarima, col = "blue")
par(mfrow = c(1, 1))

# Ljung-Box test
Box.test(residuals_sarima, lag = 24, type = "Ljung-Box")




# Υποθέτουμε ότι το αρχικό σας μοντέλο είναι SARIMA(2,1,1)(1,1,1)12
original_model <- fit_sarima
original_aic <- original_model$aic

# 1. Διεύρυνση μη εποχιακού μέρους
# ---------------------------------
# Αύξηση AR(p) -> SARIMA(3,1,1)(1,1,1)12
model_ar_up <- arima(
  ts_rain,
  order = c(3, 1, 1),
  seasonal = list(order = c(1, 1, 1), period = 12)
)

# Αύξηση MA(q) -> SARIMA(2,1,2)(1,1,1)12
model_ma_up <- arima(
  ts_rain,
  order = c(2, 1, 2),
  seasonal = list(order = c(1, 1, 1), period = 12)
)

# 2. Διεύρυνση εποχιακού μέρους
# ---------------------------------
# Αύξηση SAR(P) -> SARIMA(2,1,1)(2,1,1)12
model_sar_up <- arima(
  ts_rain,
  order = c(2, 1, 1),
  seasonal = list(order = c(2, 1, 1), period = 12)
)

# Αύξηση SMA(Q) -> SARIMA(2,1,1)(1,1,2)12
model_sma_up <- arima(
  ts_rain,
  order = c(2, 1, 1),
  seasonal = list(order = c(1, 1, 2), period = 12)
)

# 3. Πλήρης διεύρυνση
# ---------------------------------
model_full_up <- arima(
  ts_rain,
  order = c(3, 1, 2),
  seasonal = list(order = c(2, 1, 2), period = 12)
)

# 4. Σύγκριση μοντέλων
# ---------------------------------
models <- list(
  "Original" = original_model,
  "AR+1" = model_ar_up,
  "MA+1" = model_ma_up,
  "SAR+1" = model_sar_up,
  "SMA+1" = model_sma_up,
  "Full+1" = model_full_up
)

# Συνάρτηση σύγκρισης μοντέλων
compare_models <- function(model_list) {
  results <- data.frame(
    Model = names(model_list),
    AIC = sapply(model_list, function(m) m$aic),
    LogLik = sapply(model_list, function(m) m$loglik),
    stringsAsFactors = FALSE
  )
  
  # Προσθήκη διαφοράς AIC από το αρχικό
  results$DeltaAIC <- results$AIC - original_aic
  results[order(results$AIC), ]
}

# Αποτελέσματα σύγκρισης
model_comparison <- compare_models(models)
print(model_comparison)

# Έλεγχος καταλοίπων για το βέλτιστο μοντέλο
check_residuals <- function(model) {
  res <- residuals(model)
  
  par(mfrow = c(1, 3))
  acf(res, main = "ACF Καταλοίπων", col = "darkblue")
  pacf(res, main = "PACF Καταλοίπων", col = "darkred")
  qqnorm(res, main = "Q-Q Plot")
  qqline(res, col = "blue")
  par(mfrow = c(1, 1))
  
  lb_test <- Box.test(res, lag = 24, type = "Ljung-Box")
  cat("Ljung-Box test p-value:", lb_test$p.value, "\n")
}

# Επιλογή βέλτιστου μοντέλου (το μικρότερο AIC)
best_model_name <- model_comparison$Model[which.min(model_comparison$AIC)]
best_model <- models[[best_model_name]]

# 1. Έλεγχος καταλοίπων βέλτιστου μοντέλου
cat("\nΈλεγχος Καταλοίπων για το Βέλτιστο Μοντέλο:", best_model_name, "\n")
check_residuals(best_model)

# 2. Τελικό συμπέρασμα
if (model_comparison$DeltaAIC[model_comparison$Model == best_model_name] < -2) {
  cat("\nΤο αρχικό μοντέλο ΔΕΝ είναι επαρκές. Το βέλτιστο μοντέλο είναι", best_model_name)
} else {
  cat("\nΤο αρχικό μοντέλο είναι επαρκές.")
}


#2.η

# Πρόβλεψη για 24 μήνες
forecast_24m <- predict(fit_sarima, n.ahead = 24)

# Υπολογισμός χρονικών στιγμών
last_obs_time <- end(ts_rain)
last_obs_value <- tail(ts_rain, 1)

# Χρονικές στιγμές για πρόβλεψη (από τον επόμενο μήνα)
forecast_times <- seq(last_obs_time[1] + (last_obs_time[2])/12 + 1/12,
                     by = 1/12,
                     length.out = 24)

# Οπτικοποίηση
plot(ts_rain, 
     main = "Πρόβλεψη Μηνιαίας Βροχόπτωσης (2 Έτη)",
     xlab = "Έτος", 
     ylab = "Βροχόπτωση (mm)",
     xlim = c(start(ts_rain)[1], last_obs_time[1] + 3),
     ylim = c(0, max(ts_rain, forecast_24m$pred + 1.96*forecast_24m$se, na.rm = TRUE)),
     lwd = 2)

# Προσθήκη γραμμής σύνδεσης (από τελευταίο σημείο δεδομένων στο πρώτο σημείο πρόβλεψης)
segments(x0 = last_obs_time[1] + (last_obs_time[2]-1)/12,
         y0 = last_obs_value,
         x1 = forecast_times[1],
         y1 = forecast_24m$pred[1],
         col = "red", lwd = 2)

# Προσθήκη προβλέψεων
lines(forecast_times, forecast_24m$pred, col = "red", lwd = 2)
lines(forecast_times, forecast_24m$pred + 1.96 * forecast_24m$se, 
      col = "blue", lty = 2)
lines(forecast_times, forecast_24m$pred - 1.96 * forecast_24m$se, 
      col = "blue", lty = 2)

# Ετικέτες
legend("topleft",
       legend = c("Ιστορικά Δεδομένα", "Πρόβλεψη SARIMA", "95% Διάστημα Εμπιστοσύνης"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 2),
       lwd = c(2, 2, 1),
       cex = 0.8)

# Προσθήκη κατακόρυφης γραμμής στο σημείο έναρξης πρόβλεψης
abline(v = forecast_times[1], col = "green", lty = 2)
text(x = forecast_times[1], y = max(ts_rain)*0.9, 
     "Έναρξη Πρόβλεψης", pos = 4, col = "darkgreen", cex = 0.8)


# Εξαγωγή εποχιακού μοτίβου πρόβλεψης
year1 <- forecast_24m$pred[1:12]
year2 <- forecast_24m$pred[13:24]

# Σύγκριση με ιστορικό εποχιακό μοτίβο
historical_seasonal <- aggregate(as.vector(ts_rain) ~ cycle(ts_rain), FUN = mean)

par(mfrow = c(1, 2))
plot(1:12, year1, type = "o", col = "red", 
     main = "Πρόβλεψη Έτος 1 vs Ιστορικό",
     xlab = "Μήνας", ylab = "Βροχόπτωση (mm)",
     ylim = range(historical_seasonal[,2], year1))
lines(1:12, historical_seasonal[,2], type = "o", col = "blue")
legend("topright", legend = c("Πρόβλεψη", "Ιστορικό"), col = c("red", "blue"), lty = 1)

plot(1:12, year2, type = "o", col = "red", 
     main = "Πρόβλεψη Έτος 2 vs Ιστορικό",
     xlab = "Μήνας", ylab = "Βροχόπτωση (mm)",
     ylim = range(historical_seasonal[,2], year2))
lines(1:12, historical_seasonal[,2], type = "o", col = "blue")
par(mfrow = c(1, 1))


# Υπολογισμός ετήσιου μέσου όρου
yearly_avg <- c(mean(year1), mean(year2))

# Υπολογισμός αβεβαιότητας
se_avg <- c(mean(forecast_24m$se[1:12]), mean(forecast_24m$se[13:24]))

# Γράφημα ετήσιας τάσης
plot(1:2, yearly_avg, type = "b", col = "red", 
     main = "Ετήσιος Μέσος Όρος Πρόβλεψης",
     xlab = "Έτος Πρόβλεψης", ylab = "Μέση Βροχόπτωση (mm)",
     ylim = c(min(yearly_avg - 1.96*se_avg), max(yearly_avg + 1.96*se_avg)))
arrows(x0 = 1:2, y0 = yearly_avg - 1.96*se_avg, 
       x1 = 1:2, y1 = yearly_avg + 1.96*se_avg,
       angle = 90, code = 3, length = 0.1)
abline(h = mean(ts_rain), col = "blue", lty = 2)
legend("topright", legend = c("Πρόβλεψη", "Ιστορικός Μέσος Όρος"), 
       col = c("red", "blue"), lty = c(1, 2))


# 1. Υπολογισμός διαφοράς από ιστορικό μοτίβο
diff_year1 <- year1 - historical_seasonal[,2]
diff_year2 <- year2 - historical_seasonal[,2]

# 2. Έλεγχος για στατιστικά σημαντικές διαφορές
t_test_year1 <- t.test(diff_year1)
t_test_year2 <- t.test(diff_year2)

# 3. Ανάλυση αυτοσυσχέτισης υπολοίπων πρόβλεψης
res_acf <- acf(forecast_24m$pred, main = "ACF Προβλέψεων")

# 4. Υπολογισμός εύρους διακύμανσης
pred_range <- apply(rbind(forecast_24m$pred - 1.96*forecast_24m$se, 
                          forecast_24m$pred + 1.96*forecast_24m$se), 
                    2, diff)

# Ποσοστιαία διαφορά από ιστορικό μέσο όρο
cat("Διαφορά Έτους 1:", round((mean(year1) - mean(ts_rain))/mean(ts_rain)*100, 1), "%\n")
cat("Διαφορά Έτους 2:", round((mean(year2) - mean(ts_rain))/mean(ts_rain)*100, 1), "%")


# Διαχωρισμός δεδομένων (τελευταία 2 έτη για έλεγχο)
test_size <- 24
train <- window(ts_rain, end = c(end(ts_rain)[1]-2, 12))
test <- window(ts_rain, start = c(end(ts_rain)[1]-1, 1))

# Προσαρμογή μοντέλου στα training data
fit_train <- arima(train, order = c(2,1,1), seasonal = list(order = c(1,1,1), period = 12))

# Πρόβλεψη για test period
test_forecast <- predict(fit_train, n.ahead = test_size)

# Υπολογισμός σφαλμάτων
errors <- test - test_forecast$pred
mae <- mean(abs(errors))
rmse <- sqrt(mean(errors^2))

# Οπτικοποίηση ακρίβειας
plot(test, main = "Πραγματικές vs Προβλεπόμενες Τιμές")
lines(ts(test_forecast$pred, start = start(test), frequency = 12), col = "red")
