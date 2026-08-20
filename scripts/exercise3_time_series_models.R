#3.1


library(astsa)

# Δημιούργησε τη χρονοσειρά
ts_cmort <- ts(cmort, frequency = 52)

# Προσαρμογή AR(2) μέσω arima
ar2_fit <- arima(ts_cmort, order = c(2,0,0))

# Πρόβλεψη 4 εβδομάδων με κανονική διαδικασία
forecast_horizon <- 4
forecast <- predict(ar2_fit, n.ahead = forecast_horizon)

# Δημιουργία διαστήματος εμπιστοσύνης 95%
upper_bound <- forecast$pred + 1.96 * forecast$se
lower_bound <- forecast$pred - 1.96 * forecast$se

# Σχεδίαση αποτελεσμάτων
n <- length(ts_cmort)
weeks <- (n - 30):(n + forecast_horizon)

plot(weeks[1:31], ts_cmort[(n-30):n], type = "l", lwd = 2,
     ylim = range(ts_cmort[(n-30):n], upper_bound, lower_bound),
     xlim = range(weeks),
     main = "Πρόβλεψη Θνησιμότητας AR(2)",
     xlab = "Εβδομάδα", ylab = "Θνησιμότητα")

# Πρόσθεσε τις προβλέψεις με σωστά διαστήματα
forecast_weeks <- (n + 1):(n + forecast_horizon)
lines(forecast_weeks, forecast$pred, col = "red", lwd = 2, type = "b", pch = 16)
lines(forecast_weeks, upper_bound, col = "blue", lty = 2, lwd = 2)
lines(forecast_weeks, lower_bound, col = "blue", lty = 2, lwd = 2)

legend("topright",
       legend = c("Ιστορικά", "Πρόβλεψη AR(2)", "95% Διάστημα Εμπιστοσύνης"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 2),
       pch = c(NA, 16, NA),
       lwd = c(2,2,2), bty = "n")


# 1. Κατάλοιπα και διαγνωστικά
resid_ar2 <- residuals(ar2_fit)

par(mfrow = c(2,3))
plot(resid_ar2, type = "l", main = "Κατάλοιπα AR(2)", ylab = "Κατάλοιπα")
abline(h = 0, col = "red", lty = 2)
acf(resid_ar2, main = "ACF Καταλοίπων")
pacf(resid_ar2, main = "PACF Καταλοίπων")
hist(resid_ar2, breaks = 20, col = "lightblue", main = "Ιστόγραμμα Καταλοίπων")
qqnorm(resid_ar2); qqline(resid_ar2, col = "red")
plot(weeks[1:31], ts_cmort[(n-30):n], type = "l", lwd = 2, main = "Τελευταίες Τιμές", xlab = "Εβδομάδα", ylab = "Θνησιμότητα")
par(mfrow = c(1,1))

# 2. Ljung-Box test
lb <- Box.test(resid_ar2, lag = 10, type = "Ljung-Box")
cat("Ljung-Box test p-value:", lb$p.value, "\n")

# 3. Συντελεστές, AIC
cat("Συντελεστές:\n"); print(ar2_fit$coef)
cat("AIC:", ar2_fit$aic, "\n")




#3.2

library(astsa)
data(gtemp_land)

# Δημιουργία χρονοσειράς με σωστά χρονικά σημάδια
years <- 1850:2023
x_ts <- ts(gtemp_land, start = 1850, frequency = 1)

# Προσαρμογή ARIMA(1,1,1)
model <- arima(x_ts, order = c(1,1,1))

# Πρόβλεψη για 10 έτη (2024-2033)
n_forecast <- 10
forecast <- predict(model, n.ahead = n_forecast)

# Υπολογισμός διαστημάτων εμπιστοσύνης
z <- qnorm(0.975)
upper <- forecast$pred + z * forecast$se
lower <- forecast$pred - z * forecast$se

# Χρονικές σημάνσεις προβλέψεων
last_year <- end(x_ts)[1]
forecast_years <- (last_year + 1):(last_year + n_forecast)

# Σημεία σύνδεσης (τελευταία ιστορική τιμή)
last_obs <- tail(x_ts, 1)
forecast_years <- c(last_year, forecast_years)
forecast_pred <- c(last_obs, forecast$pred)
upper <- c(last_obs, upper)
lower <- c(last_obs, lower)

# Γράφημα
plot(window(x_ts, start = 2000), 
     type = "o", pch = 16, col = "blue",
     xlim = c(2000, 2033), 
     ylim = range(c(x_ts, lower, upper), na.rm = TRUE),
     main = "Πρόβλεψη Παγκόσμιων Θερμοκρασιών Ξηράς",
     xlab = "Έτος", ylab = "Απόκλιση (ºC)")

# Προσθήκη προβλέψεων
lines(forecast_years, forecast_pred, type = "o", col = "red", pch = 18, lty = 2)
lines(forecast_years, upper, col = "darkgreen", lty = 3)
lines(forecast_years, lower, col = "darkgreen", lty = 3)

# Γέμισμα διαστήματος εμπιστοσύνης
polygon(c(forecast_years, rev(forecast_years)), 
        c(upper, rev(lower)), 
        col = rgb(0, 0.5, 0, 0.2), border = NA)

legend("topleft", 
       legend = c("Ιστορικά", "Πρόβλεψη", "95% CI"),
       col = c("blue", "red", "darkgreen"),
       lty = c(1, 2, 1), pch = c(16, 18, NA),
       bg = "white")



library(astsa)
data(gtemp_land)

# 1. Δημιουργία χρονοσειράς
years <- 1850:2023
x_ts <- ts(gtemp_land, start = 1850, frequency = 1)

# 2. Διάγραμμα της αρχικής σειράς
plot(x_ts, main = "Παγκόσμια Θερμοκρασία Ξηράς (1850-2023)", ylab = "Απόκλιση (°C)", xlab = "Έτος")

# 3. Διάγραμμα αυτοσυσχέτισης (ACF) και μερικής αυτοσυσχέτισης (PACF)
par(mfrow = c(1,2))
acf(x_ts, main = "ACF αρχικής σειράς")
pacf(x_ts, main = "PACF αρχικής σειράς")
par(mfrow = c(1,1))

# 4. Υπολογισμός και διάγραμμα 1ης διαφοράς
dx_ts <- diff(x_ts)
plot(dx_ts, main = "1η Διαφορά σειράς", ylab = "Διαφορά από προηγούμενο έτος", xlab = "Έτος")

# 5. ACF και PACF της 1ης διαφοράς
par(mfrow = c(1,2))
acf(dx_ts, main = "ACF 1ης διαφοράς")
pacf(dx_ts, main = "PACF 1ης διαφοράς")
par(mfrow = c(1,1))

++++

# 1. Φόρτωση δεδομένων
library(astsa)
data(gtemp_land)
x_ts <- ts(gtemp_land, start = 1850, frequency = 1)

# 2. Εξερεύνηση στασιμότητας (βλέπουμε ότι d = 1 αρκεί)
# plot(x_ts)
# plot(diff(x_ts))

# 3. Επιλογή κατάλληλου μοντέλου
# Πρώτα ACF/PACF 1ης διαφοράς (ή δοκιμάζεις ARIMA(1,1,1), ARIMA(2,1,1) κτλ)
acf(diff(x_ts), main = "ACF 1ης διαφοράς")
pacf(diff(x_ts), main = "PACF 1ης διαφοράς")

# 4. Προσαρμογή μοντέλου (δοκιμάζουμε p = 1, q = 1, d = 1)
fit <- arima(x_ts, order = c(1,1,1))

# 5. Διαγνωστικός έλεγχος καταλοίπων
resid_fit <- residuals(fit)

par(mfrow = c(2,2))
plot(resid_fit, main = "Υπολείμματα ARIMA", ylab = "Υπολείμματα")
acf(resid_fit, main = "ACF Υπολειμμάτων")
pacf(resid_fit, main = "PACF Υπολειμμάτων")
hist(resid_fit, main = "Ιστόγραμμα Υπολειμμάτων", col = "lightblue")
par(mfrow = c(1,1))

# Ljung-Box test για λευκό θόρυβο στα υπολείμματα
lb_test <- Box.test(resid_fit, lag = 20, type = "Ljung-Box")
cat("Ljung-Box p-value:", lb_test$p.value, "\n")

# 6. Κριτήριο AIC/BIC, συντελεστές
cat("AIC:", fit$aic, "\n")
cat("Συντελεστές:\n")
print(fit$coef)

# 7. Πρόβλεψη για 10 χρόνια
n_forecast <- 10
pred <- predict(fit, n.ahead = n_forecast)

# Υπολογισμός διαστημάτων εμπιστοσύνης 95%
z <- qnorm(0.975)
upper <- pred$pred + z * pred$se
lower <- pred$pred - z * pred$se

# Έτη πρόβλεψης
last_year <- end(x_ts)[1]
forecast_years <- (last_year + 1):(last_year + n_forecast)
forecast_years <- c(last_year, forecast_years)
forecast_pred <- c(tail(x_ts, 1), pred$pred)
upper <- c(tail(x_ts, 1), upper)
lower <- c(tail(x_ts, 1), lower)

# 8. Οπτικοποίηση
plot(window(x_ts, start = 2000), type = "o", pch = 16, col = "blue",
     xlim = c(2000, last_year + n_forecast),
     ylim = range(c(x_ts, lower, upper), na.rm = TRUE),
     main = "Πρόβλεψη Παγκόσμιας Θερμοκρασίας Ξηράς",
     xlab = "Έτος", ylab = "Απόκλιση (°C)")
lines(forecast_years, forecast_pred, type = "o", col = "red", pch = 18, lty = 2)
lines(forecast_years, upper, col = "darkgreen", lty = 3)
lines(forecast_years, lower, col = "darkgreen", lty = 3)
polygon(c(forecast_years, rev(forecast_years)),
        c(upper, rev(lower)),
        col = rgb(0, 0.5, 0, 0.2), border = NA)
legend("topleft",
       legend = c("Ιστορικά", "Πρόβλεψη", "95% CI"),
       col = c("blue", "red", "darkgreen"),
       lty = c(1, 2, 1), pch = c(16, 18, NA), bg = "white")


# Δοκιμή διαφόρων συνδυασμών
fit1 <- arima(x_ts, order = c(1,1,0))
fit2 <- arima(x_ts, order = c(1,1,1))
fit3 <- arima(x_ts, order = c(2,1,1))
AIC(fit1, fit2, fit3)


# Διαγνωστικός έλεγχος υπολοίπων για fit2 (ARIMA(1,1,1))
resid_fit <- residuals(fit2)

par(mfrow = c(2, 2))

# 1. Υπόλοιπα vs Χρόνο
plot(resid_fit, type = "l", main = "Υπόλοιπα Μοντέλου", ylab = "Υπόλοιπα")

# 2. Ιστόγραμμα υπολοίπων (για κανονικότητα)
hist(resid_fit, breaks = 15, main = "Ιστόγραμμα Υπολοίπων", col = "lightblue", xlab = "Υπόλοιπα")

# 3. Q-Q plot
qqnorm(resid_fit, main = "Q-Q Plot Υπολοίπων")
qqline(resid_fit, col = "red", lwd = 2)

# 4. ACF των υπολοίπων (για λευκό θόρυβο)
acf(resid_fit, main = "ACF Υπολοίπων")

par(mfrow = c(1, 1))

# 5. Ljung-Box test για λευκό θόρυβο στα υπολείμματα
lb_test <- Box.test(resid_fit, lag = 20, type = "Ljung-Box")
cat("Ljung-Box p-value:", lb_test$p.value, "\n")

# 6. Κριτήριο AIC/BIC, συντελεστές (εμφανίζεις αν θέλεις)
cat("AIC:", fit2$aic, "\n")
cat("Συντελεστές:\n"); print(fit2$coef)



#3.3

# Βήμα 1: Εισαγωγή δεδομένων
sales_data <- data.frame(
  Year = c(2016, 2017, 2018, 2019),
  Winter = c(1, 2, 2, 1),
  Spring = c(3, 2, 4, 3),
  Summer = c(6, 7, 8, 8),
  Autumn = c(4, 5, 5, 6)
)

# Βήμα 2: Μετατροπή σε χρονοσειρά
sales_ts <- ts(c(t(sales_data[, -1])), 
               start = 2016, 
               frequency = 4)

# Βήμα 3: Υπολογισμός εποχιακών δεικτών (μέθοδος λόγου κινητού μέσου)
decompose_sales <- decompose(sales_ts, type = "multiplicative")
seasonal_indices <- decompose_sales$seasonal[1:4]

# Εμφάνιση εποχιακών δεικτών
cat("Εποχιακοί Δείκτες:\n")
print(round(seasonal_indices, 3))

# Βήμα 4: Εξάλειψη εποχικότητας (αποεποχικοποίηση)
deseasonalized_sales <- sales_ts / seasonal_indices

# Βήμα 5: Κατασκευή γραμμής τάσης
time_points <- time(sales_ts)
trend_model <- lm(deseasonalized_sales ~ time_points)
trend_line <- ts(fitted(trend_model), 
                 start = 2016, 
                 frequency = 4)

# Βήμα 6: Κυκλική μεταβολή (Σχετικά κυκλικά υπόλοιπα)
cyclic_component <- deseasonalized_sales / trend_line

# Βήμα 7: Οπτικοποίηση
par(mfrow = c(2,2))

# 1. πρωτογενής σειρά
plot(sales_ts, main = "πρωτογενείς Πωλήσεις", 
     ylab = "Πωλήσεις (εκατ. μπουκάλια)", col = "blue")
legend("topleft", "Πρωτόγονη", col = "blue", lty = 1)

# 2. Αποεποχικοποιημένες πωλήσεις
plot(deseasonalized_sales, main = "Αποεποχικοποιημένες Πωλήσεις",
     ylab = "Πωλήσεις", col = "red")
lines(trend_line, col = "darkgreen", lwd = 2)
legend("topleft", c("Αποεποχικοποιημένες", "Τάση"),
       col = c("red", "darkgreen"), lty = c(1,1))

# 3. Κυκλική συνιστώσα
plot(cyclic_component, main = "Κυκλική Μεταβολή",
     ylab = "Σχετικά Υπόλοιπα", col = "purple")
abline(h = 1, col = "gray", lty = 2)
legend("topleft", "Κυκλική", col = "purple", lty = 1)

# 4. Εποχιακοί δείκτες
barplot(seasonal_indices, 
        names.arg = c("Χειμώνας", "Άνοιξη", "Καλοκαίρι", "Φθινόπωρο"),
        main = "Εποχιακοί Δείκτες", ylab = "Δείκτης")

par(mfrow = c(1,1))
