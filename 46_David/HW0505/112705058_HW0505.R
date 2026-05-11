library(AER)
library(lmtest)
library(sandwich)
library(ggplot2)

# =====================================================
# 11.24
# =====================================================
url1 <- "http://www.principlesofeconometrics.com/poe5/data/csv/fultonfish.csv"
fish <- read.csv(url1)

#a
rf_a <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fish)
cat("\n(a) Reduced-form for lprice (with MIXED):\n")
print(coeftest(rf_a))

cat("\nMIXED coefficient & p-value:\n")
print(coeftest(rf_a)["mixed", ])

rf_a_r <- lm(lprice ~ mon + tue + wed + thu, data = fish)
f_test_a <- anova(rf_a_r, rf_a)
cat("\nJoint F-test for STORMY + MIXED:\n")
print(f_test_a)
cat("F-stat:", f_test_a$F[2], "\n")

#b
iv_b <- ivreg(lquan ~ lprice + mon + tue + wed + thu |
                stormy + mixed + mon + tue + wed + thu, data = fish)
cat("\n(b) Demand 2SLS (IV = STORMY + MIXED):\n")
print(coeftest(iv_b))

iv_b_table11_5 <- ivreg(lquan ~ lprice + mon + tue + wed + thu |
                          stormy + mon + tue + wed + thu, data = fish)
cat("\nFor comparison - Table 11.5 (IV = STORMY only):\n")
print(coeftest(iv_b_table11_5))

#c
res_iv_b <- residuals(iv_b)
sargan_aux <- lm(res_iv_b ~ mon + tue + wed + thu + stormy + mixed, data = fish)
n <- nrow(fish)
r2_sargan <- summary(sargan_aux)$r.squared
sargan_stat <- n * r2_sargan
p_sargan <- pchisq(sargan_stat, df = 1, lower.tail = FALSE)

cat("\n(c) Sargan test (manual):\n")
cat("N*R^2 =", sargan_stat, "\n")
cat("p-value (chi^2 df=1):", p_sargan, "\n")
cat("5% critical value:", qchisq(0.95, df = 1), "\n")

cat("\nDiagnostic tests from ivreg:\n")
print(summary(iv_b, diagnostics = TRUE)$diagnostics)

#d
rf_d_r <- lm(lprice ~ stormy + mixed, data = fish)
f_test_d <- anova(rf_d_r, rf_a)
cat("\n(d) Joint F-test for MON + TUE + WED + THU in reduced-form:\n")
print(f_test_d)
cat("F-stat:", f_test_d$F[2], "  p-value:", f_test_d$`Pr(>F)`[2], "\n")


# =====================================================
# 11.28
# =====================================================
url2 <- "http://www.principlesofeconometrics.com/poe5/data/csv/truffles.csv"
tru <- read.csv(url2)

#b
dem_2sls <- ivreg(p ~ q + ps + di | pf + ps + di, data = tru)
sup_2sls <- ivreg(p ~ q + pf      | ps + di + pf, data = tru)

cat("\n(b) Demand 2SLS  (P on LHS):\n")
print(coeftest(dem_2sls))
cat("\nSupply 2SLS  (P on LHS):\n")
print(coeftest(sup_2sls))

#c
a2_prime <- coef(dem_2sls)["q"]
mean_p <- mean(tru$p); mean_q <- mean(tru$q)
elas_d <- (1 / a2_prime) * (mean_p / mean_q)
cat("\n(c) Price elasticity of demand at means:\n")
cat("a2' =", a2_prime, "  mean(P) =", mean_p, "  mean(Q) =", mean_q, "\n")
cat("elasticity =", elas_d, "\n")

#d
DI_s <- 3.5; PS_s <- 22; PF_s <- 23
ad <- coef(dem_2sls); as_ <- coef(sup_2sls)

d_int  <- ad["(Intercept)"] + ad["ps"] * PS_s + ad["di"] * DI_s
d_slp  <- ad["q"]
s_int  <- as_["(Intercept)"] + as_["pf"] * PF_s
s_slp  <- as_["q"]

cat("\n(d) Curves at DI*=3.5, PS*=22, PF*=23:\n")
cat(sprintf("  Demand : P = %.3f + (%.3f)*Q\n", d_int, d_slp))
cat(sprintf("  Supply : P = %.3f + (%.3f)*Q\n", s_int, s_slp))

Q_eq <- as.numeric((s_int - d_int) / (d_slp - s_slp))
P_eq <- as.numeric(d_int + d_slp * Q_eq)
cat(sprintf("  Equilibrium  : Q = %.3f, P = %.3f\n", Q_eq, P_eq))

q_grid <- seq(0, max(tru$q) * 1.2, length.out = 100)
plot_df <- rbind(
  data.frame(Q = q_grid, P = d_int + d_slp * q_grid, curve = "Demand"),
  data.frame(Q = q_grid, P = s_int + s_slp * q_grid, curve = "Supply")
)
p_plot <- ggplot(plot_df, aes(Q, P, color = curve)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = Q_eq, y = P_eq), color = "black", size = 3, inherit.aes = FALSE) +
  annotate("text", x = Q_eq, y = P_eq,
           label = sprintf("(%.2f, %.2f)", Q_eq, P_eq),
           hjust = -0.15, vjust = -0.5) +
  labs(title = "Truffles: Supply & Demand (P on vertical axis)",
       x = "Q (ounces)", y = "P ($/ounce)") +
  theme_minimal()
print(p_plot)

#e
rf_p <- lm(p ~ ps + di + pf, data = tru)
rf_q <- lm(q ~ ps + di + pf, data = tru)
new_x <- data.frame(ps = PS_s, di = DI_s, pf = PF_s)
P_rf  <- predict(rf_p, new_x)
Q_rf  <- predict(rf_q, new_x)

cat("\n(e) Reduced-form prediction at DI*=3.5, PS*=22, PF*=23:\n")
cat(sprintf("  Structural (2SLS) : P = %.3f, Q = %.3f\n", P_eq, Q_eq))
cat(sprintf("  Reduced-form pred : P = %.3f, Q = %.3f\n", P_rf, Q_rf))

#f
dem_ols <- lm(p ~ q + ps + di, data = tru)
sup_ols <- lm(p ~ q + pf,      data = tru)
cat("\n(f) Demand OLS:\n");  print(coeftest(dem_ols))
cat("\nSupply OLS:\n");      print(coeftest(sup_ols))

cat("\n--- Coefficient comparison (q) ---\n")
cat(sprintf("Demand  2SLS q: %8.4f    OLS q: %8.4f\n",
            coef(dem_2sls)["q"], coef(dem_ols)["q"]))
cat(sprintf("Supply  2SLS q: %8.4f    OLS q: %8.4f\n",
            coef(sup_2sls)["q"], coef(sup_ols)["q"]))