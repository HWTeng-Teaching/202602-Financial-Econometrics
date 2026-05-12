library(POE5Rdata)
library(AER)   # ivreg() for 2SLS
library(car)   # linearHypothesis()

data(fultonfish)

# ============================================================
# Part (a):
# ============================================================

reduced_form_a <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed,
                     data = fultonfish)
print(summary(reduced_form_a))

f_test_stormy_mixed <- linearHypothesis(reduced_form_a,
                                        c("stormy = 0", "mixed = 0"))
print(f_test_stormy_mixed)


# ============================================================
# Part (b):
# ============================================================

demand_2iv <- ivreg(lquan ~ lprice + mon + tue + wed + thu |
                      mon + tue + wed + thu + stormy + mixed,
                    data = fultonfish)

print(summary(demand_2iv))

vars   <- c("(Intercept)", "lprice", "mon", "tue", "wed", "thu")
labels <- c("C", "ln(PRICE)", "MON", "TUE", "WED", "THU")

tbl <- data.frame(
  Variable         = labels,
  Table_11.5_book  = c(8.5059, -1.1194, -0.0254, -0.5308, -0.5664,  0.1093),
  STORMY_MIXED_R   = round(coef(demand_2iv)[vars], 4),
  row.names = NULL
)
print(tbl)


# ============================================================
# Part (c)
# ============================================================

resid_2sls  <- residuals(demand_2iv)
sargan_aux  <- lm(resid_2sls ~ mon + tue + wed + thu + stormy + mixed,
                  data = fultonfish)

n            <- nrow(fultonfish)
R2_sargan    <- summary(sargan_aux)$r.squared
sargan_stat  <- n * R2_sargan
df_sargan    <- 1   # L - K = 2 instruments - 1 endogenous = 1
p_sargan     <- pchisq(sargan_stat, df = df_sargan, lower.tail = FALSE)
print(p_sargan)


# ============================================================
# Part (d)
# ============================================================
f_test_days <- linearHypothesis(reduced_form_a,
                                c("mon = 0", "tue = 0",
                                  "wed = 0", "thu = 0"))
print(f_test_days)

f_days <- f_test_days$F[2]
print(f_days)
p_days <- f_test_days$`Pr(>F)`[2]
print(p_days)