library(plm)
library(lmtest)
library(sandwich)
library(car)

# =====================================================
# 15.6  NLS panel: ln(WAGE) on EXPER, EXPER^2, SOUTH, UNION
#       (years 1987 & 1988, N = 716 individuals)
# =====================================================
url1 <- "http://www.principlesofeconometrics.com/poe5/data/csv/nls_panel.csv"
nls  <- read.csv(url1)
names(nls) <- tolower(names(nls))

# POE5 csv stores year as 2-digit (82, 83, 85, 87, 88)
dat <- subset(nls, year %in% c(87, 88))
cat("\n[15.6] sample sizes by year:\n"); print(table(dat$year))

#a  Two separate OLS regressions (columns 1 & 2 of Table 15.10)
d87 <- subset(dat, year == 87)
d88 <- subset(dat, year == 88)

ols87 <- lm(lwage ~ exper + exper2 + south + union, data = d87)
ols88 <- lm(lwage ~ exper + exper2 + south + union, data = d88)

cat("\n(a) OLS 1987 (column 1):\n");  print(coeftest(ols87))
cat("\n(a) OLS 1988 (column 2):\n");  print(coeftest(ols88))

#c  Fixed effects within estimator (column 3)
pdat <- pdata.frame(dat, index = c("id", "year"))
fe <- plm(lwage ~ exper + exper2 + south + union,
          data = pdat, model = "within")
cat("\n(c) FE (within) estimates (column 3):\n"); print(coeftest(fe))

#d  F-test for individual effects (eq 15.20). Textbook F = 11.68.
F_text <- 11.68
N_ind  <- length(unique(pdat$id))    # 716
NT     <- nrow(pdat)                 # 1432
K      <- length(coef(fe))           # 4
df1    <- N_ind - 1                  # 715
df2    <- NT - N_ind - K             # 712
crit1  <- qf(0.99, df1, df2)
p_text <- pf(F_text, df1, df2, lower.tail = FALSE)

cat(sprintf("\n(d) F-test for no individual effects:\n"))
cat(sprintf("    F = %.2f (given), df = (%d, %d)\n", F_text, df1, df2))
cat(sprintf("    1%% critical value = %.4f, p-value = %.3g\n", crit1, p_text))

#e  FE with cluster-robust SE (column 4)
vcov_cl <- vcovHC(fe, method = "arellano", type = "HC0", cluster = "group")
fe_rob  <- coeftest(fe, vcov. = vcov_cl)
cat("\n(e) FE with cluster-robust SE (column 4):\n"); print(fe_rob)

se_fe   <- sqrt(diag(vcov(fe)))
se_rob  <- sqrt(diag(vcov_cl))
cat("\nSE comparison:\n")
print(round(data.frame(FE = se_fe, FE_robust = se_rob,
                       ratio = se_rob / se_fe), 4))

#f  Random effects (column 5) and Hausman test
re <- plm(lwage ~ exper + exper2 + south + union,
          data = pdat, model = "random")
cat("\n(f) RE estimates (column 5):\n"); print(coeftest(re))

ht <- phtest(fe, re)
cat("\nHausman test (FE vs RE):\n"); print(ht)


# =====================================================
# 15.20  STAR kindergarten data
# =====================================================
url2  <- "http://www.principlesofeconometrics.com/poe5/data/csv/star.csv"
star  <- read.csv(url2)
names(star) <- tolower(names(star))
if (!"id" %in% names(star)) star$id <- seq_len(nrow(star))
 
cat("\n\n[15.20] dim(star) =", dim(star), "\n")
 
#a  Pooled OLS
ols_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = star)
cat("\n(a) Pooled OLS:\n"); print(coeftest(ols_a))
 
# rename student id to avoid plm reserved-name warning on 'id'
star$stuid <- star$id; star$id <- NULL
 
#b  School fixed effects
pst <- pdata.frame(star, index = c("schid", "stuid"))
fe_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pst, model = "within")
cat("\n(b) FE with school fixed effects:\n"); print(coeftest(fe_b))
 
#c  F-test for school fixed effects vs pooled OLS
pool_c <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
              data = pst, model = "pooling")
ft_c <- pFtest(fe_b, pool_c)
cat("\n(c) F-test for school fixed effects:\n"); print(ft_c)
 
#d  Random effects + LM test
re_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pst, model = "random")
cat("\n(d) RE with school random effects:\n"); print(coeftest(re_d))
 
lm_d <- plmtest(pool_c, type = "bp")
cat("\nLM test for random effects (Breusch-Pagan):\n"); print(lm_d)
 
#e  t-tests on individual coefficients (eq 15.36)
vars_test <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")
b_fe  <- coef(fe_b)
b_re  <- coef(re_d)[names(coef(re_d)) != "(Intercept)"]
v_fe  <- diag(vcov(fe_b))
v_re  <- diag(vcov(re_d))[names(v_fe)]
diff  <- b_fe[vars_test] - b_re[vars_test]
v_d   <- v_fe[vars_test] - v_re[vars_test]
# when Var(b_FE) ~= Var(b_RE) (fully within-group regressor like BOY),
# the Hausman variance is ~0 and the t-stat is undefined
se_d  <- rep(NA_real_, length(v_d))
ok    <- v_d > 1e-12
se_d[ok] <- sqrt(v_d[ok])
t_d   <- diff / se_d
p_d   <- 2 * pnorm(-abs(t_d))
 
tab_e <- data.frame(FE = b_fe[vars_test], RE = b_re[vars_test],
                    diff = diff, se_diff = se_d, t = t_d, p = p_d)
cat("\n(e) t-test (eq 15.36) FE vs RE coefficient-by-coefficient:\n")
print(round(tab_e, 4))
 
#f  Mundlak test -- school means + individual deviations
# Note: small/aide/regular are mutually exclusive at the student level, so
# their school means are linearly dependent (small_m + aide_m + reg_m = 1).
# Including all three breaks the design matrix. We drop aide_m to identify
# the remaining school-mean coefficients.
star$small_m       <- ave(star$small,       star$schid)
star$tchexper_m    <- ave(star$tchexper,    star$schid)
star$boy_m         <- ave(star$boy,         star$schid)
star$white_asian_m <- ave(star$white_asian, star$schid)
star$freelunch_m   <- ave(star$freelunch,   star$schid)
 
# Mundlak via OLS: same point estimates and joint test as augmented RE,
# without the GLS singularity issue. Cluster SE at school level.
mundlak <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch
              + small_m + tchexper_m + boy_m + white_asian_m + freelunch_m,
              data = star)
vcov_m <- vcovCL(mundlak, cluster = ~ schid)
cat("\n(f) Mundlak augmented model (OLS, school-clustered SE):\n")
print(coeftest(mundlak, vcov. = vcov_m))
 
mean_vars <- c("small_m", "tchexper_m", "boy_m", "white_asian_m", "freelunch_m")
W <- linearHypothesis(mundlak, paste(mean_vars, "= 0"),
                      vcov. = vcov_m, test = "Chisq")
cat("\nMundlak joint test (H0: all school-mean coefficients = 0):\n")
print(W)
