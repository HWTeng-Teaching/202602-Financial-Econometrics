rm(list=ls())
library(POE5Rdata)
######################
#problem3.23
alpha = 0.05
mod1 = lm(price ~ I(sqft^2), data = collegetown)
b2 = coef(mod1)[[2]]
df = df.residual(mod1)


#  H0 : 40*ALPHA2 <= 13 , H1 : 40*ALPHA2 > 13
#->H0 : ALPHA2 <= 0.325 , H1 : ALPHA2 > 0.325 
tc = qt(1-alpha, df)
smod1 = summary(mod1)
se_b2 = coef(smod1)[2,2]
t_star = (b2 - 0.325)/se_b2

cat(
  "3.23(a)", "\n",
  "test statistic is: t = (alpha2_hat - 0.325)/se(alpha2_hat)", "\n",
  "rejection region: tc > ",tc, "\n",
  "test t-value : ",t_star, "\n",
  "hence t* is falls in the non-rejection region", "\n",
  sep=""
)
#3.23(b)

#  H0 : 80*ALPHA2 <= 13 , H1 : 80*ALPHA2 > 13
#->H0 : ALPHA2 <= 0.1625 , H1 : ALPHA2 > 0.1625
t_star_b = (b2 - 0.1625)/se_b2

cat(
  "3.23(b)", "\n",
  "test statistic is: t = (alpha2_hat - 0.1625)/se(alpha2_hat)", "\n",
  "rejection region: tc > ",tc, "\n",
  "test t-value : ",t_star_b, "\n",
  "hence t* is falls in the non-rejection region", "\n",
  sep=""
)
#3.23(c)
# E(price|sqft=20)_hat = alpha1_hat + 400 * alpha2_hat
b1 = coef(mod1)[[1]]
e_hat = b1 + 400 * b2
se_l = sqrt(vcov(mod1)[1,1] + 160000*vcov(mod1)[2, 2] + 800*vcov(mod1)[1,2])
tc_c = qt(1-alpha/2, df)
lowb = e_hat - tc_c*se_l
upb  = e_hat + tc_c*se_l

cat(
  "3.23(c)", "\n",
  "expected price is ",e_hat, "\n",
  "95% CI of the expected price: [",lowb,",",upb,"]", "\n",
  "In repeated sampling,95% intervals constrcted this way will contain true value", "\n",
  "of expected price", "\n",
  sep=""
)#3.23(d)
d <- collegetown$sqft == 20 
d_sqft20 <- collegetown[d, ]
s_avg = mean(d_sqft20$price)

cat(
  "3.23(d)", "\n",
  "In the sample there are 3 houses with 2000 square feet", "\n",
  "and sold ",d_sqft20$price[1],",",d_sqft20$price[2],",",d_sqft20$price[3], "\n",
  "The sample mean is ",s_avg, "\n",
  "and it is inside the interval we estimate", "\n",
  sep=""
)
