library("capm5",package='POE5Rdata')
library(AER)
data_CAPM<-capm5
#(a)pure OLS
data_CAPM$excess_msft<-data_CAPM$msft-data_CAPM$riskfree
data_CAPM$excess_mkt<-data_CAPM$mkt-data_CAPM$riskfree
OLS<-lm(excess_msft~excess_mkt,data=data_CAPM)
summary(OLS)
#(b)使用了rank as iv
data_CAPM$RANK <- rank(data_CAPM$excess_mkt, ties.method = "first")
first_stage <- lm(excess_mkt ~ RANK, data = data_CAPM)
summary(first_stage)
#(c)懷疑excessmkt有內生性，檢查v_hat in first stage是否顯著
data_CAPM$v_hat<-residuals(first_stage)
hausman_test<-lm(excess_msft~excess_mkt+v_hat,data=data_CAPM)
summary(hausman_test)
#(d)rank good iv?
IV <- ivreg(excess_msft ~ excess_mkt | RANK, data = data_CAPM)
summary(IV)
#(e)POS and rank good ivs?
data_CAPM$POS <- ifelse(data_CAPM$excess_mkt > 0, 1, 0)
first_stage2 <- lm(excess_mkt ~ RANK + POS, data = data_CAPM)
summary(first_stage2)
#(f)檢查 excess mkt exogenous?
data_CAPM$v_hat2<-residuals(first_stage2)
hausman_model<-lm(excess_msft~excess_mkt+v_hat2,data=data_CAPM)
summary(hausman_model)
#(g)IV/2SLS rank and pos
IVRP<-ivreg(excess_msft~excess_mkt|RANK+POS,data=data_CAPM)
summary(IVRP)
#(h)sargan test
data_CAPM$resid_g<-residuals(IVRP)
sargantest<-lm(resid_g~RANK+POS,data=data_CAPM)
summary(sargantest)
n <- nrow(data_CAPM)
r2_IVRP <- summary(sargantest)$r.squared
sargan_stat <- n * r2_IVRP
df_sargan<-1
psargan<-pchisq(sargan_stat,df_sargan)
cat('sargan stat=',sargan_stat,psargan)

