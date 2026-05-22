load("star.rdata")
library(plm) 
library(knitr)
library(broom)
star$studentid <- star$id
#(a)
cat("(a)\n")
OLSmod <- lm(readscore~small+aide+tchexper+boy+white_asian+freelunch,data=star)
print(summary(OLSmod))
b1 <- coef(OLSmod)[[1]]
b2 <- coef(OLSmod)[[2]]
b3 <- coef(OLSmod)[[3]]
b4 <- coef(OLSmod)[[4]]
b5 <- coef(OLSmod)[[5]]
b6 <- coef(OLSmod)[[6]]
b7 <- coef(OLSmod)[[7]]
cat("readscore:=",b1,"+ small *",b2,"+ aide *",b3,"+ tchexper *",b4
    ,"+ boy *",b5,"+ white_asian *",b6,"+ freelunch *",b7)

#(b)

cat("\n(b)\n")
Fixedmod <- plm(readscore~small+aide+tchexper+boy+white_asian+freelunch
                ,data=star,index = c("schid","studentid"),model="within")
print(summary(Fixedmod))
b_small <- coef(Fixedmod)["small"]
b_aide <- coef(Fixedmod)["aide"]
b_tchexper <- coef(Fixedmod)["tchexper"]
b_boy <- coef(Fixedmod)["boy"]
b_white_asian <- coef(Fixedmod)["white_asian"]
b_freelunch <- coef(Fixedmod)["freelunch"]
cat(  "readscore_it = alpha_i +",  b_small, "* small +",  b_aide, "* aide +"
      ,  b_tchexper, "* tchexper +",b_boy, "* boy +",  
      b_white_asian, "* white_asian +",  b_freelunch, "* freelunch + u_it"
)
#(c)
cat("\n(c)\n")
readscore.pooled <- plm(readscore~small+aide+tchexper+boy+white_asian+freelunch, 
                   model="pooling",index = c("schid","studentid"), data=star)
readscore.within <- plm(readscore~small+aide+tchexper+boy+white_asian+freelunch,
                   data=star,index = c("schid","studentid"),model="within")
print(kable(tidy(pFtest(readscore.within, readscore.pooled))
            , caption="Fixed effects test: Ho:'No fixed effects'"))

#(d)
cat("\n(d)\n")
readscore.random <- plm(readscore~small+aide+tchexper+boy+white_asian+freelunch,data=star
                        , random.method="swar",index = c("schid","studentid"),model="random")
print(kable(tidy(readscore.random), digits=4, caption=
              "Random effects results for the reading score equation"))
LMtest <- plmtest(readscore.pooled, effect="individual")
print(kable(tidy(LMtest), 
            caption="LM test for random effects in the reading score equation"))

constant <- coef(readscore.random)["(Intercept)"]
d_small <- coef(readscore.random)["small"]
d_aide <- coef(readscore.random)["aide"]
d_tchexper <- coef(readscore.random)["tchexper"]
d_boy <- coef(readscore.random)["boy"]
d_white_asian <- coef(readscore.random)["white_asian"]
d_freelunch <- coef(readscore.random)["freelunch"]
cat(  "\nreadscore_it =",constant,"+",  d_small, "* small +",  d_aide, "* aide +"
      ,  d_tchexper, "* tchexper +",d_boy, "* boy +",  
      d_white_asian, "* white_asian +",  d_freelunch, "* freelunch + u_it\n"
)
#(e)
cat("\n(e)\n")
cat("Joint test:\n")
print(kable(tidy(phtest(readscore.within, readscore.random)), caption=
        "Hausman test comparing fixed and random effects models"))
fe <- coef(summary(readscore.within))
re <- coef(summary(readscore.random))

vars <- c("small","aide","tchexper",
          "white_asian","freelunch","boy")

for(v in vars){
  
  beta_fe <- fe[v,"Estimate"]
  beta_re <- re[v,"Estimate"]
  
  var_fe <- fe[v,"Std. Error"]^2
  var_re <- re[v,"Std. Error"]^2
  
  tstat <- (beta_fe - beta_re) /
    sqrt(abs(var_fe - var_re))
  
  cat(v, ": t =", tstat, "\n")
}
#(f)
cat("\n(f)\n")
star$mean_small <- ave(star$small, star$schid)
star$mean_aide <- ave(star$aide, star$schid)
star$mean_tchexper <- ave(star$tchexper, star$schid)
star$mean_boy <- ave(star$boy, star$schid)
star$mean_white_asian <- ave(star$white_asian, star$schid)
star$mean_freelunch <- ave(star$freelunch, star$schid)
Mundlakmod <- plm(readscore ~small + aide + tchexper + boy +white_asian + freelunch+mean_freelunch,
  data = star,  index = c("schid","studentid"),model = "random")
library(car)
summary(Mundlakmod)
print(linearHypothesis(Mundlakmod,c("mean_freelunch = 0") ))
