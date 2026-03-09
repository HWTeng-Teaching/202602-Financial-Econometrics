#2.22
  #a.Regression model_totalscore/small
    Reg22.a <- lm(totalscore~small,data=star5_small)
  #for輸出好看的表格
    stargazer(Reg22.a,type="text")
  
  #b-1.Regression model_readscore/small
    Reg22.b.1 <- lm(readscore~small,data=star5_small)
    stargazer(Reg22.b.1,type="text")
  
  #b-2.Regression model_mathscore/small
    Reg22.b.2 <- lm(mathscore~small,data=star5_small)
    stargazer(Reg22.b.2,type="text")
  
  #c.Regression model_totalscore/aide
    Reg22.c <- lm(totalscore~aide,data=star5_small)
    stargazer(Reg22.c,type="text")
  
  #d-1.Regression model_readscore/aide
    Reg22.d.1 <- lm(readscore~small,data=star5_small)
    stargazer(Reg22.d.1,type="text")
  
  #d-2.Regression model_mathscore/aide
    Reg22.d.2 <- lm(mathscore~small,data=star5_small)
    stargazer(Reg22.d.2,type="text")
    
#2.25
  #a.histogram and summary statistics of FOODAWAY
    stargazer(cex5_small["foodaway"],
              type="text",
              summary.stat=c("n","mean","sd","min","p25","median","p75","max" ))
    hist(cex5_small$foodaway,
         main="Histogram of FOODAWAY",
         xlab="FOODAWAY",
         col="steelblue",
         border="white" )
  
  #b.the mean and median of FOODAWAY(conditioned)
    adv=subset(cex5_small,advanced==1)[,"foodaway", drop = FALSE]
    coll=subset(cex5_small,college==1)[,"foodaway", drop = FALSE]
    neither=subset(cex5_small,advanced==0&college==0)[,"foodaway", drop = FALSE]
    
    stargazer(adv,coll,neither,
              type="text",
              summary.stat=c("mean","median"))
  
  #c.histogram and summary statistics of ln(FOODAWAY)
    cex5_small$ln_foodaway=log(cex5_small$foodaway+1) #避免ln(0)
    stargazer(cex5_small["ln_foodaway"],
              type="text",
              summary.stat=c("n","mean","sd","min","p25","median","p75","max" ))
    hist(cex5_small$ln_foodaway,
         main="Histogram of ln(FOODAWAY)",
         xlab="ln(FOODAWAY)",
         col="steelblue",
         border="white" )
    
  #d.Regression model_ln(FOODAWAY)/income
    Reg25.d <- lm(ln_foodaway~income,data=cex5_small,na.action = na.exclude)
    stargazer(Reg25.d,type="text")
    
  #e.plot ln(FOODAWAY) against income and include (d)
    plot(cex5_small$income,cex5_small$ln_foodaway,
         xlim=c(0,max(cex5_small$income)),
         ylim=c(0,max(cex5_small$ln_foodaway)),
         xlab="household monthly Income in $100",
         ylab="ln(Foodaway) in $1",
         pch=19, #點的形狀
         col="steelblue")
    abline(Reg25.d,col="indianred",lwd=2)#粗度
    
  #f.cal the least squares residuals,plot them vs, income
    cex5_small$residuals=resid(Reg25.d)
    plot(cex5_small$income,cex5_small$residuals,
         xlim=c(0,max(cex5_small$income)),
         ylim=c(0,max(cex5_small$residuals)),
         xlab="household monthly Income in $100",
         ylab="residuals",
         pch=19, 
         col="steelblue")
  