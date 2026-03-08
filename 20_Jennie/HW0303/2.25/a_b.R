load("D:/碩一下/計量經濟/作業/HW1/25/cex5_small.rdata")
#a
hist(cex5_small$foodaway,
     main="Histogram of FOODAWAY",
     xlab="FOODAWAY ($)",
     col="blue",
     breaks=20)

summary(cex5_small$foodaway)
#b
subset_adv=subset(cex5_small,advanced==1)
mean(subset_adv$foodaway)
median(subset_adv$foodaway)

subset_college=subset(cex5_small,college==1&advanced==0)
mean(subset_college$foodaway)
median(subset_college$foodaway)

subset_no=subset(cex5_small,college==0&advanced==0)
mean(subset_no$foodaway)
median(subset_no$foodaway)

