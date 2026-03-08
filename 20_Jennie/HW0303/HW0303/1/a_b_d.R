x=c(3,2,1,-1,0)
y=c(4,2,3,1,0)

x_bar=mean(x)
y_bar=mean(y)

x1=x-x_bar
x2=(x-x_bar)^2
y1=y-y_bar
xy=(x-x_bar)*(y-y_bar)

table=data.frame(
  x,
  y,
  "x-x̄"=x1,
  "(x-x̄)^2"=x2,
  "y-ȳ"=y1,
  "(x-x̄)(y-ȳ)"=xy
)
sum_row=colSums(table)
table=rbind(table,sum_row)

table

sumxy=sum(xy)
sumx2=sum(x2)

b2=sumxy/sumx2
b1=y_bar-b2*x_bar

b2
b1

y_hat=b1+b2*x
residual=y-y_hat
residual2=residual^2
x_residual=x*residual

table2=data.frame(
  x,
  y,
  y_hat,
  residual,
  residual2,
  x_residual
)
sum_row2=colSums(table2)
table2=rbind(table2,sum_row2)
round(table2,3)

y_variance=var(y)
x_variance=var(x)
xy_covariance=cov(x,y)
xy_correlation=cor(x,y)
CV_x=100*sd(x)/x_bar

y_variance
x_variance
xy_covariance
xy_correlation
CV_x
median(x)
