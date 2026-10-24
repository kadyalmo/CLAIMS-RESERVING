#install.packages("remotes")
#remotes::install_github("Ractuary/casdata")
#1)
library(casdata)
data(package = "casdata") 
data(ppauto)

#understanding the form of data
head(ppauto,20)
str(ppauto)
dim(ppauto)
names(ppauto)

#choose one company
big <- aggregate(EarnedPremNet_B ~ GRCODE + GRNAME,
                 data = subset(ppauto,AccidentYear==1988 & DevelopmentLag==1),
                 FUN=sum)#اعلى 10 شركات من حيث اجمالي الاقساط في 1988
big <- big[order(-big$EarnedPremNet_B),]
head(big,10)#ترتيب الشركات من الاكبر للاصغر

#we choos GRCODE 1767 
co <- subset(ppauto, GRCODE==1767 )
dim(co)# معرفة عدد الصفوف والاعمده
 

#2)
#building the LDT
tri <- xtabs(CumPaidLoss_B ~ AccidentYear + DevelopmentLag,
             data=co)
tri

#LDT inspection
all(apply(tri, 1, function(x) all(diff(x)>= 0)))#Trueالنتيجه كانت

#4x4
tri4 <- unclass(tri)[1:4, 1:4]#1988-1991
tri4

tri4_known <- tri4
tri4_known[row(tri4)+ col(tri4) >5] <-NA #نخلي الجزء السفلي مجهول NA
tri4_known

n <- 4
f <- numeric(n-1)
for(j in 1:(n-1)){
  rows <- 1:(n-j)
  f[j] <- sum(tri4_known[rows, j+1]) / sum(tri4_known[rows, j])
}
round(f, 3)# العوامل

# a copy fill with expectation
full <- tri4_known

for(j in 2:n){
  for(i in 1:n){
    if(is.na(full[i,j])){
      full[i,j]<- full[i,j-1] * f[j-1]
    }
  }
}#اذا فيه خانه غير معرفه نضرب الخانه الي قبلها في المعامل
round(full)
latest <- tri4_known[cbind(1:n, n:1)]
ultimate <- full[,n]
reserve <- ultimate - latest
round(reserve)#الاحتياطي لكل سنة حادث
sum(reserve)#الاحتياطي الكلي

#Actual avalues
actual <- tri4[,4]
actual_reserve <- actual - latest

comparison <- data.frame(
  Forcast= round(reserve),#توقعي
  Actual= actual_reserve,# الواقع
  Diff= round(reserve - actual_reserve),
  Pct= round(100 * (reserve - actual_reserve) / actual_reserve, 1)
)
comparison


#3)
#from FRED CPIAUCNS we toke inflation rates 1988-1991
cpi <- c("1988"= 118.258, "1989"=123.967,
         "1990"=130.658, "1991"=136.192)
infl <- cpi[-1] / cpi[-4] - 1
round(infl * 100 ,2)
idx <- cpi ["1991"] / cpi
round(idx, 4)

#LDT & Inflation
inc <- tri4_known
inc[,2 :n ] <- tri4_known[,2 :n ] - tri4_known[,1 :(n-1)]
inc

#كم انصرف في هذي السنه بس
cal <- outer(1988:1991, 0:3, "+")
cal

#الدفعات يقيمة 1991
fac <- matrix(idx[as.character(cal)], n ,n)
inc_adj <- inc * fac
round(inc_adj)

# نفس خطوات التوقع السابقه لكن باستخدام قيم ما بعد التضخم
cum_adj <- t(apply(inc_adj, 1, cumsum))
round(cum_adj)

f_adj <- numeric(n-1)
for(j in 1:(n-1)){
  rows <- 1:(n-j)
  f_adj[j] <- sum(cum_adj[rows, j+1]) / sum(cum_adj[rows, j])
}
round(f_adj, 3)#المعاملات بعد استخدام مبالغ ما بعد التضخم

full_adj <- cum_adj

for(j in 2:n){
  for(i in 1:n){
    if(is.na(full_adj[i,j])){
      full_adj[i,j]<- full_adj[i,j-1] * f_adj[j-1]
    }
  }
}# تةوقع القيم باستخدام المعاملات والمبالغ بعد التضخم
round(full_adj)

latest_adj <- cum_adj[cbind(1:n, n:1)]
ultimate_adj <- full_adj[,n]
reserve_adj <- ultimate_adj - latest_adj
round(reserve_adj)
sum(reserve_adj)#الاحتياطي الكلي بفيمة 1991


#4)
#bornhuetter-Ferguson(BF)
prem <- sapply(1988:1991, function(y)
  co$EarnedPremNet_B[co$AccidentYear == y & co$DevelopmentLag == 1])
names(prem) <- 1988:1991
prem #استخراج مبالغ الاقساط

#ELR
elr <- tri4[1, n] / prem["1988"]
elr

cdf <- sapply(1:n, function(k) if (k < n) prod(f[k:(n-1)])else 1)
cdf_ay <- cdf[n:1]
cdf_ay

#BF
bf <- prem * elr * (1 - 1 / cdf_ay)
round(bf)
sum(bf)


#5)
#جمع النةاتج الثلاث في جدول
comp2 <- data.frame(
  ChainLadder = round(reserve),
  BF = round(bf),
  Actual = actual_reserve
  )
comp2$CL_Err_Pct <- round(100 * (comp2$ChainLadder - comp2$Actual) / comp2$Actual, 1)
comp2$BF_Err_Pct <- round(100 * (comp2$BF - comp2$Actual) / comp2$Actual, 1)
comp2
colSums(comp2[,1:3])#المجاميع الكليه للاعمده الثلاث