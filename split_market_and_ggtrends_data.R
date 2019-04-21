rm(list=ls())


spolki <- c('ACOR','AAC','AAN')


for (j in 1:length(spolki)){

sciezka=paste("C:/Users/Albert/Desktop/google trends/",spolki[j],".csv",sep="")
ggtrend=read.csv(file=sciezka,sep=",",header=T)

sciezka2=paste("C:/Users/Albert/Desktop/google trends/",spolki[j],"stock",".csv",sep="")
cenaakcji=read.csv(file=sciezka2,sep=",",header=T)

cenaakcji$date=as.Date(cenaakcji$df.tickers.ref.date)+3
ggtrend$date=as.Date(ggtrend$date)+7


gtrendstock=join(cenaakcji,ggtrend, by = "date", type = "full", match = "all")



for (i in 2:length(gtrendstock$hits)){
  gtrendstock$hitsincr[i]=(gtrendstock$hits[i]-gtrendstock$hits[i-1])/gtrendstock$hits[i-1]
  gtrendstock$hitsincrp[i]=gtrendstock$hitsincr[i-1]
}

#gtrendstock$wzrostpoprzedni[]=NA
#for(i in 2:length(gtrendstock)){
#  gtrendstock$wzrostpoprzedni[i]=gtrendstock$df.tickers.ret.closing.prices[i-1]
#}

sciezka3=paste("C:/Users/Albert/Desktop/google trends/",spolki[j],"split",".csv",sep="")
write.csv(gtrendstock,file=sciezka3)

}

scatter.smooth(x=gtrendstock$df.tickers.ret.closing.prices, y=gtrendstock$hits, main="Dist ~ Speed")
scatter.smooth(x=gtrendstock$df.tickers.ret.closing.prices, y=gtrendstock$hitsincr, main="Dist ~ Speed")
scatter.smooth(x=gtrendstock$df.tickers.ret.closing.prices, y=gtrendstock$hitsincrp, main="Dist ~ Speed")


linearMod <- lm(df.tickers.ret.closing.prices ~ hits, data=gtrendstock)
AIC(linearMod)
summary(linearMod)



modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["hits", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["hits", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(gtrendstock)-ncol(gtrendstock))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

