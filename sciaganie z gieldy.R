library(BatchGetSymbols)

###04-01-2009 - 30-12-2018

# set dates
first.date <- "2009-01-01"
last.date <- "2018-12-30"
freq.data <- 'weekly'
# set tickers
tickers <- c('ACOR','AAC','AAN')

for (i in 1:length(tickers)){

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                                                          'BGS_Cache') ) # cache in tempdir()


plik=paste("C:/Users/Albert/Desktop/google trends/",tickers[i],"stock",".csv",sep="")
write.csv(l.out, file= plik)


}
library(ggplot2)

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker,scales = 'free_y' ) 
print(p)