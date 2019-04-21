###sciagnij sobie biblioteke
library(gtrendsR)
####opis tej funkcji masz w pliku gtrendR.pdf,
library(plyr)
library(dplyr)


#tu wpisujesz tickery
x=c("AAC","AAN","AAOI","AAON","AAT","AAWW","AAXN","ABCB","ABCD","ABEO","ABG","ABM","ABR","ABTX","ABUS","AC","ACAD","ACBI","ACCO","ACHN","ACIA","ACIW","ACLS","ACMR","ACNB","ACOR","ACRE","ACRS","ACTG","ACXM","ADC","ADES","ADMA","ADMS","ADRO","ADSW","ADTN","ADUS","ADVM","AE","AEGN","AEIS","AEL","AEO","AERI","AFI","AFSI","AGEN","AGFS","AGLE","AGM","AGS","AGX","AGYS","AHH","AHT","AI","AIMC","AIMT","AIN","AINC","AIR","AIT","AJRD","AJX","AKAO","AKBA","AKCA","AKR","AKRX","AKS","ALBO","ALCO","ALDR","ALDX","ALE","ALEX","ALG","ALGT","ALNA","ALRM","ALTR","ALX","AMAG","AMBA","AMBC","AMBR","AMC","AMED","AMEH","AMKR","AMN","AMNB","AMOT","AMPE","AMPH","AMR","AMRC","AMRS","AMRX","AMSF","AMSWA","AMWD","ANAB","ANCX","ANDE","ANF","ANGO","ANH","ANIK","ANIP","AOBC","AOI","AOSL","APAM","APEI","APLS","APOG","APPF","APTI","APTS","AQ","AQUA","ARA","ARAY","ARCB","ARCH","ARDX","AREX","ARGO","ARI","ARII","ARL","ARNA","AROC","AROW","ARQL","ARR","ARRY","ARTNA","ARWR","ASC","ASGN","ASIX","ASMB","ASNA","ASNS","ASPS","ASTE","ASUR","AT","ATEN","ATGE","ATHX","ATI","ATKR","ATLO","ATNI","ATNX","ATRA","ATRC","ATRI","ATRO","ATRS","ATSG","ATU","AUBN","AVA","AVAV","AVD","AVEO","AVHI","AVID","AVNS","AVX","AVYA","AWR","AXAS","AXDX","AXE","AXGN","AXL","AXTI","AYR","AYX","AZZ","B","BABY","BANC","BANF","BANR","BAS","BATRA","BATRK","BBBY","BBGI","BBSI","BBX")


###sciaganie jest podzielone na 3 okresy ####

for (i in 1:length(x)){
  
  trend1=gtrends(keyword = x[i], geo = "", time = "2009-01-01 2012-12-31",
                gprop = c("web", "news", "images", "froogle", "youtube"),
                category = 0, hl = "en-US", low_search_volume = FALSE,
                cookie_url = "http://trends.google.com/Cookies/NID")
  
    trend2=gtrends(keyword = x[i], geo = "", time = "2012-12-23 2015-12-31",
                   gprop = c("web", "news", "images", "froogle", "youtube"),
                   category = 0, hl = "en-US", low_search_volume = FALSE,
                   cookie_url = "http://trends.google.com/Cookies/NID")
    
    trend3=gtrends(keyword = x[i], geo = "", time = "2015-12-23 2018-12-31",
                   gprop = c("web", "news", "images", "froogle", "youtube"),
                   category = 0, hl = "en-US", low_search_volume = FALSE,
                   cookie_url = "http://trends.google.com/Cookies/NID")
    
    ####dane potrzebne do wyskalowania, ostatnia wartosc z pierwszego okresy i pierwsze z drugiego
    last=last(trend1$interest_over_time$hits)
    lastdate=last(trend1$interest_over_time$date)
    first=trend2$interest_over_time$hits[2]
    firstdate=trend2$interest_over_time$date[2]
    
    last2=last(trend2$interest_over_time$hits)
    first3=first(trend3$interest_over_time$hits)
    
    ###skalowanie
    for (m in 1:length(trend2$interest_over_time$hits)){
      trend2$interest_over_time$hits[m]=trend2$interest_over_time$hits[m]*last/first
    }
    ####usuwanie powtóronych wierszy(byly one potrzebne do skalowana)
    trend2$interest_over_time <- trend2$interest_over_time[-c(1,2),]
    
    ###laczenie pierwszego okresu i drugiego
    trend=join(trend1$interest_over_time,trend2$interest_over_time, by = NULL, type = "full", match = "all")
    maksimum=max(trend$hits)
 
     ###skalowanie do wartosci od 1 do 100
    for (n in 1:length(trend$hits)){
      trend$hits[n]=trend$hits[n]*100/maksimum
    }
    
    
    ####trend3 dodawanie
    lasttrend=last(trend$hits)
    firstdate3 <- first(trend3$interest_over_time$date)
    for (m in 1:length(trend3$interest_over_time$hits)){
      trend3$interest_over_time$hits[m]=trend3$interest_over_time$hits[m]*lasttrend/first3
    }
    ####usuwanie nadmiarowych wierszy
    trend3$interest_over_time <- trend3$interest_over_time[-1,]
    ####laczenie wczesniej polaczonych danych(okres 1 i 2) i 3 okresu
    trendfinal=join(trend,trend3$interest_over_time, by = NULL, type = "full", match = "all")
    maksimumfinal=max(trendfinal$hits)
    ####skalowanie do wartsci od 1 do 100
    for (n in 1:length(trendfinal$hits)){
      trendfinal$hits[n]=trendfinal$hits[n]*100/maksimumfinal
    }
  #tu musisz wpisac sciezke
  plik=paste("C:/Users/Albert/Desktop/google trends/",x[i],".csv",sep="")
  #zapis do pliku,
  write.csv(trendfinal, file= plik)
  
  
}






  
