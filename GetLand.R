setwd("~/Dropbox/dsp/getLand")
#Sys.setlocale(category="LC_ALL",locale="C")
library("jsonlite")
library("curl")
library("httr")
####讀入建案清單####
f<-file("building.csv",encoding="UTF-8")
f2 <- file("buildingGeo.csv",encoding="big5")
list<-read.csv(f,encoding="big5",stringsAsFactors=F)
building <- read.csv(f2,encoding="big5",stringsAsFactors=F)
building<-building[,-1]

####篩出尚未查的####
list<-list[-match(list[,3],building[,3],nomatch=F),]
####資料清理####
#清理出第一個地號
df<-list[,5]
#df<-iconv(df,"UTF-8", "big5")
#只抓取第一個出現的地號做代表
for (i in 1:length(df)) {
  if (grepl(";",df[i])==T) {
    df[i]<-substr(df[i],1,regexpr(";",df[i])[1]-1)
  }
}
####檢查地號格式####
for (i in 1:length(df)) {
  if (grepl("段",df[i])==F) { print(paste(i,"沒有段")) }
  if (grepl("地號",df[i])==F) { print(paste(i,"沒有地號")) }
  if (grepl("地地",df[i])==T) { print(paste(i,"贅字")) }
  if (grepl("號號",df[i])==T) { print(paste(i,"贅字")) }
}

list[138,5]<-"烈嶼鄉烈濱劃測段276地號"
####透過"http://posland.g0v.io/?landno="查地號座標####
rm(data)
data<-list()
for( i in 1:nrow(list)){
  url <- paste("?landno=","金門縣", list[i,5], sep='')
  html <- content(GET("http://posland.g0v.io/", path = url),as = 'text')  #as='parsed' or 'text
  data <- rbind(data, html[1])
  wait<-abs(rnorm(1,3,3))
  print(paste("等待",wait,"秒"))
  Sys.sleep(wait)
  print(paste("done. 進行第",i,"筆完成..，還有",nrow(list)-i,"筆"))
}
####已知回應種類####
#[25,]"[{\"sectStr\":\"6Z2S5bGx5q61\",\"sect\":\"0097\",\"officeStr\":\"6YeR6ZaA\",\"office\":\"WA\",\"landno\":\"10450001\",\"cx\":118.339401,\"cy\":24.445817,\"lx\":118.339184,\"ly\":24.445641,\"rx\":118.339618,\"ry\":24.446008},\"source: http://maps.nlsc.gov.tw/\"]"
#[26,]"<html>\r\n<head><title>404 Not Found</title></head>\r\n<body bgcolor=\"white\">\r\n<center><h1>404 Not Found</h1></center>\r\n<hr><center>nginx/1.6.2</center>\r\n</body>\r\n</html>\r\n"
#[181,] "[{"msg":"已達當日使用次數"},"source: http://maps.nlsc.gov.tw/"]"
#[13,] "[{"msg":"地號查詢無資料!"},"source: http://maps.nlsc.gov.tw/"]" 
####回應資料清理####
rm(dat)
dat<-c("0","0")
for ( t in 1:length(data)) {
  land<-fromJSON(data[[t]])
  if (is.null(land[[1]]$msg)) {
    dat<-rbind(dat,c(land[[1]]$cy,land[[1]]$cx))
  }
  else {
    if (land[[1]]$msg=="地號查詢無資料!") {
      dat<-rbind(dat,c("無","無"))
    }
    if (land[[1]]$msg=="已達當日使用次數") {
      dat<-rbind(dat,c("del","del"))
    }
  }
}

dat<-dat[-1,]

building.geo<-cbind(list[c(1:(i)),c(7,2,3,5)],dat)
names(building.geo) <- c("建號","發文日期","文號","地號","lat","lon")
building.geo <- building.geo[-match(building.geo$lat=="del"),]
write.csv(building.geo,file="buildingGeo.csv",fileEncoding="big5")

#Sys.setlocale(category="LC_ALL",locale="")
#dat<-fromJSON("http://posland.g0v.io/?landno=金門縣金城鎮祥瑞段68地號")
#dat
#dat[[1]]$cy
#dat[[1]]$cx

#舊#透過"http://posland.g0v.io/?landno="查詢地號
for (i in 1:length(df)) {
  c<-unlist(paste("http://posland.g0v.io/?landno=","金門縣",df[i],sep=""))
  #c<-URLencode(c)
  #c<-iconv(c,"UTF-8","big5")
  land<-fromJSON(c)
  Sys.sleep(abs(rnorm(1,3,3)))
  if (grepl("無資料",land[1])) {
    dat<-rbind(dat,c("","")) 
    print(paste(i,"/",length(df),"空值",sep=""))
  }
  else {
    if (i == 1) {
      dat<-c(land[[1]]$cy,land[[1]]$cx)
    }
    else {
      dat<-rbind(dat,c(land[[1]]$cy,land[[1]]$cx))
    }
    print(paste(i,"/",length(df),sep=""))
    Sys.sleep(abs(rnorm(1,7,3)))
  }
}


