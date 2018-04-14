library(RCurl)
library(stringr)
library(XML)

url<-"https://bcy.net/coser/toppost100"

standardHeader<-list(
  from="eddie@r-datacollection.com",
  'user-agent'=str_c(R.version$platform,
                     R.version$version.string,sep=", ")
)

GetIteamList<-function(url=NA,httpheader,htmlfile=NA){
  if(is.na(url)&is.na(htmlfile)){
    stop("Please provide the html file or url")
  }
  if(is.na(htmlfile) & !is.na(url)){
    td<-htmlParse(getURL(url=url, httpheader= httpheader))
    IteamList<-paste("https://bcy.net",xpathSApply(td,path="//ul/li[@class='_box']/a",xmlGetAttr,name="href"),sep="")
  }
  if(!is.na(htmlfile) & is.na(url)){
    IteamList<-xpathSApply(htmlParse(htmlfile),path="//ul/li[@class='_box']/a",xmlGetAttr,name="href")
  }
  
  IteamList
}

PicDownload<-function(IteamList,httpheader){
  dir.create(paste0("E:\\image\\",Sys.Date()))
  for(i in 1:length(IteamList)){
    pic_source<-htmlParse(getURL(IteamList[i],httpheader = httpheader))
    pic_list<-xpathSApply(pic_source,path="//article/div/div/img",xmlGetAttr,name="src")
    if(length(pic_list!=0)){
      for(j in 1:length(pic_list)){
        writeBin(getBinaryURL(pic_list[j]),paste0("E:\\image\\",Sys.Date(),"\\",i,"-",j,".jpg"))
      }
    }
  }
}






