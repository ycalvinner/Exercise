library(Rwebdriver)
library(XML)
library(stringr)
# Run the server
start_session(root="http://localhost:4444/wd/hub/",browser="chrome")
# Get the subject list.
post.url("https://bcy.net/coser")
pagesource<-page_source()
Encoding(pagesource)<-"utf-8"
pagesource
url_list<-str_extract_all(pagesource,"coser/detail/\\d*/\\d*")
url_list<-str_c("https://bcy.net/coser/detail",unlist(str_extract_all(unlist(url_list),"/\\d*/\\d*")))

# Create the handle
handle<-getCurlHandle(useragent=str_c(R.version$platform,R.version$version.string,sep=", "),httpheader=c(from="111@qq.com"))

# Define download function
downloadPIC<-function(url_list){
  dir.create(paste0("E:\\image\\",Sys.Date()))
  for(i in 1:length(url_list)){
    post.url(url=url_list[i])
    pagesource_pic<-page_source()
    Encoding(pagesource_pic)<-"utf-8"
    pic_list<-unlist(str_extract_all(pagesource_pic,"https:.*?\\.jpg"))
    for(j in 1:length(pic_list)){
      writeBin(getBinaryURL(pic_list[j]),paste0("E:\\image\\",Sys.Date(),"\\",i,"-",j,".jpg"))
    }
  }
}

# Download
downloadPIC(url_list)
