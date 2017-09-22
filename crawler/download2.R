bcy<-function(){
  library(curl)
  library(rvest)
  library(XML)
  library(RCurl)
print("type in the data you want to catch with the format:yyyymmdd")
date_data<-scan()

dir.create(paste("E:/image/ss/daily_",date_data,sep=""))
post_html<-read_html(curl(paste("http://bcy.net/coser/toppost100?type=week&date=",date_data,sep=""), handle = new_handle("useragent" = "Mozilla/5.0")))
post_url_list<-paste("http://bcy.net",html_nodes(post_html,xpath="//div[@class='work-thumbnail__topBd']/a")%>%html_attr("href"),sep="")

for(i in 1:length(post_url_list)){
  tryCatch({
    post_pic_html<-read_html(curl(post_url_list[i],handle=new_handle("useragent"="MOzilla/5.0")))},
    error=function(e){cat("Error:",conditionMessage(e),"\n")})
  post_pic_url<-html_nodes(post_pic_html,xpath="//div[@class='post__content js-content-img-wrap js-fullimg js-maincontent mb20']/img")%>%html_attr("src")
  for(j in 1:length(post_pic_url)){
    print(paste(i,"_",j))
    tryCatch({
      download.file(post_pic_url[j],paste("E:/image/ss/",date_data,"/",i,"_",j,".jpg",sep=""),mode="wb")
    },error=function(e){cat("ERROR :",conditionMessage(e),"\n")})
    Sys.sleep(0.5)
  }
}
}



