pic<-function(){
library(rvest)
library(XML)
    print("please enter pages you want to download")
    pages<-scan()
for(page in 1:pages){    
    p<-(page-1)*25
    td<-read_html(paste("https://www.douban.com/group/haixiuzu/discussion?start=",p,sep=""),encoding="utf-8")
    post_list<-html_nodes(td,xpath="//tr/td[@class='title']/a")%>%html_attr("href")
    for(i in 1:length(post_list)){
      tryCatch({
        post_url<-read_html(post_list[i],encoding="utf-8")},
        error=function(e){cat("Error:",conditionMessage(e),"\n")})
      post_pic<-html_nodes(post_url,xpath="//div[@class='topic-figure cc']/img")
      post_pic_url<-html_attr(post_pic,"src")
      for(j in 1:length(post_pic_url)){
        print(paste(page,"_",i,"_",j))
        tryCatch({
          download.file(post_pic_url[j],paste("E:/image/","ss/",page,"_",i,"_",j,".jpg",sep=""),mode="wb")
          },error=function(e){cat("ERROR :",conditionMessage(e),"\n")})
        Sys.sleep(0.5)
      }
    }
}
}