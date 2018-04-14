library(XML)
library(RCurl)
library(stringr)
shell.exec("D:/5_Development/crawler/SeleniumServer.bat")

# SendMail<-function(receiver_list){
#   library(RDCOMClient)
#   content_body<-paste0("Hi all<br> &nbsp;&nbsp;&nbsp;&nbsp;This is an automation email that tells you ",doc_name,"is effective, please check this information.<br> &nbsp;&nbsp;&nbsp;&nbsp;Any question please contact me by reply this mail.")
#   outApp<-COMCreate("Outlook.Application")
#   outMail<-outApp$CreateItem(0)
#   outMail[["To"]]=receiver_list
#   outMail[["subject"]]="AQMS_Utility_Report_Notification"
#   outMail[["HTMLbody"]]=content_body
#   outMail$Send()
# }

GetFileList<-function(){
  library(RSelenium)
    #
    # library(Rwebdriver)
    # start_session(root = "http://localhost:4444/wd/hub/", browser = "chrome")
    # post.url("https://aq3rglsapp01.ap.lilly.com:8080/dca")
    # Buttion_SUZ_Procedure<-element_xpath_find(value='//*[@id="divNavigationTree"]/ul/li[6]/div/span[3]')
    # element_click(ID=Buttion_SUZ_Procedure)
    # Buttion_Evaluation<-element_xpath_find('//*[@id="divNavigationTree"]/ul/li[6]/ul/li[2]/div/span[3]')
    # element_click(ID=Buttion_Evaluation)
    # Buttion_LOADMORE<-element_xpath_find('//*[@id="divNavigationTree"]/ul/li[6]/ul/li[2]/ul/li[31]')
    # element_click(ID=Buttion_LOADMORE)
    # Buttion_Report<-element_xpath_find('//*[@id="divNavigationTree"]/ul/li[6]/ul/li[2]/ul/li[57]/div/span[3]')
    # element_click(ID=Buttion_Report)
    # Buttion_Report_AQMS<-element_xpath_find('//*[@id="divNavigationTree"]/ul/li[6]/ul/li[2]/ul/li[57]/ul/li[2]/div/span[3]')
    # element_click(ID=Buttion_Report_AQMS)
    
    remDr<-remoteDriver(remoteServerAddr="localhost",port=4444L,browserName="chrome")
    url<-"https://aq3rglsapp01.ap.lilly.com:8080/dca"
    remDr$open(silent = TRUE)
    remDr$navigate(url)
    #### click---------------------------------------------------
    #remDr$mouseMoveToLocation(webElement=remDr$findElement(using="xpath",value='//*[@id="divNavigationTree"]/ul/li[6]/div/span[3]')$clickElement())
    #remDr$click()
    #### or click---------------------------------------------------
    #remDr$webElement=remDr$findElement(using="xpath",value='//*[@id="divNavigationTree"]/ul/li[6]/div/span[3]')$clickElement()
    #anyone would be OK
    Sys.sleep(3)
    remDr$findElement(using="xpath",value='//*[@id="divNavigationTree"]/ul/li[@data-id="SUZ_Procedure"]/div/span[@data-name="SUZ_Procedure"]')$clickElement()
    Sys.sleep(1)
    remDr$findElement(using="xpath",value='//*[@id="divNavigationTree"]/ul/li[@data-id="SUZ_Procedure"]/ul/li[2]/div/span[@data-name="Evaluation"]')$clickElement()
    Sys.sleep(1)
    remDr$findElement(using="xpath",value='//*[@id="divNavigationTree"]/ul/li[@data-id="SUZ_Procedure"]//span[@class="emr-ui-tree-load-more-content"]')$clickElement()
    Sys.sleep(1)
    remDr$findElement(using="xpath",value='//*[@id="divNavigationTree"]/ul/li[@data-id="SUZ_Procedure"]//span[@data-name="WL_Utility Monitoring Evaluation Report"]')$clickElement()
    Sys.sleep(1)
    remDr$findElement(using="xpath",value='//*[@id="divNavigationTree"]/ul/li[@data-id="SUZ_Procedure"]//span[@data-name="AQMS Evaluation Report"]')$clickElement()
    Sys.sleep(1)    
    source_result<-remDr$getPageSource()[[1]][1]
    source_result<-htmlParse(source_result)
    doc_list<<-xpathSApply(source_result,'//*[contains(text(),"AQMS Evaluation Report")]/parent::*/following::ul/li/div/span',xmlValue) ##All Result
    remDr$close()
}



data_process<-function(){
    library(dplyr)
    GetFileList()
    #               should_result_file_number->|                           |->if No  file->do nothing
    #                                          -->compare all with should-->
    #Fetch_all_result->all_result_file_number->|                           |->if Yes file->|                               |->if Yes->dO NOTHING
    #                                                                                      ->compare wether email is sent--|
    #Add an column to all result indicate wether email is sent or not----------------------|                               |->if No ->send email->Add remark into email remark data.
    
    #' Should: result file number
    if(str_sub(Sys.Date(),6,7) %in% c("01","02","03")) doc_numb_should<-1
    if(str_sub(Sys.Date(),6,7) %in% c("04","05","06")) doc_numb_should<-2
    if(str_sub(Sys.Date(),6,7) %in% c("07","08","09")) doc_numb_should<-3
    if(str_sub(Sys.Date(),6,7) %in% c("10","11","12")) doc_numb_should<-4
    
    #' Indeed: Fetch all result this year
    doc_list_current_year<-doc_list[str_sub(str_extract_all(doc_list,pattern="SZTR\\d{5}"),5,6) %in% str_sub(Sys.Date(),3,4)]
    #' Indeed: Current file number
    doc_numb_current<-length(doc_list_current_year)
    
    #' if no sent history file exist, create a new mail sent histroy object and would be write to hard disk later.
    #' if there's a sent history file exists, merge the sent history to the latest file list object.
    if(!file.exists("~/email_mark.csv")){
      email_mark<-data.frame(file=doc_list_current_year,MailSent=NA,stringsAsFactors = FALSE)
    }else{
      email_mark_temp<-read.csv("~/email_mark.csv",stringsAsFactors = FALSE)
      email_mark<-data.frame(file=doc_list_current_year,stringsAsFactors = FALSE)
      email_mark<-left_join(email_mark,email_mark_temp,by="file")
      rm(email_mark_temp)
    }
    #' email related
    #' email receiver
    receiver_list<-paste("jiang_hong_jun@lilly.com","gong_jie_qiong@lilly.com","tang_li_juan@lilly.com","wang_li_ming@lilly.com","zhu_ming_jie@lilly.com","he_peng@lilly.com","yang_rui_zhen_tracy@lilly.com","zhang_wei_min_x1@lilly.com","zheng_zhao_wei@lilly.com",sep=";",collapse = NULL)                                                                                   #####################config
    
    if(round(doc_numb_current)==round(doc_numb_should) & any(is.na(email_mark$MailSent))){
        doc_name<-email_mark$file[is.na(email_mark$MailSent)]
        email_mark$MailSent[is.na(email_mark$MailSent)]<-"Yes"
        write.csv(email_mark,file="~/email_mark.csv",row.names=FALSE)

        
        #SendMail(receiver_list)        
        library(RDCOMClient)
        content_body<-paste0("Hi all,<br> &nbsp;&nbsp;&nbsp;&nbsp;This email is produced automatically to tells you the utility AQMS periodic report",doc_name,"is effective, please check this information.<br> &nbsp;&nbsp;&nbsp;&nbsp;If you have any question, please contact me by reply this mail.")
        outApp<-COMCreate("Outlook.Application")
        outMail<-outApp$CreateItem(0)
        outMail[["To"]]=receiver_list
        outMail[["subject"]]="AQMS_Utility_Report_Notification"
        outMail[["HTMLbody"]]=content_body
        outMail$Send()
        
    }
}

data_process()