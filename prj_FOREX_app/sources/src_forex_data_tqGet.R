
# Download FOREX data using the tidyquant::tq_get() ----
fnc_tqGet_FX_Data<-function(FX_List=FX_list,path_Input=path_input_tqGet
                            ,last_Week_Delete=(Sys.Date()-7)
                            ,yesterday_Date=(Sys.Date()-1)){
  # FX_List=FX_list; path_Input=path_input_tqGet; last_Week_Delete=(Sys.Date()-7); yesterday_Date=(Sys.Date()-1)

  while(wday(yesterday_Date,week_start=1)%in%c(6,7)){
    yesterday_Date<-yesterday_Date%m-%days(1)
  }
  
  if(wday(yesterday_Date,week_start=1)%notin%c(6,7)){
    
    for(Periodicity in c("daily", "weekly")){
      # Periodicity<-"weekly"
      
      FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".*Rds")
      
      pb<-progress_bar$new(total=length(FX_List))
      
      for(FX_Symbol in names(FX_List)){
        # FX_Symbol<-"CADCHF"
        
        if(paste0(FX_Symbol,"_",Periodicity,".Rds")%in%FX_Data_List){
          
          FX_Data<-read_rds(file.path(path_Input,Periodicity,paste0(FX_Symbol,"_",Periodicity,".Rds")))%>%
            filter(!date%>%is.na())%>%
            filter(date<last_Week_Delete)%>%
            select(c("symbol","date","open","high","low","close","adjusted"))%>%
            na.omit()
          
          if(yesterday_Date>=(max(FX_Data$date)+1)){
            
            FX_Symbol_Yahoo<-paste0(FX_Symbol,"=X")
            FX_Data_Get<-tq_get(FX_Symbol_Yahoo
                                ,get="stock.prices"
                                ,from=(max(FX_Data$date))
                                ,to=yesterday_Date
                                ,periodicity=Periodicity)%>%
              mutate(symbol=FX_Symbol)%>%
              select(c("symbol","date","open","high","low","close","adjusted"))
            
            FX_Data<-bind_rows(FX_Data,FX_Data_Get)%>%
              arrange(date)%>%
              distinct()%>%
              filter(!date%>%is.na())%>%
              select(c("symbol","date","open","high","low","close","adjusted"))%>%
              na.omit()
            
          }
          
        }else{
          
          FX_Data<-tibble(symbol=as.character(NA)
                          ,date=as.Date(NA)
                          ,open=as.numeric(NA)
                          ,high=as.numeric(NA)
                          ,low=as.numeric(NA)
                          ,close=as.numeric(NA)
                          ,adjusted=as.numeric(NA)
          )
          
          # if(Periodicity=="daily"){
          #   from_Date<-ymd("2024-01-01")
          # }else if(Periodicity=="weekly"){
          #   from_Date<-ymd("2020-01-01")
          # }else if(Periodicity=="monthly"){
          #   from_Date<-ymd("2000-01-01")
          # }
          
          from_Date<-ymd("2000-01-01")

          FX_Symbol_Yahoo<-paste0(FX_Symbol,"=X")
          FX_Data_Get<-tq_get(FX_Symbol_Yahoo
                              ,get="stock.prices"
                              ,from=from_Date
                              ,to=yesterday_Date
                              ,periodicity=Periodicity)%>%
            mutate(symbol=FX_Symbol)%>%
            select(c("symbol","date","open","high","low","close","adjusted"))
          
          FX_Data<-bind_rows(FX_Data,FX_Data_Get)%>%
            arrange(date)%>%
            distinct()%>%
            filter(!date%>%is.na())%>%
            select(c("symbol","date","open","high","low","close","adjusted"))%>%
            na.omit()
          
        }
        
        saveRDS(FX_Data,file.path(path_Input,Periodicity,paste0(FX_Symbol,"_",Periodicity,".Rds")))
        
        pb$tick()
      }
      print(paste0("FX ",Periodicity," symbols updated!"))
    }
    
  }else{
    
    print("The FOREX market is closed during Saturday/Sunday!")
    
  }
  
  return("FX symbols updated!")
  
}

# fnc_tqGet_FX_Data()


