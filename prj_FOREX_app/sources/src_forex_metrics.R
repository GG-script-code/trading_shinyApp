
# Compute FOREX metrics ----
fnc_metrics_FX_Data<-function(path_Input=path_input_tqGet,path_Output=path_output_metrics,overwrite_Metrics=TRUE){
  # path_Input=path_input_tqGet; path_Output=path_output_metrics; overwrite_Metrics=TRUE
  
  for(Periodicity in c("daily","weekly")){
    # Periodicity<-"daily"
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".*Rds")
    # list FOREX metrics COMPUTED
    FX_Metrics_List<-list.files(file.path(path_Output,Periodicity),pattern=".*Rds")
    
    if(overwrite_Metrics){
      FX_Metrics_List<-paste0("overwrite_",FX_Metrics_List)
    }
    
    pb<-progress_bar$new(total=(length(FX_Data_List)))
    
    for(FX_Dataset in FX_Data_List){
      # FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"EURCHF")%>%which()]
      
      print(FX_Dataset)
      
      # Upload the data for the FX symbol
      FX_Data<-read_rds(file.path(path_Input,Periodicity,FX_Dataset))
      # Initialize the name of the new FX metrics file
      FX_Metrics_File<-paste0(max(FX_Data$date)%>%str_remove_all("-"),"_",FX_Dataset)
      
      # If the new file is not in the FX matrics list,compute the metrics with the new data
      if(FX_Metrics_File%notin%FX_Metrics_List){
        
        # Compute the metrics
        FX_Data%<>%
          filter_at(vars(open,high,low,close,adjusted),any_vars(!is.na(.)))%>%
          # ichimoku
          mutate(nine_period_high=rollapplyr(high,9,max,fill=NA)
                 ,nine_period_low=rollapplyr(low,9,min,fill=NA)
                 # Tenkan-sen (Conversion Line): (9-period high+9-period low)/2))
                 ,tenkan_sen=(nine_period_high+nine_period_low)/2
                 ,period26_high=rollapplyr(high,26,max,fill=NA)
                 ,period26_low=rollapplyr(low,26,min,fill=NA)
                 # Kijun-sen (Base Line): (26-period high+26-period low)/2))
                 ,kijun_sen=(period26_high+period26_low)/2
                 # Senkou Span A (Leading Span A): (Conversion Line+Base Line)/2))
                 ,senkou_span_a=lag((tenkan_sen+kijun_sen)/2,26)
                 ,period52_high=rollapplyr(high,52,max,fill=NA)
                 ,period52_low=rollapplyr(low,52,min,fill=NA)
                 # Senkou Span B (Leading Span B): (52-period high+52-period low)/2))
                 ,senkou_span_b=lag((period52_high+period52_low)/2,26)
                 # The most current closing price plotted 26 time periods behind (optional)
                 ,chikou_span=lag(close,26))%>%
          select(-c(nine_period_high,nine_period_low
                    ,period26_high,period26_low
                    ,period52_high,period52_low))%>%
          # Relative Strenght Index
          tq_mutate(select=close,mutate_fun=RSI,n=10,col_rename="RSI_10")%>%
          tq_mutate(select=close,mutate_fun=RSI,n=14,col_rename="RSI_14")%>%
          tq_mutate(select=close,mutate_fun=RSI,n=20,col_rename="RSI_20")%>%
          # MACD
          tq_mutate(select=c("close"),mutate_fun=MACD,maType=SMA,nFast=12,nSlow=26,nSig=9,percent=TRUE)%>%
          rename("macd_SMA"="macd","signal_SMA"="signal")%>%
          mutate(histo_SMA=macd_SMA-signal_SMA)%>%
          tq_mutate(select=c("close"),mutate_fun=MACD,maType=EMA,nFast=12,nSlow=26,nSig=9,percent=TRUE)%>%
          rename("macd_EMA"="macd","signal_EMA"="signal")%>%
          mutate(histo_EMA=macd_EMA-signal_EMA)%>%
          # Bollinger Bands
          tq_mutate(select=close,mutate_fun=BBands,maType=SMA,n=2,sd=2)%>%
          rename("BB_dn_SMA"="dn","BB_mavg_SMA"="mavg","BB_up_SMA"="up","BB_pctB_SMA"="pctB")%>%
          tq_mutate(select=close,mutate_fun=BBands,maType=EMA,n=2,sd=2)%>%
          rename("BB_dn_EMA"="dn","BB_mavg_EMA"="mavg","BB_up_EMA"="up","BB_pctB_EMA"="pctB")%>%
          # Stochastic Index
          tq_mutate(select=c("high","low","close"),mutate_fun=stoch,maType=SMA,nFastK=20,nFastD=5,nSlowD=5,smooth=1)%>%
          select(-c(fastK,fastD))%>%
          rename("stoch_SMA"="stoch")%>%
          tq_mutate(select=c("high","low","close"),mutate_fun=stoch,maType=EMA,nFastK=20,nFastD=5,nSlowD=5,smooth=1)%>%
          rename("stoch_EMA"="stoch")%>%
          # Average Directional Movement Index
          tq_mutate(select=c("high","low","close"),mutate_fun=ADX,maType=SMA,n=14)%>%
          rename("ADX_SMA"="ADX")%>%
          select(-c(DIp,DIn,DX))%>%
          tq_mutate(select=c("high","low","close"),mutate_fun=ADX,maType=EMA,n=14)%>%
          rename("ADX_EMA"="ADX")%>%
          # Volatility
          tq_mutate(select=c("open","high","low","close"),mutate_fun=volatility,maType=SMA,n=10,calc="yang.zhang",N=260,mean0=FALSE,col_rename="volatility")
        
        saveRDS(FX_Data,file.path(path_Output,Periodicity,FX_Metrics_File))
        
      }else{
        # The FX metrics are updated and I have to update the FX data file
        if(FX_Metrics_File%in%FX_Metrics_List){
          message("METRICS: the latest computed dataset is the ",FX_Metrics_File," !!")
        }
      }
      
      #remove old metrics if the Max date in the FX data is bigger than the old one
      {
        old_FX_File_List<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_Dataset)%>%which()]
        
        older_FX_File_List<-old_FX_File_List[(old_FX_File_List<FX_Metrics_File)%>%which()]
        
        if(length(older_FX_File_List)>0){
          
          for(old_FX_File in older_FX_File_List){
            
            old_FX_File_Date<-old_FX_File%>%str_extract("[0-9]{8}")%>%ymd()
            
            if(max(FX_Data$date)>ymd(old_FX_File_Date)){
              file.remove(file.path(path_Output,Periodicity,old_FX_File))
              message(old_FX_File," removed \1\n")
            }else{
              message("NO older file in the metrics")
            }
          }
        }
      }
      pb$tick()
    }
    print(paste0("FX ",Periodicity," metrics updated!"))
  }
  
  return("FX metrics updated!")
  
}

# fnc_metrics_FX_Data()


# Compute FOREX correlation ----
fnc_correlation_FX_Data<-function(path_Input=path_input_tqGet,path_Output=path_output_metrics){
  # path_Input=path_input_tqGet; path_Output=path_output_metrics
  
  for(Periodicity in c("daily","weekly")){
    # Periodicity<-"weekly"
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".*Rds")
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".*Rds")
    
    df_Dates<-tibble(date=as.Date(NA))
    
    for(FX_Dataset in FX_Data_List){
      #FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"CADCHF")%>%which()]
      
      # Upload the data for the FX symbol
      FX_Data<-read_rds(file.path(path_Input,Periodicity,FX_Dataset))
      
      df_Dates<-bind_rows(df_Dates,FX_Data["date"])
    }
    
    df_Dates<-df_Dates%>%
      filter(!date%>%is.na())%>%
      group_by(date)%>%
      summarise(n=n())
    
    if(Periodicity=="daily"){
      from_date_filter<-(Sys.Date()%m-%months(6))
    }else if(Periodicity=="weekly"){
      from_date_filter<-(Sys.Date()%m-%months(24))
    }else if(Periodicity=="monthly"){
      from_date_filter<-(Sys.Date()%m-%months(48))
    }
    
    df_Dates_flt<-df_Dates%>%
      filter(n==length(FX_Data_List))%>%
      filter(date>=from_date_filter)%>%
      select(date)
    
    for(FX_Dataset in FX_Data_List){
      #FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"CADCHF")%>%which()]
      
      # Upload the data for the FX symbol
      FX_Data<-read_rds(file.path(path_Input,Periodicity,FX_Dataset))
      
      df_Dates_flt<-df_Dates_flt%>%
        na.omit()%>%
        left_join(FX_Data[,c("date","adjusted")],by=join_by("date"))
      
      colnames(df_Dates_flt)[which(colnames(df_Dates_flt)=="adjusted")]<-FX_Data$symbol%>%unique()
    }
    
    fromDate<-min(df_Dates_flt$date)
    toDate<-max(df_Dates_flt$date)
    
    FX_data_corr_pearson<-df_Dates_flt%>%
      select(-c(date))%>%
      cor(method="pearson")%>%
      round(1)
    
    saveRDS(list(corr=FX_data_corr_pearson
                 ,from_to_dates=c(fromDate, toDate))
            ,file.path(path_Output,Periodicity
                       ,paste0("corr_pearson_",Periodicity,".Rds")))
    
    FX_data_corr_kendall<-df_Dates_flt%>%
      select(-c(date))%>%
      cor(method="kendall")%>%
      round(1)
    
    saveRDS(list(corr=FX_data_corr_kendall
                 ,from_to_dates=c(fromDate, toDate))
            ,file.path(path_Output,Periodicity
                       ,paste0("corr_kendall_",Periodicity,".Rds")))
    
    FX_data_corr_spearman<-df_Dates_flt%>%
      select(-c(date))%>%
      cor(method="spearman")%>%
      round(1)
    
    saveRDS(list(corr=FX_data_corr_spearman
                 ,from_to_dates=c(fromDate, toDate))
            ,file.path(path_Output,Periodicity
                       ,paste0("corr_spearman_",Periodicity,".Rds")))
    
    message(Periodicity," correlation computed!!!")
  }
  return("Correlation computed!!!")
}



