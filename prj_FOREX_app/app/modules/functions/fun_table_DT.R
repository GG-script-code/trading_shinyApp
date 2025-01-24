#https://rstudio.github.io/DT/

css_styling_FTP_complete<-JS(
  "function(settings,json) {",
  "$(this.api().table().header()).css({'background-color': '#2668B2','color': '#fff'});",
  "$(this.api().table().rows().nodes()).each(function(index) {
    if (index % 2 === 0) {
      $(this).css({'background-color': '#000000','color': '#fff'});
    } else {
      $(this).css({'background-color': '#FF0000','color': '#fff'});
    }
  });",
  "$(this).find('td:nth-child(1)').css({'background-color': '#2668B2','color': '#fff'});",
  "$(this).find('td:nth-child(2)').css('background-color','#007FFF');",
  "$(this).find('td:nth-child(3)').css('background-color','#0055FF');",
  "$(this).find('td:nth-child(4)').css('background-color','#002BFF');",
  "}")

fun_DT_FX_summary<-function(FX_Periodicity="daily" # "daily" "weekly"
                            ,path_Output_Tables=path_output_tables
                            ,path_Output_Metrics=path_output_metrics
                            ,css_Styling_FTP_Complete=css_styling_FTP_complete
                            ,save_Widget=FALSE){
  
  # FX_Periodicity="daily";path_Output_Tables=path_output_tables;path_Output_Metrics=path_output_metrics;css_Styling_FTP_Complete=css_styling_FTP_complete
  
  path_Output_DT<-file.path(path_Output_Tables,"DT",FX_Periodicity)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  
  FX_data_summary<-tibble(symbol=as.character(NA)
                          ,from=as.character(NA)
                          ,to=as.character(NA)
                          ,close=as.numeric(NA)
                          ,close_D=as.numeric(NA)
                          ,close_Perc=as.character(NA)
                          #,close_S=as.numeric(NA)
                          ,RSI10=as.numeric(NA)
                          #,RSI10_S=as.numeric(NA)
                          ,RSI14=as.numeric(NA)
                          #,RSI14_S=as.numeric(NA)
                          ,RSI20=as.numeric(NA)
                          #,RSI20_S=as.numeric(NA)
                          ,stoch_SMA=as.numeric(NA)
                          #,stoch_SMA_S=as.numeric(NA)
                          ,stoch_EMA=as.numeric(NA)
                          #,stoch_EMA_S=as.numeric(NA)
                          ,adx_SMA=as.numeric(NA)
                          #,adx_SMA_S=as.numeric(NA)
                          ,adx_EMA=as.numeric(NA)
                          #,adx_EMA_S=as.numeric(NA)
                          ,volatility=as.numeric(NA)
                          #,volatility_S=as.numeric(NA)
  )
  
  for(FX_Metrics_File in FX_Metrics_List){
    # FX_selected="GBPUSD";
    # FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_selected)%>%which()]
    
    FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
      #arrange(date%>%desc())%>%
      slice_tail(n=10)
    
    dates_from_to<-FX_Metrics_Data$date%>%last(2)
    
    (fx_close<-FX_Metrics_Data$close%>%last(1))
    last_two_observations<-FX_Metrics_Data$close%>%last(2)
    (fx_close_delta<-(last_two_observations[2]-last_two_observations[1]))%>%round(digits=4)
    delta_perc<-(((last_two_observations[2]/last_two_observations[1])-1)*100)%>%round(digits=2)
    (fx_close_delta_perc<-paste0(delta_perc,"%"))
    index_lm<-(1:3)
    fx_lm<-stats::lm(FX_Metrics_Data$close%>%last(3)~index_lm)
    (fx_close_slope<-coef(fx_lm)[2])
    
    (fx_rsi10<-FX_Metrics_Data$RSI_10%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$RSI_10%>%last(3)~index_lm)
    (fx_rsi10_slope<-coef(fx_lm)[2])
    (fx_rsi14<-FX_Metrics_Data$RSI_14%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$RSI_14%>%last(3)~index_lm)
    (fx_rsi14_slope<-coef(fx_lm)[2])
    (fx_rsi20<-FX_Metrics_Data$RSI_20%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$RSI_20%>%last(3)~index_lm)
    (fx_rsi20_slope<-coef(fx_lm)[2])
    
    (fx_stochSMA<-FX_Metrics_Data$stoch_SMA%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$stoch_SMA%>%last(3)~index_lm)
    (fx_stochSMA_slope<-coef(fx_lm)[2])
    (fx_stochEMA<-FX_Metrics_Data$stoch_EMA%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$stoch_EMA%>%last(3)~index_lm)
    (fx_stochEMA_slope<-coef(fx_lm)[2])
    
    (fx_adxSMA<-FX_Metrics_Data$ADX_SMA%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$ADX_SMA%>%last(3)~index_lm)
    (fx_adxSMA_slope<-coef(fx_lm)[2])
    (fx_adxEMA<-FX_Metrics_Data$ADX_EMA%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$ADX_EMA%>%last(3)~index_lm)
    (fx_adxEMA_slope<-coef(fx_lm)[2])
    
    (fx_volatility<-(FX_Metrics_Data$volatility%>%last(1))*100)
    fx_lm<-stats::lm(FX_Metrics_Data$volatility%>%last(3)~index_lm)
    (fx_volatility_slope<-(coef(fx_lm)[2])*100)
    
    FX_data_summary%<>%add_row(tibble_row(symbol=FX_Metrics_Data$symbol%>%unique()
                                          ,from=dates_from_to[1]%>%format( "%d/%m")
                                          ,to=dates_from_to[2]%>%format( "%d/%m")
                                          ,close=fx_close
                                          ,close_D=fx_close_delta
                                          ,close_Perc=fx_close_delta_perc
                                          #,close_S=fx_close_slope
                                          ,RSI10=fx_rsi10
                                          #,RSI10_S=fx_rsi10_slope
                                          ,RSI14=fx_rsi14
                                          #,RSI14_S=fx_rsi14_slope
                                          ,RSI20=fx_rsi20
                                          #,RSI20_S=fx_rsi20_slope
                                          ,stoch_SMA=fx_stochSMA
                                          #,stoch_SMA_S=fx_stochSMA_slope
                                          ,stoch_EMA=fx_stochEMA
                                          #,stoch_EMA_S=fx_stochEMA_slope
                                          ,adx_SMA=fx_adxSMA
                                          #,adx_SMA_S=fx_adxSMA_slope
                                          ,adx_EMA=fx_adxEMA
                                          #,adx_EMA_S=fx_adxEMA_slope
                                          ,volatility=fx_volatility
                                          #,volatility_S=fx_volatility_slope
    ))%>%
      na.omit()%>%
      arrange(symbol)%>%
      distinct()
  }
  
  table_DT_FX_summary<-FX_data_summary%>%
    rename("FX"="symbol","cls"="close","cls %"="close_Perc","cls D"="close_D"
           ,"rsi10"="RSI10","rsi14"="RSI14","rsi20"="RSI20"
           ,"stochSMA"="stoch_SMA","stochEMA"="stoch_EMA"
           ,"adxSMA"="adx_SMA","adxEMA"="adx_EMA"
           ,"vol"="volatility"
    )%>%
    formattable::formattable()%>%
    formattable::as.datatable(class='compact cell-border stripe'
                              ,extensions=c('Responsive','Buttons')
                              ,rownames=FALSE
                              ,options=list(autoWidth=T,paging=F,scrollX=F,searching=T,ordering=T
                                            ,columnDefs=list(list(className='dt-center'
                                                                  ,targets=0:(ncol(FX_data_summary)-1)))
                                            ,buttons=list('copy'
                                                          ,list(extend='collection'
                                                                ,buttons=c('csv','excel','pdf')
                                                                ,text='Download'))
                                            ,dom='tr'
                                            ,searchHighlight=FALSE
                                            ,initComplete=css_Styling_FTP_Complete)
                              ,caption=htmltools::tags$caption(style='caption-side: top
                                                               ;text-align: center
                                                               ;color:black
                                                               ;font-size:100%;'
                                                               ,paste0('Summary table'))
    )%>%
    formatRound(columns=c("cls","cls D"),digits=4)%>%
    formatRound(columns=c("rsi10","rsi14","rsi20"
                          ,"stochSMA","stochEMA"
                          ,"adxSMA","adxEMA"
                          ,"vol"),digits=1)
  
  if(save_Widget){
    htmlwidgets::saveWidget(table_DT_FX_summary
                            ,file=file.path(path_Output_DT,FX_Periodicity
                                            ,paste0("table_",FX_Periodicity,"_FX_summary.html")))
  }
  
  return(table_DT_FX_summary)
}
#-------------------------------------------------------------------------------

