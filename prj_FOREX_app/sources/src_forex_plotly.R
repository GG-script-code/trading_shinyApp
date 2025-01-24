#' https://jtr13.github.io/cc19/technical-analysis-for-stocks-using-plotly.html

fnc_plotly_FX_Graphs<-function(output_Graphs_Path=output_graphs_path,output_Metrics_Path=output_metrics_path,save_Widget=FALSE){
  
  # output_Graphs_Path=output_graphs_path; output_Metrics_Path=output_metrics_path
  
  output_Plotly_Path<-file.path(output_Graphs_Path,"plotly")
  
  FX_Metrics_List<-list.files(output_Metrics_Path,pattern=".*Rds")
  
  FX_Graphs_List<-list()
  
  pb<-progress_bar$new(total=(length(FX_Metrics_List))
                       ,format="  downloading [:bar] :percent in :elapsed")
  
  for(FX_Metrics_File in FX_Metrics_List){
    # FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,"CADCHF")%>%which()]
    FX_Metrics_Data<-read_rds(file.path(output_Metrics_Path,FX_Metrics_File))
    FX_file_name<-FX_Metrics_File%>%str_remove_all("[.]Rds")
    FX_name<-FX_file_name%>%str_extract("[a-zA-Z]{6}")
    # layout parameters
    {
      buttons_list<-list(
        list(count=1,label="1m",step="month",stepmode="backward")
        ,list(count=3,label="3m",step="month",stepmode="backward")
        ,list(count=6,label="6m",step="month",stepmode="backward")
        ,list(count=1,label="YTD",step="year",stepmode="todate")
        ,list(step="all")
      )
      
      xaxis_candlestick_layout_list<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                                       ," To : ",max(FX_Metrics_Data$date))
                                          ,rangebreaks=list(list(bounds=list("sat","sun")))
                                          ,rangeslider=list(visible=TRUE)
                                          ,rangeselector=list(buttons=buttons_list))
      
      xaxis_lines_layout_list<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                                 ," To : ",max(FX_Metrics_Data$date))
                                    #,rangebreaks=list(list(bounds=list("sat","mon")))
                                    ,rangeslider=list(visible=TRUE)
                                    ,rangeselector=list(buttons=buttons_list))
      
      yaxis_layout_list<-list(autorange=TRUE,fixedrange=FALSE)
      
    }
    
    # CANDLESTICK --------------------------------------------------------------
    plot_candlestick<-plot_ly(FX_Metrics_Data)%>%
      add_trace(x=~date,open=~open,high=~high,low=~low,close=~close
                ,name='OHLC',type='candlestick'
                ,increasing=list(line=list(color='green',width=2)
                                 ,fillcolor='rgba(0,0,0,0)') # Transparent
                ,decreasing=list(line=list(color='red',width=2)
                                 ,fillcolor='rgba(0,0,0,0)') # Transparent
                ,legendgroup='one')%>%
      add_trace(x=~date,y=~close,name='close',type='scatter',mode='lines'
                ,line=list(color='white',width=0.5,dash='line')
                ,marker=NULL,width=1,legendgroup='two')%>%
      layout(showlegend=T,title=FX_name
             ,xaxis=xaxis_candlestick_layout_list,yaxis=list.append(yaxis_layout_list,title="candlestick")
             ,paper_bgcolor='black',plot_bgcolor='black')
    
    if(save_Widget){
      htmlwidgets::saveWidget(plot_candlestick
                              ,file=file.path(output_Plotly_Path,paste0("plot_",FX_name,"_candlestick.html")))
    }
    
    FX_Graphs_List[[FX_name]][["candlestick"]]<-plot_candlestick
    
    # ICHIMOKU -----------------------------------------------------------------
    plot_ichimoku<-plot_ly(FX_Metrics_Data)%>%
      add_trace(x=~date,y=~close,name='close',type='scatter',mode='lines'
                ,line=list(color='black',width=1.5,dash='line')
                ,marker=NULL,width=1,legendgroup='one')%>%
      add_trace(x=~date,y=~tenkan_sen,name='tenkan_sen',type='scatter',mode='lines'
                ,line=list(color='orange',width=1,dash='line')
                ,marker=NULL,width=1,legendgroup='two')%>%
      add_trace(x=~date,y=~kijun_sen,name='kijun_sen',type='scatter',mode='lines'
                ,line=list(color='purple',width=1,dash='line')
                ,marker=NULL,width=1,legendgroup='two')%>%
      add_trace(x=~date,y=~senkou_span_a,name='senkou_span_a',type='scatter',mode='lines'
                ,line=list(color='red',width=1,dash='line')
                ,marker=NULL,width=1,legendgroup='two')%>%
      add_trace(x=~date,y=~senkou_span_b,name='senkou_span_b',type='scatter',mode='lines'
                ,line=list(color='blue',width=1,dash='line')
                ,marker=NULL,width=1,legendgroup='two')%>%
      add_trace(x=~date,y=~chikou_span,name='chikou_span',type='scatter',mode='lines'
                ,line=list(color='green',width=1,dash='line')
                ,marker=NULL,width=1,legendgroup='two')%>%
      layout(showlegend=T,title=FX_name
             ,xaxis=xaxis_lines_layout_list,yaxis=list.append(yaxis_layout_list,title="ichimoku")
             ,paper_bgcolor='white',plot_bgcolor='white')

    if(save_Widget){
      htmlwidgets::saveWidget(plot_ichimoku
                              ,file=file.path(output_Plotly_Path,paste0("plot_",FX_name,"_ichimoku.html")))
    }
    
    FX_Graphs_List[[FX_name]][["ichimoku"]]<-plot_ichimoku
    # RSI ----------------------------------------------------------------------
    plot_RSI<-plot_ly(FX_Metrics_Data)%>%
      add_trace(x=c(~min(date),~max(date)),y=c(70,70),name='overbought',type='scatter',mode='lines'
                ,line=list(color='green',width=1,dash='dot')
                ,marker=NULL,legendgroup='one')%>%
      add_trace(x=c(~min(date),~max(date)),y=c(50,50),name='shift',type='scatter',mode='lines'
                ,line=list(color='orange',width=1,dash='dot')
                ,marker=NULL,legendgroup='one')%>%
      add_trace(x=c(~min(date),~max(date)),y=c(30,30),name='oversell',type='scatter',mode='lines'
                ,line=list(color='red',width=1,dash='dot')
                ,marker=NULL,legendgroup='one')%>%
      add_trace(x=~date,y=~RSI_14,name='RSI 14',type='scatter',mode='lines'
                ,line=list(color='black'),marker=NULL,legendgroup='two')%>%
      add_trace(x=~date,y=~RSI_10,name='RSI 10',type='scatter',mode='lines'
                ,line=list(color='navy'),marker=NULL,legendgroup='three')%>%
      add_trace(x=~date,y=~RSI_20,name='RSI 20',type='scatter',mode='lines'
                ,line=list(color='purple'),marker=NULL,legendgroup='four')%>%
      layout(showlegend=T,title=FX_name
             ,xaxis=xaxis_lines_layout_list,yaxis=list.append(yaxis_layout_list,title="RSI")
             ,paper_bgcolor='black',plot_bgcolor='white')
    
    if(save_Widget){
      htmlwidgets::saveWidget(plot_RSI
                              ,file=file.path(output_Plotly_Path,paste0("plot_",FX_name,"_RSI.html")))
    }
    
    FX_Graphs_List[[FX_name]][["RSI"]]<-plot_RSI
    
    pb$tick()
  }
  
  return(FX_Graphs_List)
  gc()
}

# plotly_graphs<-fnc_plotly_FX_Graphs(save_Widget=FALSE)
# 
# plotly_graphs$CADCHF$RSI
