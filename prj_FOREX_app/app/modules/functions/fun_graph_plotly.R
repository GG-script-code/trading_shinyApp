#' https://jtr13.github.io/cc19/technical-analysis-for-stocks-using-plotly.html

# layout parameters
{
  buttons_list<-list(
    list(count=7,label="1w",step="day",stepmode="backward")
    ,list(count=1,label="1m",step="month",stepmode="backward")
    ,list(count=3,label="3m",step="month",stepmode="backward")
    ,list(count=1,label="YTD",step="year",stepmode="todate")
    ,list(step="all")
  )
  
  yaxis_layout_list<-list(autorange=TRUE,fixedrange=FALSE)
  
}

# parameters 
{
  time_delta<-months(6)
}

# candlestick ----
fnc_plotly_FX_candlestick<-function(FX_Selected="CADCHF"
                                    ,FX_Periodicity="daily" # "daily" "weekly"
                                    ,time_Delta=time_delta
                                    ,path_Output_Graphs=path_output_graphs
                                    ,path_Output_Metrics=path_output_metrics
                                    ,buttons_List=buttons_list
                                    ,yaxis_Layout_List=yaxis_layout_list
                                    ,save_Widget=FALSE){
  
  # FX_Selected="CADCHF"; FX_Periodicity="daily"; time_Delta=time_delta; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Candlestick_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                                     ," To : ",max(FX_Metrics_Data$date))
                                        ,rangebreaks=list(list(bounds=list("sat","sun")))
                                        ,rangeslider=list(visible=TRUE)
                                        ,rangeselector=list(buttons=buttons_List))
  }
  
  # CANDLESTICK
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
              ,marker=NULL,legendgroup='two')%>%
    # BBands SMA
    add_trace(x=~date,y=~BB_up_SMA,name='BB_up_SMA',type='scatter',mode='lines'
              ,line=list(color='green',width=1,dash='line')
              ,legendgroup='three')%>%
    add_trace(x=~date,y=~BB_mavg_SMA,name='BB_mavg_SMA',type='scatter',mode='lines'
              ,line=list(color='navy',width=1,dash='line')
              ,legendgroup='three')%>%
    add_trace(x=~date,y=~BB_dn_SMA,name='BB_dn_SMA',type='scatter',mode='lines'
              ,line=list(color='red',width=1,dash='line')
              ,legendgroup='three')%>%
    # BBands EMA
    add_trace(x=~date,y=~BB_up_EMA,name='BB_up_EMA',type='scatter',mode='lines'
              ,line=list(color='darkgreen',width=1,dash='lines+markers')
              ,legendgroup='four')%>%
    add_trace(x=~date,y=~BB_mavg_EMA,name='BB_mavg_EMA',type='scatter',mode='lines'
              ,line=list(color='purple',width=1,dash='lines+markers')
              ,legendgroup='four')%>%
    add_trace(x=~date,y=~BB_dn_EMA,name='BB_dn_EMA',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='lines+markers')
              ,legendgroup='four')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Candlestick_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="candlestick")
           ,paper_bgcolor='black',plot_bgcolor='black')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_candlestick
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_candlestick.html")))
  }
  
  return(plot_candlestick)
  gc()
}

# ichimoku ----
fnc_plotly_FX_ichimoku<-function(FX_Selected="CADCHF"
                                 ,FX_Periodicity="daily" # "daily" "weekly"
                                 ,time_Delta=time_delta
                                 ,path_Output_Graphs=path_output_graphs
                                 ,path_Output_Metrics=path_output_metrics
                                 ,buttons_List=buttons_list
                                 ,yaxis_Layout_List=yaxis_layout_list
                                 ,save_Widget=FALSE){
  
  # FX_Selected="CADCHF"; FX_Periodicity="daily"; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  # ICHIMOKU 
  plot_ichimoku<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=~date,y=~close,name='close',type='scatter',mode='lines'
              ,line=list(color='black',width=1.5,dash='line')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~tenkan_sen,name='tenkan_sen',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='line')
              ,marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~kijun_sen,name='kijun_sen',type='scatter',mode='lines'
              ,line=list(color='purple',width=1,dash='line')
              ,marker=NULL,legendgroup='three')%>%
    add_trace(x=~date,y=~senkou_span_a,name='senkou_span_a',type='scatter',mode='lines'
              ,line=list(color='green',width=1,dash='line')
              ,marker=NULL,legendgroup='four')%>%
    add_trace(x=~date,y=~senkou_span_b,name='senkou_span_b',type='scatter',mode='lines'
              ,line=list(color='red',width=1,dash='line')
              ,marker=NULL,legendgroup='five')%>%
    add_trace(x=~date,y=~chikou_span,name='chikou_span',type='scatter',mode='lines'
              ,line=list(color='blue',width=1,dash='line')
              ,marker=NULL,legendgroup='six')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="ichimoku")
           ,paper_bgcolor='white',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_ichimoku
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_ichimoku.html")))
  }
  
  return(plot_ichimoku)
  gc()
}

# RSI : Relative Strength Index ----
fnc_plotly_FX_RSI<-function(FX_Selected="CADCHF"
                            ,FX_Periodicity="daily" # "daily" "weekly"
                            ,time_Delta=time_delta
                            ,path_Output_Graphs=path_output_graphs
                            ,path_Output_Metrics=path_output_metrics
                            ,buttons_List=buttons_list
                            ,yaxis_Layout_List=yaxis_layout_list
                            ,save_Widget=FALSE){
  
  # FX_Selected="CADCHF"; FX_Periodicity="daily"; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  # RSI
  plot_RSI<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=c(~min(date),~max(date)),y=c(70,70),name='overbought',type='scatter',mode='lines'
              ,line=list(color='green',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(50,50),name='shift',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(30,30),name='oversold',type='scatter',mode='lines'
              ,line=list(color='red',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~RSI_14,name='RSI 14',type='scatter',mode='lines'
              ,line=list(color='black'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~RSI_10,name='RSI 10',type='scatter',mode='lines'
              ,line=list(color='navy'),marker=NULL,legendgroup='three')%>%
    add_trace(x=~date,y=~RSI_20,name='RSI 20',type='scatter',mode='lines'
              ,line=list(color='purple'),marker=NULL,legendgroup='four')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="RSI")
           ,paper_bgcolor='black',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_RSI
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_RSI.html")))
  }
  
  return(plot_RSI)
  gc()
}

# Stochastic ----
fnc_plotly_FX_stoch<-function(FX_Selected="CADCHF"
                              ,FX_Periodicity="daily" # "daily" "weekly"
                              ,time_Delta=time_delta
                              ,path_Output_Graphs=path_output_graphs
                              ,path_Output_Metrics=path_output_metrics
                              ,buttons_List=buttons_list
                              ,yaxis_Layout_List=yaxis_layout_list
                              ,save_Widget=FALSE){
  
  # FX_Selected="EURCHF"; FX_Periodicity="daily"; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  # Stochastic
  plot_stoch<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=c(~min(date),~max(date)),y=c(.80,.80),name='overbought',type='scatter',mode='lines'
              ,line=list(color='green',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(.50,.50),name='shift',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(.20,.20),name='oversold',type='scatter',mode='lines'
              ,line=list(color='red',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~fastK,name='fastK',type='scatter',mode='lines'
              ,line=list(color='coral'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~fastD,name='fastD',type='scatter',mode='lines'
              ,line=list(color='purple'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~stoch_SMA,name='stoch_SMA',type='scatter',mode='lines'
              ,line=list(color='black'),marker=NULL,legendgroup='three')%>%
    add_trace(x=~date,y=~stoch_EMA,name='stoch_EMA',type='scatter',mode='lines'
              ,line=list(color='blue'),marker=NULL,legendgroup='three')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="Stoch")
           ,paper_bgcolor='black',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_stoch
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_stoch.html")))
  }
  
  return(plot_stoch)
  gc()
}

# Average Directional Movement ----
fnc_plotly_FX_ADX<-function(FX_Selected="CADCHF"
                            ,FX_Periodicity="daily" # "daily" "weekly"
                            ,time_Delta=time_delta
                            ,path_Output_Graphs=path_output_graphs
                            ,path_Output_Metrics=path_output_metrics
                            ,buttons_List=buttons_list
                            ,yaxis_Layout_List=yaxis_layout_list
                            ,save_Widget=FALSE){
  
  # FX_Selected="EURCHF"; FX_Periodicity="daily"; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  # ADX
  plot_ADX<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=c(~min(date),~max(date)),y=c(25,25),name='shift',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~DIp,name='+DI',type='scatter',mode='lines'
              ,line=list(color='green'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~DIn,name='-DI',type='scatter',mode='lines'
              ,line=list(color='red'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~DX,name='DX',type='scatter',mode='lines'
              ,line=list(color='coral'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~ADX_SMA,name='ADX_SMA',type='scatter',mode='lines'
              ,line=list(color='black'),marker=NULL,legendgroup='three')%>%
    add_trace(x=~date,y=~ADX_EMA,name='ADX_EMA',type='scatter',mode='lines'
              ,line=list(color='navy'),marker=NULL,legendgroup='three')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="ADX")
           ,paper_bgcolor='black',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_ADX
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_ADX.html")))
  }
  
  return(plot_ADX)
  gc()
}

# Volatility ----
fnc_plotly_FX_volatility<-function(FX_Selected="CADCHF"
                                   ,FX_Periodicity="daily" # "daily" "weekly"
                                   ,time_Delta=time_delta
                                   ,path_Output_Graphs=path_output_graphs
                                   ,path_Output_Metrics=path_output_metrics
                                   ,buttons_List=buttons_list
                                   ,yaxis_Layout_List=yaxis_layout_list
                                   ,save_Widget=FALSE){
  
  # FX_Selected="EURCHF"; FX_Periodicity="daily"; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  # Volatility
  plot_Volatility<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=c(~min(date),~max(date)),y=c(0.1,0.1),name='low',type='scatter',mode='lines'
              ,line=list(color='green',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(0.2,0.2),name='medium',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(0.4,0.4),name='high',type='scatter',mode='lines'
              ,line=list(color='red',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~volatility,name='volatility',type='scatter',mode='lines'
              ,line=list(color='navy'),marker=NULL,legendgroup='two')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="ADX")
           ,paper_bgcolor='black',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_Volatility
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_Volatility.html")))
  }
  
  return(plot_Volatility)
  gc()
}

# MACD : Moving Average Convergence Divergence ----
fnc_plotly_FX_MACD<-function(FX_Selected="CADCHF"
                             ,FX_Periodicity="daily" # "daily" "weekly"
                             ,time_Delta=time_delta
                             ,path_Output_Graphs=path_output_graphs
                             ,path_Output_Metrics=path_output_metrics
                             ,buttons_List=buttons_list
                             ,yaxis_Layout_List=yaxis_layout_list
                             ,save_Widget=FALSE){
  
  # FX_Selected="EURCHF"; FX_Periodicity="daily"; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  # MACD
  plot_MACD<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=~date,y=~histo_SMA,name='histo_SMA',type='bar'
              ,line=list(color='yellow'),marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~macd_SMA,name='macd_SMA',type='scatter',mode='lines'
              ,line=list(color='black'),marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~signal_SMA,name='signal_SMA',type='scatter',mode='lines'
              ,line=list(color='red'),marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~histo_EMA,name='histo_EMA',type='bar'
              ,line=list(color='orange'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~macd_EMA,name='macd_EMA',type='scatter',mode='lines'
              ,line=list(color='navy'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~signal_EMA,name='signal_EMA',type='scatter',mode='lines'
              ,line=list(color='purple'),marker=NULL,legendgroup='two')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="MACD")
           ,paper_bgcolor='black',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_MACD
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_MACD.html")))
  }
  
  return(plot_MACD)
  gc()
}

