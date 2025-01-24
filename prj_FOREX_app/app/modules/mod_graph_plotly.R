
source(file.path(path_functions,"fun_graph_plotly.R"))

#-------------------------------------------------------------------------------

graph_plotly_UI<-function(id){
  ns<-NS(id,"graph_plotly_output")
  
  plotly::plotlyOutput(ns,width="100%",height="500px")
  
}

graph_plotly_server<-function(id,FX_Selected,FX_Periodicity,save_Widget,grp_Type="FX_candlestick"){
  moduleServer(id,function(input,output,session){
    
    ns<-session$ns
    
    output$graph_plotly_output<-plotly::renderPlotly({
      
      if(FX_Periodicity=="daily"){
        
        if(grp_Type=="FX_candlestick"){
          fnc_plotly_FX_candlestick(FX_Selected=FX_Selected()
                                    ,FX_Periodicity="daily"
          )
        }else if(grp_Type=="FX_ichimoku"){
          fnc_plotly_FX_ichimoku(FX_Selected=FX_Selected()
                                 ,FX_Periodicity="daily"
          )
        }else if(grp_Type=="FX_rsi"){
          fnc_plotly_FX_RSI(FX_Selected=FX_Selected()
                            ,FX_Periodicity="daily"
          )
        }else if(grp_Type=="FX_stoch"){
          fnc_plotly_FX_stoch(FX_Selected=FX_Selected()
                              ,FX_Periodicity="daily"
          )
        }else if(grp_Type=="FX_adx"){
          fnc_plotly_FX_ADX(FX_Selected=FX_Selected()
                            ,FX_Periodicity="daily"
          )
        }else if(grp_Type=="FX_volatility"){
          fnc_plotly_FX_volatility(FX_Selected=FX_Selected()
                                   ,FX_Periodicity="daily"
          )
        }else if(grp_Type=="FX_macd"){
          fnc_plotly_FX_MACD(FX_Selected=FX_Selected()
                             ,FX_Periodicity="daily"
          )
        }
        
      }else if(FX_Periodicity=="weekly"){
        
        if(grp_Type=="FX_candlestick"){
          fnc_plotly_FX_candlestick(FX_Selected=FX_Selected()
                                    ,FX_Periodicity="weekly"
          )
        }else if(grp_Type=="FX_ichimoku"){
          fnc_plotly_FX_ichimoku(FX_Selected=FX_Selected()
                                 ,FX_Periodicity="weekly"
          )
        }else if(grp_Type=="FX_rsi"){
          fnc_plotly_FX_RSI(FX_Selected=FX_Selected()
                            ,FX_Periodicity="weekly"
          )
        }else if(grp_Type=="FX_stoch"){
          fnc_plotly_FX_stoch(FX_Selected=FX_Selected()
                              ,FX_Periodicity="weekly"
          )
        }else if(grp_Type=="FX_adx"){
          fnc_plotly_FX_ADX(FX_Selected=FX_Selected()
                            ,FX_Periodicity="weekly"
          )
        }else if(grp_Type=="FX_volatility"){
          fnc_plotly_FX_volatility(FX_Selected=FX_Selected()
                                   ,FX_Periodicity="weekly"
          )
        }else if(grp_Type=="FX_macd"){
          fnc_plotly_FX_MACD(FX_Selected=FX_Selected()
                             ,FX_Periodicity="weekly"
          )
        }
        
      }
      
    })
  })
}
