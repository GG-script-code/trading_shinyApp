#'
rm(list=ls())
gc()
options(warn=-1)

#   Variables ----
path_folder<<-getwd()

#   Sources ----
{
    source(file.path(path_folder, "parameters", "prm_paths.R"))
    source(file.path(path_parameters, "prm_FX_list.R"))
    
    source(file.path(path_documentation, "doc_FX_academy_scripts.R"))
    
    source(file.path(path_packages, "pkg_install_packages.R"))
    
    source(file.path(path_sources, "src_useful_functions.R"))
    source(file.path(path_sources, "src_forex_tqGet.R"))
    source(file.path(path_sources, "src_forex_metrics.R"))
    source(file.path(path_sources, "src_forex_plotly.R"))
}

#   DATA ----
##' Update the FX data
fnc_tqGet_FX_Data(last_Week_Delete=(Sys.Date()-7),yesterday_Date=(Sys.Date()-1))
##' Update the FX metrics
fnc_metrics_FX_Data(overwrite_Metrics=F)
fnc_correlation_FX_Data()

# Modules ----
source(file.path(path_modules, "mod_graph_ggplot.R"))
source(file.path(path_modules, "mod_graph_plotly.R"))
source(file.path(path_modules, "mod_table_DT.R"))

# UI ----
source(file.path(path_app, "ui_Header.R"))
source(file.path(path_app, "ui_Sidebar.R"))
source(file.path(path_app, "ui_Body.R"))
#source(file.path(path_app, "src_dbControlbar.R"))
#source(file.path(path_app, "src_dbFooter.R"))

ui<-shinydashboardPlus::dashboardPage(
    header
    ,sidebar
    ,body
    ,tags$head(
        tags$style(HTML(".main-sidebar {background-color: #00AED1 !important;}"))
    )
    ,controlbar=NULL
    ,footer=NULL
    ,skin=c("blue-light")
    ,freshTheme=NULL
    ,preloader=NULL
    ,md=FALSE
    ,options=list(sidebarExpandOnHover=TRUE)
    ,scrollToTop=TRUE
)

# SERVER ----
server<-function(input, output, session){
    #> header
    
    #> body
    # FX_summary_tab------------------------------------------------------------
    table_DT_server("tbl_DT_FX_summary_d1"
                    ,FX_Periodicity="daily"
                    ,tbl_Type="FX_summary")
    table_DT_server("tbl_DT_FX_summary_w1"
                    ,FX_Periodicity="weekly"
                    ,tbl_Type="FX_summary")
    
    graph_ggplot_server("grp_ggplot_FX_correlation_d1"
                        ,FX_Periodicity="daily"
                        ,grp_Type="FX_correlation")
    graph_ggplot_server("grp_ggplot_FX_correlation_w1"
                        ,FX_Periodicity="weekly"
                        ,grp_Type="FX_correlation")
    # FX_daily_tab--------------------------------------------------------------
    graph_plotly_server("grp_plotly_FX_candlestick_d1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_d1)%>%
                            bindCache(input$slcInp_FXrate_d1)%>%
                            bindEvent(input$buttInp_getFXgraphs_d1)
                        ,FX_Periodicity="daily"
                        ,grp_Type="FX_candlestick"
    )
    graph_plotly_server("grp_plotly_FX_rsi_d1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_d1)%>%
                            bindCache(input$slcInp_FXrate_d1)%>%
                            bindEvent(input$buttInp_getFXgraphs_d1)
                        ,FX_Periodicity="daily"
                        ,grp_Type="FX_rsi"
    )
    graph_plotly_server("grp_plotly_FX_ichimoku_d1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_d1)%>%
                            bindCache(input$slcInp_FXrate_d1)%>%
                            bindEvent(input$buttInp_getFXgraphs_d1)
                        ,FX_Periodicity="daily"
                        ,grp_Type="FX_ichimoku"
    )
    graph_plotly_server("grp_plotly_FX_stoch_d1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_d1)%>%
                            bindCache(input$slcInp_FXrate_d1)%>%
                            bindEvent(input$buttInp_getFXgraphs_d1)
                        ,FX_Periodicity="daily"
                        ,grp_Type="FX_stoch"
    )
    graph_plotly_server("grp_plotly_FX_adx_d1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_d1)%>%
                            bindCache(input$slcInp_FXrate_d1)%>%
                            bindEvent(input$buttInp_getFXgraphs_d1)
                        ,FX_Periodicity="daily"
                        ,grp_Type="FX_adx"
    )
    graph_plotly_server("grp_plotly_FX_volatility_d1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_d1)%>%
                            bindCache(input$slcInp_FXrate_d1)%>%
                            bindEvent(input$buttInp_getFXgraphs_d1)
                        ,FX_Periodicity="daily"
                        ,grp_Type="FX_volatility"
    )
    graph_plotly_server("grp_plotly_FX_macd_d1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_d1)%>%
                            bindCache(input$slcInp_FXrate_d1)%>%
                            bindEvent(input$buttInp_getFXgraphs_d1)
                        ,FX_Periodicity="daily"
                        ,grp_Type="FX_macd"
    )
    # FX_weekly_tab-------------------------------------------------------------
    graph_plotly_server("grp_plotly_FX_candlestick_w1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_w1)%>%
                            bindCache(input$slcInp_FXrate_w1)%>%
                            bindEvent(input$buttInp_getFXgraphs_w1)
                        ,FX_Periodicity="weekly"
                        ,grp_Type="FX_candlestick"
    )
    graph_plotly_server("grp_plotly_FX_rsi_w1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_w1)%>%
                            bindCache(input$slcInp_FXrate_w1)%>%
                            bindEvent(input$buttInp_getFXgraphs_w1)
                        ,FX_Periodicity="weekly"
                        ,grp_Type="FX_rsi"
    )
    graph_plotly_server("grp_plotly_FX_ichimoku_w1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_w1)%>%
                            bindCache(input$slcInp_FXrate_w1)%>%
                            bindEvent(input$buttInp_getFXgraphs_w1)
                        ,FX_Periodicity="weekly"
                        ,grp_Type="FX_ichimoku"
    )
    graph_plotly_server("grp_plotly_FX_stoch_w1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_w1)%>%
                            bindCache(input$slcInp_FXrate_w1)%>%
                            bindEvent(input$buttInp_getFXgraphs_w1)
                        ,FX_Periodicity="weekly"
                        ,grp_Type="FX_stoch"
    )
    graph_plotly_server("grp_plotly_FX_adx_w1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_w1)%>%
                            bindCache(input$slcInp_FXrate_w1)%>%
                            bindEvent(input$buttInp_getFXgraphs_w1)
                        ,FX_Periodicity="weekly"
                        ,grp_Type="FX_adx"
    )
    graph_plotly_server("grp_plotly_FX_volatility_w1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_w1)%>%
                            bindCache(input$slcInp_FXrate_w1)%>%
                            bindEvent(input$buttInp_getFXgraphs_w1)
                        ,FX_Periodicity="weekly"
                        ,grp_Type="FX_volatility"
    )
    graph_plotly_server("grp_plotly_FX_macd_w1"
                        ,FX_Selected=reactive(input$slcInp_FXrate_w1)%>%
                            bindCache(input$slcInp_FXrate_w1)%>%
                            bindEvent(input$buttInp_getFXgraphs_w1)
                        ,FX_Periodicity="weekly"
                        ,grp_Type="FX_macd"
    )
}

# Preview the UI in the console ----
shinyApp(ui=ui, server=server)


# END OF THE SCRIPT ----