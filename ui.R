library(shiny)
library(visNetwork)
library(shinyjs)
library(DT)
library(igraph)

ui <- fluidPage(
  # Custom CSS for modern styling
  tags$head(
    tags$style(HTML("
      /* Global styles */
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f8fafc;
        color: #334155;
      }
      
      /* Header styling */
      .app-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 15px 30px;
        background: linear-gradient(135deg, #0F828C 0%, #1E3A8A 100%);
        color: white;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin-bottom: 25px;
        border-radius: 0 0 8px 8px;
      }
      
      .app-title {
        margin: 0;
        flex-grow: 1;
        font-weight: 600;
        font-size: 1.8rem;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
      }
      
      .app-logo {
        height: 120px;
        filter: drop-shadow(0 2px 4px rgba(0,0,0,0.3));
      }
      
      /* Panel styling */
      .panel-style {
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
        padding: 20px;
        margin-bottom: 20px;
        border-left: 4px solid #0F828C;
      }
      
      .panel-header {
        color: #0F828C;
        font-weight: 600;
        margin-top: 0;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 1px solid #e2e8f0;
      }
      
      /* Button styling */
      .btn-primary-custom {
        background-color: #0F828C;
        border: none;
        color: white;
        width: 100%;
        margin-bottom: 8px;
        border-radius: 6px;
        transition: all 0.3s;
      }
      
      .btn-primary-custom:hover {
        background-color: #0c6871;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      .btn-warning-custom {
        background-color: #F39F5A;
        border: none;
        color: white;
        width: 100%;
        margin-bottom: 8px;
        border-radius: 6px;
        transition: all 0.3s;
      }
      
      .btn-warning-custom:hover {
        background-color: #e08c47;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      .btn-danger-custom {
        background-color: #872341;
        border: none;
        color: white;
        width: 100%;
        margin-bottom: 8px;
        border-radius: 6px;
        transition: all 0.3s;
      }
      
      .btn-danger-custom:hover {
        background-color: #6c1c34;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      /* Input styling */
      .form-control {
        border-radius: 6px;
        border: 1px solid #cbd5e1;
        padding: 8px 12px;
        margin-bottom: 12px;
      }
      
      .form-control:focus {
        border-color: #0F828C;
        box-shadow: 0 0 0 3px rgba(15, 130, 140, 0.2);
      }
      
      /* Network visualization container */
      #network {
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
        background-color: white;
      }
      
      /* Table styling */
      .dataTables_wrapper {
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
        padding: 15px;
        background-color: white;
      }
      
      /* Text output styling */
      .shiny-text-output {
        background-color: #f1f5f9;
        border-radius: 6px;
        padding: 15px;
        border-left: 3px solid #0F828C;
        font-family: 'Courier New', monospace;
        max-height: 200px;
        overflow-y: auto;
      }
      
      /* HR styling */
      hr {
        border-top: 1px solid #e2e8f0;
        margin: 20px 0;
      }
    "))
  ),
  
  # Header with title on left and logo on right
  div(class = "app-header",
      h1(class = "app-title", "DAGGR: Directed Acyclic Graph Generator for Random numbers"),
      tags$img(src = "DAGGR_logo.svg", class = "app-logo", alt = "App Logo")
  ),
  
  fluidRow(
    # Left column for node controls
    column(3,
           div(class = "panel-style",
               h4(class = "panel-header", icon("database"), " Data Import/Export"),
               fileInput("load_data", "Load Network Data", 
                         accept = c(".rds", ".RData", ".rda"),
                         buttonLabel = "Browse...",
                         placeholder = "No file selected"),
               downloadButton("download_data", "Download Data", class = "btn-primary-custom"),
               hr(),
               h4(class = "panel-header", icon("circle-nodes"), " Node Controls"),
               textInput("node_name", "Node Name", "", placeholder = "Enter node name"),
               selectInput("node_type", "Random Variable Type", 
                           choices = c("normal", "binomial", "other")),
               conditionalPanel(
                 condition = "input.node_type == 'normal'",
                 textInput("node_mean", "Mean", value = 0, placeholder = "Enter mean"),
                 textInput("node_sd", "Standard Deviation", value = 1, placeholder = "Enter SD")
               ),
               conditionalPanel(
                 condition = "input.node_type == 'binomial'",
                 textInput("node_p", "Probability (p)", value = 0.5, placeholder = "Enter probability"),
                 textInput("node_size", "Size (n)", value = 1, placeholder = "Enter size")
               ),
               actionButton("add_node", "Add/Update Node", class = "btn-primary-custom", icon = icon("plus")),
               actionButton("delete_node_btn", "Delete Node", class = "btn-warning-custom", icon = icon("trash")),
               actionButton("clear_nodes", "Clear All Nodes", class = "btn-danger-custom", icon = icon("broom"))
           )
    ),
    
    # Middle column for network visualization
    column(6,
           visNetworkOutput("network", height = "500px"),
           div(class = "panel-style",
               h4(class = "panel-header", icon("code"), " Stochastic Expressions"),
               verbatimTextOutput("complete_expressions")
           ),
           div(class = "panel-style",
               h4(class = "panel-header", icon("table"), " Node Attributes"),
               DTOutput("node_table")
           )
    ),
    
    # Right column for edge controls
    column(3,
           div(class = "panel-style",
               h4(class = "panel-header", icon("link"), " Edge Controls"),
               textInput("from_node", "From Node (name)", "", placeholder = "Source node"),
               textInput("to_node", "To Node (name)", "", placeholder = "Target node"),
               textInput("edge_value", "Edge Value (any type)", value = "1", placeholder = "Edge value/weight"),
               actionButton("add_edge", "Add/Update Edge", class = "btn-primary-custom", icon = icon("link")),
               actionButton("delete_edge_btn", "Delete Edge", class = "btn-warning-custom", icon = icon("unlink")),
               htmlOutput("edge_error"),
               actionButton("clear_edges", "Clear All Edges", class = "btn-danger-custom", icon = icon("broom"))
           )
    )
  ) 
)
