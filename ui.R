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
        background: linear-gradient(135deg, #1a4b8c 0%, #2c3e50 100%);
        color: white;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
        margin-bottom: 25px;
        border-radius: 0 0 8px 8px;
      }
      
      .app-title {
        margin: 0;
        flex-grow: 1;
        font-weight: 600;
        font-size: 1.8rem;
        text-shadow: 1px 1px 3px rgba(0,0,0,0.3);
        letter-spacing: 0.5px;
      }
      
      .app-logo {
        height: 120px;
        filter: drop-shadow(0 2px 6px rgba(0,0,0,0.4));
      }
      
      /* Panel styling */
      .panel-style {
        background-color: white;
        border-radius: 10px;
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.08);
        padding: 22px;
        margin-bottom: 24px;
        border-top: 1px solid #f1f5f9;
        border-left: 4px solid #3a7ca5;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }
      
      .panel-style:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 16px rgba(0, 0, 0, 0.1);
      }
      
      .panel-header {
        color: #2c3e50;
        font-weight: 600;
        margin-top: 0;
        margin-bottom: 18px;
        padding-bottom: 12px;
        border-bottom: 1px solid #e8edf2;
        font-size: 1.3rem;  /* Increased from 1.1rem */
        display: flex;
        align-items: center;
        gap: 10px;  /* Increased from 8px */
      }
      
      .panel-header i {
        font-size: 1.4rem;  /* Added for icon sizing */
      }
      
      /* Button styling */
      .btn-primary-custom {
        background: linear-gradient(to bottom, #3a7ca5, #2a5c7a);
        border: none;
        color: white;
        width: 100%;
        margin-bottom: 10px;
        border-radius: 8px;
        padding: 10px;
        font-weight: 500;
        transition: all 0.3s;
        box-shadow: 0 3px 6px rgba(58, 124, 165, 0.2);
      }
      
      .btn-primary-custom:hover {
        background: linear-gradient(to bottom, #4a8cb5, #3a6c8a);
        transform: translateY(-2px);
        box-shadow: 0 6px 10px rgba(58, 124, 165, 0.3);
      }
      
      .btn-warning-custom {
        background: linear-gradient(to bottom, #e67e22, #d35400);
        border: none;
        color: white;
        width: 100%;
        margin-bottom: 10px;
        border-radius: 8px;
        padding: 10px;
        font-weight: 500;
        transition: all 0.3s;
        box-shadow: 0 3px 6px rgba(230, 126, 34, 0.2);
      }
      
      .btn-warning-custom:hover {
        background: linear-gradient(to bottom, #f39c12, #e67e22);
        transform: translateY(-2px);
        box-shadow: 0 6px 10px rgba(230, 126, 34, 0.3);
      }
      
      .btn-danger-custom {
        background: linear-gradient(to bottom, #c0392b, #922b21);
        border: none;
        color: white;
        width: 100%;
        margin-bottom: 10px;
        border-radius: 8px;
        padding: 10px;
        font-weight: 500;
        transition: all 0.3s;
        box-shadow: 0 3px 6px rgba(192, 57, 43, 0.2);
      }
      
      .btn-danger-custom:hover {
        background: linear-gradient(to bottom, #e74c3c, #c0392b);
        transform: translateY(-2px);
        box-shadow: 0 6px 10px rgba(192, 57, 43, 0.3);
      }
      
      /* Input styling */
      .form-control {
        border-radius: 8px;
        border: 1px solid #d9e2ec;
        padding: 10px 14px;
        margin-bottom: 14px;
        transition: all 0.3s;
        background-color: #f8fafc;
      }
      
      .form-control:focus {
        border-color: #3a7ca5;
        box-shadow: 0 0 0 3px rgba(58, 124, 165, 0.2);
        background-color: white;
      }
      
      /* Network visualization container */
      #network {
        border-radius: 10px;
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.08);
        background-color: white;
        overflow: hidden;
      }
      
      /* Table styling */
      .dataTables_wrapper {
        border-radius: 10px;
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.08);
        padding: 18px;
        background-color: white;
      }
      
      /* Text output styling */
      .shiny-text-output {
        background-color: #f1f5f9;
        border-radius: 8px;
        padding: 18px;
        border-left: 4px solid #3a7ca5;
        font-family: 'Courier New', monospace;
        max-height: 200px;
        overflow-y: auto;
        line-height: 1.5;
      }
      
      /* HR styling */
      hr {
        border-top: 1px solid #e8edf2;
        margin: 22px 0;
      }
      
      /* Custom select input styling */
      .selectize-input {
        border-radius: 8px;
        border: 1px solid #d9e2ec;
        padding: 10px 14px;
        background-color: #f8fafc;
      }
      
      .selectize-input.focus {
        border-color: #3a7ca5;
        box-shadow: 0 0 0 3px rgba(58, 124, 165, 0.2);
      }
      
      /* File input button styling */
      .btn-file {
        background: linear-gradient(to bottom, #3a7ca5, #2a5c7a);
        color: white;
        border-radius: 8px;
        padding: 8px 16px;
        transition: all 0.3s;
      }
      
      .btn-file:hover {
        background: linear-gradient(to bottom, #4a8cb5, #3a6c8a);
      }
    "))
  ),
  
  # Header with title on left and logo on right
  div(class = "app-header",
      h1(class = "app-title", "DAGGR: Directed Acyclic Graph Generator for Random variables"),
      tags$img(src = "DAGGR_logo.svg", class = "app-logo", alt = "App Logo")
  ),
  
  fluidRow(
    # Left column for node controls
    column(3,
           div(class = "panel-style",
               h4(class = "panel-header", icon("circle-nodes", class = "fa-lg"), " Node Controls"),
               textInput("node_name", "Node Name", "", placeholder = "Enter node name"),
               selectInput("node_type", "Random Variable Type", 
                           choices = c("normal", "binomial")),
               conditionalPanel(
                 condition = "input.node_type == 'normal'",
                 textInput("node_mean", "Mean (optional)", value = "", placeholder = "Any Type"),
                 textInput("node_sd", "SD (optional)", value = "", placeholder = "Any Type")
               ),
               conditionalPanel(
                 condition = "input.node_type == 'binomial'",
                 textInput("node_p", "Probability p (optional)", value = "", placeholder = "Any Type"),
                 textInput("node_size", "Size n (optional)", value = "", placeholder = "Any Type")
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
               h4(class = "panel-header", icon("code", class = "fa-lg"), " Stochastic Expressions"),
               verbatimTextOutput("complete_expressions")
           ),
           div(class = "panel-style",
               h4(class = "panel-header", icon("table", class = "fa-lg"), " Node Attributes"),
               DTOutput("node_table")
           )
    ),
    
    # Right column for edge controls
    column(3,
           div(class = "panel-style",
               h4(class = "panel-header", icon("link", class = "fa-lg"), " Edge Controls"),
               textInput("from_node", "From Node (name)", "", placeholder = "Source node"),
               textInput("to_node", "To Node (name)", "", placeholder = "Target node"),
               textInput("edge_value", "Edge Value (optional)", value = "", placeholder = "Any Type"),
               actionButton("add_edge", "Add/Update Edge", class = "btn-primary-custom", icon = icon("link")),
               actionButton("delete_edge_btn", "Delete Edge", class = "btn-warning-custom", icon = icon("unlink")),
               htmlOutput("edge_error"),
               actionButton("clear_edges", "Clear All Edges", class = "btn-danger-custom", icon = icon("broom")),
               hr(),
               h4(class = "panel-header", icon("database", class = "fa-lg"), " Data Import/Export"),
               fileInput("load_data", "Load Network Data", 
                         accept = c(".rds", ".RData", ".rda"),
                         buttonLabel = "Browse...",
                         placeholder = "No file selected"),
               downloadButton("download_data", "Download Data", class = "btn-primary-custom")
           )
    )
  ) 
)