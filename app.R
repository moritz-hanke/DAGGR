library(shiny)
library(visNetwork)
library(shinyjs)
library(DT)
library(igraph)
library(base64enc)

ui <- fluidPage(
  useShinyjs(),
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
      
      /* Download link styling */
      .download-ready {
        background-color: #d4edda;
        border: 1px solid #c3e6cb;
        border-radius: 8px;
        padding: 15px;
        margin-top: 15px;
        text-align: center;
      }
      
      .download-link {
        display: inline-block;
        padding: 10px 20px;
        background: linear-gradient(to bottom, #28a745, #20c997);
        color: white;
        text-decoration: none;
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.3s;
        margin-top: 10px;
      }
      
      .download-link:hover {
        background: linear-gradient(to bottom, #20c997, #28a745);
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(40, 167, 69, 0.3);
        color: white;
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
           div(class = "panel-style",
               h4(class = "panel-header", "DAG"),
               visNetworkOutput("network", height = "500px"),
               
               h4(class = "panel-header", icon("code", class = "fa-lg"), " Stochastic Expressions"),
               verbatimTextOutput("complete_expressions"),
               
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
               # NEW: Edge style selection
               selectInput("edge_style", "Edge Style", 
                           choices = c("Solid" = FALSE, "Dashed" = TRUE), 
                           selected = FALSE),
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
               actionButton("generate_download", "Generate Download Link", class = "btn-primary-custom", icon = icon("download")),
               uiOutput("download_ui")
           )
    )
  ) 
)

server <- function(input, output, session) {
  # Reactive values to store nodes and edges
  rv <- reactiveValues(
    nodes = data.frame(
      id = integer(), 
      label = character(), 
      type = character(),
      mean = numeric(),
      sd = numeric(),
      p = numeric(),
      size = numeric(),
      x = numeric(),
      y = numeric(),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = integer(), 
      to = integer(), 
      value = character(),  # Changed to character
      label = character(), 
      dashes = logical(),   # NEW: Add dashes column for edge style
      stringsAsFactors = FALSE
    ),
    has_cycles = FALSE,
    edge_error = NULL,
    download_uri = NULL
  )
  
  # Initialize layout coordinates
  observe({
    if (nrow(rv$nodes) > 0) {
      rv$layout_coords <- setNames(
        lapply(1:nrow(rv$nodes), function(i) {
          list(x = rv$nodes$x[i], y = rv$nodes$y[i])
        }),
        as.character(rv$nodes$id)
      )
    }
  })
  
  # Capture node positions when network is rendered or updated
  observe({
    # Trigger position capture after network is stable
    if (!is.null(input$network_positions)) {
      positions <- input$network_positions
      if (length(positions) > 0) {
        # Update our stored coordinates
        for (node_id in names(positions)) {
          if (node_id %in% names(rv$layout_coords)) {
            rv$layout_coords[[node_id]] <- list(
              x = positions[[node_id]]$x,
              y = positions[[node_id]]$y
            )
          }
        }
      }
    }
  })
  
  # Function to check for cycles in the graph
  check_for_cycles <- function() {
    if (nrow(rv$edges) == 0) return(FALSE)
    
    g <- graph_from_data_frame(
      d = rv$edges[, c("from", "to")],
      vertices = rv$nodes$id,
      directed = TRUE
    )
    
    !is_dag(g)
  }
  
  # Function to generate complete expressions for all nodes
  generate_complete_expressions <- function(as_list = FALSE) {
    if (nrow(rv$nodes) == 0) {
      if (as_list) return(list()) else return("No nodes defined in the network")
    }
    
    expressions <- lapply(rv$nodes$id, function(node_id) {
      node_label <- rv$nodes$label[rv$nodes$id == node_id]
      node_type <- rv$nodes$type[rv$nodes$id == node_id]
      
      # Find incoming edges (parent nodes)
      incoming_edges <- rv$edges[rv$edges$to == node_id, ]
      
      # Build the linear combination part
      if (nrow(incoming_edges) > 0) {
        sources <- sapply(incoming_edges$from, function(src_id) {
          src_label <- rv$nodes$label[rv$nodes$id == src_id]
          weight <- incoming_edges$label[incoming_edges$from == src_id]
          paste0(weight, "*", src_label)  # Added parentheses around weight
        })
        linear_part <- paste(sources, collapse = " + ")
      } else {
        linear_part <- NULL
      }
      
      # Generate the stochastic term with placeholders for missing parameters
      stochastic_term <- switch(
        node_type,
        "normal" = {
          mean_val <- ifelse(is.na(rv$nodes$mean[rv$nodes$id == node_id]) | rv$nodes$mean[rv$nodes$id == node_id] == "", 
                             paste0("mean.", node_label), 
                             rv$nodes$mean[rv$nodes$id == node_id])
          sd_val <- ifelse(is.na(rv$nodes$sd[rv$nodes$id == node_id]) | rv$nodes$sd[rv$nodes$id == node_id] == "", 
                           paste0("sd.", node_label), 
                           rv$nodes$sd[rv$nodes$id == node_id])
          paste0("rnorm(n, mean=", mean_val, ", sd=", sd_val, ")")
        },
        "binomial" = {
          p_val <- ifelse(is.na(rv$nodes$p[rv$nodes$id == node_id]) || rv$nodes$p[rv$nodes$id == node_id] == "", 
                          paste0("p.", node_label), 
                          rv$nodes$p[rv$nodes$id == node_id])
          size_val <- ifelse(is.na(rv$nodes$size[rv$nodes$id == node_id]) || rv$nodes$size[rv$nodes$id == node_id] =="", 
                             paste0("size.", node_label), 
                             rv$nodes$size[rv$nodes$id == node_id])
          paste0("rbinom(n, size=", size_val, ", prob=", p_val, ")")
        },
        "0"  # Fallback
      )
      
      # Combine the parts with <- assignment
      if (!is.null(linear_part)) {
        expr <- paste0(node_label, " <- ", linear_part, " + ", stochastic_term)
      } else {
        expr <- paste0(node_label, " <- ", stochastic_term)
      }
      
      expr
    })
    
    if (as_list) {
      return(unlist(expressions))
    } else {
      return(paste(unlist(expressions), collapse = "\n"))
    }
  }
  
  # Generate download link when button is clicked
  observeEvent(input$generate_download, {
    req(nrow(rv$nodes) > 0)  # Ensure there are nodes to download
    
    # Create network data
    network_data <- list(
      nodes = rv$nodes,
      edges = rv$edges,
      expressions = generate_complete_expressions(as_list = TRUE)
    )
    
    # Create a temporary file
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(network_data, temp_file)
    
    # Read the file as binary and convert to data URI
    file_data <- readBin(temp_file, "raw", file.info(temp_file)$size)
    file.remove(temp_file)
    
    # Create data URI
    b64 <- base64enc::base64encode(file_data)
    uri <- paste0("data:application/octet-stream;base64,", b64)
    
    rv$download_uri <- uri
  })
  
  # Render download link UI
  output$download_ui <- renderUI({
    req(rv$download_uri)
    
    tags$div(
      class = "download-ready",
      h5("Download Ready:"),
      tags$a(
        href = rv$download_uri,
        download = paste0("DAGGR-data-", Sys.Date(), ".rds"),
        icon("download"),
        "Click to download RDS file",
        class = "download-link"
      )
    )
  })
  
  # Load network data from file (updated to handle expressions)
  observeEvent(input$load_data, {
    req(input$load_data)
    
    tryCatch({
      ext <- tools::file_ext(input$load_data$name)
      
      if (ext == "rds") {
        loaded_data <- readRDS(input$load_data$datapath)
      } else if (ext %in% c("RData", "rda")) {
        env <- new.env()
        load(input$load_data$datapath, envir = env)
        loaded_data <- get(ls(env)[1], envir = env)
      } else {
        stop("Unsupported file format")
      }
      
      if (!all(c("nodes", "edges") %in% names(loaded_data))) {
        stop("Invalid data structure - must contain 'nodes' and 'edges'")
      }
      
      required_node_cols <- c("id", "label")
      if (!all(required_node_cols %in% colnames(loaded_data$nodes))) {
        stop(paste("Nodes data must contain columns:", paste(required_node_cols, collapse = ", ")))
      }
      
      required_edge_cols <- c("from", "to")
      if (!all(required_edge_cols %in% colnames(loaded_data$edges))) {
        stop(paste("Edges data must contain columns:", paste(required_edge_cols, collapse = ", ")))
      }
      
      # NEW: Handle old data that doesn't have dashes column
      if (!"dashes" %in% colnames(loaded_data$edges)) {
        loaded_data$edges$dashes <- FALSE  # Default to solid edges
      }
      
      if (!"size" %in% colnames(loaded_data$nodes)) {
        loaded_data$nodes$size <- 1
      }
      
      rv$nodes <- loaded_data$nodes
      rv$edges <- loaded_data$edges
      rv$has_cycles <- check_for_cycles()
      
      showNotification("Network data loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Output complete expressions
  output$complete_expressions <- renderText({
    generate_complete_expressions()
  })
  
  # Update cycle status text
  output$cycle_status <- renderText({
    if (rv$has_cycles) {
      "Cycle detected in network! This may cause problems."
    } else {
      "No cycles detected in network."
    }
  })
  
  # Add or update a node
  observeEvent(input$add_node, {
    if (input$node_name == "") {
      showNotification("Please enter a node name", type = "warning")
      return()
    }
    
    existing_node_index <- which(rv$nodes$label == input$node_name)
    
    # Get current positions BEFORE adding new node
    current_positions <- isolate(rv$layout_coords)
    
    if (length(current_positions) > 0) {
      # Get all x and y coordinates
      x_coords <- sapply(current_positions, function(coord) coord$x)
      y_coords <- sapply(current_positions, function(coord) coord$y)
      
      # Calculate reasonable placement (right of the rightmost node)
      max_x <- max(x_coords, na.rm = TRUE)
      min_x <- min(x_coords, na.rm = TRUE)
      avg_y <- mean(y_coords, na.rm = TRUE)
      
      new_x <- max_x + 100  # Place to the right of rightmost node
      new_y <- avg_y
      
    } else {
      # Default position if no nodes exist
      new_x <- 0
      new_y <- 0
    }
    
    if (length(existing_node_index) > 0) {
      rv$nodes$type[existing_node_index] <- input$node_type
      rv$nodes$mean[existing_node_index] <- ifelse(input$node_type == "normal", input$node_mean, NA)
      rv$nodes$sd[existing_node_index] <- ifelse(input$node_type == "normal", input$node_sd, NA)
      rv$nodes$p[existing_node_index] <- ifelse(input$node_type == "binomial", input$node_p, NA)
      rv$nodes$size[existing_node_index] <- ifelse(input$node_type == "binomial", input$node_size, NA)
      showNotification(paste("Node", input$node_name, "updated"), type = "message")
    } else {
      new_id <- ifelse(nrow(rv$nodes) == 0, 1, max(rv$nodes$id) + 1)
      
      new_node <- data.frame(
        id = new_id,
        label = input$node_name,
        type = input$node_type,
        mean = ifelse(input$node_type == "normal", input$node_mean, NA),
        sd = ifelse(input$node_type == "normal", input$node_sd, NA),
        p = ifelse(input$node_type == "binomial", input$node_p, NA),
        size = ifelse(input$node_type == "binomial", input$node_size, NA),
        x = new_x,
        y = new_y,
        stringsAsFactors = FALSE
      )
      
      rv$nodes <- rbind(rv$nodes, new_node)
      rv$layout_coords[[as.character(new_id)]] <- list(x = new_x, y = new_y)
      showNotification(paste("Node", input$node_name, "added"), type = "message")
    }
    
    updateTextInput(session, "node_name", value = "")
    updateNumericInput(session, "node_mean", value = "")
    updateNumericInput(session, "node_sd", value = "")
    updateNumericInput(session, "node_p", value = "")
    updateNumericInput(session, "node_size", value = "")
  })
  
  # Handle node dragging - update positions in real-time
  observeEvent(input$node_positions, {
    positions <- input$node_positions
    if (!is.null(positions) && length(positions) > 0) {
      # Update our stored coordinates
      for (node_id in names(positions)) {
        rv$layout_coords[[node_id]] <- list(
          x = positions[[node_id]]$x,
          y = positions[[node_id]]$y
        )
      }
      
      # Also update the nodes dataframe
      for (i in 1:nrow(rv$nodes)) {
        node_id <- as.character(rv$nodes$id[i])
        if (node_id %in% names(positions)) {
          rv$nodes$x[i] <- positions[[node_id]]$x
          rv$nodes$y[i] <- positions[[node_id]]$y
        }
      }
    }
  })
  
  # Delete node based on node name input
  observeEvent(input$delete_node_btn, {
    if (input$node_name == "") {
      showNotification("Please enter a node name to delete", type = "warning")
      return()
    }
    
    # Check if node exists
    node_exists <- input$node_name %in% rv$nodes$label
    
    if (!node_exists) {
      showNotification(paste("Node", input$node_name, "not found"), type = "error")
      return()
    }
    
    node_id <- rv$nodes$id[rv$nodes$label == input$node_name]
    rv$nodes <- rv$nodes[rv$nodes$id != node_id, ]
    rv$edges <- rv$edges[rv$edges$from != node_id & rv$edges$to != node_id, ]
    rv$has_cycles <- check_for_cycles()
    
    showNotification(paste("Node", input$node_name, "deleted"), type = "message")
    updateTextInput(session, "node_name", value = "")
  })
  
  # Add a new edge with validation
  observeEvent(input$add_edge, {
    # Reset error message
    rv$edge_error <- NULL
    
    # Validate inputs
    if (input$from_node == "" || input$to_node == "") {
      rv$edge_error <- "Please enter both from and to node names"
      return()
    }
    
    # Check if nodes exist
    from_exists <- input$from_node %in% rv$nodes$label
    to_exists <- input$to_node %in% rv$nodes$label
    
    if (!from_exists || !to_exists) {
      missing_nodes <- c()
      if (!from_exists) missing_nodes <- c(missing_nodes, input$from_node)
      if (!to_exists) missing_nodes <- c(missing_nodes, input$to_node)
      rv$edge_error <- paste("Node(s) not found:", paste(missing_nodes, collapse = ", "))
      return()
    }
    
    if (input$from_node == input$to_node) {
      rv$edge_error <- "Cannot create edge to the same node"
      return()
    }
    
    from_id <- rv$nodes$id[rv$nodes$label == input$from_node]
    to_id <- rv$nodes$id[rv$nodes$label == input$to_node]
    
    if(is.na(input$edge_value) | input$edge_value == ""){
      tmp_edge_value <- paste0("coef.", input$from_node, ".", input$to_node)
    }else{
      tmp_edge_value <- input$edge_value
    }
    
    # NEW: Get edge style from input
    edge_style <- as.logical(input$edge_style)
    
    temp_edges <- rbind(
      rv$edges,
      data.frame(
        from = from_id,
        to = to_id,
        value = tmp_edge_value,  # Now accepts any character input
        label = tmp_edge_value,  # Use the input directly as label
        dashes = edge_style,     # NEW: Store edge style
        stringsAsFactors = FALSE
      )
    )
    
    g <- graph_from_data_frame(
      d = temp_edges[, c("from", "to")],
      vertices = rv$nodes$id,
      directed = TRUE
    )
    
    if (!is_dag(g)) {
      rv$edge_error <- "This edge would create a cycle in the network! Not added."
      return()
    }
    
    existing_edge <- rv$edges[
      rv$edges$from == from_id & rv$edges$to == to_id,
    ]
    
    if (nrow(existing_edge) > 0) {
      showNotification("Edge already exists. Updating its value and style.", type = "warning")
      rv$edges[rv$edges$from == from_id & rv$edges$to == to_id, "value"] <- ""
      rv$edges[rv$edges$from == from_id & rv$edges$to == to_id, "label"] <- tmp_edge_value
      rv$edges[rv$edges$from == from_id & rv$edges$to == to_id, "dashes"] <- edge_style
    } else {
      rv$edges <- temp_edges
      showNotification("Edge added successfully", type = "message")
    }
    
    rv$has_cycles <- check_for_cycles()
  })
  
  # Display edge error message
  output$edge_error <- renderUI({
    if (!is.null(rv$edge_error)) {
      tags$div(
        style = "color: red; margin-top: 10px; margin-bottom: 10px;",
        rv$edge_error
      )
    }
  })
  
  # Delete edge based on from/to inputs
  observeEvent(input$delete_edge_btn, {
    # Validate inputs
    if (input$from_node == "" || input$to_node == "") {
      rv$edge_error <- "Please enter both from and to node names to delete edge"
      return()
    }
    
    # Check if nodes exist
    from_exists <- input$from_node %in% rv$nodes$label
    to_exists <- input$to_node %in% rv$nodes$label
    
    if (!from_exists || !to_exists) {
      missing_nodes <- c()
      if (!from_exists) missing_nodes <- c(missing_nodes, input$from_node)
      if (!to_exists) missing_nodes <- c(missing_nodes, input$to_node)
      rv$edge_error <- paste("Node(s) not found:", paste(missing_nodes, collapse = ", "))
      return()
    }
    
    from_id <- rv$nodes$id[rv$nodes$label == input$from_node]
    to_id <- rv$nodes$id[rv$nodes$label == input$to_node]
    
    # Check if edge exists
    edge_exists <- any(rv$edges$from == from_id & rv$edges$to == to_id)
    
    if (!edge_exists) {
      rv$edge_error <- "Edge does not exist"
      return()
    }
    
    # Delete the edge
    rv$edges <- rv$edges[!(rv$edges$from == from_id & rv$edges$to == to_id), ]
    rv$has_cycles <- check_for_cycles()
    showNotification("Edge deleted successfully", type = "message")
    rv$edge_error <- NULL
  })
  
  # Clear all nodes (and edges)
  observeEvent(input$clear_nodes, {
    rv$nodes <- data.frame(
      id = integer(), 
      label = character(), 
      type = character(),
      mean = character(),
      sd = character(),
      p = character(),
      size = character(),
      stringsAsFactors = FALSE
    )
    rv$edges <- data.frame(
      from = integer(), 
      to = integer(), 
      value = character(), 
      label = character(), 
      dashes = logical(),  # NEW: Include dashes column
      stringsAsFactors = FALSE
    )
    rv$has_cycles <- FALSE
    rv$download_uri <- NULL
  })
  
  # Clear all edges
  observeEvent(input$clear_edges, {
    rv$edges <- data.frame(
      from = integer(), 
      to = integer(), 
      value = character(), 
      label = character(), 
      dashes = logical(),  # NEW: Include dashes column
      stringsAsFactors = FALSE
    )
    rv$has_cycles <- FALSE
    rv$download_uri <- NULL
  })
  
  # Render the network visualization
  output$network <- renderVisNetwork({
    if (nrow(rv$nodes) > 0) {
      nodes_with_titles <- rv$nodes
      nodes_with_titles$title <- apply(nodes_with_titles, 1, function(node) {
        if (node["type"] == "normal") {
          paste0("Normal(μ=", node["mean"], ", σ=", node["sd"], ")")
        } else {
          paste0("Binomial(n=", node["size"], ", p=", node["p"], ")")
        } 
      })
      
      if (rv$has_cycles) {
        g <- graph_from_data_frame(
          d = rv$edges[, c("from", "to")],
          vertices = rv$nodes$id,
          directed = TRUE
        )
        nodes_in_cycles <- unique(unlist(igraph::feedback_arc_set(g)))
        nodes_with_titles$color <- ifelse(
          nodes_with_titles$id %in% nodes_in_cycles,
          "#FF9999",
          "#9AC0CD"
        )
      }
      
      # Prepare edges with conditional coloring and style
      edges_with_style <- rv$edges
      edges_with_style$value <- NULL
      
      # Convert edge labels to numeric values for comparison
      edge_values <- suppressWarnings(as.numeric(edges_with_style$label))
      
      # Apply color based on numeric value (light red for negative, light blue for zero/positive)
      edges_with_style$color <- ifelse(
        !is.na(edge_values) & edge_values < 0,
        "#FF9999",  # Light red for negative values
        "#9AC0CD"   # Light blue for zero/positive values
      )
      
      # For non-numeric values, use default color
      edges_with_style$color[is.na(edge_values)] <- "#9AC0CD"
      
      # NEW: Apply edge style (dashes)
      edges_with_style$dashes <- edges_with_style$dashes
      
      visNetwork(nodes_with_titles, edges_with_style) %>%
        visNodes(shape = "circle") %>%
        visEdges(
          arrows = "to",
          width = 1,
          font = list(size = 15),
          smooth = FALSE,
          dashes = ~dashes  # NEW: Apply dashes style
        ) %>%
        visOptions(manipulation = FALSE) %>%
        visPhysics(enabled = FALSE) %>%
        visIgraphLayout(type = "full") %>% 
        visInteraction(
          hover = TRUE,
          dragNodes = TRUE,
          dragView = TRUE,
          zoomView = TRUE
        ) %>%
        visEvents(
          dragEnd = "function(params) {
          if(params.nodes.length > 0){
            Shiny.setInputValue('node_dragged', params.nodes[0]);
            Shiny.setInputValue('node_positions', this.getPositions());
          }
        }"
        )
    }
  })
  
  # Display node attributes in a table
  output$node_table <- renderDT({
    datatable(rv$nodes, options = list(pageLength = 5))
  })
}


# Custom JavaScript to handle position tracking
jscode <- '
Shiny.addCustomMessageHandler("injectScript", function(message) {
  // Function to send positions to Shiny
  function sendPositionsToShiny() {
    var positions = network.getPositions();
    Shiny.setInputValue("network_positions", positions);
  }
  
  // Send positions initially
  setTimeout(sendPositionsToShiny, 500);
  
  // Send positions periodically to ensure we capture them
  setInterval(sendPositionsToShiny, 1000);
});

Shiny.addCustomMessageHandler("getPositions", function(message) {
  var positions = network.getPositions();
  Shiny.setInputValue("network_positions", positions);
  alert("Positions captured: " + Object.keys(positions).length + " nodes");
});
'

shinyApp(ui = ui, server = server)