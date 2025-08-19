library(shiny)
library(visNetwork)
library(shinyjs)
library(DT)
library(igraph)

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
    edge_error = NULL
  )
  
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
  
  # Download handler for network data (now includes expressions)
  output$download_data <- downloadHandler(
    filename = function() {
      paste("DAGGR-data-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      network_data <- list(
        nodes = rv$nodes,
        edges = rv$edges,
        expressions = generate_complete_expressions(as_list = TRUE)
      )
      saveRDS(network_data, file)
    }
  )
  
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
        stringsAsFactors = FALSE
      )
      
      rv$nodes <- rbind(rv$nodes, new_node)
      showNotification(paste("Node", input$node_name, "added"), type = "message")
    }
    
    updateTextInput(session, "node_name", value = "")
    updateNumericInput(session, "node_mean", value = "")
    updateNumericInput(session, "node_sd", value = "")
    updateNumericInput(session, "node_p", value = "")
    updateNumericInput(session, "node_size", value = "")
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
        visInteraction(hover = TRUE) 
    }
  })
  
  # Display node attributes in a table
  output$node_table <- renderDT({
    datatable(rv$nodes, options = list(pageLength = 5))
  })
}