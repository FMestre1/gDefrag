prioritize <-
  function(nodes, edges, method, re_scale = TRUE, shape=TRUE, shape_name_out = "priorities_shape", overwrite=TRUE){#FUNCTION 3
    
    
    #Required functions
    metric_iic <- function(n, e){#metrica IIC do Saura; O input e a tabela de edges
      mygraph <- igraph::graph_from_data_frame(e[,1:2], directed = FALSE, vertices = n)
      dist_tp <- as.data.frame(igraph::distances(mygraph))
      topo_col <- rep(NA, nrow(e))
      for(i in 1:nrow(e)){
        row1 <- e[i,]
        nodeA <- as.numeric(row1[1])
        nodeB <- as.numeric(row1[2])
        dist_tp1 <- dist_tp[as.character(nodeA),as.character(nodeB)]
        topo_col[i] <- dist_tp1
      }
      newT <- cbind(e,topo_col)
      comp1 <- sum((newT[,"value_A"]*newT[,"value_B"])/(1+newT[,ncol(newT)]))  # changed from col numbers to col names
      return(comp1)
    }
    #from: https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
    merge.with.order <- function(x,y, ..., sort = TRUE, keep_order)
    {
      # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
      add.id.column.to.data <- function(DATA)
      {
        data.frame(DATA, id... = seq_len(nrow(DATA)))
      }
      # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
      order.by.id...and.remove.it <- function(DATA)
      {
        # gets in a data.frame with the "id..." column.  Orders by it and returns it
        if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
        
        ss_r <- order(DATA$id...)
        ss_c <- colnames(DATA) != "id..."
        DATA[ss_r, ss_c]
      }
      
      # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
      # tmp()
      
      if(!missing(keep_order))
      {
        if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
        if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
        # if you didn't get "return" by now - issue a warning.
        warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
      } else {return(merge(x=x,y=y,..., sort = sort))}
    }
    
    if (!(method %in% c("value", "between", "IIC", "AWM"))) stop("Invalid 'method'.")
    
    #Get data
    
    node_T <- data.frame(nodes)
    edge_T <- data.frame(edges)
    
    if (method == "value"){#OK!
      value_sum <- edge_T[, "value_A"] + edge_T[, "value_B"] #⚠️ ALTERADO: objeto era 'area_sum' e mudei para 'value_sum'
      tab1 <- cbind(as.numeric(rownames(edge_T)), value_sum)
      colnames(tab1) <- c("Edge_ID", "Total_Area")
      tab1 <- as.data.frame(tab1)
      tab1 <- tab1[order(tab1[,2]), ]
      t2 <- cbind(tab1[,1],1:nrow(tab1))
      #colnames(result) <- c("edge_ID", "Prior_order")
      t2 <- t2[ order(t2[,1]), ][,2]
      
      if(re_scale == TRUE) t2 <- (t2-min(t2))/(max(t2)-min(t2))*100
      
      result <- cbind(edge_T, t2)
      #colnames(result)[10] <- "priorization"
      result <- as.data.frame(result)
    }
    
    if (method == "between"){#OK!
      nd <- node_T  # changed from node_T$nodes
      mygraph <- igraph::graph_from_data_frame(edge_T[,1:2], directed = FALSE, vertices = nd)
      out_C <- igraph::edge_betweenness(mygraph, e = igraph::E(mygraph), directed = FALSE)
      
      if(re_scale == TRUE) out_C <- (out_C-min(out_C))/(max(out_C)-min(out_C))*100
      
      result <- cbind(edge_T, out_C)
      result <- as.data.frame(result)
    }
    
    if (method == "IIC"){#OK!
      
      #Importance of each link
      #dI = ((I1-I2)/I1)*100
      
      #Output vector
      dI <- rep(NA, nrow(edge_T))
      
      #Starting IIC value
      I1 <- metric_iic(n=node_T, e=edge_T)
      
      #Compute, removing one link at a time
      for(i in 1:nrow(edge_T)){
        edge_T2 <- edge_T[-i,]
        I2 <- metric_iic(n=node_T, e=edge_T2)
        val1 <- ((I1-I2)/I1)*100
        dI[i] <- val1
      }
      
      if(re_scale == TRUE) dI <- (dI-min(dI))/(max(dI)-min(dI))*100
      
      result <- cbind(edge_T, dI)
      result <- as.data.frame(result)
      
    }
    
    if (method == "AWM") {
      #⚠️ ALTERADO: Abaixo troquei numero das colunas para nomes porque edge_T mudou devido aos ajustes na função edge.creation
      nodes_areas <- node_T[, c("node_ID", "pol_area")] 
      nodes_A_prop <- edge_T[, c("node_A", "value_A")]
      nodes_B_prop <- edge_T[, c("node_B", "value_B")]
      
      ndA <- merge.with.order(x=nodes_A_prop, y=nodes_areas, by.x="node_A", 
                              by.y="node_ID", keep_order=1)
      ndB <- merge.with.order(x=nodes_B_prop, y=nodes_areas, by.x="node_B", 
                              by.y="node_ID", keep_order=1)
      table1 <- cbind(ndA, ndB)
      
      metric <- (table1[,2]*table1[,6])+(table1[,5]*table1[,3])
      
      if (re_scale == TRUE) {
        metric <- (metric - min(metric)) / (max(metric) - min(metric)) * 100
      }
      
      result <- cbind(edge_T, metric)
      result <- as.data.frame(result)
    }
    
    colnames(result)[ncol(result)] <- "priorization"
    
    values(edges)$priorization <- result$priorization
    
    
    if (shape == TRUE){  
      terra::writeVector(edges, paste0(shape_name_out, ".shp"), overwrite = overwrite)
      message("Shapefiles created! Check the working directory, please.")
      
    }
    
    return(edges)
  }
