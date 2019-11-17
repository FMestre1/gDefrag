prioritize <-
function(nodes, edges, method, shape=FALSE, shape_name_out = "priorities_shape", shape_name_nodes_edges = "nodes_with_edges"){#FUNCTION 3

  #Arguments:
  #node_T - Nodes data frame;  # NOW nodes = out1
  #edge_T - Edges data frame;  # NOW edges = out2
  #method - prioritization method:
  #1. value - connect acording to the atribute of the nodes (priority to higher values);
  #2. traffic - connect habitats separated by roads with the least traffic;
  #3. IIC - increase connectivity (with IICnum);
  #4. between - edge betweenness;
  #traffic_column - columns of edges@data contining the traffic values of roads

  if (!(method %in% c("value", "between", "IIC", "AWM"))) stop("Invalid 'method'.")

  #node_T <- nodes@data  # new #16-11-2019 - changes in sp and rgdal
  #edge_T <- edges@data  # new #16-11-2019 - changes in sp and rgdal
  
  node_T <- slot(nodes, "data")  #16-11-2019 - changes in sp and rgdal
  edge_T <- slot(edges, "data")  #16-11-2019 - changes in sp and rgdal
  
  #node_T[ , "node_ID"] <- as.character(node_T[ , "node_ID"])  # NEW
  #edge_T[ , "node_A"] <- as.character(edge_T[ , "node_A"])  # NEW
  #edge_T[ , "node_B"] <- as.character(edge_T[ , "node_B"])  # NEW

  if (method == "value"){#OK!
    area_sum <- edge_T[, "value_A"] + edge_T[, "value_B"]
    tab1 <- cbind(as.numeric(rownames(edge_T)), area_sum)
    colnames(tab1) <- c("Edge_ID", "Total_Area")
    tab1 <- as.data.frame(tab1)
    tab1 <- tab1[order(tab1[,2]), ]
    t2 <- cbind(tab1[,1],1:nrow(tab1))
    #colnames(result) <- c("edge_ID", "Prior_order")
    t2 <- t2[ order(t2[,1]), ][,2]
    t2 <- (t2-min(t2))/(max(t2)-min(t2))*100
    result <- cbind(edge_T, t2)
    #colnames(result)[10] <- "priorization"
    result <- as.data.frame(result)
  }

  #if(method=="traffic"){#OK!
  #tab1 <- cbind(as.numeric(rownames(edge_T)),edge_T[ , traffic_column])
  #tab1 <- as.data.frame(tab1)
  #tab1 <- tab1[ order(-tab1[,2]), ]
  #r1 <- unique(tab1[,2])
  #r2 <- 1:length(r1)
  #r3 <- as.data.frame(cbind(r1,r2))
  #t1 <- tab1[,2]
  #t2 <- rep(NA,length(t1))
  #for(x in 1:length(t2)) t2[x]<-r3[r3[]==t1[x],][,2]
  #t3 <- cbind(tab1[,1],t2)
  #t2 <- t3[ order(t3[,1]), ][,2]
  #result <- cbind(edge_T,t2)
  #colnames(result)[12] <- "Priority_order"
  #result <- as.data.frame(result)
  #}

  if (method == "between"){#OK!
    nd <- node_T  # changed from node_T$nodes
    mygraph <- graph_from_data_frame(edge_T, directed = FALSE, vertices = nd)
    out_C <- edge.betweenness.estimate(mygraph, e = E(mygraph), directed = FALSE, cutoff = 0)
    out_C <- (out_C-min(out_C))/(max(out_C)-min(out_C))*100
    result <- cbind(edge_T, out_C)
    result <- as.data.frame(result)
  }

  if (method == "IIC"){#OK!

    metric <- function(n, e){#metrica IIC do Saura; O input e a tabela de edges

      nd <- n  # changed from nd <- n$nodes
      mygraph <- graph_from_data_frame(e, directed=FALSE, vertices=nd)
      dist_tp <- as.data.frame(distances(mygraph))
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

    #Importance of each link
    #dI = ((I1-I2)/I1)*100

    #Output vector
    dI <- rep(NA, nrow(edge_T))
	
	#Starting IIC value
    I1 <- metric(n=node_T, e=edge_T)

    #Compute, removing one link at a time
    for(i in 1:nrow(edge_T)){
      edge_T2 <- edge_T[-i,]
      I2 <- metric(n=node_T, e=edge_T2)
      val1 <- ((I1-I2)/I1)*100
      dI[i] <- val1
    }

    dI <- (dI-min(dI))/(max(dI)-min(dI))*100

    result <- cbind(edge_T, dI)
    result <- as.data.frame(result)

  }

  if (method == "AWM"){

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
 
  nodes_areas <- node_T[,c(1,5)]
  nodes_A_prop <- edge_T[,c(1,8)]
  nodes_B_prop <- edge_T[,c(2,9)]
  ndA <- merge.with.order(x=nodes_A_prop, y=nodes_areas, by.x="node_A", 
  by.y="node_ID", keep_order=1)
  ndB <- merge.with.order(x=nodes_B_prop, y=nodes_areas, by.x="node_B", 
  by.y="node_ID", keep_order=1)
  table1 <- cbind(ndA,ndB)
  #names(table1) <- c("node_A", "Ah", "At", "node_B", "Bh", "Bt")
  metric <- (table1[,2]*table1[,6])+(table1[,5]*table1[,3])
  hab_prop <- (metric-min(metric))/(max(metric)-min(metric))*100
  result <- cbind(edge_T, hab_prop)
  result <- as.data.frame(result)
  }

  #colnames(result)[10] <- "priorization"
  colnames(result)[ncol(result)] <- "priorization"

  #edges@data <- data.frame(edges@data, priorization = result[ , "priorization"])  # new##16-11-2019 - changes in sp and rgdal
  slot(edges, "data") <- data.frame(slot(edges, "data"), priorization = result[ , "priorization"])  # new##16-11-2019 - changes in sp and rgdal
  
  #return(result)
  
  #Para seleccionar so os nodos com edges
  #nodes_w_edges_ID <- unique(c(edges@data[,1], edges@data[,2]))#16-11-2019 - changes in sp and rgdal
  nodes_w_edges_ID <- unique(c(slot(edges, "data")[,1], slot(edges, "data")[,2]))#16-11-2019 - changes in sp and rgdal
  nodes2 <- nodes
  nodes2 <- nodes2[nodes2$node_ID %in% nodes_w_edges_ID,]
    

  if (shape == TRUE){  
  suppressWarnings(writeOGR(edges, ".", shape_name_out, driver="ESRI Shapefile",overwrite_layer=TRUE))
  suppressWarnings(writeOGR(nodes2, ".", shape_name_nodes_edges, driver="ESRI Shapefile",overwrite_layer=TRUE))
  message("Two shapefiles created! Check the working directory, please.")

 }
  
  return(edges) # new
}