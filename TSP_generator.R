# Scripts pour générer un TSP aléatoire.
# 
# 

PPV <- function(TSP) {
	current <- TSP$warehouse
	node_list <- as.numeric(rownames(TSP$nodes))
	T <- c()
	d <- TSP$D
	
	print(node_list)
	print(T)
	while (length(node_list) > 0) {
		print(node_list); print(current);print(d)
		d <- d[,node_list[node_list != current]]
		node_list <- node_list[node_list != current]
		current <- as.numeric(which(d[current,] == min(d[current,])))
		T <- append(T, current)
	}
	return(T)
}

calcul_distancier <- function(nodes) {
	n <- dim(nodes)[1]
	D <- matrix(nrow = n, ncol = n)
	for (i in 1:n) {
		for (j in 1:n) {
			if (i == j) {
				D[i,j] <- Inf
			} else {
				D[i,j] <- sqrt(
					(nodes[i,"x"]-nodes[j,"x"])^2 +
						(nodes[i,"y"]-nodes[j,"y"])^2
				)
			}
		}
	}
	colnames(D) <- as.character(1:n)
	rownames(D) <- as.character(1:n)
	return(D)
}

generate_TSP <- function(
		n = 10,
		xlim = c(0,100),
		ylim = c(0,100),
		warehouse = 1
) {
	TSP <- list()
	node_coord <- c()
	for (i in 1:n) {
		node_coord <- append(
			node_coord,
			c(sample(x = xlim[1]:xlim[2], size = 1),
			  sample(x = ylim[1]:ylim[2], size = 1))
		)
	}
	node_coord <- matrix(node_coord, ncol = 2, byrow = TRUE)
	colnames(node_coord) <- c("x", "y")
	rownames(node_coord) <- as.character(1:n)
	TSP$nodes <- node_coord
	TSP$warehouse <- warehouse
	TSP$D <- calcul_distancier(node_coord)
	
	
	return(TSP)
}

a <- (generate_TSP())
print(PPV(a))

