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

generer_TSP <- function(
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

afficher_plan <- function(TSP) {
	p <- ggplot() + 
		geom_point(aes(x = as.vector(TSP$nodes[,"x"]),
					   y = as.vector(TSP$nodes[,"y"]),
					   size = 5,
					   color = 'blue'),
				   label = rownames(TSP$nodes)) +
		geom_point(aes(x = TSP$nodes[1,"x"],
					   y = TSP$nodes[1,"y"],
					   color = 'red',
					   size = 5)) +
		# geom_label(
		# 	aes(x = as.vector(TSP$nodes[,"x"]),
		# 		y = as.vector(TSP$nodes[,"y"]),
		# 		size = 5,
		# 		color = 'blue'),
		# 	label = rownames(TSP$nodes)
		# ) +
		theme(legend.position="none") +
		labs(x = "x", y = "y")
	return(p)
}

afficher_tournee <- function(TSP, T) {
	p <- afficher_plan(TSP)
	edges_list <- c()
	for (i in 1:(length(T)-1)) {
		x <- c(TSP$nodes[as.numeric(T[i]),"x"], TSP$nodes[as.numeric(T[i+1]),"x"])
		y <- c(TSP$nodes[as.numeric(T[i]),"y"], TSP$nodes[as.numeric(T[i+1]),"y"])
		#print(x);print(y)
		loop_input = paste(
			"geom_line(aes(x = c(",
			x[1],", ",x[2],")",
			", y = c(",
			y[1],", ",y[2],")",
			", color = 'black'))",
			sep = ""
		)
		p <- p + eval(parse(text = loop_input))
	}
	return(p)
}

# Calculer le PPV à partir d'un TSP
# Renvoi une liste T avec l'ordre des noeuds visités

PPV <- function(TSP) {
	current <- TSP$warehouse
	T <- c(current)
	nodes <- rownames(TSP$nodes)[rownames(TSP$nodes) != current]
	while (length(nodes) > 0) {
		current <- nodes[which(TSP$D[current,nodes] == min(TSP$D[current,nodes]))]
		T <- append(T, current)
		nodes <- nodes[nodes != current]
	}
	T <- append(T, TSP$warehouse)
	return(T)
}


swap_edges <- function(T, u, v) {
	# print(u); print(v); print(T)
	i = which(T[1:(length(T)-1)] == u)
	j = which(T[1:(length(T)-1)] == v)
	# print(i); print(j)
	while (j > i) {
		mid <- T[j]
		T[j] <- T[i]
		T[i] <- mid
		i = i + 1
		j = j - 1
	}
	return(T)
}

deux_opt <- function(TSP, T, nbrmax = 1000) {
	
	n <- dim(TSP$nodes)[1]
	# print(T)
	SWAP <- TRUE
	nbr = 0
	while (SWAP & nbr < nbrmax + 1) {
		SWAP <- FALSE
		for (i in 2:(n-2)) {
			for (j in (i+1):(n-1)) {
				
				delta <- -TSP$D[T[i-1],T[i]]-TSP$D[T[j],T[j+1]]+
					TSP$D[T[i-1],T[j]] + TSP$D[T[i],T[j+1]]
				# print(paste("i = ",i,", j = ",j,", delta = ",delta))
				if (delta < 0) {
					SWAP = TRUE
					nbr = nbr + 1
					T <- swap_edges(T, T[i], T[j])
					# print(T)
				}
			}
		}
	}
	
	return(T)
}

calcul_cout <- function(TSP, T) {
	sum <- 0
	for (i in 1:(length(T)-1)) {
		sum <- sum + TSP$D[as.numeric(T[i]),as.numeric(T[i+1])]
	}
	return(sum)
}

enveloppe_convexe <- function(TSP, info = FALSE) {
	
	# Sort une liste de noeud faisant le tour de l'enveloppe convexe
	# # On a les infos suivantes :
	
	n <- length(TSP$nodes[,'x'])
	X <- TSP$nodes[,'x']
	Y <- TSP$nodes[,'y']
	
	Z <- complex(real = X, imaginary = Y)
	
	if (info) {
		print("=== Début ===")
		print(Z)
	}
	
	# Trouver wini
	w <- 1
	for (i in 2:n) {
		if (Y[i] < Y[w]) {
			w <- i
		}
	}
	# Dans w on a l'indice du premier noeud
	
	# l'angle min à respecter
	Omin <- 0
	# Le point initial A prend la valeur initiale w
	A <- w
	zD <- Z[A] + 1
	# B prend 0 pour permettre une première itération
	B <- 0
	# On ajoute w à la liste de noeud
	T <- c(w)
	
	while (B != w & Omin <1000) {
		
		if (info) {
			print(paste("=== Noeud actuel :",A))
		}
		
		B <- A
		O <- Inf
		
		for (i in (1:n)[1:n != A]) {
			
			if (info) {
				print(paste("       Noeud testé :",i))
			}
			
			# # calcul l'ange entre DAB
			# # zd - zA
			# ALPHA <- c(1,0)
			# BETA <- c(X[i]-X[A],Y[i]-Y[A])
			# GAMMA <- c(
			# 	(ALPHA[1]*BETA[1]+ALPHA[2]*BETA[2])/(BETA[1]^2+BETA[2]^2),
			# 	(ALPHA[1]*ALPHA[2]-BETA[1]*BETA[2])/(BETA[1]^2+BETA[2]^2)
			# )
			# ARGAMMA <- 2 * atan(
			# 	GAMMA[2]/
			# 		(GAMMA[1] + sqrt(GAMMA[1]^2+GAMMA[2]^2))
			# )
			# 
			# if (info) {
			# 	print(paste("      Valeur de l'angle avec",A,":",ARGAMMA))
			# 	print(paste("      Borne sup trouvée:",O))
			# 	print(paste("      Borne inf à respecter:",Omin))
			# }
			# 
			# 
			# if (ARGAMMA < O & ARGAMMA > Omin) {
			# 	O <- ARGAMMA
			# 	B <- i
			# }

			ARGAMMA <- Arg((zD-Z[A])/(Z[i]-Z[A]))
			if (Y[i]>Y[A]) {
				ARGAMMA <- -1*Arg((zD-Z[A])/(Z[i]-Z[A]))
			} else {
				ARGAMMA <- 2*pi-Arg((zD-Z[A])/(Z[i]-Z[A]))
			}
			
			if (info) {
				print(paste("      Valeur de l'angle avec",A,":",ARGAMMA))
				print(paste("      Borne sup trouvée:",O))
				print(paste("      Borne inf à respecter:",Omin))
			}
			
			if (ARGAMMA < O & ARGAMMA > Omin) {
				O <- ARGAMMA
				B <- i
			}
			
		}
		
		Omin <- O
		T <- c(T, B)
		A <- B
		zD <- Z[A] + 1
		
		print(T)
	}
	return(T)
}