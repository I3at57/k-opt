# Exemple Shamos

tsp <- generer_TSP()
windows()
afficher_plan(tsp)
T <- enveloppe_convexe(tsp, info = TRUE)
windows()
afficher_tournee(tsp, T)






# Exemple PPV
tsp <- generer_TSP()
afficher_plan(tsp)
T <- PPV(tsp)
afficher_tournee(tsp, T)
T
deux_opt(tsp, T)
afficher_tournee(tsp, deux_opt(tsp, T))
tsp <- generer_TSP()
T <- PPV(tsp)
a <- afficher_tournee(tsp, T)
T2 <- deux_opt(tsp, T, nbrmax = Inf)
b <- afficher_tournee(tsp, T2)
figure <- ggarrange(a, b, ncol = 2, nrow = 1)
figure
calcul_cout(tsp, T)
calcul_cout(tsp, T2)
