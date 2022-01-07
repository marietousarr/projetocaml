# projetocaml : Implémentation de l'algorithme de Fold-Fulkerson

Binôme : Mariétou SARR et Félix MURAT

Pour exécuter/compiler aller dans le répertoire ocaml-maxflow-project-master.

Pour compiler : make build

Pour exécuter le programme par défault : make demo

Pour exécuter le programme sur un graphe, une source et un puits en particulier : make maxflow_of chemin_de_notre_graphe numero_noeud_source numero_noeud_puits

Pour supprimer les fichiers créés après une exécution : make clean

Ce projet ne marche que pour les graphes avec des capacités inférieures à max_int = 2147483647.
