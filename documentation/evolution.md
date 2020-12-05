# Evolution du projet

## Taches a effectuees
- [ ] Implementation tour a tour avec serveur central 
- [ ] Implementation en roue libre
- [ ] Implementation sans serveur central
    - [ ] en PoW
    - [ ] en Pos

## Reponses aux problematiques

### Comment s'assurer que l'auteur n'injecte pas plusieurs lettres pour un bloc?
    - Dans le tour par tour, si un auteur inject plusieurs lettres, il a plus d'avantage de point donc 
    le jeux n'est pas equilibre pour eviter cela, le serveur central verifie s'il deja injecter un mot 
    avant de le propager dans le reseaux. 

### Descriptions de l'implementation des fonctions src/consensus.ml
    Dans le consensu chaque mots est lie a un score


### En "Roue libre" quelles modification faut-il pour l'algorithme de consensus?
->Remplir

### L'implementation resite t-elle aux attaques? Si non quelles modifs faut-il pour resister?
->Remplir

### Comment modifier le system pour eliminer le serveur central(PoW ou Pos)?
->Remplir

### Comment modifier l'algorithme de consensus, passage Pow -> Pos ou Pos -> Pow
->Remplir

### 

## Questions soulevees

* En roue libre, est-ce que les auteurs a leur tours peuvent soumettre des lettres a tout moment ?

* la fonction "fitness( st:store_word w:word)" dans "consensus.ml" calcul t'elle le score le 
total de "w" à genesis(mot de depart)?

* Pour le peer-to-peer sans serveur central a quelle moment on considere de passer a la periode
suivant pour miner le prochain block:
    - Est-ce quand tous les auteurs auront tous soumis une lettre
    
    - Ou lorsque le temps de la periode a laquelle on se trouve s'est écoulé et qu'on ait obtenu au moins un mot donné par un politicien.
    
    - En integrant sois PoW est-ce que les regles de jeux change? ou qu'est-ce qu'il faut modif 
    dans les regles, puisque que le PoW est quelque chose de tres different.
    
    - En integrant PoS les regles du jeux peuvent etre intact seul la partie du choix de la tete reste à determiner selon les votes des participants par exemple.

    (Periode == intervalle de temps)
