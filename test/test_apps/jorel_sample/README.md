# Jorel sample

Ce projet est un exemple pour montrer comment utiliser la mise à jour _à chaud_ d'une application avec [Jorel](https://github.com/emedia-project/jorel).

## Préparation

Pour jouer avec cet exemple, commencez par [cloner ce répo](https://github.com/ForYourCrashOnly/jorel_sample/fork).

Afin de ne pas perdre de temps avec l'écriture de code, nous avons mis, dans les branches 0.0.1 et 0.0.2, le code des différentes version de l'application. 

## Création de la version 0.0.1

Pour commencer, placez vous dans la branche 0.0.1.

```
git checkout 0.0.1
```

Dans cette branche, vous trouverez l'arborescence suivante :

```
.
├── erlang.mk
├── Makefile
├── README.md
└── src
    ├── jorel_sample_app.erl
    ├── jorel_sample.erl
    └── jorel_sample_sup.erl
```

Dans le `Makefile` nous déclarons l'utilisation du plugin `jorel` via les lignes :

```
DEP_PLUGINS = jorel
REL_DEPS = jorel

dep_jorel = git https://github.com/emedia-project/jorel.git master
```

Nous allons donc commencer par contruire la version 0.0.1 (branche 0.0.1), puis nous modifierons le code et contruirons la version 0.0.2 
