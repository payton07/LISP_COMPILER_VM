# LISP_COMPILER_VM

Un compilateur Lisp et une Machine Virtuelle (VM) à accumulateur implémentés en Common Lisp.

Ce projet a été réalisé dans le cadre du module de Compilation (M1 GL). Il démontre la chaîne complète de transformation du code : de l'expression Lisp de haut niveau jusqu'à l'exécution d'instructions bytecode sur une architecture simulée.

## 🚀 Fonctionnalités

### Machine Virtuelle (VM)
*   **Architecture** : Von Neumann (Code et Données unifiés), Registre `R0` (Accumulateur).
*   **Composants** :
    *   `R0` : Registre principal de travail.
    *   `SP` (Stack Pointer) : Gestion de la pile (descendante).
    *   `FP` (Frame Pointer) : Gestion des contextes de fonctions.
    *   `HP` (Heap Pointer) : Gestion de la mémoire dynamique (ascendante).
    *   `CP` (Code Pointer) : Gestion du chargement incrémental de code.
*   **Persistance** : Le code peut être chargé séquentiellement (définitions puis appels).

### Compilateur
*   **Core** : Arithmétique, Comparaisons, `IF`, `LET`, `DEFUN` (Récursif).
*   **Extensions MAX** :
    *   **Listes** : `CONS`, `CAR`, `CDR`, `RPLACA`, `RPLACD` (Allocation sur le Tas).
    *   **Contrôle** : Boucles `WHILE`.
    *   **Mémoire** : `SETQ` (Assignation), `AREF` (Accès direct).

## 🛠️ Utilisation

### Prérequis
*   Un interpréteur Common Lisp (ex: `clisp` ou `sbcl`).

### Démarrage Rapide
Lancer l'environnement et charger les fichiers :

```lisp
(load "vm.lisp")
(load "compiler.lisp")

;; Créer une VM nommée 'DEMO
(vm-create 'DEMO)

;; Compiler et exécuter du code
(vm-cle 'DEMO '(* 10 20)) 
;; -> RESULTAT : 200
```

### Exécuter les Tests
Vous pouvez lancer les tests manuellement ou utiliser le **Makefile** fourni :

```bash
# Via le Makefile (Recommandé)
make test

# Manuellement
clisp test_Fibo_6.lisp
clisp test_Fibo_Persistence.lisp
clisp test_Max.lisp
```
### Exemple éxecution
Execution de factorielle 20 :
```bash
(vm-create 'FACT)
(vm-cle 'FACT '(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))))
(vm-cle 'FACT '(fact 20))
```

## 📚 Documentation
Pour une explication détaillée de l'architecture et du fonctionnement interne (Registres, Pile, Cycle d'exécution), consultez le [Guide de Présentation](PRESENTATION_GUIDE.md).

## 🏗️ Structure du Projet
*   `vm.lisp` : Implémentation de la VM, du jeu d'instructions et du chargeur.
*   `compiler.lisp` : Le compilateur Lisp vers ASM.
*   `PRESENTATION_GUIDE.md` : documentation technique détaillée.
