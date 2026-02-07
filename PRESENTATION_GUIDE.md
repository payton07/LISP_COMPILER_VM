# 🎓 Kit de Survie : Compilateur & Machine Virtuelle Lisp
**Projet :** LISP_COMPILER_VM  
**Langage Hôte :** Common Lisp  
**Objectif :** Implémentation complète : VM, Compilateur, Runtime.

---

## 1. 🏗️ La Structure de la VM : Design & Choix
Le cœur du projet est la structure `vm`. Voici pourquoi elle est définie ainsi :

```lisp
(defstruct vm
  (name "VM")      ;; Identifiant convivial
  (memory ...)     ;; Architecture unifiée (Von Neumann)
  (r0 nil)         ;; Registre de travail (ex-ACC)
  (r1 nil)         ;; Registre temporaire
  (pc 0)           ;; Séquenceur
  (sp 9999)        ;; Gestion de pile
  (fp 9999)        ;; Gestion de contexte
  (hp 0)           ;; Gestion dynamique
  (cp 0)           ;; Gestion code persistant
  (labels ...)     ;; Table des symboles (Linker dynamique)
  (running t))     ;; État
```

### Pourquoi ces choix ?
1.  **Mémoire Unifiée (Array 10000)** : J'ai choisi une architecture **Von Neumann** simulée où le code et les données partagent le même espace d'adressage linéaire.
    *   **Avantage** : Simplicité d'implémentation (un seul tableau).
    *   **Flexibilité** : La frontière entre le Tas (Heap, qui monte) et la Pile (Stack, qui descend) est dynamique. On utilise l'espace au maximum.
2.  **Architecture à Accumulateur (R0)** :
    *   **Pourquoi ?** C'est le modèle le plus simple pour un compilateur. Au lieu de gérer l'allocation de registres (complexe), on assume que **chaque opération** prend son entrée dans R0 et met son résultat dans R0.
    *   **R1 (Registre Auxiliaire)** : Sert uniquement à stocker temporairement le 2ème opérande d'une opération binaire (ex: `ADD`) pendant qu'on manipule la pile.
3.  **Hash-Table pour les Labels** :
    *   Au lieu de résoudre les adresses de saut en "2 passes" pures lors de la compilation, la VM garde une table de symboles (`labels`). Cela permet un "linkage" dynamique : `vm-load` peut résoudre un saut vers `FIB` même si `FIB` a été chargé bien avant.

---

## 2. ⚙️ Les Principes de la Compilation
Le compilateur (`compiler.lisp`) est un traducteur **S-Expression -> ASM**.

### La Stratégie de Compilation
Il parcourt l'arbre syntaxique Lisp (AST) récursivement.
*   **Invariant** : "Le code généré pour une expression laisse TOUJOURS le résultat de cette expression dans `R0`."

### Exemple de traduction : `(+ A B)`
C'est une opération binaire. Le schéma est toujours le même :
1.  **Compiler A** -> `R0 = A`.
2.  **`PUSH R0`** -> Sauvegarde A sur la pile (car on a besoin de R0 pour calculer B).
3.  **Compiler B** -> `R0 = B`.
4.  **`PUSH R0`** -> Sauvegarde B sur la pile.
5.  **`POP R1`** -> Récupère B dans R1.
6.  **`POP R0`** -> Récupère A dans R0.
7.  **`ADD R1`** -> `R0 = R0 + R1`.

*Pourquoi PUSH/POP ?* Pour garantir que le calcul de B n'écrase pas le résultat de A, même si B est une expression très complexe (ex: un autre appel de fonction).

### Gestion des Variables (`compile-var`)
Le compilateur ne connait pas les adresses mémoire absolues des variables locales. Il utilise des **offsets relatifs à FP**.
*   **Argument n** : `FP + offset` (Positif, car poussé avant l'appel).
*   **Variable locale n** : `FP - offset` (Négatif, car poussé après l'entrée dans la fonction).

---

## 3. 📥 Le Chargement du Code (`vm-load`)
C'est l'étape de "Linkage" et de "Loading".

1.  **Code Pointer (CP)** : La VM retient l'adresse `cp` où s'arrête le code actuel. Le nouveau code est écrit à partir de là.
2.  **Résolution des Labels** :
    *   Passe 1 : On parcourt le code ASM. Si on voit `(LABEL X)`, on associe `X` -> `adresse_courante` dans la table `vm-labels`.
    *   Passe 2 : On écrit les instructions en mémoire. Les instructions de saut (`JMP`, `JSR`) utiliseront la table pour trouver l'adresse cible au moment de l'exécution (ou `vm-load` pourrait les patcher, ici la VM fait le lookup dynamique dans `jump-to`).
3.  **Mise à jour** : `cp` est avancé pour que le prochain chargement n'écrase rien.

---

## 4. 🏃 L'Exécution (`vm-run`) : Au cœur de la machine

C'est ici que le code prend vie. La VM exécute une boucle infinie (tant que `running` est vrai) :
1.  **Fetch** : Récupère l'instruction à l'adresse `PC`.
2.  **Decode & Execute** : Exécute l'action correspondante (ADD, PUSH, JMP...).
3.  **Update** : Met à jour `PC` (sauf si saut).

### 🔍 Zoom sur la dynamique des registres (SP, FP, CP)

C'est la partie la plus critique à comprendre. Visualisons l'état de la machine lors d'un appel de fonction `(fib 10)`.

**État Initial :**
*   **CP (Code Pointer)** : Pointe à la fin du code chargé (ex: 50). Le code est statique en bas de la mémoire (0..49).
*   **SP (Stack Pointer)** : Pointe tout en haut de la mémoire (9999). La pile est vide.
*   **FP (Frame Pointer)** : Pointe aussi en haut (9999). Aucun contexte de fonction actif.

#### Étape 1 : Préparation de l'appel (Caller)
On veut appeler `fib(10)`.
1.  `LOAD 10` -> `R0 = 10`.
2.  `PUSH R0` -> On empile l'argument.
    *   `SP` passe de 9999 à **9998**.
    *   `Mem[9999] = 10`.

#### Étape 2 : Saut vers la fonction (JSR)
L'instruction `JSR FIB` est exécutée.
1.  Empile l'adresse de retour (PC actuel + 1, disons 105).
    *   `SP` passe de 9998 à **9997**.
    *   `Mem[9998] = 105`.
2.  `PC` saute à l'adresse de `FIB` (disons 10).

#### Étape 3 : Prologue de la fonction (Callee)
C'est ici que `FP` entre en jeu pour créer un "cadre" stable.
1.  `SAVE-FP` : Sauvegarde l'ancien FP (9999) sur la pile.
    *   `SP` passe de 9997 à **9996**.
    *   `Mem[9997] = 9999`.
2.  `SET-FP` : Définit le nouveau FP.
    *   `FP = SP + 1` = **9997**.
    *   Maintenant, `FP` pointe sur l'endroit où on a sauvé l'ancien FP.

**📸 État de la Pile à cet instant :**

| Adresse | Contenu | Description | Registres |
| :--- | :--- | :--- | :--- |
| 9999 | 10 | Argument n (Arg 0) | |
| 9998 | 105 | Adresse de Retour (PC) | |
| 9997 | 9999 | Ancien FP (Saved FP) | **<- FP (Actuel)** |
| 9996 | ... | (Espace libre pour variables locales) | **<- SP (Sommet)** |

**Pourquoi c'est génial ?**
*   Pour accéder à l'argument `n` : C'est toujours `FP + 2` (9997 + 2 = 9999).
*   Pour accéder à une variable locale : Ce sera `FP - 1`, `FP - 2`...
*   Peu importe combien de `PUSH` on fait ensuite pour des calculs intermédiaires (`SP` va descendre à 9990, 9980...), **`FP` reste fixe à 9997**. C'est notre phare dans la tempête.

#### Étape 4 : Épilogue et Retour
Quand la fonction a fini (résultat dans `R0`) :
1.  `RESTORE-FP` : `FP = POP()`.
    *   On lit la valeur en 9997 (qui est 9999). `FP` redevient 9999.
    *   `SP` remonte à 9997.
2.  `RTN` : `PC = POP()`.
    *   On lit la valeur en 9998 (qui est 105). `PC` redevient 105.
    *   `SP` remonte à 9998.
3.  On est revenu chez l'appelant ! Il ne reste plus qu'à dépiler l'argument (`POP R1` pour nettoyer) et `SP` revient à 9999.

---

## 5. 🚀 Les Extensions "MAX"
Tu as transformé une simple calculatrice en vrai langage.

1.  **Le Tas (Heap)** :
    *   Géré par `HP`. Indépendant de la pile.
    *   Permet aux données de survivre à la fin d'une fonction (contrairement à la pile).
2.  **Listes Chaînées** :
    *   `CONS` alloue 2 cellules consécutives au niveau de `HP`.
    *   Lisp n'est rien d'autre que des pointeurs vers ces cellules.
3.  **Boucles** :
    *   Ajout de `WHILE`. Le compilateur génère la structure de contrôle "Test -> Saut Sortie -> Corps -> Saut Début".

---

## 6. 🎭 Scénario de Démo (Pas à pas)

Ouvre ton terminal et lance `clisp`.

### Démo 1 : Persistance et Appel
*"Je crée une VM persistante. Je définis `fib` une fois, puis je peux l'appeler autant que je veux."*

```lisp
(load "vm.lisp")
(load "compiler.lisp")
(vm-create 'DEMO)

;; 1. Chargement de la définition
(vm-cle 'DEMO '(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))

;; 2. Appel ultérieur (le code est ajouté à la suite)
(vm-cle 'DEMO '(fib 6))
```
**Résultat :** `8`.

### Démo 2 : Extensions MAX (Listes et Boucles)
*"Démonstration des capacités avancées : Manipulation de mémoire dynamique et boucles impératives."*

```lisp
(vm-cle-max 'DEMO 
  '(let ((L (cons 10 (cons 20 nil)))
         (sum 0))
     (while L
       (setq sum (+ sum (car L)))
       (setq L (cdr L)))
     sum))
```
**Résultat :** `30`.

---