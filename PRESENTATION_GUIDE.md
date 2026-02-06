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
  (acc nil)        ;; Registre de travail
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
2.  **Architecture à Accumulateur (ACC)** :
    *   **Pourquoi ?** C'est le modèle le plus simple pour un compilateur. Au lieu de gérer l'allocation de registres (complexe), on assume que **chaque opération** prend son entrée dans ACC et met son résultat dans ACC.
    *   **R1 (Registre Auxiliaire)** : Sert uniquement à stocker temporairement le 2ème opérande d'une opération binaire (ex: `ADD`) pendant qu'on manipule la pile.
3.  **Hash-Table pour les Labels** :
    *   Au lieu de résoudre les adresses de saut en "2 passes" pures lors de la compilation, la VM garde une table de symboles (`labels`). Cela permet un "linkage" dynamique : `vm-load` peut résoudre un saut vers `FIB` même si `FIB` a été chargé bien avant.

---

## 2. ⚙️ Les Principes de la Compilation
Le compilateur (`compiler.lisp`) est un traducteur **S-Expression -> ASM**.

### La Stratégie de Compilation
Il parcourt l'arbre syntaxique Lisp (AST) récursivement.
*   **Invariant** : "Le code généré pour une expression laisse TOUJOURS le résultat de cette expression dans `ACC`."

### Exemple de traduction : `(+ A B)`
C'est une opération binaire. Le schéma est toujours le même :
1.  **Compiler A** -> `ACC = A`.
2.  **`PUSH ACC`** -> Sauvegarde A sur la pile (car on a besoin de ACC pour calculer B).
3.  **Compiler B** -> `ACC = B`.
4.  **`PUSH ACC`** -> Sauvegarde B sur la pile.
5.  **`POP R1`** -> Récupère B dans R1.
6.  **`POP ACC`** -> Récupère A dans ACC.
7.  **`ADD R1`** -> `ACC = ACC + R1`.

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

## 4. 🏃 L'Exécution (`vm-run`) : Le Cycle Fetch-Decode-Execute
C'est une boucle `while (running)`.

1.  **Fetch** : `instr = memory[PC]`. On récupère l'instruction courante.
2.  **Increment** : `PC = PC + 1`.
3.  **Execute** : On dispatch selon le type d'instruction (ex: `ADD` appelle `op-add`).

### Zoom sur la Pile et les Registres (Runtime)
C'est là que la magie opère, surtout pour les fonctions.

**Scénario : Appel de fonction `(fib 10)`**

1.  **Avant l'appel (Caller)** :
    *   On empile l'argument `10`. `SP` descend.
    *   `JSR FIB` : Empile l'adresse de retour (PC actuel). `SP` descend. Saute à `FIB`.

2.  **Entrée dans la fonction (Callee - Prologue)** :
    *   `SAVE-FP` : On empile l'ancien `FP`. C'est le lien dynamique vers le contexte de l'appelant.
    *   `SET-FP` : `FP = SP + 1`. Maintenant, `FP` pointe sur l'ancien FP sauvegardé. C'est notre nouveau point de repère stable.
    *   *La pile ressemble à : [Args] [RetAddr] [OldFP] <- FP*

3.  **Corps de la fonction** :
    *   Les variables locales (`LET`) sont empilées sous FP.
    *   On y accède via `LREF -1`, `LREF -2`...

4.  **Sortie (Epilogue)** :
    *   `RESTORE-FP` : `FP = Pop()`. On remet `FP` à sa valeur d'avant l'appel. On restaure le contexte de l'appelant.
    *   `RTN` : `PC = Pop()`. On dépile l'adresse de retour et on y saute.

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

## 7. ❓ Questions Pièges

**Q: Pourquoi `LREF -1` et pas juste `POP` pour lire une variable ?**
R: Parce qu'on peut avoir besoin de lire une variable plusieurs fois ! `POP` détruit la donnée de la pile. `LREF` (Local Reference) lit la donnée à un offset fixe sans modifier le pointeur de pile. C'est de l'accès aléatoire, pas séquentiel.

**Q: Que se passe-t-il si j'appelle une fonction récursive infinie ?**
R: La pile va grandir vers le bas (`SP--`) jusqu'à rencontrer le tas (`HP`) ou atteindre 0. La VM détectera un "Stack Overflow".

**Q: Comment ton compilateur gère l'ordre d'évaluation ?**
R: Strictement de gauche à droite. Pour `(+ A B)`, je compile A, je le push, je compile B, je le push. C'est standard et sûr.
