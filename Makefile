# Makefile pour le projet LISP_COMPILER_VM

# Variables
LISP=clisp
TEST_FIBO=test_Fibo_6.lisp
TEST_PERSIST=test_Fibo_Persistence.lisp
TEST_MAX=test_Max.lisp

.PHONY: all test test-fibo test-persist test-max help clean

all: test

# Lance tous les tests
test: test-fibo test-persist test-max
	@echo "\n✅ Tous les tests ont été exécutés avec succès."

test-fibo:
	@echo "\n>>> Exécution du Test Fibonacci (Base)..."
	$(LISP) $(TEST_FIBO)

test-persist:
	@echo "\n>>> Exécution du Test de Persistance (CP)..."
	$(LISP) $(TEST_PERSIST)

test-max:
	@echo "\n>>> Exécution du Test Extensions MAX (Listes/While)..."
	$(LISP) $(TEST_MAX)

# Aide
help:
	@echo "Commandes disponibles :"
	@echo "  make test          : Lance toute la suite de tests"
	@echo "  make test-fibo     : Lance uniquement le test Fibonacci"
	@echo "  make test-persist  : Lance le test de persistance du code"
	@echo "  make test-max      : Lance le test des extensions (listes, boucles)"
	@echo "  make clean         : Supprime les fichiers temporaires"

# Nettoyage (si des fichiers compiled .fas ou logs traînent)
clean:
	rm -f *.fas *.lib *.mem
	@echo "🧹 Projet nettoyé."
