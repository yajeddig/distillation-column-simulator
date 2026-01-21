# Installation avec uv

Ce projet utilise [uv](https://github.com/astral-sh/uv) pour la gestion des dépendances Python - un gestionnaire de paquets ultra-rapide et moderne.

## Installation d'uv

### macOS/Linux
```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

### Windows
```powershell
powershell -c "irm https://astral.sh/uv/install.ps1 | iex"
```

### Via pip (alternative)
```bash
pip install uv
```

## Démarrage rapide

### 1. Compiler le backend Fortran
```bash
cd backend
make
cd ..
```

### 2. Installer les dépendances Python avec uv
```bash
# Créer l'environnement virtuel et installer les dépendances
uv sync

# Ou manuellement
uv venv
source .venv/bin/activate  # Linux/macOS
# .venv\Scripts\activate   # Windows
uv pip install -e .
```

### 3. Lancer l'interface web
```bash
uv run streamlit run frontend/app.py
```

## Gestion des dépendances

### Ajouter une dépendance
```bash
uv add package-name
```

### Ajouter une dépendance de développement
```bash
uv add --dev pytest
```

### Mettre à jour les dépendances
```bash
uv lock --upgrade
```

### Installer depuis le lock file
```bash
uv sync --frozen
```

## Commandes utiles

```bash
# Exécuter un script avec uv
uv run python script.py

# Exécuter l'application Streamlit
uv run streamlit run frontend/app.py

# Lancer les tests (une fois configurés)
uv run pytest

# Formater le code
uv run black .

# Linter le code
uv run ruff check .
```

## Pourquoi uv ?

✅ **10-100x plus rapide** que pip  
✅ **Résolution de dépendances fiable** (comme Poetry)  
✅ **Lockfile automatique** pour la reproductibilité  
✅ **Compatible pip** - fonctionne avec requirements.txt  
✅ **Pas de fichiers volumineux** (.venv dans .gitignore)  

## Migration depuis pip

Si vous avez un `requirements.txt` existant:
```bash
uv pip install -r requirements.txt
```

Le fichier `pyproject.toml` remplace `requirements.txt` pour une gestion moderne des dépendances.
