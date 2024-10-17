[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/gbdrt/pyro-intro/HEAD)

# Une courte introduction à Pyro

https://pyro.ai/

Pyro est un langage probabiliste construit sur Python et PyTorch.

Comparé aux autres langages probabilistes, Pyro a mis l'accent sur l'inférence variationelle (SVI).
PyTorch permet par ailleurs d'intégrer des réseaux de neurones complets dans les modèles probabilistes pour capturer des interactions complexes entre variables aléatoires.

Le notebook `pyro-intro.ipynb` contient deux exercices introductifs pour se familiariser avec ce langage.

## Installation

Le fichiers `requirements.txt` contient la listes de packages necessaires.
Pour tout installer :

```
pip install -r requirements.txt
```

On peut ensuite executer le notebook `pyro-intro.ipynb`, par exemple avec Jupyter lab dans le dossier courant : 

```
jupyter lab
```