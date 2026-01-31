# Wumpus World Simulator (wumpus-simulator-vue)

Wumpus world simulation interface for IAS4A

## Install the dependencies
```bash
yarn
# or
npm install
```

### Start the app in development mode (hot-code reloading, error reporting, etc.)
```bash
quasar dev
```


### Lint the files
```bash
yarn lint
# or
npm run lint
```


### Format the files
```bash
yarn format
# or
npm run format
```



### Build the app for production
```bash
quasar build
```

### Customize the configuration
See [Configuring quasar.config.js](https://v2.quasar.dev/quasar-cli-vite/quasar-config-js).


#  Création agent intelligent pour le jeu du Wumpus
Nous avons modifié 3 fichiers pour créer un agent capable d'analyser son environnement et agir en conséquence.

## models.ts
Nous définissons dans ce fichier la composition du fichier json qui permet d'enregistrer les différents éléments du monde
que va rencontrer le hunter. Nous avons ajouté au fichier initial : 
- safeCells pour enregistrer les cases considérées sans danger
- breezesuspectCells où sont enregistrés les cases voisines à celle sentant une brise
- stenchsuspectCells où sont enregistrés les cases voisines à celle sentant une odeur
- pitCells où sont enregistrés les cases qui sont à coup sûr des trous
- wumpusCells où sont enregistrés les cases qui sont à coup sûr des Wumpus

## Wumpus-store.ts
C'est dans ce fichier que la logique d'analyse du monde se trouve. Ici il y a les règles pour labelliser les cases :
- Les cases safes : quand le hunter se déplace la case est automatiquement considérée safe puis enregistré dans le fichier json dans le tableau
correspondant et effacé des autres le cas échéant
- Les cases breeze et stench suspect : quand le hunter sent une brise ou une odeur les cases adjacentes qui ne sont pas safe sont mises 
dans les tableaux suspect selon la nature du Percept
- Les cases Pit et Wumpus : quand la brise ou l'odeur est perçu sur deux cases différentes alors cette case devient une case wumpus ou pit et les tableaux sont
mis-à-jour
Pour parer au problème de tourner à droite ou à gauche met-à-jour l'analyse des percept, elle n'est plus effectué lorsque on se trouve sur une case safe.

## server.pl
Dans ce fichier nous avons codé la logique de notre agent pour suggérer des actions. En utilisant les informations dans le fichier json, l'agent va 
proposer des actions.
