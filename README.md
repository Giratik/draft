# Wumpus World Simulator (wumpus-simulator-vue)

mode d'emploi

## Install the dependencies
```bash
yarn
# or
npm install
```

### Start the app in development mode (hot-code reloading, error reporting, etc.)
```bash
npx quasar dev
```
### Build the app for production
```bash
quasar build
```

### Customize the configuration
See [Configuring quasar.config.js](https://v2.quasar.dev/quasar-cli-vite/quasar-config-js).


---
# Faire fonctionner le hunter

3 fichiers à run dans 3 terminaux différents
 ```bash
npx quasar dev
```

```bash
swipl server.pl
# ensuite sur swipl
?- run.
```

```bash
swipl logic.pl
# ensuite sur swipl
?- run_hunter.
```

Le hunter n'a actuellement aucun désir de gagner, il cherche seulement à parcourir le plateau.

# reste à faire
- lorsque le hunter démarre à côté d'un puit, il tourne en rond => forcer le hunter à prendre un risque
- actuellement le hunter ne sait jouer que sur un plateau 5*5 => rendre dynamique en fonction de la configuration du monde
- le hunter n'a pas encore comme objectif de sortir lorsqu'il récupère l'or => à implémenter
- faire en sorte de baisser la probabilité du hunter à retourner sur ces pas
afficher les probas correctement dans le débug ?
- vérifier si c'est pita et clpfd friendly