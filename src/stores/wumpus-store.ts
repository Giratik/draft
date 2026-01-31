import { defineStore, acceptHMRUpdate } from 'pinia';
import {
  WorldState,
  Percept,
  Fluents,
  GameState,
  HunterState,
  ActionResponse,
  Action,
  Position, // Import Position
} from 'src/components/models';

interface WumpusState {
  worldState: WorldState;
  percepts: Percept[];
  previousFluents: Fluents | undefined;
  hunterState: HunterState;
  action: Action;
}

export const useWumpusStore = defineStore('wumpus', {
  state: (): WumpusState => ({
    worldState: new WorldState(),
    previousFluents: undefined,
    percepts: [],
    hunterState: new HunterState(),
    action: Action.None,
  }),
  getters: {
    gridSize: (state) => {
      if (state.worldState.eternals) {
        console.log(state.worldState.eternals.cells.length);
        return Math.sqrt(state.worldState.eternals.cells.length);
      } else {
        return 0;
      }
    },
    isGameOver: (state) => {
      return state.worldState.fluents?.game_state === GameState.Finished;
    },
    score: (state) => {
      return state.worldState.fluents ? state.worldState.fluents.score : 0;
    },
  },
  actions: {
    async initGame() {
      fetch('http://localhost:8080/default', {
        method: 'PUT',
      })
        .then((response) => response.json())
        .then((json) => {
          this.worldState = new WorldState(json);
          this.percepts = [];
          console.log(json);
          this.hunterState = new HunterState(this.worldState, this.percepts);
        });
    },
    async initGameWithSize(size: number) {
      fetch('http://localhost:8080/init', {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ size }),
      })
        .then((response) => response.json())
        .then((json) => {
          this.worldState = new WorldState(json.state);
          this.percepts = json.percepts;
          console.log(json);
          this.hunterState = new HunterState(this.worldState, this.percepts);
        });
    },
    async resetGame() {
      fetch('http://localhost:8080/init', {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ size: 1 }),
      })
        .then((response) => response.json())
        .then((json) => {
          this.worldState = new WorldState(json.state);
          this.percepts = json.percepts;
          console.log(json);
          this.hunterState = new HunterState(this.worldState, this.percepts);
        });
    },
    async performSimAction(action: string) {
      console.log('Current safeCells:', this.hunterState.beliefs.safeCells);
      console.log('Current breezesuspectCells:', this.hunterState.beliefs.breezesuspectCells);
      console.log('Current stenchsuspectCells:', this.hunterState.beliefs.stenchsuspectCells);
      console.log('Current pitCells:', this.hunterState.beliefs.pitCells);
      console.log('Current wumpusCells:', this.hunterState.beliefs.wumpusCells);

      fetch('http://localhost:8080/sim', {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          ...this.worldState,
          previous_fluents: this.previousFluents
            ? this.previousFluents
            : this.worldState.fluents,
          plan: action,
        }),
      })
        .then((response) => response.json())
        .then((json) => {
          this.previousFluents = this.worldState.fluents;
          this.worldState.fluents = json.fluents;
          this.percepts = json.percepts;
          this.hunterState.percepts = this.percepts;

          // Helper function
          const addUniqueCell = (list: Position[], cell: Position) => {
            if (!list.some((c) => c.x === cell.x && c.y === cell.y)) {
              list.push(cell);
              console.log(`Added ${cell.x},${cell.y} to list`);
            }
          };

          // Get current position
          const currentPosition: Position = {
            x: this.worldState.fluents.fat_hunter.c.x,
            y: this.worldState.fluents.fat_hunter.c.y,
          };
          // Skip updates if already in a safe cell
          const alreadyInSafeCells = this.hunterState.beliefs.safeCells.some(
            (cell) => cell.x === currentPosition.x && cell.y === currentPosition.y
          );
          if (alreadyInSafeCells) {
            console.log(
              `Skipping cell updates since current cell ${currentPosition.x},${currentPosition.y} is already in safeCells`
            );
            return;
          }
          const gridSize = Math.sqrt(this.worldState.eternals.cells.length);

          // Remove the current cell from stenchsuspectCells if it's there
          const stenchsuspectIndex = this.hunterState.beliefs.stenchsuspectCells.findIndex(
            (cell) => cell.x === currentPosition.x && cell.y === currentPosition.y
          );
          if (stenchsuspectIndex !== -1) {
            this.hunterState.beliefs.stenchsuspectCells.splice(stenchsuspectIndex, 1);
            console.log(`Removed ${currentPosition.x},${currentPosition.y} from stenchsuspectCells`);
          }

          // Remove the current cell from breezesuspectCells if it's there
          const breezesuspectIndex = this.hunterState.beliefs.breezesuspectCells.findIndex(
            (cell) => cell.x === currentPosition.x && cell.y === currentPosition.y
          );
          if (breezesuspectIndex !== -1) {
            this.hunterState.beliefs.breezesuspectCells.splice(breezesuspectIndex, 1);
            console.log(`Removed ${currentPosition.x},${currentPosition.y} from breezesuspectCells`);
          }
          
          // Now determine if the current cell is safe
          const isSafe = true; // Assume it's safe for now

          if (isSafe) {
            addUniqueCell(this.hunterState.beliefs.safeCells, currentPosition);
          console.log(`Adding current postion X ${currentPosition.x} and postion Y ${currentPosition.y} to Safecells`);
         }

          // Find neighboring cells and update stenchsuspectCells and pitCells if has breeze,
          if (this.percepts.includes(Percept.Breeze)) {
            const neighbors = this.getNeighboringCells(currentPosition, gridSize);

            neighbors.forEach((neighbor) => {
              // check if all conditions pass.
              if (this.neighborCellConditionsApprove(neighbor, gridSize)) {
                // Add neighbor cells to pitCell or update already in
                const breezesuspectIndex = this.hunterState.beliefs.breezesuspectCells.findIndex(
                  (breezesus) => breezesus.x === neighbor.x && breezesus.y === neighbor.y
                );
                //Add to pitCell if already in breezesuspectCells
                if (breezesuspectIndex !== -1) {
                  this.hunterState.beliefs.pitCells.push(neighbor);
                  this.hunterState.beliefs.breezesuspectCells.splice(breezesuspectIndex, 1);
                  console.log(`Removed ${neighbor.x},${neighbor.y} from breezesuspectCells to pitCells`);
                } else {
                  addUniqueCell(this.hunterState.beliefs.breezesuspectCells, neighbor);                  
                }
              }
            });
          }

          // Find neighboring cells and update stenchsuspectCells and pitCells if has stench,
         if (this.percepts.includes(Percept.Stench)) {
            const neighbors = this.getNeighboringCells(currentPosition, gridSize);

            neighbors.forEach((neighbor) => {
              // check if all conditions pass.
              if (this.neighborCellConditionsApprove(neighbor, gridSize)) {
                // Add neighbor cells to pitCell or update already in
                const stenchsuspectIndex = this.hunterState.beliefs.stenchsuspectCells.findIndex(
                  (stenchsus) => stenchsus.x === neighbor.x && stenchsus.y === neighbor.y
                );
                //Add to wumpusCell if already in stenchsuspectCells
                if (stenchsuspectIndex !== -1) {
                  this.hunterState.beliefs.wumpusCells.push(neighbor);
                  this.hunterState.beliefs.stenchsuspectCells.splice(stenchsuspectIndex, 1);
                  console.log(`Removed ${neighbor.x},${neighbor.y} from stenchsuspectCells to wumpusCells`);
                } else {
                  // Add to pitCell if not already in
                  addUniqueCell(this.hunterState.beliefs.stenchsuspectCells, neighbor);
                }
                  
                }
            });
          }
        });
    },

    isWall(position: Position): boolean {
      if (!this.worldState.eternals || !this.worldState.eternals.eat_walls) {
        return false; // Or handle the case where eternals are not initialized properly
      }
      return this.worldState.eternals.eat_walls.some(
        (wall) => wall.c.x === position.x && wall.c.y === position.y
      );
    },
    getNeighboringCells(position: Position, gridSize: number): Position[] {
      const neighbors: Position[] = [
        { x: position.x - 1, y: position.y }, // West
        { x: position.x + 1, y: position.y }, // East
        { x: position.x, y: position.y - 1 }, // North
        { x: position.x, y: position.y + 1 }, // South
      ];

      return neighbors.filter(
        (neighbor) =>
          neighbor.x >= 0 && neighbor.x < gridSize && neighbor.y >= 0 && neighbor.y < gridSize
      );
    },

    neighborCellConditionsApprove(neighbor: Position, gridSize: number) {
      const isNeighborWall = this.isWall(neighbor);
    const isInSafeCells = this.hunterState.beliefs.safeCells.some(
        (safeCell) => safeCell.x === neighbor.x && safeCell.y === neighbor.y
      );
  // Check if neighbor is already on pitCells
    const alreadyInPitCells = this.hunterState.beliefs.pitCells.some(
        (pitCell) => pitCell.x === neighbor.x && pitCell.y === neighbor.y
      );
              const alreadyInWumpusCell = this.hunterState.beliefs.wumpusCells.some(
        (WumpusCell) => WumpusCell.x === neighbor.x && WumpusCell.y === neighbor.y
      );

          return(
      neighbor.x >= 0 &&
      neighbor.x < gridSize &&
      neighbor.y >= 0 &&
      neighbor.y < gridSize &&
      !isNeighborWall &&
      !isInSafeCells &&
      !alreadyInPitCells &&
           !alreadyInWumpusCell
    )
  },
    //important
    async getHunterAction() {
      console.log('safeCells', this.hunterState.beliefs.safeCells); //CHECK
      console.log('breezesuspectCells', this.hunterState.beliefs.breezesuspectCells); //CHECK
      console.log('stenchsuspectCells', this.hunterState.beliefs.stenchsuspectCells); //CHECK
            console.log('pitCells', this.hunterState.beliefs.pitCells);
       console.log('wumpusCells', this.hunterState.beliefs.wumpusCells); //CHECK

      fetch('http://localhost:8081/action', {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ ...this.hunterState }),
      })
        .then((response) => response.json())
        .then((json: ActionResponse) => {
          this.hunterState = json.hunterState;
          this.action = json.action;
        });
    },
  },
});

if (import.meta.hot) {
  import.meta.hot.accept(acceptHMRUpdate(useWumpusStore, import.meta.hot));
}