export interface Position {
  x: number;
  y: number;
}

export interface ObjectWithId {
  id: string;
}

export enum EpistemicValue {
  orTrue = 'orTrue',
  knownTrue = 'knownTrue',
  knownFalse = 'knownFalse',
}

export interface BeliefWithEpistemicValue<B> {
  belief: B[];
  epistemicValue: EpistemicValue;
}

export interface LocationFact {
  c: Position;
}

export type EAt = LocationFact;

export interface EAtExit extends EAt {
  e: ObjectWithId;
}

export interface EAtPit extends EAt {
  p: ObjectWithId;
}

export interface EAtWalls extends EAt {
  w: ObjectWithId;
}

export interface EAtWumpus extends EAt {
  w: ObjectWithId;
}

export type EatTypes = EAtExit | EAtPit | EAtWalls | EAtWumpus;

export type FAt = LocationFact;

export interface FAtGold extends FAt {
  g: ObjectWithId;
}

export interface FAtHunter extends FAt {
  h: ObjectWithId;
}

export interface Has {
  h: ObjectWithId;
}

export interface HasGold extends Has {
  g: ObjectWithId;
}

export interface HasArrow extends Has {
  a: ObjectWithId;
}

export type HasTypes = HasGold | HasArrow;

export interface Fatal {
  p: Position;
}

export enum GameState {
  Running = 'running', //Game is running
  Finished = 'finished', //Game is finished
}

export enum Direction {
  North = 'north',
  South = 'south',
  East = 'east',
  West = 'west',
}

export interface Dir {
  h: ObjectWithId;
  d: Direction;
}

export interface VisitedItem {
  to: Position;
  from: Position;
}

export interface CertainEternals {
  eat_exit: EAtExit;
  eat_walls: EAtWalls[];
  cells: Position[];
}

export interface FullyObservableEternals extends CertainEternals {
  eat_pit: EAtPit[];
  eat_wumpus: EAtWumpus[];
}

export interface UncertainEternals {
  eat_pit: BeliefWithEpistemicValue<EAtPit>[];
  eat_wumpus: BeliefWithEpistemicValue<EAtWumpus>[];
}

export enum EternalElement {
  Exit = 'E',
  Pit = 'P',
  Walls = 'Wall',
  Wumpus = 'W',
}

export interface UncertailFluents {
  fatal: BeliefWithEpistemicValue<Fatal>[];
}
export interface Fluents {
  alive: ObjectWithId[];
  dir: Dir[];
  fat_gold: FAtGold[];
  fat_hunter: FAtHunter;
  has_arrow: HasArrow[];
  has_gold: HasGold[];
  visited: VisitedItem[];
  score: number;
  game_state: GameState;
}

export enum FluentElement {
  Gold = 'G',
  Hunter = 'H',
  AliveWumpus = 'A',
}

export enum Percept {
  Bump = 'bump',
  Glitter = 'glitter',
  Stench = 'stench',
  Breeze = 'breeze',
  Scream = 'scream',
}

export enum Action {
  Move = 'move',
  Grab = 'grab',
  Shoot = 'shoot',
  Climb = 'climb',
  Left = 'left',
  Right = 'right',
  None = 'none',
}
//Wumpus board model
export class WorldState {
  eternals: FullyObservableEternals | undefined;
  fluents: Fluents | undefined;
  constructor(json?: any) {
    if (json?.eternals && json?.fluents) {
      this.eternals = json.eternals;
      this.fluents = json.fluents;
    } else {
      this.eternals = undefined;
      this.fluents = undefined;
    }
  }
  eAtPosition(position: Position): EternalElement[] {
    if (!this.eternals) {
      //TODO: return nothing
      return [];
    }
    const eternalsAtPosition = [];
    if (
      this.eternals.eat_exit.c.x === position.x &&
      this.eternals.eat_exit.c.y === position.y
    ) {
      eternalsAtPosition.push(EternalElement.Exit);
    }
    for (const pitEAt of this.eternals.eat_pit) {
      if (pitEAt.c.x === position.x && pitEAt.c.y === position.y) {
        eternalsAtPosition.push(EternalElement.Pit);
        break;
      }
    }
    //TODO: maybe do the wall check here, and move gridSize calc to here
    for (const wumpusEAt of this.eternals.eat_wumpus) {
      if (wumpusEAt.c.x === position.x && wumpusEAt.c.y === position.y) {
        eternalsAtPosition.push(EternalElement.Wumpus);
        break;
      }
    }
    return eternalsAtPosition;
  }
  fAtPosition(position: Position): FluentElement[] {
    if (!this.fluents) {
      return [];
    }
    const fluentsAtPosition = [];
    for (const goldFAt of this.fluents.fat_gold) {
      if (goldFAt.c.x === position.x && goldFAt.c.y === position.y) {
        fluentsAtPosition.push(FluentElement.Gold);
        break;
      }
    }
    if (
      this.fluents.fat_hunter.c.x === position.x &&
      this.fluents.fat_hunter.c.y === position.y
    ) {
      fluentsAtPosition.push(FluentElement.Hunter);
    }
    const wumpusAtPosition = this.eternals?.eat_wumpus.find((eAtWumpus) => {
      return eAtWumpus.c.x === position.x && eAtWumpus.c.y === position.y;
    });
    if (
      wumpusAtPosition &&
      this.fluents.alive.find(({ id }) => id === wumpusAtPosition.w.id)
    ) {
      fluentsAtPosition.push(FluentElement.AliveWumpus);
    }
    return fluentsAtPosition;
  }
}

export interface Beliefs {
  step: number;
  certain_eternals: CertainEternals;
  certain_fluents: Fluents;
  uncertain_eternals: UncertainEternals;
  uncertain_fluents: UncertailFluents;
  safeCells: Position[];
  breezesuspectCells: Position[];
  stenchsuspectCells: Position[];
  pitCells: Position[];
  wumpusCells: Position[];
}

export class HunterState {
  beliefs: Beliefs;
  percepts: Percept[];
  constructor(worldState?: WorldState, percepts?: Percept[], json?: any) {
    if (worldState?.eternals && worldState.fluents && percepts) {
      this.percepts = percepts;
      this.beliefs = {
        step: 0,
        certain_eternals: {
          eat_exit: worldState.eternals.eat_exit,
          eat_walls: worldState.eternals.eat_walls,
          cells: worldState.eternals.cells,
        },
        certain_fluents: {
          game_state: worldState.fluents.game_state,
          score: worldState.fluents.score,
          alive: [{ id: 'hunter' }] as ObjectWithId[],
          dir: worldState.fluents.dir,
          fat_gold: [],
          fat_hunter: worldState.fluents.fat_hunter,
          has_arrow: worldState.fluents.has_arrow,
          has_gold: [],
          visited: worldState.fluents.visited,
        },
        uncertain_eternals: {
          eat_pit: [],
          eat_wumpus: [],
        },
        uncertain_fluents: {
          fatal: [],
        },
        safeCells: [],
        breezesuspectCells: [],
        stenchsuspectCells: [],
        pitCells: [],
        wumpusCells: [],
      };
    } else if (json?.beliefs && json?.percepts) {
      this.beliefs = json.beliefs;
      this.percepts = json.percepts;
    } else {
      // @ts-expect-error TODO: add a good default value
      this.beliefs = {};
      this.percepts = [];
    }
  }
}

export interface ActionResponse {
  hunterState: HunterState;
  action: Action;
}