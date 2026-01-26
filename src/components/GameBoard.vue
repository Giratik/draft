<template>
  <div
    class="board"
    v-if="
      wumpusStore.worldState.eternals?.cells.length &&
      wumpusStore.worldState.eternals?.cells.length > 0
    "
  >
    <div class="row" v-for="n in wumpusStore.gridSize + 1" :key="n">
      <div class="col" v-for="m in wumpusStore.gridSize" :key="m">
        <!-- TODO: Investigate! Why is this hack necessary???? -->
        <div
          class="cell"
          v-if="
            wumpusStore.worldState.eternals?.cells[
              -(n - wumpusStore.gridSize) * wumpusStore.gridSize + m - 1
            ] !== undefined
          "
        >
          <!-- {{
            wumpusStore.worldState.eternals?.cells[
              -(n - wumpusStore.gridSize) * wumpusStore.gridSize + m
            ]
          }} -->
          <BoardCell
            :gridSize="wumpusStore.gridSize"
            :pos="
              wumpusStore.worldState.eternals?.cells[
                -(n - wumpusStore.gridSize) * wumpusStore.gridSize + m - 1
              ]
            "
          />
        </div>
      </div>
    </div>
    <div class="gameover-message" v-if="wumpusStore.isGameOver">GAMEOVER</div>
  </div>
</template>

<script setup lang="ts">
import { useWumpusStore } from 'src/stores/wumpus-store';
import BoardCell from './BoardCell.vue';

const wumpusStore = useWumpusStore();
</script>

<style scoped lang="scss">
.board {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  width: 100%;
  border: 3px solid red;
  position: relative;
}

.gameover-message {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  font-size: 24px;
  font-weight: bold;
  color: red;
}
</style>
