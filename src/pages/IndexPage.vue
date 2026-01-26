<template>
  <q-page class="col items-center justify-evenly flex-center">
    <div class="row q-py-md">
      <q-btn @click="initGame">Default Book Game</q-btn>
      <q-btn @click="resetWithSize">Reset to custom size Game</q-btn>
      <q-input
        :disable="wumpusStore.isGameOver"
        v-model.number="gameSize"
        label="Game Size"
        type="number"
      ></q-input>
      <q-btn @click="resetGame">Reset to default size Game</q-btn>
    </div>
    <div class="row">
      <div class="col hunter-board">
        <GameBoard />
      </div>
      <!-- <div class="col server-board">
        <GameBoard />
      </div> -->
    </div>
    <div class="row">
      <q-btn @click="requestHunterAction" :disable="wumpusStore.isGameOver"
        >Request Hunter Action</q-btn
      >
      <q-input
        :disable="wumpusStore.isGameOver"
        v-model="wumpusStore.action"
        label="Hunter Action"
        type="text"
      ></q-input>
      <q-btn @click="forward" :disable="wumpusStore.isGameOver">Forward</q-btn>
      <q-btn @click="left" :disable="wumpusStore.isGameOver">Left</q-btn>
      <q-btn @click="right" :disable="wumpusStore.isGameOver">Right</q-btn>
      <q-btn @click="shoot" :disable="wumpusStore.isGameOver">Shoot</q-btn>
      <q-btn @click="grab" :disable="wumpusStore.isGameOver">Grab</q-btn>
      <q-btn @click="exit" :disable="wumpusStore.isGameOver">Exit</q-btn>
      <span>Score = {{ wumpusStore.score }}</span>
    </div>
    <!-- <example-component
      title="Example component"
      active
      :todos="todos"
      :meta="meta"
    ></example-component> -->
  </q-page>
</template>

<script setup lang="ts">
import { useWumpusStore } from 'src/stores/wumpus-store';
import GameBoard from 'components/GameBoard.vue';
import { Action } from 'src/components/models';
import { ref } from 'vue';

const wumpusStore = useWumpusStore();

const gameSize = ref(1);

function initGame() {
  wumpusStore.initGame();
}

function resetWithSize() {
  wumpusStore.initGameWithSize(+gameSize.value);
}

function resetGame() {
  wumpusStore.resetGame();
}

function forward() {
  wumpusStore.performSimAction(Action.Move);
}

function left() {
  wumpusStore.performSimAction(Action.Left);
}

function right() {
  wumpusStore.performSimAction(Action.Right);
}

function shoot() {
  wumpusStore.performSimAction(Action.Shoot);
}

function grab() {
  wumpusStore.performSimAction(Action.Grab);
}

function exit() {
  wumpusStore.performSimAction(Action.Climb);
}

function requestHunterAction() {
  wumpusStore.getHunterAction();
}
</script>
