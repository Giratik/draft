<template>
  <div class="box col">
    <div class="top-section row">
      <template v-if="eternals.length === 0">
        <span>/</span>
      </template>
      <template v-else>
        <span>E:</span>
        <span v-for="eternal in eternals" :key="eternal">{{ eternal }}</span>
      </template>
    </div>
    <div class="top-middle-section row">
      <template v-if="fluents.length === 0">
        <span>/</span>
      </template>
      <template v-else>
        <span>F:</span>
        <span v-for="fluent in fluents" :key="fluent">{{ fluent }}</span>
      </template>
    </div>
    <template v-if="percepts">
      <div class="bottom-middle-section row">
        <span>P:</span>
        <span v-for="percept in wumpusStore.percepts" :key="percept">{{
          percept
        }}</span>
      </div>
    </template>
    <div class="bottom-section row">
      <span>({{ props.pos.x }}, {{ props.pos.y }})</span>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue';
import { FluentElement, Position } from './models';

import { useWumpusStore } from 'src/stores/wumpus-store';
import { isComputedPropertyName } from 'typescript';

const wumpusStore = useWumpusStore();

interface Props {
  pos: Position;
  gridSize: number;
}

const props = defineProps<Props>();

const atLimit = computed(() => {
  const max = props.gridSize - 1;
  return (
    props.pos.x === 0 ||
    props.pos.x === max ||
    props.pos.y === 0 ||
    props.pos.y === max
  );
});

const eternals = computed(() => wumpusStore.worldState.eAtPosition(props.pos));
const fluents = computed(() => wumpusStore.worldState.fAtPosition(props.pos));
const percepts = computed(() => fluents.value.includes(FluentElement.Hunter));
</script>

<style scoped lang="scss">
.box {
  width: 75px;
  height: 75px;
  background-color: v-bind('atLimit ? "grey" : "blue"');
  color: white;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  font-size: 8px;
  border: 1px solid black;
}

.top-section,
.top-middle-section,
.bottom-middle-section,
.bottom-section {
  display: flex;
  justify-content: space-between;
  align-content: start;
  width: 100%;
}

.top-section span,
.top-middle-section span,
.bottom-middle-section span {
  flex: 1;
  text-align: center;
  align-content: start;
}

.bottom-section span {
  align-self: flex-end;
}
</style>
