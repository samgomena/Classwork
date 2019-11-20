<template>
  <v-app>
    <v-content class="dogs-layout">
      <v-container fill-height>
        <div class="dogs-overlay">
          <h1 class="display-2 text-xs-center">Choose your favorite dogs</h1>
          <v-card class="dog-card">
            <transition name="fade">
              <v-img v-if="currentDogLink" height="400px" :src="currentDogLink"></v-img>
            </transition>

            <v-card-actions>
              <v-spacer></v-spacer>
              <v-btn icon @click="addToFavorites" :disabled="isAlreadyInFavorites">
                <v-icon>favorite</v-icon>
              </v-btn>

              <v-btn icon @click="loadNewDog">
                <v-icon>forward</v-icon>
              </v-btn>
            </v-card-actions>
          </v-card>
          <v-container grid-list-md fluid>
            <v-layout wrap>
              <transition-group name="slide" tag="v-layout" class="wrap">
                <v-flex xs6 sm4 md2 v-for="(pet, index) in favoriteDogs" :key="pet">
                  <app-dog :dog="pet" @remove="removeFromFavorites(index)"></app-dog>
                </v-flex>
              </transition-group>
            </v-layout>
          </v-container>
        </div>
      </v-container>
    </v-content>
  </v-app>
</template>


<script>
import axios from "axios";
import Dog from "./components/Dog";

export default {
  components: {
    appDog: Dog
  },
  data() {
    return {
      currentDogLink: "",
      favoriteDogs: []
    };
  },

  methods: {
    loadNewDog() {
      this.currentDogLink = "";
      axios.get("https://dog.ceo/api/breeds/image/random").then(response => {
        this.currentDogLink = response.data.message;
      });
    },

    addToFavorites() {
      this.favoriteDogs.push(this.currentDogLink);
    },
    removeFromFavorites(index) {
      this.favoriteDogs.splice(index, 1);
    }
  },
  created() {
    this.loadNewDog();
  },
  computed: {
    isAlreadyInFavorites() {
      return this.favoriteDogs.indexOf(this.currentDogLink) > -1;
    }
  }
};
</script>

<style>
img {
  max-width: 100%;
}

h1 {
  padding-bottom: 15px;
}

.dogs-layout {
  width: 100%;
  background: #fff center repeat;
  background-image: url("https://github.com/VueVixens/projects/blob/master/petshop/images/bg3.jpg?raw=true");
}

.dogs-overlay {
  width: 100%;
  padding: 20px;
  background-color: #fff;
  display: flex;
  flex-direction: column;
  align-items: center;
}

@media (max-width: 768px) {
  .dogs-overlay {
    margin: 0;
  }
}

.dog-card {
  width: 100%;
  max-width: 600px;
}

.fade-enter-active,
.fade-leave-active {
  transition: opacity 1s ease;
}
.fade-enter,
.fade-leave-to {
  opacity: 0;
}

.slide-enter-active {
  transition: all 0.3s ease;
}
.slide-enter,
.slide-leave-to {
  transform: translateX(10px);
  opacity: 0;
}

.slide-leave-active {
  position: absolute;
}

.slide-move {
  transition: transform 0.5s;
}
</style>