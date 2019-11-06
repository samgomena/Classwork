"use strict";

const height_el = document.getElementById("height");
const width_el = document.getElementById("width");

(() => {
    height_el.innerText = window.innerHeight;
    width_el.innerText = window.innerWidth;

    window.addEventListener("resize", event => {
        height_el.innerText = window.innerHeight;
        width_el.innerText = window.innerWidth;
    });
})();

