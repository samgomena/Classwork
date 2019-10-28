"use strict";

const form_to_print = document.getElementById("form_to_print");

form_to_print.addEventListener("submit", target => {
    console.log(form_to_print.elements);
});