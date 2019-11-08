"use strict";

const form = document.getElementById("form");

const name = document.getElementById("name");
const email = document.getElementById("email");
const notes = document.getElementById("notes");

form.addEventListener("submit", event => {
    // Prevent submitting
    event.preventDefault();

    console.log(
        `
        Name: ${name.value}
        Email: ${email.value}
        Notes: ${notes.value}
        `
    );
});