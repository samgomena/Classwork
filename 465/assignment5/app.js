"use strict";

const express = require("express");
const bodyParser = require("body-parser");

const app = express();
const port = process.env.PORT || 8080;

app.use("/", express.static(__dirname));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({extended: false}));

app.post("/submit", (req, res) => {

    res.status(200).send(`
        name: ${req.body.name}<br>
        email: ${req.body.email}<br>
        notes: ${req.body.notes}<br>
    `)
});

app.listen(port, () => {
    console.log(`open on port: ${port}.`);
});