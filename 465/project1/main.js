"use strict";

const states = {
    "default-state": {
        gradients: {},
        transitionSpeed: 10000,
    },
};

/**
 * Get a randomly generated number in the range [`min`, `max`]. I.e. inclusively.
 * 
 * @param {int} min a lower bound for the range of numbers to choose from
 * @param {int} max an upper bound for the range of numbers to choose from
 */
function get_random_between(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

+(async function fetchGradientData() {

    const gradient_json = await fetch("assets/gradients.json", 
    {
        headers : { 
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        },
    });

    const gradient_data = await gradient_json.json();

    const num_states = get_random_between(2, 10);
    const default_state = [
        // Grab random gradients from gradient array
        gradient_data[
            get_random_between(0, gradient_data.length - 1)
        ].gradient,
        gradient_data[
            get_random_between(0, gradient_data.length - 1)
        ].gradient,
        gradient_data[
            get_random_between(0, gradient_data.length - 1)
        ].gradient,
    ]

    const granimInstance = new Granim({
        element: '#canvas',
        name: 'interactive-gradient',
        elToSetClassOn: '#canvas-wrapper',
        direction: 'diagonal',
        isPausedWhenNotInView: true,
        stateTransitionSpeed: 500,
        states : {
            "default-state": {
                gradients: default_state,
                transitionSpeed: 10000
            },
        }
    });
})();
