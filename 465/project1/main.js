"use strict";

function get_random_between(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

;;(async function fetchGradientData() {
    // const gradient_data = await fetch("assets/gradients.json", 
    // {
    //     headers : { 
    //         'Content-Type': 'application/json',
    //         'Accept': 'application/json'
    //     },
    // }).then(data => data.json())
    // .then(gradient_json => {
    //     console.log(gradient_json);
    // });
    const gradient_json = await fetch("assets/gradients.json", 
    {
        headers : { 
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        },
    });

    const gradient_data = await gradient_json.json();

    const num_states = get_random_between(2, 10);

    console.log(gradient_data);

    const granimInstance = new Granim({
        element: '#canvas',
        name: 'interactive-gradient',
        elToSetClassOn: '.canvas-wrapper',
        direction: 'diagonal',
        isPausedWhenNotInView: true,
        stateTransitionSpeed: 500,
        states : {
            "default-state": {
                gradients: [
                    ['#B3FFAB', '#12FFF7'],
                    ['#ADD100', '#7B920A'],
                    ['#1A2980', '#26D0CE']
                ],
                transitionSpeed: 10000
            },
            "violet-state": {
                gradients: [
                    ['#9D50BB', '#6E48AA'],
                    ['#4776E6', '#8E54E9']
                ],
                transitionSpeed: 2000
            },
            "orange-state": {
                gradients: [ ['#FF4E50', '#F9D423'] ],
                loop: false
            }
        }
    });
})()

// With jQuery
$('#default-state-cta').on('click', function(event) {
    event.preventDefault();
    granimInstance.changeState('default-state');
    setClass('#default-state-cta')
});
$('#violet-state-cta').on('click', function(event) {
    event.preventDefault();
    granimInstance.changeState('violet-state');
    setClass('#violet-state-cta')
});
$('#orange-state-cta').on('click', function(event) {
    event.preventDefault();
    granimInstance.changeState('orange-state');
    setClass('#orange-state-cta')
});

function setClass(element) {
    $('.canvas-interactive-wrapper a').removeClass('active');
    $(element).addClass('active');
};