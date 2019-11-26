"use strict";

;(() => {

    const rndm_bt = (min, max) => {
        min = Math.ceil(min);
        max = Math.floor(max);
        return Math.floor(Math.random() * (max - min + 1)) + min;
    }

    const get_random_color = () => {
        return `#${rndm_bt(0, 255).toString(16)}${rndm_bt(0, 255).toString(16)}${rndm_bt(0, 255).toString(16)}`;
    }

    const color_btn = document.getElementById("random_color_btn");
    
    // Set random background on page load
    document.body.style.backgroundColor = get_random_color();
    
    color_btn.addEventListener("click", () => {
        document.body.style.backgroundColor = get_random_color();
    })
})();