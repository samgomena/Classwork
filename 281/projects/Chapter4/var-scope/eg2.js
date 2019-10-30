var a = 1;

function someFunction() {
    var a = 2;

    function anotherFunction() {
        console.log(a); // the output will be 2
    }

    anotherFunction();
}

someFunction();

console.log(a); // 1