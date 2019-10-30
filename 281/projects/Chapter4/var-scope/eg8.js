function greet(who) {
    var iterations = 0;
    return function() {
        console.log(++iterations);
        return 'Hello ' + who + '!';
    };
}

var greeting = greet('World');
console.log(typeof greeting);      //function
console.log(typeof greeting());    //string & iterations=1
console.log(greeting());           //Hello World! & iterations=2
console.log(greeting("Universe")); //Hello World! & iterations=3

