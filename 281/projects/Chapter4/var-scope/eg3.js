var a = 1;

function someFunction() {
  //warning! var keyword omitted
  //strict mode will flag this
  a = 2;

  function anotherFunction() {
    console.log(a); // the output will be 2
  }
  
  anotherFunction();
}

someFunction();

console.log(a); //the output will be 2