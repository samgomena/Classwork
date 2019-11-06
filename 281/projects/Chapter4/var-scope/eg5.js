var myVariable = 'Global Scope';

function myFunction() {
  window.myVariable = 'Something Else';
}

myFunction();

console.log(myVariable); // Something Else


