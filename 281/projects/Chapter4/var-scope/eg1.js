var agloballydefinedvariable = 'Global';

function someFunction() {
  var alocallydefinedvariable = 'Local';
  console.log(agloballydefinedvariable);   // Global
}

someFunction();
// Uncaught ReferenceError: aldv is not defined
console.log(alocallydefinedvariable); 