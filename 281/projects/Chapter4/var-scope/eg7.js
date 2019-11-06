var city = "LA";
var team = "Lakers";

function showTeam() {
  console.log(city + " " + team);
}

function showCity() {
  city = "Moscow";
  console.log(city);
}

showTeam(); // LA Lakers

showCity(); // Moscow

/*
Because in showCity (above) 
the variable "city" is not defined using "var"
city references the global variable and overwrites it
leading to the problem below :)
*/

showTeam(); // Moscow Lakers