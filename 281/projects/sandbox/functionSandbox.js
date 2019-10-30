//returns true iff arr contains at least one even number
var hasOneEven = function(arr){
	//predicate -- a function that returns a boolean
//	arr.forEach(function(n){
//		return n % 2 == 0;
//	});
	for (var i=0; i<arr.length; ++i){
		if(arr[i] % 2 === 0){
			return true;
		}
	}
	return false;
}; //statement terminator because this is a variable 

var a = [1, 3, 7, 21,];


