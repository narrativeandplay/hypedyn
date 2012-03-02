var factlist = [];

function createFact(name, type, id) {
	newfact = new Object();
	newfact.name = name;
	newfact.type = type;
	newfact.value = null;
	
	if (id == undefined) {
		id = genID(factlist);
	}
	
	newfact.id = id;
	factlist[id] = newfact;
	
	return newfact;
}

// assume value is of the correct type
function setFact( id, value) {
	var fact = factlist[id];
	fact.value = value;
	
	factsDebug();
}

// return the html code 
function factsDebug() {
	var temp="";
	for (i in factlist) {
		temp += factlist[i].id+" "+factlist[i].name+"::  "+factlist[i].value+"<br>";
	}
	var title="<br><text> Fact Debug </text><br>";
	document.getElementById("fact_debug").innerHTML = title+"<text>"+temp+"</text>";
}

